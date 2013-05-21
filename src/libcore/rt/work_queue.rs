// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use container::Container;
use vec::MutableVector;
use unstable::intrinsics::*;
use unstable::sync::UnsafeAtomicRcBox;
use clone::Clone;
use uint;
use cast;
use vec;
use ptr;
use kinds::Owned;
use unstable::sync::AtomicUint;

/**
 * Implementation of the Chase & Lev Work-Stealing deque.
 *
 * This requires using owned pointers to data in order to ensure that the data is freed at the
 * right time and place, it also allows for some of the operations to be atomic, which would not be
 * possible if the data was bigger than a pointer.
 *
 * One key difference from Chase & Lev is that this implementation zeroes out the location in
 * the circular buffer, which also indicates a race (that was lost), so that is checked before
 * bumping up top.
 *
 * The code uses a lot of unsafe code and therefore isn't appropriate for general usage.
 *
 */

/*
 * NOTE: Use a better memory-management scheme, copying as we are now is not optimal, though the
 * fact that it is just a pointer copy does lessen the impact somewhat.
 */

pub struct WorkQueue<T> {
    priv top : AtomicUint,
    priv bottom : AtomicUint,
    priv active_buffer: ~UnsafeAtomicRcBox<WorkBuffer<T>>
}

pub struct WorkBuffer<T> {
    priv buf: ~[~T]
}

#[deriving(Eq)]
pub enum QueueResult<T> {
    Empty,
    Abort,
    Have(T)
}

/// Gets the numerical value of the actual owned pointer, instead of trying
/// to go to the header offset (and triggering a segfault)
fn owned_ptr_val<T>(a : &~T) -> uint {
    unsafe {
        let p : &uint = cast::transmute(a);
        *p
    }
}

/// Helper to unpack the buffer and make it a borrowed pointer
unsafe fn unpack_buffer<'a, T:Owned>(box: &'a UnsafeAtomicRcBox<WorkBuffer<T>>) -> &'a mut WorkBuffer<T> {
    let b = box.get();
    cast::transmute(b)
}

pub impl<T:Owned> WorkQueue<T> {
    fn new() -> WorkQueue<T> {
        static INIT_QUEUE_SIZE : uint = 64;
        WorkQueue {
            top: AtomicUint(0),
            bottom: AtomicUint(0),
            active_buffer: ~UnsafeAtomicRcBox::new(WorkBuffer::new(INIT_QUEUE_SIZE))
        }
    }

    fn push(&mut self, o:~T) {
        unsafe {
            let b = self.bottom.load();
            let t = self.top.load();

            let buf_ref = (*self.active_buffer).clone();
            let mut buf = buf_ref.get();

            let new_buf;
            let size = (b - t);
            if size >= (&*buf).len()-1 {
                let b = (&mut *buf).grow(b,t);
                new_buf = ~UnsafeAtomicRcBox::new(b);
                self.active_buffer = new_buf.clone();
                buf = new_buf.get();
            }

            rtdebug!("WorkQueue::push: pushed element");
            (&mut *buf).put(b, o);
            self.bottom.store(b+1);
        }
    }

    fn pop(&mut self) -> QueueResult<~T> {
        let b = self.bottom.load() - 1;

        let buf_ref = (*self.active_buffer).clone();
        let buf = unsafe {unpack_buffer(&buf_ref)};

        self.bottom.store(b);

        let t = self.top.load();
        let size = (b - t) as int;
        if size < 0 {
            self.bottom.store(t);
            return Empty;
        }

        let o = buf.take(b);

        if size > 0 {
            self.try_shrink(b, t);
            return Have(o);
        }

        let val = if !self.cas_top(t, t+1) {
            Empty
        } else {
            Have(o)
        };

        self.bottom.store(t+1);

        return val;
    }

    fn steal(&mut self) -> QueueResult<~T> {
        let t = self.top.load();
        let b = self.bottom.load();

        let buf_ref;
        let buf;

        unsafe {
            buf_ref = (*self.active_buffer).clone();
            buf = unpack_buffer(&buf_ref);
        }

        let size = (b - t) as int;
        if size <= 0 {
            return Empty;
        }

        let o = buf.take(t);
        // The original just uses the the cas to check if it worked,
        // but because we actually take the value, a race can also be
        // detected when we get a zero value.
        if owned_ptr_val(&o) != 0 && self.cas_top(t, t+1) {
            rtdebug!("WorkQueue::steal: stole work");
            Have(o)
        } else {
            rtdebug!("WorkQueue::steal: lost race");
            Abort
        }
    }

    fn is_empty(&self) -> bool {
        let top = self.top.load() as int;
        let bottom = self.bottom.load() as int;
        (top - bottom) <= 0
    }

    priv fn cas_top(&mut self, old:uint, new:uint) -> bool {
        self.top.cas(old, new) == old
    }

    priv fn try_shrink(&mut self, bot:uint, top:uint) {
        let size = (bot - top);

        unsafe {
            let buf_ref = (*self.active_buffer).clone();
            let buf = buf_ref.get();

            if size < ((&*buf).len()/3) { // 3 is the K from the paper, K <= 3
                let b = (*buf).shrink(bot,top);
                let new_buf = ~UnsafeAtomicRcBox::new(b);
                self.active_buffer = new_buf;
            }
        }
    }
}

pub impl<T> WorkBuffer<T> {
    fn new(size:uint) -> WorkBuffer<T> {
        // Initialize the buffer to 0
        let buf = unsafe {vec::from_fn(size, |_| init())};


        WorkBuffer { buf:buf }
    }

    /**
     * Takes the element from the buffer. This is unsafe
     * because there may not be a valid element at the location.
     */
    unsafe fn take(&mut self, idx:uint) -> ~T {
        let i = self.wrap(idx);
        // This effectively pretends that we are just
        // moving a value from some location, not moving
        // it from inside a vector
        do vec::as_mut_buf(self.buf) |p,_| {
            let p = cast::transmute(ptr::mut_offset(p, i));
            cast::transmute(atomic_xchg(p, 0))
        }
    }

    /**
     * Puts an alement in the buffer
     */
    unsafe fn put(&mut self, idx:uint, t:~T) {
        let i = self.wrap(idx);
        self.buf.unsafe_set(i, t);
    }

    /**
     * Gets the length of the buffer
     */
    fn len(&self) -> uint {
        self.buf.len()
    }

    /**
     * Doubles the size of the buffer and moves everything into the
     * new buffer, which is returned. The current buffer is now empty.
     */
    fn grow(&mut self, bot:uint, top:uint) -> WorkBuffer<T> {
        rtdebug!("WorkBuffer::grow: growing to %u", self.len() << 1);
        let mut buf = WorkBuffer::new(self.len() << 1);

        for uint::range(top, bot) |i| {
            buf.put(i, self.take(i));
        }

        buf
    }

    /**
     * Halves the size of the buffer and moves everything into the
     * new buffer, which is returned. The current buffer is now empty.
     */
    fn shrink(&mut self, bot:uint, top:uint) -> WorkBuffer<T> {
        rtdebug!("WorkBuffer::shrink: shrinking to %u", self.len() >> 1);
        let mut buf = WorkBuffer::new(self.len() >> 1);

        for uint::range(top, bot) |i| {
            buf.put(i, self.take(i));
        }

        buf
    }

    priv fn wrap(&self, i:uint) -> uint {
        let l = self.len();
        (i & (l-1))
    }
}

#[cfg(test)]
mod test {
    extern mod std;

    use super::*;
    use uint;
    use iter::*;
    use vec::*;

    #[test]
    fn workbuf() {
        let mut b = WorkBuffer::new(32);
        b.put(1, ~1u8);
        b.put(2, ~1u8);
        b.put(1, ~2u8);
        assert_eq!(b.take(1), ~2);
    }

    #[test]
    fn workbuf_grow() {
        let mut b1 = WorkBuffer::new(32);
        b1.put(1, ~1);
        b1.put(2, ~2);
        b1.put(3, ~3);
        let mut b2 = b1.grow(4,1);

        assert_eq!(b2.take(1), ~1);
        assert_eq!(b2.take(2), ~2);
        assert_eq!(b2.take(3), ~3);
    }

    #[test]
    fn work_queue_empty() {
        let q = WorkQueue::new::<uint>();
        assert!(q.is_empty());
    }

    #[test]
    fn work_queue_basic() {
        let mut q = WorkQueue::new();

        q.push(~1);
        q.push(~2);
        q.push(~3);
        q.push(~4);

        assert_eq!(q.pop(), Have(~4));
        assert_eq!(q.pop(), Have(~3));
        assert_eq!(q.pop(), Have(~2));
        assert_eq!(q.pop(), Have(~1));
        assert_eq!(q.pop(), Empty);
        assert_eq!(q.pop(), Empty);
        assert!(q.is_empty());
    }

    #[test]
    fn work_queue_grow() {
        let mut q = WorkQueue::new();

        for uint::range(0, 72) |i| {
            q.push(~i);
        }

        for uint::range_rev(72,0) |i| {
            assert_eq!(q.pop(), Have(~(i-1)));
        }

        assert_eq!(q.pop(), Empty);

    }

    #[test]
    fn work_queue_steal() {
        let mut q = WorkQueue::new();

        for uint::range(0, 72) |i| {
            q.push(~i);
        }

        for uint::range(0, 72) |i| {
            assert_eq!(q.steal(), Have(~i));
        }

        assert_eq!(q.steal(), Empty);

    }

    #[test]
    fn work_queue_concurrent_steal() {
        use rt::thread::Thread;
        use rt::test;
        use unstable::sync::UnsafeAtomicRcBox;
        use cell::Cell;

        static NUM_ITEMS : uint = (1 << 13);
        static NUM_THREADS : uint = 8;

        // Box the queue to share it between threads
        let wq = WorkQueue::new::<uint>();
        let qbox = UnsafeAtomicRcBox::new(wq);

        let mut threads : ~[Thread] = ~[];

        // Populate the queue
        let wq = qbox.get();
        for uint::range(1, NUM_ITEMS) |i| {
            unsafe {
                (*wq).push(~i);
            }
        }

        // Start the threads a-stealin'
        for NUM_THREADS.times {
            let my_box = Cell(qbox.clone());
            let t = do test::spawntask_thread {
                unsafe {
                    let my_box = my_box.take();
                    let wq = my_box.get();

                    let mut prev : uint = 0;

                    loop {
                        let stole = (&mut *wq).steal();
                        match stole {
                            Empty => break,
                            Have(i) => {
                                assert!(*i > prev);
                                prev = *i;
                                // Pretend to do work. This is ok, because
                                // we know this is the only task in the thread
                                unsafe { usleep(1000); }
                            }
                            Abort => ()
                        }
                    }
                }
            };
            threads.push(t);
        }

        // Kill the array, which makes us wait for the threads
        // to finish
        let _ = threads;

        unsafe {
            assert!((*wq).is_empty());
        }

        extern "C" {
            fn usleep(us:uint) -> int;
        }
    }

    #[bench]
    fn bench_queue_push(b: &mut std::test::BenchHarness) {
        let mut q = ~WorkQueue::new();
        do b.iter {
            q.push(~1u);
        }
    }

    #[bench]
    fn bench_queue_pop(b: &mut std::test::BenchHarness) {
        let mut q = ~WorkQueue::new();
        for (1 << 10).times {
            q.push(~1u);
        }
        do b.iter {
            q.pop();
        }
    }
}
