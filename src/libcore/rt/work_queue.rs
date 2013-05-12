use unstable::intrinsics::{init, atomic_cxchg, atomic_xchg};
use u64;
use cast;
use vec;
use ptr;

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
 * TODO: Use a better memory-management scheme, copying as we are now is not optimal.
 */

pub struct WorkQueue<T> {
    priv top : u64,
    priv bottom : u64,
    priv active_buffer: WorkBuffer<T>
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

pub impl<T> WorkQueue<T> {
    fn new() -> WorkQueue<T> {
        static INIT_QUEUE_SIZE : uint = 64;
        WorkQueue {
            top: 0,
            bottom: 0,
            active_buffer: WorkBuffer::new(INIT_QUEUE_SIZE)
        }
    }

    fn push(&mut self, o:~T) {
        let b = self.bottom;
        let t = self.top;

        let size = (b - t) as uint;
        if size >= self.active_buffer.len()-1 {
            self.active_buffer = self.active_buffer.grow(b,t);
        }

        self.active_buffer.put(b, o);
        self.bottom = b+1;
    }

    fn pop(&mut self) -> QueueResult<~T> {
        let b = self.bottom - 1;
        let t, o, size;

        { // scoping because borrowck
            {
                let buf = &mut self.active_buffer;

                self.bottom = b;
                t = self.top;
                size = (b - t) as int;
                if size < 0 {
                    self.bottom = t;
                    return Empty;
                }

                o = buf.take(b);

            }
            if size > 0 {
                self.try_shrink(b, t);
                return Have(o);
            }
        };

        let val = if !self.cas_top(t, t+1) {
            Empty
        } else {
            Have(o)
        };

        self.bottom = t+1;

        return val;
    }


    fn steal(&mut self) -> QueueResult<~T> {
        let t = self.top;
        let b = self.bottom;

        let size = (b - t) as int;
        if size <= 0 {
            return Empty;
        }

        let o = self.active_buffer.take(t);
        // The original just uses the the cas to check if it worked,
        // but because we actually take the value, a race can also be
        // detected when we get a zero value.
        if owned_ptr_val(&o) != 0 && self.cas_top(t, t+1) {
            Have(o)
        } else {
            Abort
        }
    }

    priv fn cas_top(&mut self, old:u64, new:u64) -> bool {
        let old = old as int;
        let new = new as int;
        unsafe {
            atomic_cxchg(cast::transmute(&mut self.top), old, new) == old
        }
    }

    priv fn try_shrink(&mut self, bot:u64, top:u64) {
        let size = (bot - top) as uint;
        if size < (self.active_buffer.len()/3) { // 3 is the K from the paper, K <= 3
            self.active_buffer = self.active_buffer.shrink(bot, top);
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
    unsafe fn take(&mut self, idx:u64) -> ~T {
        let i = self.wrap(idx);
        // This effectively pretends that we are just
        // moving a value from some location, not moving
        // it from inside a vector
        do vec::as_mut_buf(self.buf) |p,_| {
            let p = cast::transmute(ptr::mut_offset(p, i));
            cast::transmute(atomic_xchg(p, 0))
        }
    }

    unsafe fn put(&mut self, idx:u64, t:~T) {
        let i = self.wrap(idx);
        self.buf.unsafe_set(i, t);
    }

    fn len(&self) -> uint {
        self.buf.len()
    }

    fn grow(&mut self, bot:u64, top:u64) -> WorkBuffer<T> {
        debug!("Growing Buffer: %u -> %u", top as uint, bot as uint);
        let mut buf = WorkBuffer::new(self.len() << 1);
        for u64::range(top, bot) |i| {
            buf.put(i, self.take(i));
        }

        buf
    }

    fn shrink(&mut self, bot:u64, top:u64) -> WorkBuffer<T> {
        debug!("Shrinking Buffer: %u -> %u", top as uint, bot as uint);
        let mut buf = WorkBuffer::new(self.len() >> 1);
        for u64::range(top, bot) |i| {
            buf.put(i, self.take(i));
        }

        buf
    }

    priv fn wrap(&self, i:u64) -> uint {
        let l = self.len() as u64;
        (i & (l-1)) as uint
    }
}

#[cfg(test)]
mod test {
    extern mod std;

    use super::*;
    use u64;
    use comm;
    use comm::*;
    use task;
    use libc;

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
    }

    #[test]
    fn work_queue_grow() {
        let mut q = WorkQueue::new();

        for u64::range(0, 72) |i| {
            q.push(~i);
        }

        for u64::range_rev(72,0) |i| {
            assert_eq!(q.pop(), Have(~(i-1)));
        }

        assert_eq!(q.pop(), Empty);

    }

    #[test]
    fn work_queue_steal() {
        let mut q = WorkQueue::new();

        for u64::range(0, 72) |i| {
            q.push(~i);
        }

        for u64::range(0, 72) |i| {
            assert_eq!(q.steal(), Have(~i));
        }

        assert_eq!(q.steal(), Empty);

    }

    #[test] #[ignore(reason="Long test")]
    fn work_queue_concurrent() {

        /*
         * This test does some... questionable things.
         *
         * It normally passes though, so that's good
         */

        use cast;
        let mut q = ~WorkQueue::new();

        for u64::range(1,256) |i| {
            q.push(~i);
        }

        //Magic pointer casting so we can pass it to other tasks
        let ptr : *u8 = unsafe { cast::transmute::<&WorkQueue<u64>, *u8>(q) };

        let mut chans = ~[];

        for 8.times {
            let (port, chan) = comm::stream();

            let mut task = task::task();
            task.sched_mode(task::SingleThreaded);

            do task.spawn {
                port.recv(); //Sync so it doesn't just immediately stop because there aren't any
                             // objects in the queue
                unsafe { do task::atomically {
                    let q  = cast::transmute::<*u8, &mut WorkQueue<u64>>(ptr);

                    let mut prev = 0;

                    loop {
                        let stole = q.steal();
                        match stole {
                            Empty => break,
                            Have(ref a) => {
                                // Make sure we always get a bigger element
                                assert!(prev < **a);
                                prev = **a;
                            }
                            Abort => ()
                        }
                    }

                    debug!("Task Ended");
                }}
            };
            chans.push(chan);
        }

        for chans.each |c| { c.send(()); }

        for u64::range(0, (1u64 << 16)) |i| {
            q.push(~(i+256));
        }

        loop {
            match q.pop() {
                Have(_) => loop,
                _ => break
            }
        }

        debug!("Owner Done");

        // Wait for the other threads to finish, this is easier than synchronizing properly
        unsafe {
            libc::sleep(5);
        }
    }

    #[bench]
    fn bench_queue_push(b: &mut std::test::BenchHarness) {
        let mut q = ~WorkQueue::new();
        do b.iter {
            q.push(~1u64);
        }
    }

    #[bench]
    fn bench_queue_pop(b: &mut std::test::BenchHarness) {
        let mut q = ~WorkQueue::new();
        for (1 << 10).times {
            q.push(~1u64);
        }
        do b.iter {
            q.pop();
        }
    }

    fn is_empty(&self) -> bool {
        return self.queue.is_empty();
    }
}
