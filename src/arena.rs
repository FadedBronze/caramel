use std::{alloc::{alloc, dealloc, Layout}, cell::RefCell};

pub type Challenger<'arena, T> = &'arena mut T;

pub struct Arena {
    layout: Layout,
    start: *mut u8,
    offset: RefCell<usize>,
}

impl<'arena> Arena {
    pub fn new(bytes: usize) -> Self {
        let layout = Layout::from_size_align(bytes, std::mem::align_of::<u8>()).unwrap();

        let memory = unsafe {
            alloc(layout)
        };

        Arena { offset: RefCell::new(0), start: memory, layout }
    }
    pub fn save<T: Sized>(&self, value: T) -> Challenger<'arena, T> {
        let size = std::mem::size_of::<T>();
        let mut offset = self.offset.borrow_mut();

        assert!(*offset + size <= self.layout.size());

        unsafe {
            let start = (self.start as *mut T).add(*offset);
            *start = value;
            *offset += size;
            &mut* start
        }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start, self.layout);
        }
    }
}

#[cfg(test)]
mod test {
    use super::Arena;

    #[test]
    fn test_alloc() {
        let arena = Arena::new(300);
        let x = arena.save("Hello World");
        let z = arena.save("Hello Word!");
        let w = arena.save("Hello Wold!");
        assert_eq!(&"Hello Word!", z);
        assert_eq!(&"Hello Wold!", w);
        assert_eq!(&"Hello World", x);
    } 
}
