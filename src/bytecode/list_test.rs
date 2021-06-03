use std::ptr;

#[derive(Debug)]
struct Node<T: Ord> {
    next: Option<Box<Node<T>>>,
    value: T,
}

struct List<T: Ord> {
    head: Option<Box<Node<T>>>,
}

impl<T> List<T>
where
    T: Ord,
{
    fn insert(&mut self, value: T) -> &mut Node<T> {
        unsafe { &mut *self.insert0(value) }
    }

    fn insert0(&mut self, value: T) -> *mut Node<T> {
        let mut node = Box::new(Node { next: None, value });
        let mut prev = ptr::null_mut() as *mut Box<Node<T>>;
        let mut head = &mut self.head;
        while let Some(h) = head {
            if h.value > node.value {
                prev = h;
                head = &mut h.next;
            } else if h.value == node.value {
                return &mut **h;
            } else {
                break;
            }
        }
        let result = &mut *node as *mut _;
        if prev != ptr::null_mut() {
            let prev = unsafe { &mut *prev };
            node.next = prev.next.take();
            prev.next = Some(node);
        } else {
            self.head = Some(node);
        }
        result
    }

    fn back(&mut self) -> &mut Option<Box<Node<T>>> {
        let mut anchor = &mut self.head;
        while let Some(node) = anchor {
            anchor = &mut node.next;
        }
        anchor
    }
}

#[test]
fn insert() {
    let mut list = List { head: None };
    let result = list.insert(10);
    println!("{:?}", result);
    let result = list.insert(1);
    println!("{:?}", result);
    let result = list.insert(10);
    println!("{:?}", result);
    let result = list.insert(0);
    println!("{:?}", result);
    let mut head = list.head;
    while let Some(h) = head {
        println!("{}", h.value);
        head = h.next;
    }
}
