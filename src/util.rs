use std::collections::LinkedList;

pub trait Insertable {
    fn insert(&mut self, at: usize, val: Self::Item);
    type Item;
}

impl<T> Insertable for LinkedList<T> {
    fn insert(&mut self, at: usize, val: Self::Item) {
        let mut remain = self.split_off(at);
        self.push_back(val);
        self.append(&mut remain);
    }
    type Item = T;
}

#[test]
fn test_insertable() {
    let mut list = LinkedList::new();
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    list.insert(1, 4);
    assert_eq!(list.pop_front().unwrap(), 1);
    assert_eq!(list.pop_front().unwrap(), 4);
    assert_eq!(list.pop_front().unwrap(), 2);
    assert_eq!(list.pop_front().unwrap(), 3);
}
