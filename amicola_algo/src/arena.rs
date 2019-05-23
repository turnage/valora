use std::collections::HashMap;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(usize);

pub struct Arena<T> {
    store: HashMap<ItemId, T>,
    id_counter: usize,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            id_counter: 0,
        }
    }

    pub fn insert(&mut self, v: T) -> ItemId {
        let id = ItemId(self.id_counter);
        self.store.insert(id, v);
        self.id_counter += 1;
        id
    }

    pub fn with_element

    pub fn get_mut(&mut self, id: ItemId) -> Option<&mut T> {
        self.store.get_mut(&id)
    }
}