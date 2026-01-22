use serde::{Deserialize, Serialize};
use std::vec::Vec;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NonEmpty<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn singleton(first: T) -> NonEmpty<T> {
        NonEmpty {
            first: first,
            rest: vec![],
        }
    }

    pub fn push(&mut self, value: T) {
        self.rest.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.rest.pop()
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.rest.last_mut().unwrap_or_else(|| &mut self.first)
    }
}

impl<T: Clone> NonEmpty<T> {
    pub fn last(&self) -> &T {
        self.rest.last().unwrap_or_else(|| &self.first)
    }

    pub fn pop_or_first(&mut self) -> T {
        self.pop().unwrap_or_else(|| self.first.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pop_singleton() {
        let mut nonempty: NonEmpty<usize> = NonEmpty::singleton(1);
        assert_eq!(nonempty.pop(), None);
        assert_eq!(nonempty.pop_or_first(), 1);
    }

    #[test]
    fn push_then_pop() {
        let mut nonempty: NonEmpty<usize> = NonEmpty::singleton(1);
        nonempty.push(2);
        assert_eq!(nonempty.pop_or_first(), 2);
        assert_eq!(nonempty.pop_or_first(), 1);
    }
}
