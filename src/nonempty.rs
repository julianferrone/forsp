use std::vec::Vec;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NonEmpty<T> {
    first: T,
    rest: Vec<T>
}

impl<T> NonEmpty<T> {
    fn singleton(first: T) -> NonEmpty<T> {
        NonEmpty {
            first: first,
            rest: vec![]
        }
    }

    fn push(&mut self, value: T) {
        self.rest.push(value)
    }

    fn pop(&mut self) -> Option<T> {
        self.rest.pop()
    }
}

impl<T: Clone> NonEmpty<T> {
    fn pop_or_first(&mut self) -> T {
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
