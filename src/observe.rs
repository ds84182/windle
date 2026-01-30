use std::{
  cell::{Cell, OnceCell},
  rc::{Rc, Weak},
};

pub(crate) type ObserverFn<'a, T> = dyn Fn(&T, &T) + 'a;

pub(crate) struct Observable<T> {
  value: T,
  observers: OnceCell<Rc<ObserverList<T>>>,
}

pub(crate) struct Observer<T> {
  ptr: Rc<ObserverData<T>>,
}

impl<T> Observable<T> {
  pub(crate) const fn new(initial: T) -> Self {
    Self {
      value: initial,
      observers: OnceCell::new(),
    }
  }

  pub(crate) fn get(&self) -> &T {
    &self.value
  }

  pub(crate) fn set(&mut self, new_value: T) {
    let old_value = std::mem::replace(&mut self.value, new_value);
    let Some(observers) = self.observers.get() else {
      return;
    };
    let mut maybe_cur = observers.head();
    while let Some(cur) = maybe_cur {
      (cur.func)(&old_value, &self.value);
      maybe_cur = cur.next();
    }
  }

  pub(crate) fn set_if_changed(&mut self, new_value: T) -> bool
  where
    T: PartialEq,
  {
    if new_value == self.value {
      false
    } else {
      self.set(new_value);
      true
    }
  }

  pub(crate) fn update(&mut self, f: impl FnOnce(&T) -> T) {
    self.set(f(&self.value))
  }

  pub(crate) fn update_if_changed(&mut self, f: impl FnOnce(&T) -> T)
  where
    T: PartialEq,
  {
    self.set_if_changed(f(&self.value));
  }

  pub(crate) fn observe(&self, f: impl Fn(&T, &T) + 'static) -> Observer<T> {
    let observer = Observer {
      ptr: Rc::new(ObserverData {
        prev: Cell::new(ObserverLink::NIL),
        next: Cell::new(ObserverLink::NIL),
        func: f,
      }),
    };

    self
      .observers
      .get_or_init(|| ObserverList::new())
      .push(&observer.ptr);

    observer
  }
}

enum ObserverLink<T> {
  Observer(Weak<ObserverData<T>>),
  ListExtrema(Weak<ObserverList<T>>),
}

impl<T> ObserverLink<T> {
  const NIL: Self = ObserverLink::ListExtrema(Weak::new());
}

struct ObserverData<T, F: ?Sized = ObserverFn<'static, T>> {
  prev: Cell<ObserverLink<T>>,
  next: Cell<ObserverLink<T>>,
  func: F,
}

impl<T, F: ?Sized> ObserverData<T, F> {
  fn next(&self) -> Option<Rc<ObserverData<T>>> {
    let next = self.next.replace(ObserverLink::NIL);
    let res = match &next {
      ObserverLink::Observer(observer) => observer.upgrade(),
      ObserverLink::ListExtrema(_) => None,
    };
    self.next.set(next);
    res
  }
}

impl<T, F: ?Sized> Drop for ObserverData<T, F> {
  fn drop(&mut self) {
    // Remove from observer list to avoid breaking linkages to future observers.

    let prev_weak = self.prev.replace(ObserverLink::NIL);
    let next_weak = self.next.replace(ObserverLink::NIL);

    let prev_observer;
    let prev_list;

    // Update previous' next ptr or list head.
    let prev_next = match &prev_weak {
      ObserverLink::Observer(prev_weak) => {
        prev_observer = prev_weak.upgrade();
        prev_observer.as_ref().map(|prev| &prev.next)
      }
      ObserverLink::ListExtrema(list_weak) => {
        prev_list = list_weak.upgrade();
        prev_list.as_ref().map(|list| &list.head)
      }
    };

    let next_observer;
    let next_list;

    // Update next's previous ptr or list tail.
    let next_prev = match &next_weak {
      ObserverLink::Observer(next_weak) => {
        next_observer = next_weak.upgrade();
        next_observer.as_ref().map(|next| &next.prev)
      }
      ObserverLink::ListExtrema(list_weak) => {
        next_list = list_weak.upgrade();
        next_list.as_ref().map(|list| &list.tail)
      }
    };

    if let Some(prev_next) = prev_next {
      prev_next.set(next_weak);
    }

    if let Some(next_prev) = next_prev {
      next_prev.set(prev_weak);
    }
  }
}

struct ObserverList<T> {
  head: Cell<ObserverLink<T>>,
  tail: Cell<ObserverLink<T>>,
}

impl<T> ObserverList<T> {
  fn new() -> Rc<ObserverList<T>> {
    Rc::new_cyclic(|this| ObserverList {
      head: Cell::new(ObserverLink::ListExtrema(this.clone())),
      tail: Cell::new(ObserverLink::ListExtrema(this.clone())),
    })
  }

  fn head(&self) -> Option<Rc<ObserverData<T>>> {
    let head = self.head.replace(ObserverLink::NIL);
    let res = match &head {
      ObserverLink::Observer(observer) => observer.upgrade(),
      ObserverLink::ListExtrema(_) => None,
    };
    self.head.set(head);
    res
  }

  fn push(&self, observer: &Rc<ObserverData<T>>) {
    let tail = self.tail.replace(ObserverLink::NIL);
    match tail {
      ObserverLink::ListExtrema(list) => {
        // Empty list, set head and tail to the added observer
        observer.next.set(ObserverLink::ListExtrema(list.clone()));
        observer.prev.set(ObserverLink::ListExtrema(list.clone()));
        self
          .head
          .set(ObserverLink::Observer(Rc::downgrade(&observer)));
        self
          .tail
          .set(ObserverLink::Observer(Rc::downgrade(&observer)));
      }
      ObserverLink::Observer(tail_weak) => {
        // Tail must be alive to still be present in the list
        let tail = tail_weak.upgrade().expect("Tail of observer list is gone");
        let new_next = tail
          .next
          .replace(ObserverLink::Observer(Rc::downgrade(&observer)));
        observer.prev.set(ObserverLink::Observer(tail_weak));
        observer.next.set(new_next);
        self
          .tail
          .set(ObserverLink::Observer(Rc::downgrade(&observer)));
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use std::{
    cell::{Cell, OnceCell},
    rc::Rc,
  };

  use crate::observe::Observable;

  #[test]
  fn test_observe() {
    let mut observable = Observable::new(0);

    observable.update(|x| x + 1);
    assert_eq!(*observable.get(), 1);

    let observer1_result = Rc::new(OnceCell::new());
    let observer1 = observable.observe({
      let observer1_result = observer1_result.clone();
      move |old, new| {
        observer1_result.set((*old, *new)).unwrap();
      }
    });

    let observer2_result = Rc::new(Cell::new((0, 0)));
    let _observer2 = observable.observe({
      let observer2_result = observer2_result.clone();
      move |old, new| {
        observer2_result.set((*old, *new));
      }
    });

    let observer3_result = Rc::new(Cell::new((0, 0)));
    let observer3 = observable.observe({
      let observer3_result = observer3_result.clone();
      move |old, new| {
        observer3_result.set((*old, *new));
      }
    });

    observable.update(|x| x + 1);
    assert_eq!(observer1_result.get(), Some(&(1, 2)));
    assert_eq!(observer2_result.get(), (1, 2));
    assert_eq!(observer3_result.get(), (1, 2));

    drop(observer1);
    // Should not panic. observer1 uses OnceCell and panics on double initialization.
    observable.update(|x| x + 1);
    assert_eq!(observer2_result.get(), (2, 3));
    assert_eq!(observer3_result.get(), (2, 3));

    let observer4_result = Rc::new(Cell::new((0, 0)));
    let _observer4 = observable.observe({
      let observer4_result = observer4_result.clone();
      move |old, new| {
        observer4_result.set((*old, *new));
      }
    });

    // Drop an observer in the middle of the list, which should retain the link to observer4.
    drop(observer3);
    observable.update(|x| x + 1);
    assert_eq!(observer2_result.get(), (3, 4));
    assert_eq!(observer3_result.get(), (2, 3));
    assert_eq!(observer4_result.get(), (3, 4));

    // TODO: Tests for set_if_changed and update_if_changed
  }
}
