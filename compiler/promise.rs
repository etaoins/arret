use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, Condvar, Mutex};

struct Inner<T>
where
    T: Send + Clone,
{
    value: Mutex<Option<T>>,
    waker: Condvar,
}

/// Handle to a complete an associated promise
pub struct Complete<T>
where
    T: Send + Clone,
{
    inner: Arc<Inner<T>>,
}

impl<T> Complete<T>
where
    T: Send + Clone,
{
    /// Sets the associated promise as complete
    pub fn set(self, new_value: T) {
        let mut value_lock = self.inner.value.lock().unwrap();
        value_lock.replace(new_value);
        drop(value_lock);

        self.inner.waker.notify_all();
    }
}

/// Promise of a future computation
///
/// The Promise is initially incomplete. Once its been completed it will return clones of the
/// completed value until it's dropped.
#[derive(Clone)]
pub struct Promise<T>
where
    T: Send + Clone,
{
    inner: Arc<Inner<T>>,
}

impl<T> Promise<T>
where
    T: Send + Clone,
{
    /// Waits until the promise is complete and returns a clone of its value
    pub fn value(&self) -> T {
        let mut value_lock = self.inner.value.lock().unwrap();

        loop {
            match value_lock.deref() {
                Some(value) => break value.clone(),
                None => {
                    value_lock = self.inner.waker.wait(value_lock).unwrap();
                }
            }
        }
    }
}

/// Creates new completer and promise
pub fn promise<T>() -> (Complete<T>, Promise<T>)
where
    T: Send + Clone,
{
    let inner = Arc::new(Inner {
        value: Mutex::new(None),
        waker: Condvar::new(),
    });

    (
        Complete {
            inner: inner.clone(),
        },
        Promise { inner },
    )
}

/// Concurrent map of keys to values where each key is only calculated once
pub struct PromiseMap<K, V>
where
    K: std::hash::Hash,
    V: Send + Clone,
{
    promises: Mutex<HashMap<K, Promise<V>>>,
}

impl<K, V> PromiseMap<K, V>
where
    K: std::hash::Hash + Eq,
    V: Send + Clone,
{
    /// Creates a new empty `PromiseMap`
    pub fn new() -> Self {
        PromiseMap {
            promises: Mutex::new(HashMap::new()),
        }
    }

    /// Fetches the value from the promise map or inserts it if it does not exist
    ///
    /// Each key will only be calculated once. If a calculation is already in progress on another
    /// thread the current thread will block until the existing calculation completes.
    pub fn get_or_insert_with<F>(&self, key: K, func: F) -> V
    where
        F: FnOnce() -> V,
    {
        let mut promises_lock = self.promises.lock().unwrap();
        if let Some(promise) = promises_lock.get(&key) {
            let cloned_promise = promise.clone();
            drop(promises_lock);

            return cloned_promise.value();
        }

        let (completer, promise) = promise();
        promises_lock.insert(key, promise);
        drop(promises_lock);

        // Build a new value. This is presumably expensive
        let value = func();
        completer.set(value.clone());
        value
    }
}

impl<K, V> Default for PromiseMap<K, V>
where
    K: std::hash::Hash + Eq,
    V: Send + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
