use std::collections::HashMap;
use std::ops::Deref;
use std::sync::{Arc, Condvar, Mutex, RwLock};

struct Inner<T>
where
    T: Send + Clone,
{
    value: Mutex<Option<T>>,
    waker: Condvar,
}

/// Handle to a complete an associated promise
pub struct Completer<T>
where
    T: Send + Clone,
{
    inner: Arc<Inner<T>>,
}

impl<T> Completer<T>
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
pub fn promise<T>() -> (Completer<T>, Promise<T>)
where
    T: Send + Clone,
{
    let inner = Arc::new(Inner {
        value: Mutex::new(None),
        waker: Condvar::new(),
    });

    (
        Completer {
            inner: inner.clone(),
        },
        Promise { inner },
    )
}

/// Create an immediately completed promise
pub fn completed<T>(value: T) -> Promise<T>
where
    T: Send + Clone,
{
    Promise {
        inner: {
            Arc::new(Inner {
                value: Mutex::new(Some(value)),
                waker: Condvar::new(),
            })
        },
    }
}

/// Concurrent map of keys to values where each key is only calculated once
pub struct PromiseMap<K, V>
where
    K: std::hash::Hash,
    V: Send + Clone,
{
    promises: RwLock<HashMap<K, Promise<V>>>,
}

impl<K, V> PromiseMap<K, V>
where
    K: std::hash::Hash + Eq,
    V: Send + Clone,
{
    /// Creates a new empty `PromiseMap`
    pub fn new() -> Self {
        PromiseMap {
            promises: RwLock::new(HashMap::new()),
        }
    }

    /// Inserts a completed value directly in to a promise map
    ///
    /// This bypasses locking by using a mutable self.
    pub fn insert(&mut self, key: K, value: V) {
        let promises = self.promises.get_mut().unwrap();
        promises.insert(key, completed(value));
    }

    /// Fetches the value from the promise map or inserts it if it does not exist
    ///
    /// Each key will only be calculated once. If a calculation is already in progress on another
    /// thread the current thread will block until the existing calculation completes.
    pub fn get_or_insert_with<F>(&self, key: K, func: F) -> V
    where
        F: FnOnce() -> V,
    {
        // Opportunistically try to fetch the promise with a read lock
        let promises_read = self.promises.read().unwrap();
        if let Some(promise) = promises_read.get(&key) {
            let cloned_promise = promise.clone();
            drop(promises_read);

            return cloned_promise.value();
        }

        drop(promises_read);

        // Try again with a write lock to ensure another thread didn't already insert
        let mut promises_write = self.promises.write().unwrap();
        if let Some(promise) = promises_write.get(&key) {
            let cloned_promise = promise.clone();
            drop(promises_write);

            return cloned_promise.value();
        }

        let (completer, promise) = promise();
        promises_write.insert(key, promise);
        drop(promises_write);

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
