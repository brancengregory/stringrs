use extendr_api::prelude::*;
use lazy_static::lazy_static;
use lru::LruCache;
use regex::Regex;
use std::num::NonZeroUsize;
use std::sync::{Arc, RwLock};

lazy_static! {
    // Create a thread-safe LRU cache with an RwLock
    static ref REGEX_CACHE: Arc<RwLock<LruCache<String, Regex>>> = Arc::new(RwLock::new(
        LruCache::new(NonZeroUsize::new(100).unwrap()) // Set the cache size limit to 100 entries
    ));
}

#[extendr]
pub fn r_string_detect(s: Robj, pattern: String) -> extendr_api::Result<Logicals> {
    // Try to coerce the input into a Vec<String>.
    let s_vec: Vec<String> = if let Some(vec) = s.as_str_vector() {
        vec.into_iter().map(|s| s.to_string()).collect()
    } else if let Some(list) = s.as_list() {
        // If it's a list, iterate over its elements.
        list.iter()
            .map(|(_name, val)| {
                val.as_str()
                    .ok_or_else(|| Error::Other("List element not a string".into()))
                    .map(|s| s.to_string())
            })
            .collect::<Result<Vec<String>>>()?
    } else {
        return Err(Error::Other("Expected a character vector or list".into()));
    };

    // Retrieve or compile the regex.
    let regex = {
        let cache = REGEX_CACHE.read().unwrap();
        if let Some(re) = cache.peek(&pattern) {
            re.clone()
        } else {
            drop(cache);
            let mut cache = REGEX_CACHE.write().unwrap();
            let re = Regex::new(&pattern).map_err(|e| Error::Other(e.to_string()))?;
            cache.put(pattern.clone(), re.clone());
            re
        }
    };

    // Map each string to an Rbool based on regex matching.
    let results: Vec<Rbool> = s_vec
        .iter()
        .map(|x| {
            if regex.is_match(x) {
                Rbool::true_value()
            } else {
                Rbool::false_value()
            }
        })
        .collect();

    // Convert the Vec<Rbool> into an R object.
    let robj = Robj::from(results);
    // Try to convert the Robj into Logicals.
    robj.try_into()
        .map_err(|_| Error::Other("Conversion to Logicals failed".into()))
}

// Macro to generate exports
extendr_module! {
    mod stringrs;
    fn r_string_detect;
}
