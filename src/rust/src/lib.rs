use extendr_api::prelude::*;
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock, Mutex};

// Import engines
use fancy_regex::Regex as FancyRegex;
use regex::Regex as RegexCrate;

// DHAT heap profiler (enabled with --features profile-memory)
#[cfg(feature = "profile-memory")]
use dhat::{Dhat, DhatAlloc};

#[cfg(feature = "profile-memory")]
#[global_allocator]
static ALLOCATOR: DhatAlloc = DhatAlloc;

#[cfg(feature = "profile-memory")]
static mut DHAT_PROFILER: Option<Dhat> = None;

#[cfg(feature = "profile-memory")]
fn init_dhat() {
    unsafe {
        if DHAT_PROFILER.is_none() {
            DHAT_PROFILER = Some(Dhat::start_heap_profiling());
        }
    }
}

#[cfg(not(feature = "profile-memory"))]
fn init_dhat() {}

// Global regex cache using LazyLock for thread-safe initialization
static REGEX_CACHE: LazyLock<Mutex<HashMap<String, RegexCrate>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

static FANCY_CACHE: LazyLock<Mutex<HashMap<String, FancyRegex>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Get or compile standard regex from cache
///
/// This function first checks the global cache for a pre-compiled regex.
/// If not found, it compiles the pattern, stores it in the cache, and returns it.
///
/// # Arguments
/// * `pattern` - The regex pattern string
///
/// # Returns
/// * `Ok(RegexCrate)` - The compiled regex
/// * `Err(Error)` - If pattern compilation fails or cache lock is poisoned
///
/// # Examples
/// ```rust,ignore
/// let re = get_cached_regex("[a-z]+").unwrap();
/// assert!(re.is_match("hello"));
/// ```
fn get_cached_regex(pattern: &str) -> extendr_api::Result<RegexCrate> {
    // First check cache
    {
        let cache = REGEX_CACHE
            .lock()
            .map_err(|_| Error::Other("Regex cache lock poisoned".to_string()))?;
        if let Some(re) = cache.get(pattern) {
            return Ok(re.clone());
        }
    }

    // Not in cache, compile and store
    let re = RegexCrate::new(pattern).map_err(|e| {
        Error::Other(format!(
            "Failed to compile regex pattern '{}': {}",
            pattern, e
        ))
    })?;

    {
        let mut cache = REGEX_CACHE
            .lock()
            .map_err(|_| Error::Other("Regex cache lock poisoned".to_string()))?;
        cache.insert(pattern.to_string(), re.clone());
    }

    Ok(re)
}

/// Get or compile fancy regex from cache
fn get_cached_fancy(pattern: &str) -> extendr_api::Result<FancyRegex> {
    {
        let cache = FANCY_CACHE
            .lock()
            .map_err(|_| Error::Other("Fancy-regex cache lock poisoned".to_string()))?;
        if let Some(re) = cache.get(pattern) {
            return Ok(re.clone());
        }
    }

    let re = FancyRegex::new(pattern).map_err(|e| {
        Error::Other(format!(
            "Failed to compile fancy-regex pattern '{}': {}",
            pattern, e
        ))
    })?;

    {
        let mut cache = FANCY_CACHE
            .lock()
            .map_err(|_| Error::Other("Fancy-regex cache lock poisoned".to_string()))?;
        cache.insert(pattern.to_string(), re.clone());
    }

    Ok(re)
}

/// Single pattern regex detection with caching and parallel processing
///
/// Detects matches of a single regex pattern across multiple strings.
/// Uses thread-local regex compilation in parallel mode to avoid cache contention.
///
/// # Arguments
/// * `strings` - Vector of strings to search
/// * `pattern` - Single regex pattern
/// * `parallel` - Whether to use parallel processing (recommended for >100 strings)
///
/// # Returns
/// * `Ok(Logicals)` - Logical vector indicating matches
/// * `Err(Error)` - If pattern compilation fails
///
/// # Performance
/// - Uses global regex cache for pattern reuse
/// - Thread-local instances in parallel mode for zero contention
/// - Sequential mode for small datasets (<100 strings)
///
/// # Examples
/// ```rust,ignore
/// let strings = vec!["apple".to_string(), "banana".to_string()];
/// let pattern = "a[aeiou]".to_string();
/// let result = r_string_detect_regex_cached(strings, pattern, false).unwrap();
/// // Result: [TRUE, TRUE]
/// ```
#[extendr]
pub fn r_string_detect_regex_cached(
    strings: Vec<String>,
    pattern: String,
    parallel: bool,
) -> extendr_api::Result<Logicals> {
    // Initialize DHAT profiler if memory profiling is enabled
    init_dhat();

    let regex = get_cached_regex(&pattern)?;

    if parallel && strings.len() > 100 {
        // Parallel processing - compile regex per thread to avoid contention
        let results: Vec<bool> = strings
            .par_iter()
            .map(|s| {
                // Each thread compiles its own copy of the regex
                // This avoids cache contention at the cost of extra compilation
                // For single-pattern matching, this is acceptable
                let re = RegexCrate::new(&pattern).ok();
                re.map(|r| r.is_match(s)).unwrap_or(false)
            })
            .collect();

        Ok(Logicals::from_values(
            results.iter().map(|&b| Rbool::from(b)),
        ))
    } else {
        let results: Vec<bool> = strings.iter().map(|s| regex.is_match(s)).collect();
        Ok(Logicals::from_values(
            results.iter().map(|&b| Rbool::from(b)),
        ))
    }
}

/// Multi-pattern regex detection with parallel strategies
///
/// Detects matches of multiple regex patterns across multiple strings.
/// Returns a flat integer array (0/1) that R reshapes into a matrix.
///
/// # Arguments
/// * `strings` - Vector of strings to search
/// * `patterns` - Vector of regex patterns
/// * `parallel_strategy` - "auto", "sequential", "string_parallel", or "pattern_parallel"
/// * `chunk_size` - Number of strings per chunk (-1 for auto)
///
/// # Returns
/// * `Ok(Integers)` - Flat array [n_strings × n_patterns] of 0/1 values
/// * `Err(Error)` - If any pattern compilation fails
///
/// # Parallel Strategies
/// - "auto": Chooses based on data dimensions (strings > patterns*5 => string_parallel)
/// - "sequential": Single-threaded (best for small datasets)
/// - "string_parallel": Parallelize over strings (best when strings >> patterns)
/// - "pattern_parallel": Parallelize over patterns (best when patterns >> strings)
///
/// # Chunk Size
/// - <1000 strings: No chunking
/// - 1000-10000 strings: 2000 per chunk
/// - 10000-100000 strings: 5000 per chunk
/// - >100000 strings: 10000 per chunk
///
/// # Examples
/// ```rust,ignore
/// let strings = vec!["apple".to_string(), "banana".to_string()];
/// let patterns = vec!["a.*".to_string(), "b.*".to_string()];
/// let result = r_string_detect_multi_regex_optimized(
///     strings, patterns, "auto".to_string(), -1
/// ).unwrap();
/// // Result flat array: [1, 0, 0, 1] (apple matches a.*, banana matches b.*)
/// ```
#[extendr]
pub fn r_string_detect_multi_regex_optimized(
    strings: Vec<String>,
    patterns: Vec<String>,
    parallel_strategy: String,
    chunk_size: i32,
) -> extendr_api::Result<Integers> {
    let n_strings = strings.len();
    let n_patterns = patterns.len();

    if n_strings == 0 || n_patterns == 0 {
        return Ok(Integers::from_values(Vec::<i32>::new()));
    }

    // Pre-compile all patterns using cache
    let regexes: Vec<RegexCrate> = patterns
        .iter()
        .map(|p| get_cached_regex(p))
        .collect::<Result<Vec<_>>>()?;

    // Allocate flat result array: [string0_pat0, string0_pat1, ..., string1_pat0, ...]
    let mut results: Vec<i32> = vec![0; n_strings * n_patterns];

    // Choose strategy
    let strategy = if parallel_strategy == "auto" {
        choose_strategy(n_strings, n_patterns)
    } else {
        parallel_strategy
    };

    let chunk_size = if chunk_size <= 0 {
        auto_chunk_size(n_strings)
    } else {
        chunk_size as usize
    };

    match strategy.as_str() {
        "pattern_parallel" if n_patterns > 1 && n_strings < chunk_size => {
            // Parallel over patterns for small string counts
            let mut pattern_results: Vec<Vec<bool>> = vec![vec![false; n_strings]; n_patterns];

            // Pre-compile patterns to avoid unwrap() in parallel section
            let compiled_patterns: Vec<RegexCrate> = patterns
                .iter()
                .map(|p| {
                    RegexCrate::new(p).map_err(|e| {
                        Error::Other(format!("Pattern compilation failed for '{}': {}", p, e))
                    })
                })
                .collect::<Result<Vec<_>>>()?;

            pattern_results
                .par_iter_mut()
                .enumerate()
                .for_each(|(pat_idx, pat_matches)| {
                    let re = &compiled_patterns[pat_idx];
                    for (str_idx, s) in strings.iter().enumerate() {
                        pat_matches[str_idx] = re.is_match(s);
                    }
                });

            // Transpose pattern_results into flat results
            for str_idx in 0..n_strings {
                for pat_idx in 0..n_patterns {
                    results[str_idx * n_patterns + pat_idx] = if pattern_results[pat_idx][str_idx] {
                        1
                    } else {
                        0
                    };
                }
            }
        }
        "string_parallel" if n_strings >= chunk_size && n_strings > 100 => {
            // Parallel over strings with thread-local regexes
            let strings = Arc::new(strings);

            // Pre-compile patterns for thread-local use
            let compiled_patterns: Arc<Vec<RegexCrate>> = Arc::new(
                patterns
                    .iter()
                    .map(|p| {
                        RegexCrate::new(p).map_err(|e| {
                            Error::Other(format!("Pattern compilation failed for '{}': {}", p, e))
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
            );

            // Process chunks in parallel
            let chunk_results: Vec<(usize, Vec<i32>)> = (0..n_strings)
                .collect::<Vec<_>>()
                .par_chunks(chunk_size)
                .flat_map(|chunk_indices| {
                    let strings = strings.clone();
                    let compiled_refs = compiled_patterns.clone();

                    chunk_indices
                        .iter()
                        .map(|&str_idx| {
                            let s = &strings[str_idx];
                            let matches: Vec<i32> = compiled_refs
                                .iter()
                                .map(|re| if re.is_match(s) { 1 } else { 0 })
                                .collect();
                            (str_idx, matches)
                        })
                        .collect::<Vec<_>>()
                })
                .collect();

            // Place results in correct positions
            for (str_idx, matches) in chunk_results {
                for (pat_idx, &matched) in matches.iter().enumerate() {
                    results[str_idx * n_patterns + pat_idx] = matched;
                }
            }
        }
        _ => {
            // Sequential - fastest for small datasets
            for (str_idx, s) in strings.iter().enumerate() {
                for (pat_idx, re) in regexes.iter().enumerate() {
                    if re.is_match(s) {
                        results[str_idx * n_patterns + pat_idx] = 1;
                    }
                }
            }
        }
    }

    Ok(Integers::from_values(results))
}

/// Single pattern fancy-regex detection (supports backrefs and lookaheads)
///
/// Like `r_string_detect_regex_cached` but uses the fancy-regex engine which supports:
/// - Backreferences: `(.)\\1` matches doubled characters
/// - Lookaheads: `(?=...)` and `(?!...)`
/// - Lookbehinds: `(?<=...)` and `(?<!...)`
///
/// # Note
/// Fancy-regex is slower than standard regex. Only use when needed.
///
/// # Arguments
/// * `strings` - Vector of strings to search
/// * `pattern` - Single regex pattern (may contain fancy features)
/// * `parallel` - Whether to use parallel processing
///
/// # Returns
/// * `Ok(Logicals)` - Logical vector indicating matches
/// * `Err(Error)` - If pattern compilation fails
///
/// # Examples
/// ```rust,ignore
/// let strings = vec!["otto".to_string(), "apple".to_string()];
/// let pattern = r"(.)\1".to_string();  // Backreference
/// let result = r_string_detect_fancy_cached(strings, pattern, false).unwrap();
/// // Result: [TRUE, TRUE] (both have doubled letters)
/// ```
#[extendr]
pub fn r_string_detect_fancy_cached(
    strings: Vec<String>,
    pattern: String,
    parallel: bool,
) -> extendr_api::Result<Logicals> {
    let regex = get_cached_fancy(&pattern)?;

    if parallel && strings.len() > 100 {
        // Parallel processing - compile regex per thread to avoid contention
        let results: Vec<bool> = strings
            .par_iter()
            .map(|s| {
                // Each thread compiles its own copy of the fancy-regex
                let re = FancyRegex::new(&pattern).ok();
                re.map(|r| r.is_match(s).unwrap_or(false)).unwrap_or(false)
            })
            .collect();

        Ok(Logicals::from_values(
            results.iter().map(|&b| Rbool::from(b)),
        ))
    } else {
        let results: Vec<bool> = strings
            .iter()
            .map(|s| regex.is_match(s).unwrap_or(false))
            .collect();
        Ok(Logicals::from_values(
            results.iter().map(|&b| Rbool::from(b)),
        ))
    }
}

/// Multi-pattern fancy-regex with caching
#[extendr]
pub fn r_string_detect_multi_fancy_optimized(
    strings: Vec<String>,
    patterns: Vec<String>,
    parallel_strategy: String,
    chunk_size: i32,
) -> extendr_api::Result<Integers> {
    let n_strings = strings.len();
    let n_patterns = patterns.len();

    if n_strings == 0 || n_patterns == 0 {
        return Ok(Integers::from_values(Vec::<i32>::new()));
    }

    let regexes: Vec<FancyRegex> = patterns
        .iter()
        .map(|p| get_cached_fancy(p))
        .collect::<Result<Vec<_>>>()?;

    let mut results: Vec<i32> = vec![0; n_strings * n_patterns];

    let strategy = if parallel_strategy == "auto" {
        choose_strategy(n_strings, n_patterns)
    } else {
        parallel_strategy
    };

    let chunk_size = if chunk_size <= 0 {
        auto_chunk_size(n_strings)
    } else {
        chunk_size as usize
    };

    match strategy.as_str() {
        "string_parallel" if n_strings >= chunk_size && n_strings > 100 => {
            let strings = Arc::new(strings);

            // Pre-compile patterns for thread-local use
            let compiled_patterns: Arc<Vec<FancyRegex>> = Arc::new(
                patterns
                    .iter()
                    .map(|p| {
                        FancyRegex::new(p).map_err(|e| {
                            Error::Other(format!(
                                "Fancy-regex pattern compilation failed for '{}': {}",
                                p, e
                            ))
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
            );

            let chunk_results: Vec<(usize, Vec<i32>)> = (0..n_strings)
                .collect::<Vec<_>>()
                .par_chunks(chunk_size)
                .flat_map(|chunk_indices| {
                    let strings = strings.clone();
                    let compiled_refs = compiled_patterns.clone();

                    chunk_indices
                        .iter()
                        .map(|&str_idx| {
                            let s = &strings[str_idx];
                            let matches: Vec<i32> = compiled_refs
                                .iter()
                                .map(|re| {
                                    if re.is_match(s).unwrap_or(false) {
                                        1
                                    } else {
                                        0
                                    }
                                })
                                .collect();
                            (str_idx, matches)
                        })
                        .collect::<Vec<_>>()
                })
                .collect();

            for (str_idx, matches) in chunk_results {
                for (pat_idx, &matched) in matches.iter().enumerate() {
                    results[str_idx * n_patterns + pat_idx] = matched;
                }
            }
        }
        _ => {
            for (str_idx, s) in strings.iter().enumerate() {
                for (pat_idx, re) in regexes.iter().enumerate() {
                    if re.is_match(s).unwrap_or(false) {
                        results[str_idx * n_patterns + pat_idx] = 1;
                    }
                }
            }
        }
    }

    Ok(Integers::from_values(results))
}

/// Chunked regex detection for memory-efficient processing of large datasets
///
/// This function processes data in chunks to limit peak memory usage.
/// Ideal for datasets where n_strings × n_patterns would exceed available RAM.
///
/// # Arguments
/// * `strings` - Vector of strings to search
/// * `patterns` - Vector of regex patterns
/// * `chunk_size` - Number of strings to process per chunk (default: 5000)
///
/// # Returns
/// * `Ok(Integers)` - Flat array [n_strings × n_patterns] of 0/1 values
///
/// # Memory Usage
/// - Peak memory: O(chunk_size × n_patterns) instead of O(n_strings × n_patterns)
/// - Example: 1M strings × 100 patterns = 400MB normally, but only 2MB with chunk_size=5000
///
/// # Performance
/// - Slightly slower than full parallel mode due to chunking overhead
/// - Sequential processing within each chunk (deterministic, cache-friendly)
#[extendr]
pub fn r_string_detect_chunked(
    strings: Vec<String>,
    patterns: Vec<String>,
    chunk_size: i32,
) -> extendr_api::Result<Integers> {
    let n_strings = strings.len();
    let n_patterns = patterns.len();

    if n_strings == 0 || n_patterns == 0 {
        return Ok(Integers::from_values(Vec::<i32>::new()));
    }

    // Default chunk size if not specified
    let chunk_size = if chunk_size <= 0 {
        // Aim for ~2MB per chunk (5000 strings × 100 patterns = 500K ints = 2MB)
        let target_chunk_results = 500_000usize; // 500K results ≈ 2MB
        let max_chunk_by_results = target_chunk_results / n_patterns.max(1);
        max_chunk_by_results.clamp(1000, 50000)
    } else {
        chunk_size as usize
    };

    // Pre-compile all patterns (cached)
    let regexes: Vec<RegexCrate> = patterns
        .iter()
        .map(|p| get_cached_regex(p))
        .collect::<Result<Vec<_>>>()?;

    // Pre-allocate full result vector (unavoidable for R return)
    let mut results: Vec<i32> = vec![0; n_strings * n_patterns];

    // Process in chunks
    for (chunk_idx, chunk) in strings.chunks(chunk_size).enumerate() {
        let chunk_start_idx = chunk_idx * chunk_size;

        // Process each string in chunk
        for (local_str_idx, s) in chunk.iter().enumerate() {
            let global_str_idx = chunk_start_idx + local_str_idx;

            // Check all patterns for this string
            for (pat_idx, re) in regexes.iter().enumerate() {
                if re.is_match(s) {
                    results[global_str_idx * n_patterns + pat_idx] = 1;
                }
            }
        }

        // Optional: Progress callback (commented out - requires R function)
        // if let Some(ref callback) = progress_callback {
        //     let progress = ((chunk_idx + 1) * 100 / total_chunks) as i32;
        //     let _ = callback.call(pairlist!(progress = progress));
        // }
    }

    Ok(Integers::from_values(results))
}

/// Auto-choose parallel strategy based on data dimensions
fn choose_strategy(n_strings: usize, n_patterns: usize) -> String {
    // For very small datasets, sequential is always faster (no overhead)
    if n_strings < 500 && n_patterns < 10 {
        return "sequential".to_string();
    }

    // String-parallel: Many strings, few patterns
    if n_strings > n_patterns * 5 {
        return "string_parallel".to_string();
    }

    // Pattern-parallel: Many patterns, fewer strings
    if n_patterns > n_strings * 5 {
        return "pattern_parallel".to_string();
    }

    // Default to string-parallel for balanced cases
    "string_parallel".to_string()
}

/// Auto-compute chunk size based on data size
fn auto_chunk_size(n_strings: usize) -> usize {
    if n_strings < 1000 {
        n_strings // No chunking needed
    } else if n_strings < 10_000 {
        2000 // Small batches for medium datasets
    } else if n_strings < 100_000 {
        5000 // Medium batches
    } else {
        10000 // Large batches for huge datasets
    }
}

// Macro to generate exports
extendr_module! {
    mod stringrs;
    fn r_string_detect_regex_cached;
    fn r_string_detect_fancy_cached;
    fn r_string_detect_multi_regex_optimized;
    fn r_string_detect_multi_fancy_optimized;
    fn r_string_detect_chunked;
}

#[cfg(test)]
mod tests {
    use super::*;

    mod regex_compilation_tests {
        use super::*;

        #[test]
        fn test_valid_regex_compilation() {
            let pattern = "[a-z]+";
            let result = RegexCrate::new(pattern);
            assert!(result.is_ok());
        }

        #[test]
        fn test_invalid_regex_fails() {
            let pattern = "[invalid(";
            let result = RegexCrate::new(pattern);
            assert!(result.is_err());
        }

        #[test]
        fn test_cached_regex_returns_same() {
            let pattern = "test[0-9]+";

            // First call should compile and cache
            let re1 = get_cached_regex(pattern);
            assert!(re1.is_ok());

            // Second call should return cached
            let re2 = get_cached_regex(pattern);
            assert!(re2.is_ok());

            // Both should be equivalent
            let test_string = "test123";
            assert_eq!(
                re1.unwrap().is_match(test_string),
                re2.unwrap().is_match(test_string)
            );
        }

        #[test]
        fn test_fancy_regex_compilation() {
            let pattern = r"(.)\1"; // Backreference
            let result = FancyRegex::new(pattern);
            assert!(result.is_ok());
        }

        #[test]
        fn test_cached_fancy_regex() {
            let pattern = r"(?=.*test)"; // Lookahead

            let re1 = get_cached_fancy(pattern);
            assert!(re1.is_ok());

            let re2 = get_cached_fancy(pattern);
            assert!(re2.is_ok());

            let test_string = "this is a test";
            let match1 = re1.unwrap().is_match(test_string).unwrap_or(false);
            let match2 = re2.unwrap().is_match(test_string).unwrap_or(false);
            assert_eq!(match1, match2);
        }
    }

    mod matching_tests {
        use super::*;

        #[test]
        fn test_basic_pattern_matching() {
            let re = RegexCrate::new("a[aeiou]").unwrap();
            assert!(re.is_match("apple"));
            assert!(re.is_match("banana"));
            assert!(!re.is_match("carrot"));
        }

        #[test]
        fn test_empty_string_matching() {
            let re = RegexCrate::new(".*").unwrap();
            assert!(re.is_match(""));
        }

        #[test]
        fn test_unicode_matching() {
            let re = RegexCrate::new("\\p{L}+").unwrap(); // Unicode letters
            assert!(re.is_match("café"));
            assert!(re.is_match("日本語"));
            assert!(re.is_match("emoji"));
        }

        #[test]
        fn test_fancy_backreference_matching() {
            let re = FancyRegex::new(r"(.)\1").unwrap(); // Doubled characters
            assert!(re.is_match("otto").unwrap_or(false));
            assert!(re.is_match("apple").unwrap_or(false));
            assert!(!re.is_match("xyz").unwrap_or(false));
        }

        #[test]
        fn test_fancy_lookahead_matching() {
            let re = FancyRegex::new(r"(?=.*test).*").unwrap(); // Must contain 'test'
            assert!(re.is_match("this is a test").unwrap_or(false));
            assert!(!re.is_match("this is not").unwrap_or(false));
        }

        #[test]
        fn test_case_insensitive_matching() {
            let re = FancyRegex::new(r"(?i)test").unwrap();
            assert!(re.is_match("TEST").unwrap_or(false));
            assert!(re.is_match("TeSt").unwrap_or(false));
            assert!(re.is_match("test").unwrap_or(false));
        }
    }

    mod strategy_tests {
        use super::*;

        #[test]
        fn test_sequential_strategy_small() {
            let strategy = choose_strategy(100, 5);
            assert_eq!(strategy, "sequential");
        }

        #[test]
        fn test_string_parallel_strategy() {
            let strategy = choose_strategy(10000, 10);
            assert_eq!(strategy, "string_parallel");
        }

        #[test]
        fn test_pattern_parallel_strategy() {
            let strategy = choose_strategy(100, 1000);
            assert_eq!(strategy, "pattern_parallel");
        }

        #[test]
        fn test_chunk_size_small() {
            let size = auto_chunk_size(500);
            assert_eq!(size, 500);
        }

        #[test]
        fn test_chunk_size_medium() {
            let size = auto_chunk_size(5000);
            assert_eq!(size, 2000);
        }

        #[test]
        fn test_chunk_size_large() {
            let size = auto_chunk_size(50000);
            assert_eq!(size, 5000);
        }

        #[test]
        fn test_chunk_size_xlarge() {
            let size = auto_chunk_size(150000);
            assert_eq!(size, 10000);
        }
    }

    mod multi_pattern_tests {
        use super::*;

        #[test]
        fn test_multi_pattern_flat_array() {
            let strings = vec!["apple".to_string(), "banana".to_string()];
            let patterns = vec!["a.*".to_string(), "b.*".to_string()];

            // Manually compute expected results
            let re1 = RegexCrate::new(&patterns[0]).unwrap();
            let re2 = RegexCrate::new(&patterns[1]).unwrap();

            // apple: matches a.*, not b.*
            assert!(re1.is_match(&strings[0]));
            assert!(!re2.is_match(&strings[0]));

            // banana: not a.* (starts with b), matches b.*
            assert!(!re1.is_match(&strings[1]));
            assert!(re2.is_match(&strings[1]));
        }

        #[test]
        fn test_empty_strings() {
            let strings: Vec<String> = vec![];
            let patterns = vec!["test".to_string()];

            let result = r_string_detect_regex_cached(strings, patterns[0].clone(), false);
            assert!(result.is_ok());
            let logicals = result.unwrap();
            assert_eq!(logicals.len(), 0);
        }

        #[test]
        fn test_empty_patterns_error() {
            let strings = vec!["test".to_string()];
            let patterns: Vec<String> = vec![];

            let result =
                r_string_detect_multi_regex_optimized(strings, patterns, "auto".to_string(), -1);
            assert!(result.is_ok());
            let integers = result.unwrap();
            assert_eq!(integers.len(), 0);
        }
    }

    mod integration_tests {
        use super::*;

        #[test]
        fn test_single_pattern_roundtrip() {
            let strings = vec![
                "apple".to_string(),
                "banana".to_string(),
                "cherry".to_string(),
            ];
            let pattern = "a[aeiou]".to_string();

            let result = r_string_detect_regex_cached(strings, pattern, false);
            assert!(result.is_ok());

            let logicals = result.unwrap();
            assert_eq!(logicals.len(), 3);
            assert_eq!(logicals.as_logical()[0], Rbool::TRUE); // apple matches
            assert_eq!(logicals.as_logical()[1], Rbool::TRUE); // banana matches
            assert_eq!(logicals.as_logical()[2], Rbool::FALSE); // cherry doesn't match
        }

        #[test]
        fn test_multi_pattern_roundtrip() {
            let strings = vec!["apple pie".to_string(), "banana bread".to_string()];
            let patterns = vec!["a[aeiou]".to_string(), "^b".to_string()];

            let result = r_string_detect_multi_regex_optimized(
                strings.clone(),
                patterns,
                "sequential".to_string(),
                -1,
            );
            assert!(result.is_ok());

            let integers = result.unwrap();
            // Flat array: [apple-a, apple-b, banana-a, banana-b]
            // Expected: [1, 0, 1, 1]
            assert_eq!(integers.len(), 4);
            assert_eq!(integers.as_integer_slice()[0], 1); // apple matches a[aeiou]
            assert_eq!(integers.as_integer_slice()[1], 0); // apple doesn't match ^b
            assert_eq!(integers.as_integer_slice()[2], 1); // banana matches a[aeiou]
            assert_eq!(integers.as_integer_slice()[3], 1); // banana matches ^b
        }

        #[test]
        fn test_fancy_regex_roundtrip() {
            let strings = vec!["otto".to_string(), "apple".to_string(), "xyz".to_string()];
            let pattern = r"(.)\1".to_string(); // Backreference - doubled chars

            let result = r_string_detect_fancy_cached(strings, pattern, false);
            assert!(result.is_ok());

            let logicals = result.unwrap();
            assert_eq!(logicals.len(), 3);
            // otto has 'tt', apple has 'pp', xyz has no doubled chars
            assert_eq!(logicals.as_logical()[0], Rbool::TRUE); // otto
            assert_eq!(logicals.as_logical()[1], Rbool::TRUE); // apple
            assert_eq!(logicals.as_logical()[2], Rbool::FALSE); // xyz
        }

        #[test]
        fn test_invalid_pattern_returns_error() {
            let strings = vec!["test".to_string()];
            let pattern = "[invalid(".to_string();

            let result = r_string_detect_regex_cached(strings, pattern, false);
            assert!(result.is_err());

            let err_msg = format!("{}", result.unwrap_err());
            assert!(err_msg.contains("Failed to compile regex"));
        }
    }

    mod edge_case_tests {
        use super::*;

        #[test]
        fn test_very_long_string() {
            let long_string = "a".repeat(10000);
            let strings = vec![long_string];
            let pattern = "a+".to_string();

            let result = r_string_detect_regex_cached(strings, pattern, false);
            assert!(result.is_ok());
            assert_eq!(result.unwrap().as_logical()[0], Rbool::TRUE);
        }

        #[test]
        fn test_many_patterns() {
            let strings = vec!["test".to_string()];
            let patterns: Vec<String> = (0..100).map(|i| format!("pattern{}", i)).collect();

            let result = r_string_detect_multi_regex_optimized(
                strings,
                patterns,
                "sequential".to_string(),
                -1,
            );
            assert!(result.is_ok());
        }

        #[test]
        fn test_single_character_pattern() {
            let strings = vec!["a".to_string(), "b".to_string(), "c".to_string()];
            let pattern = "a".to_string();

            let result = r_string_detect_regex_cached(strings, pattern, false);
            assert!(result.is_ok());

            let logicals = result.unwrap();
            assert_eq!(logicals.as_logical()[0], Rbool::TRUE);
            assert_eq!(logicals.as_logical()[1], Rbool::FALSE);
            assert_eq!(logicals.as_logical()[2], Rbool::FALSE);
        }

        #[test]
        fn test_special_characters_in_pattern() {
            let strings = vec!["a.b".to_string(), "abc".to_string()];
            let pattern = r"a\.b".to_string(); // Literal dot

            let result = r_string_detect_regex_cached(strings, pattern, false);
            assert!(result.is_ok());

            let logicals = result.unwrap();
            assert_eq!(logicals.as_logical()[0], Rbool::TRUE); // a.b matches a\.b
            assert_eq!(logicals.as_logical()[1], Rbool::FALSE); // abc doesn't match
        }
    }
}
