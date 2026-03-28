//! Common utilities for benchmarks
//!
//! This module provides shared test data generation and helper functions
//! used across all benchmark suites.

use rand::distributions::{Alphanumeric, DistString};
use rand::rngs::StdRng;
use rand::SeedableRng;
use regex::Regex as RegexCrate;

/// Standard benchmark sizes: (n_strings, n_patterns)
pub const BENCH_SIZES: [(usize, usize); 6] = [
    (100, 5),      // Micro
    (1_000, 10),   // Small
    (10_000, 50),  // Medium
    (100_000, 10), // Large strings, few patterns
    (10_000, 100), // Many patterns
    (50_000, 25),  // Balanced large
];

/// Thread counts for parallel scaling benchmarks
pub const THREAD_COUNTS: [usize; 5] = [1, 2, 4, 8, 16];

/// Chunk sizes for chunking benchmarks
pub const CHUNK_SIZES: [usize; 4] = [1_000, 2_000, 5_000, 10_000];

/// Generate random alphanumeric strings of specified length
pub fn generate_test_strings(count: usize, length: usize) -> Vec<String> {
    let mut rng = StdRng::seed_from_u64(42);
    (0..count)
        .map(|_| Alphanumeric.sample_string(&mut rng, length))
        .collect()
}

/// Generate realistic test patterns with varying complexity
pub fn generate_test_patterns(count: usize) -> Vec<String> {
    let base_patterns = vec![
        // Simple literals (30%)
        "abc",
        "def",
        "xyz",
        // Character classes (40%)
        "[a-z]+",
        "[A-Z]{3}",
        "[0-9]{2,4}",
        "[a-zA-Z]+",
        // Complex patterns (30%)
        r"\b\w{5,}\b",
        r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}",
        r"https?://[^\s]+",
        r"\b\d{3}-\d{2}-\d{4}\b",
    ];

    (0..count)
        .map(|i| base_patterns[i % base_patterns.len()].to_string())
        .collect()
}

/// Generate patterns with fancy-regex features
pub fn generate_fancy_patterns(count: usize) -> Vec<String> {
    let fancy_patterns = vec![
        r"(.)\1",      // Backreference
        r"(?=.*abc)",  // Lookahead
        r"(?!.*xyz)",  // Negative lookahead
        r"(\w+)\s+\1", // Word backreference
    ];

    (0..count)
        .map(|i| fancy_patterns[i % fancy_patterns.len()].to_string())
        .collect()
}

/// Pre-compile regexes for fair comparison
pub fn precompile_regexes(patterns: &[String]) -> Vec<RegexCrate> {
    patterns
        .iter()
        .map(|p| RegexCrate::new(p).expect("Invalid test pattern"))
        .collect()
}

/// Calculate throughput metric (strings per second)
pub fn calculate_throughput(n_strings: usize, elapsed_secs: f64) -> f64 {
    if elapsed_secs > 0.0 {
        n_strings as f64 / elapsed_secs
    } else {
        0.0
    }
}

/// Generate strings with specific match density
pub fn generate_strings_with_matches(
    count: usize,
    length: usize,
    pattern: &str,
    target_density: f64,
) -> Vec<String> {
    let mut rng = StdRng::seed_from_u64(42);
    let re = RegexCrate::new(pattern).expect("Invalid pattern");
    let mut strings = Vec::with_capacity(count);
    let mut matches = 0;

    while strings.len() < count {
        let s = Alphanumeric.sample_string(&mut rng, length);
        let is_match = re.is_match(&s);
        let should_match = (matches as f64 / (strings.len() as f64 + 1.0)) < target_density;

        if is_match == should_match || strings.len() == 0 {
            if is_match {
                matches += 1;
            }
            strings.push(s);
        }
    }

    strings
}

/// Benchmark configuration for parameterized tests
#[derive(Clone, Debug)]
pub struct BenchConfig {
    pub n_strings: usize,
    pub n_patterns: usize,
    pub string_length: usize,
    pub name: String,
}

impl BenchConfig {
    pub fn new(n_strings: usize, n_patterns: usize, string_length: usize) -> Self {
        let name = format!(
            "{}_strings_{}_patterns_{}_len",
            n_strings, n_patterns, string_length
        );
        Self {
            n_strings,
            n_patterns,
            string_length,
            name,
        }
    }
}

impl Default for BenchConfig {
    fn default() -> Self {
        Self::new(1_000, 10, 100)
    }
}
