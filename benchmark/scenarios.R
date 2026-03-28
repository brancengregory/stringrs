#!/usr/bin/env Rscript
# Benchmark scenarios definition
#
# Scenarios define the test configurations for benchmarking.
# Each scenario specifies:
#   - name: identifier for the scenario
#   - n_strings: number of strings to test
#   - n_patterns: number of regex patterns
#   - string_length: average length of test strings

#' Full benchmark suite scenarios
SCENARIOS_FULL <- list(
  # Single pattern scaling
  list(name = "micro_single", n_strings = 100, n_patterns = 1, string_length = 50),
  list(name = "small_single", n_strings = 1000, n_patterns = 1, string_length = 100),
  list(name = "medium_single", n_strings = 10000, n_patterns = 1, string_length = 100),
  list(name = "large_single", n_strings = 100000, n_patterns = 1, string_length = 100),
  list(name = "xlarge_single", n_strings = 500000, n_patterns = 1, string_length = 100),
  
  # Multi-pattern (balanced)
  list(name = "small_multi", n_strings = 1000, n_patterns = 10, string_length = 100),
  list(name = "medium_multi", n_strings = 10000, n_patterns = 10, string_length = 100),
  list(name = "large_multi", n_strings = 100000, n_patterns = 10, string_length = 100),
  list(name = "xlarge_multi", n_strings = 500000, n_patterns = 10, string_length = 100),
  
  # Many patterns
  list(name = "many_patterns_small", n_strings = 1000, n_patterns = 100, string_length = 100),
  list(name = "many_patterns_medium", n_strings = 10000, n_patterns = 100, string_length = 100),
  list(name = "many_patterns_large", n_strings = 50000, n_patterns = 100, string_length = 100),
  
  # Few patterns, many strings (cache efficiency)
  list(name = "cache_friendly", n_strings = 100000, n_patterns = 5, string_length = 100),
  
  # String length variations
  list(name = "short_strings", n_strings = 50000, n_patterns = 10, string_length = 20),
  list(name = "medium_strings", n_strings = 50000, n_patterns = 10, string_length = 100),
  list(name = "long_strings", n_strings = 50000, n_patterns = 10, string_length = 1000),
  list(name = "very_long_strings", n_strings = 10000, n_patterns = 10, string_length = 10000),
  
  # Edge cases
  list(name = "single_string_many_patterns", n_strings = 1, n_patterns = 100, string_length = 1000),
  list(name = "many_strings_single_pattern", n_strings = 100000, n_patterns = 1, string_length = 50)
)

#' Quick test scenarios (subset for rapid testing)
SCENARIOS_QUICK <- list(
  list(name = "quick_single", n_strings = 1000, n_patterns = 1, string_length = 100),
  list(name = "quick_multi", n_strings = 1000, n_patterns = 10, string_length = 100),
  list(name = "quick_many_patterns", n_strings = 5000, n_patterns = 50, string_length = 100)
)

#' Generate synthetic test data
generate_test_data <- function(n_strings, n_patterns, string_length, seed = 42) {
  set.seed(seed)
  
  # Generate random alphanumeric strings
  chars <- c(letters, LETTERS, 0:9)
  
  strings <- replicate(n_strings, {
    paste(sample(chars, string_length, replace = TRUE), collapse = "")
  }, simplify = FALSE) %>%
    unlist() %>%
    as.character()
  
  # Generate diverse patterns
  patterns <- generate_diverse_patterns(n_patterns)
  
  list(
    strings = strings,
    patterns = patterns,
    n_strings = n_strings,
    n_patterns = n_patterns,
    string_length = string_length
  )
}

#' Generate diverse pattern types
generate_diverse_patterns <- function(n) {
  pattern_types <- list(
    # Simple literals (20%)
    function() paste(sample(letters, 3, replace = TRUE), collapse = ""),
    
    # Character classes (30%)
    function() sprintf("[%s]{%d}",
                      paste(sample(letters, 5, replace = TRUE), collapse = ""),
                      sample(2:5, 1)),
    
    # Anchored patterns (20%)
    function() paste0("^", paste(sample(letters, 3, replace = TRUE), collapse = "")),
    
    # Complex patterns (30%)
    function() sample(c(
      "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}",  # Email-like
      "https?://[^\\s]+",  # URL-like
      "\\b\\d{3}-\\d{2}-\\d{4}\\b",  # SSN-like
      "(?i)\\b\\w{5,}\\b",  # 5+ letter words (case insensitive)
      "[aeiou]{2,}",  # Vowel sequences
      "\\d{2,}\\D+\\d{2,}"  # Number-letter-number
    ), 1)
  )
  
  map_chr(1:n, function(i) {
    type_idx <- ((i - 1) %% length(pattern_types)) + 1
    pattern_types[[type_idx]]()
  })
}

#' Generate test data with specific match density
generate_test_data_with_density <- function(n_strings, pattern, target_density, 
                                            string_length = 100, seed = 42) {
  set.seed(seed)
  
  chars <- c(letters, LETTERS, 0:9)
  strings <- character(n_strings)
  matches <- 0
  attempts <- 0
  max_attempts <- n_strings * 100
  
  for (i in seq_len(n_strings)) {
    found <- FALSE
    while (!found && attempts < max_attempts) {
      s <- paste(sample(chars, string_length, replace = TRUE), collapse = "")
      is_match <- grepl(pattern, s, perl = TRUE)
      current_density <- matches / max(i - 1, 1)
      
      should_match <- current_density < target_density
      
      if (is_match == should_match || i == 1) {
        strings[i] <- s
        if (is_match) matches <- matches + 1
        found <- TRUE
      }
      attempts <- attempts + 1
    }
    
    if (!found) {
      # Fallback: just generate random string
      strings[i] <- paste(sample(chars, string_length, replace = TRUE), collapse = "")
    }
  }
  
  strings
}
