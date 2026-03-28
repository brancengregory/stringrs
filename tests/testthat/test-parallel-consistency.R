test_that("all parallel strategies return identical results", {
  set.seed(42)
  
  # Test various data sizes
  test_cases <- list(
    list(n = 100, patterns = 2, name = "small"),
    list(n = 1000, patterns = 5, name = "medium"),
    list(n = 5000, patterns = 10, name = "large")
  )
  
  for (case in test_cases) {
    strings <- replicate(case$n, paste(sample(letters, 20, replace = TRUE), collapse = ""))
    patterns <- replicate(case$patterns, paste(sample(letters, 3), collapse = ""))
    
    # Run with different strategies
    result_seq <- string_detect(strings, patterns, parallel = "sequential")
    result_string <- string_detect(strings, patterns, parallel = "string_parallel")
    result_pattern <- string_detect(strings, patterns, parallel = "pattern_parallel")
    result_auto <- string_detect(strings, patterns, parallel = "auto")
    
    # All should produce identical results
    expect_equal(result_seq, result_string, 
                 info = sprintf("Sequential vs string_parallel mismatch for %s", case$name))
    expect_equal(result_seq, result_pattern,
                 info = sprintf("Sequential vs pattern_parallel mismatch for %s", case$name))
    expect_equal(result_seq, result_auto,
                 info = sprintf("Sequential vs auto mismatch for %s", case$name))
  }
})

test_that("thread-safe cache doesn't corrupt data", {
  set.seed(123)
  
  # Run multiple parallel operations with overlapping patterns
  strings <- replicate(1000, paste(sample(letters, 30, replace = TRUE), collapse = ""))
  
  # First run with some patterns
  patterns_1 <- c("a[aeiou]", "^b", "[aeiou]{2}")
  result_1 <- string_detect(strings, patterns_1, parallel = "string_parallel")
  
  # Second run with overlapping patterns
  patterns_2 <- c("a[aeiou]", "t$", "^c")  # a[aeiou] is repeated
  result_2 <- string_detect(strings, patterns_2, parallel = "pattern_parallel")
  
  # Third run with original patterns (should use cache)
  result_3 <- string_detect(strings, patterns_1, parallel = "auto")
  
  # Results should be consistent
  expect_equal(result_1, result_3,
               info = "Cached results should match original")
})

test_that("parallel performance scales with workers", {
  skip_if_not_installed("bench")
  skip_on_ci()  # Don't run performance tests on CI
  
  set.seed(42)
  strings <- replicate(10000, paste(sample(letters, 50, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$", "e.*e")
  
  # Benchmark different strategies
  bm <- bench::mark(
    sequential = string_detect(strings, patterns, parallel = "sequential"),
    string_parallel = string_detect(strings, patterns, parallel = "string_parallel"),
    pattern_parallel = string_detect(strings, patterns, parallel = "pattern_parallel"),
    auto = string_detect(strings, patterns, parallel = "auto"),
    iterations = 3,
    check = TRUE  # Verify results are identical
  )
  
  # At minimum, results should be identical (already verified by check=TRUE)
  expect_true(all(bm$`itr/sec` > 0))
  
  # Print results for manual inspection
  print(bm)
})

test_that("deterministic results across runs", {
  set.seed(42)
  strings <- replicate(500, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$")
  
  # Run multiple times with same data
  results <- replicate(5, {
    string_detect(strings, patterns, parallel = "string_parallel")
  }, simplify = FALSE)
  
  # All should be identical
  for (i in 2:5) {
    expect_equal(results[[1]], results[[i]],
                 info = sprintf("Run %d differs from run 1", i))
  }
})

test_that("handles edge case data sizes in parallel", {
  # Single string
  result <- string_detect("test", "e", parallel = "string_parallel")
  expect_equal(result, TRUE)
  
  # Two strings
  result <- string_detect(c("test", "data"), "e", parallel = "string_parallel")
  expect_equal(result, c(TRUE, FALSE))
  
  # Single pattern
  result <- string_detect(c("test", "data"), "e", parallel = "pattern_parallel")
  expect_equal(result, c(TRUE, FALSE))
  
  # Many patterns, few strings (pattern_parallel should be optimal)
  strings <- c("abcdefghij")
  patterns <- replicate(100, paste(sample(letters, 2), collapse = ""))
  
  result <- string_detect(strings, patterns, parallel = "pattern_parallel")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 101)  # string + 100 patterns
})

test_that("error handling works in parallel context", {
  strings <- c("test", "data")
  invalid_pattern <- "[invalid("
  
  # Should error in parallel context too
  expect_error(
    string_detect(strings, invalid_pattern, parallel = "string_parallel"),
    "compilation failed"
  )
  
  expect_error(
    string_detect(strings, invalid_pattern, parallel = "pattern_parallel"),
    "compilation failed"
  )
})
