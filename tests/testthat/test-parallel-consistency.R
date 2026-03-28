test_that("parallel processing returns correct results", {
  set.seed(42)
  
  # Test various data sizes - parallel strategy is auto-detected
  test_cases <- list(
    list(n = 100, patterns = 2, name = "small"),
    list(n = 1000, patterns = 5, name = "medium"),
    list(n = 5000, patterns = 10, name = "large")
  )
  
  for (case in test_cases) {
    strings <- replicate(case$n, paste(sample(letters, 20, replace = TRUE), collapse = ""))
    patterns <- replicate(case$patterns, paste(sample(letters, 3), collapse = ""))
    
    # Single call - parallel strategy auto-detected
    result <- string_detect(strings, patterns)
    
    # Should return wide tibble with correct dimensions
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), case$n)
    expect_equal(ncol(result), case$patterns + 1)  # string + patterns
  }
})

test_that("thread-safe cache doesn't corrupt data", {
  set.seed(123)
  
  # Run multiple parallel operations with overlapping patterns
  strings <- replicate(1000, paste(sample(letters, 30, replace = TRUE), collapse = ""))
  
  # First run with some patterns
  patterns_1 <- c("a[aeiou]", "^b", "[aeiou]{2}")
  result_1 <- string_detect(strings, patterns_1)
  
  # Second run with overlapping patterns
  patterns_2 <- c("a[aeiou]", "t$", "^c")  # a[aeiou] is repeated
  result_2 <- string_detect(strings, patterns_2)
  
  # Third run with original patterns (should use cache)
  result_3 <- string_detect(strings, patterns_1)
  
  # Results should be consistent
  expect_equal(result_1, result_3,
               info = "Cached results should match original")
})

test_that("parallel processing performance", {
  skip_if_not_installed("bench")
  skip_on_ci()  # Don't run performance tests on CI
  
  set.seed(42)
  strings <- replicate(10000, paste(sample(letters, 50, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$", "e.*e")
  
  # Benchmark - parallel strategy auto-detected
  bm <- bench::mark(
    auto_parallel = string_detect(strings, patterns),
    iterations = 3
  )
  
  # Should complete successfully
  expect_true(all(bm$`itr/sec` > 0))
  
  # Print results for manual inspection
  print(bm)
})

test_that("deterministic results across runs", {
  set.seed(42)
  strings <- replicate(500, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$")
  
  # Run multiple times with same data (parallel strategy auto-detected)
  results <- replicate(5, {
    string_detect(strings, patterns)
  }, simplify = FALSE)
  
  # All should be identical
  for (i in 2:5) {
    expect_equal(results[[1]], results[[i]],
                 info = sprintf("Run %d differs from run 1", i))
  }
})

test_that("handles edge case data sizes", {
  # Single string
  result <- string_detect("test", "e")
  expect_equal(result, TRUE)
  
  # Two strings
  result <- string_detect(c("test", "data"), "e")
  expect_equal(result, c(TRUE, FALSE))
  
  # Many patterns, few strings
  strings <- c("abcdefghij")
  patterns <- replicate(100, paste(sample(letters, 2), collapse = ""))
  
  result <- string_detect(strings, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 101)  # string + 100 patterns
})

test_that("error handling works", {
  strings <- c("test", "data")
  invalid_pattern <- "[invalid("
  
  # Should error for invalid pattern (panics in Rust, caught by extendr)
  expect_error(
    string_detect(strings, invalid_pattern)
  )
})
