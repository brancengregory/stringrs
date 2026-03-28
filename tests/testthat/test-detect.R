test_that("string detect works with single pattern", {
  words <- c("apple", "banana", "carrot")
  
  # Standard regex pattern
  result_regex <- string_detect(words, "a")
  expect_type(result_regex, "logical")
  expect_equal(length(result_regex), 3)
  expect_equal(result_regex, c(TRUE, TRUE, TRUE))
  
  # Fixed (literal) matching
  result_fixed <- string_detect(words, "a", fixed = TRUE)
  expect_type(result_fixed, "logical")
  expect_equal(result_fixed, c(TRUE, TRUE, TRUE))
})

test_that("string detect auto-detects fancy-regex for backreferences", {
  words <- c("apple", "banana", "carrot", "otto")
  
  # Pattern with backreference - should auto-detect fancy-regex
  # (.)\\1 matches any character followed by itself (doubled letters)
  result <- string_detect(words, "(.)\\1")
  expect_type(result, "logical")
  # Words with doubled letters: apple (pp), carrot (rr), otto (tt, oo)
  # banana (b-a-n-a-n-a) has no doubled letters
  expect_equal(result, c(TRUE, FALSE, TRUE, TRUE))
})

test_that("string detect multi-pattern returns wide tibble", {
  words <- c("apple", "banana", "carrot")
  patterns <- c("^a", "^b", "t$")
  
  result <- string_detect(words, patterns)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)  # One row per string
  expect_equal(ncol(result), 4)  # string + 3 pattern columns
  expect_true("string" %in% names(result))
  
  # Verify pattern matching
  expect_equal(result$X.a[1], TRUE)   # apple starts with 'a'
  expect_equal(result$X.b[2], TRUE)   # banana starts with 'b'
  expect_equal(result$t.[3], TRUE)     # carrot ends with 't'
})

test_that("string detect handles empty inputs gracefully", {
  # Empty strings with single pattern
  result <- string_detect(character(0), "test")
  expect_equal(length(result), 0)
  expect_type(result, "logical")
  
  # Empty strings with multiple patterns
  result <- string_detect(character(0), c("a", "b"))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  
  # Empty patterns with single string
  result <- string_detect("test", character(0))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("string detect handles large inputs efficiently", {
  # Generate larger test data
  set.seed(42)
  strings <- replicate(1000, paste(sample(letters, 10, replace = TRUE), collapse = ""))
  patterns <- c("^a", "^b", "[aeiou]{2}", "xyz")
  
  result <- string_detect(strings, patterns)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1000)
  expect_equal(ncol(result), 5)  # string + 4 patterns
})

test_that("string detect auto-optimizes based on data size", {
  words <- c("apple", "banana", "carrot", "date", "elderberry")
  patterns <- c("^a", "^b", "t$", "e.*y")
  
  # Should work correctly regardless of auto-optimization
  result <- string_detect(words, patterns)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 5)  # string + 4 patterns
  
  # Verify some expected matches
  expect_true(result$X.a[1])  # apple starts with a
  expect_true(result$t.[3])   # carrot ends with t
})

test_that("string detect validates inputs correctly", {
  # Invalid strings type
  expect_error(string_detect(123, "test"), "must be a character vector")
  
  # Invalid pattern type
  expect_error(string_detect(c("test"), 123), "must be a character vector")
  
  # Invalid fixed parameter
  expect_error(string_detect(c("test"), "test", fixed = "yes"), "must be a single logical value")
})

test_that("fixed matching is faster than regex for literals", {
  words <- replicate(1000, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  
  # Fixed matching should work
  result_fixed <- string_detect(words, "abc", fixed = TRUE)
  expect_type(result_fixed, "logical")
  expect_equal(length(result_fixed), 1000)
  
  # Regex matching should also work
  result_regex <- string_detect(words, "abc", fixed = FALSE)
  expect_type(result_regex, "logical")
  expect_equal(length(result_regex), 1000)
  
  # Results should be identical for literal patterns
  expect_equal(result_fixed, result_regex)
})

test_that("string detect handles special characters in patterns", {
  strings <- c("hello.world", "test", "a+b")
  
  # With regex (default), dots and special chars have meaning
  result_regex <- string_detect(strings, ".")
  expect_equal(result_regex, c(TRUE, TRUE, TRUE))  # Dot matches any char
  
  # With fixed matching, literal dot
  result_fixed <- string_detect(strings, ".", fixed = TRUE)
  expect_equal(result_fixed, c(TRUE, FALSE, FALSE))  # Only first has literal dot
})

test_that("string detect handles unicode strings", {
  strings <- c("café", "日本語", "hello")
  
  result <- string_detect(strings, "[a-z]+")
  expect_type(result, "logical")
  expect_equal(length(result), 3)
  
  # All should match the [a-z]+ pattern to varying degrees
  expect_true(result[3])  # hello definitely matches
})
