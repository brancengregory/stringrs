test_that("string detect works", {
  words <- c("apple", "banana", "carrot")
  patterns <- c("a(\\w)\\1")
  expect_equal(
    string_detect(words, patterns),
    c(TRUE, FALSE, TRUE)
  )
})