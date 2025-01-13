

test_that("padding works", {
  
  orig <- c(T, F, T)
  
  expect_identical(
    pad_bits(orig),
    c(F, F, F, F, F,  T, F, T)
  )
  
  expect_identical(
    pad_bits(orig, side = 'l'),
    c(F, F, F, F, F,  T, F, T)
  )
  
  expect_identical(
    pad_bits(orig, side = 'r'),
    c(T, F, T,  F, F, F, F, F)
  )
  
  expect_identical(
    pad_bits(orig, value = TRUE),
    c(T, T, T, T, T,  T, F, T)
  )
  
  expect_identical(
    pad_bits(orig, nbits = 16),
    c(F, F, F, F, F, F, F, F, 
      F, F, F, F, F,  T, F, T)
  )
  
  
})


test_that("padding works if condition already met", {
  
  orig <- c(T, F, T, T,  T, T, T, F)
  
  expect_identical(
    pad_bits(orig),
    c(T, F, T, T,  T, T, T, F)
  )
  
  expect_identical(
    pad_bits(orig, side = 'l'),
    c(T, F, T, T,  T, T, T, F)
  )
  
  expect_identical(
    pad_bits(orig, side = 'r'),
    c(T, F, T, T,  T, T, T, F)
  )
  
  expect_identical(
    pad_bits(orig, value = TRUE),
    c(T, F, T, T,  T, T, T, F)
  )
  
  expect_identical(
    pad_bits(orig, nbits = 16),
    c(F, F, F, F, F, F, F, F, 
      T, F, T, T,  T, T, T, F)
  )
})

