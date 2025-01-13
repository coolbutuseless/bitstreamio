

test_that("int-to-bits scalar conversion works", {
  
  expect_identical(uint_to_bits(1, nbits = 3), c(F, F, T))
  expect_identical(uint_to_bits(2, nbits = 2), c(   T, F))
  expect_identical(uint_to_bits(3, nbits = 2), c(   T, T))
  expect_identical(uint_to_bits(4, nbits = 3), c(T, F, F))
  
  expect_identical(bits_to_uint(c(      T)), 1L)
  expect_identical(bits_to_uint(c(   T, F)), 2L)
  expect_identical(bits_to_uint(c(   T, T)), 3L)
  expect_identical(bits_to_uint(c(T, F, F)), 4L)
  
})


test_that("int-to-bits verctor conversion works", {
  expect_identical(uint_to_bits(c(1, 4), nbits = 3), c(F, F, T,  T, F, F))
  expect_identical(bits_to_uint(c(F, F, T,  T, F, F), nbits = 3), c(1L, 4L))
})


test_that("int-to-bits round trip", {
  
  xorig <- 0:7
  bits <- uint_to_bits(xorig, nbits = 3)
  xout <- bits_to_uint(bits , nbits = 3)
  expect_identical(xout, xorig)
  
  # Truncated to 3 bits
  xorig <- c(0:15)
  bits <- uint_to_bits(xorig, nbits = 3)
  xout <- bits_to_uint(bits , nbits = 3)
  expect_identical(xout, c(0:7, 0:7))
})

