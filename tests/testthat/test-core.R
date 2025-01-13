
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# MSB
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("Read 0 bits returns logical(0) (MSB)", {
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, 'r')
  
  
  expect_identical(
    bs_read_bit(bs, 0), logical(0)
  )
  
  expect_equal(bs$bit_count, 0)
  
  bs_close(bs)
})



test_that("Read lots of bits works (MSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, 'r')
  
  
  expect_identical(
    bs_read_bit(bs, 16), 
    c(
      F, F, F, F,  F, F, T, T,
      T, F, F, F,  F, F, T, F
    )
  )
  
  bs_close(bs)
  
})



test_that("Read bit-at-a-time works (MSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, 'r')
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), T)
  
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), F)
  
  bs_close(bs)
})



test_that("Read a few bit-at-a-time works (MSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, 'r')
  
  expect_identical(bs_read_bit(bs, 4), c(F, F, F, F))
  expect_identical(bs_read_bit(bs, 3), c(F, F, T))
  expect_identical(bs_read_bit(bs, 2), c(T, T))
  expect_identical(bs_read_bit(bs, 4), c(F, F, F, F))
  expect_identical(bs_read_bit(bs, 3), c(F, T, F))
  
  bs_close(bs)
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# MSB
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("Read 0 bits returns logical(0) (LSB)", {
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, 'r', msb_first = FALSE)
  
  
  expect_identical(
    bs_read_bit(bs, 0), logical(0)
  )
  
  expect_equal(bs$bit_count, 0)
  
  bs_close(bs)
})



test_that("Read lots of bits works (LSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, mode = 'r', msb_first = FALSE)
  
  
  expect_identical(
    bs_read_bit(bs, 16), 
    c(
      T, T, F, F,  F, F, F, F,
      F, T, F, F,  F, F, F, T
    )
  )
  
  bs_close(bs)
})



test_that("Read bit-at-a-time works (LSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, mode = 'r', msb_first = FALSE)
  
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), T)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), F)
  expect_identical(bs_read_bit(bs, 1), T)
  
  bs_close(bs)
})



test_that("Read a few bit-at-a-time works (LSB)", {
  
  raw_vec <- as.raw(c(3, 130))
  bs <- bs_open(raw_vec, mode = 'r', msb_first = FALSE)
  
  expect_identical(bs_read_bit(bs, 4), c(T, T, F, F))
  expect_identical(bs_read_bit(bs, 3), c(F, F, F))
  expect_identical(bs_read_bit(bs, 2), c(F, F))
  expect_identical(bs_read_bit(bs, 4), c(T, F, F, F))
  expect_identical(bs_read_bit(bs, 3), c(F, F, T))
  
  bs_close(bs)
})


