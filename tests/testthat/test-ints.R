

int3 <- c(
  F, F, F, 
  F, F, T, 
  F, T, F, 
  F, T, T, 
  T, F, F, 
  T, F, T, 
  T, T, F, 
  T, T, T
)




test_that("read/write unsigned integers works", {

  bs <- bs_open(raw(), 'w')
  bs_write_uint(bs, x = 0:7, nbits = 3)
  expect_true(bs_is_aligned(bs, nbits = 8))
  bs_flush(bs)
  bs$bit_count
  raw_vec <- bs_close(bs)
  raw_vec
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read as bits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw_vec, 'r')
  bits <- bs_read_bit(bs, 24)
  bs_close(bs)

  expect_identical(bits, int3)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read as 3-bit integers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw_vec, 'r')
  ints <- bs_read_uint(bs, 3, 8)
  bs_close(bs)

  expect_identical(ints, 0:7)
 })
