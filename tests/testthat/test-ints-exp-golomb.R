



test_that("read/write unsigned integers as exp golomb works", {

  set.seed(1)
  xorig <- sample(100, 20)
  
  bs <- bs_open(raw(), 'w')
  bs_write_uint_exp_golomb(bs, x = xorig)
  bs_align(bs)
  bs_flush(bs)
  bs$bit_count
  raw_vec <- bs_close(bs)
  raw_vec
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read integers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw_vec, 'r')
  ints <- bs_read_uint_exp_golomb(bs, length(xorig))
  bs_close(bs)

  expect_identical(ints, xorig)
 })
