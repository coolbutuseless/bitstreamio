


test_that("test one works", {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write bits to a raw vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw(), "w")
  bs_write_bit(bs, c(TRUE , FALSE, FALSE, FALSE))
  bs_write_byte(bs, c(1, 127))  # write unaligned byte values
  bs_write_bit(bs, c(FALSE, FALSE, FALSE, TRUE))
  
  raw_vec <- bs_close(bs)
  raw_vec
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read bits back from raw vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw_vec, mode = 'r')
  expect_identical(bs_read_bit (bs, 4), c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(bs_read_byte(bs, 2), as.raw(c(1, 127)))
  expect_identical(bs_read_bit (bs, 4), c(FALSE, FALSE, FALSE, TRUE))
  bs_close(bs)
})



test_that("test two works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write bits to a raw vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw(), "w")
  bs_write_uint(bs,  0:7 , nbits = 3) # write 8 * 3-bit integers
  bs_write_uint(bs, 10:15, nbits = 4) # write 6 * 4-bit integers
  raw_vec <- bs_close(bs)
  raw_vec
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read bits back from raw vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bs <- bs_open(raw_vec, mode = 'r')
  expect_identical(bs_read_uint(bs, nbits = 3, n = 8),  0:7 )
  expect_identical(bs_read_uint(bs, nbits = 4, n = 6), 10:15)
  bs_close(bs)
})

