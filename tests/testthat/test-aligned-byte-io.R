

test_that("aligned byte read works", {
  
  set.seed(1)
  raw_vec <- as.raw(sample(255, 255))
  
  bs <- bs_open(raw_vec, 'r')
  out <- bs_read_byte(bs, 255)
  expect_identical(out, raw_vec)
  bs_close(bs)
  
  
  bs <- bs_open(raw_vec, 'r')
  bits <- bs_read_bit(bs, 8)
  out <- bs_read_byte(bs, 254)
  expect_identical(out, raw_vec[-1])
  bs_close(bs)
  
})




test_that("aligned byte write works", {
  
  set.seed(1)
  raw_vec <- as.raw(1:10)
  
  bs <- bs_open(raw(), 'w')
  bs_write_byte(bs, raw_vec)
  out <- bs_close(bs)
  expect_identical(out, raw_vec)
  
  
  bs <- bs_open(raw(), 'w')
  bs_write_bit(bs, rep(F, 16))
  bs$bit_count
  bs_write_byte(bs, raw_vec)
  bs$bit_count
  out <- bs_close(bs)
  expect_length(out, length(raw_vec) + 2)
  expect_identical(out, c(as.raw(c(0, 0)), raw_vec))
  
})

