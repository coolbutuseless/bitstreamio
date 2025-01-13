

test_that("bs_advance() works", {
  
  bs <- bs_open(as.raw(1:3), 'r')
  bs_advance(bs, 8)
  expect_identical(bs_read_byte(bs, 2), as.raw(2:3))
  bs_close(bs)
  
  
  bs <- bs_open(as.raw(1:3), 'r')
  bs_advance(bs, 4)
  expect_identical(
    bs_read_byte(bs, 1),
    #           '1' lower nibble    '2' uppernibble
    bits_to_raw(c(F, F, F, T,       F, F, F, F)) 
  )
  bs_close(bs)
  
  
  bs <- bs_open(as.raw(0:255), 'r')
  bs_advance(bs, 254 * 8)
  expect_identical(
    bs_read_byte(bs, 1),
    #           '254'\
    bits_to_raw(c(T, T, T, T,       T, T, T, F)) 
  )
  bs_close(bs)
})

