
test_that("exp-golomb can't encode negative numbers", {
  expect_error(int_to_exp_golumn(-1L))
  expect_error(int_to_exp_golumn(-1.0))
})



eg <- list(
  c(F, T, F),                # 1
  c(F, T, T),                # 2
  c(F, F, T, F, F),          # 3
  c(F, F, T, F, T),          # 4
  c(F, F, T, T, F),          # 5
  c(F, F, T, T, T),          # 6
  c(F, F, F, T, F, F, F),    # 7 
  c(F, F, F, T, F, F, T)     # 8
)

# Special case for zero
eg0 <- T


test_that("exp_golonb scalars work", {
  
  expect_identical(uint_to_exp_golomb_bits(0), eg0)
  expect_identical(uint_to_exp_golomb_bits(1), eg[[1]])
  expect_identical(uint_to_exp_golomb_bits(2), eg[[2]])
  expect_identical(uint_to_exp_golomb_bits(3), eg[[3]])
  expect_identical(uint_to_exp_golomb_bits(4), eg[[4]])
  expect_identical(uint_to_exp_golomb_bits(5), eg[[5]])
  expect_identical(uint_to_exp_golomb_bits(6), eg[[6]])
  expect_identical(uint_to_exp_golomb_bits(7), eg[[7]])
  expect_identical(uint_to_exp_golomb_bits(8), eg[[8]])
  
  expect_identical(exp_golomb_bits_to_uint(eg0    , n = 1), 0L)
  expect_identical(exp_golomb_bits_to_uint(eg[[1]], n = 1), 1L)
  expect_identical(exp_golomb_bits_to_uint(eg[[2]], n = 1), 2L)
  expect_identical(exp_golomb_bits_to_uint(eg[[3]], n = 1), 3L)
  expect_identical(exp_golomb_bits_to_uint(eg[[4]], n = 1), 4L)
  expect_identical(exp_golomb_bits_to_uint(eg[[5]], n = 1), 5L)
  expect_identical(exp_golomb_bits_to_uint(eg[[6]], n = 1), 6L)
  expect_identical(exp_golomb_bits_to_uint(eg[[7]], n = 1), 7L)
  expect_identical(exp_golomb_bits_to_uint(eg[[8]], n = 1), 8L)
  
})



test_that("exp_golonb vectors work", {
  expect_identical(uint_to_exp_golomb_bits(c(1, 2)), c(eg[[1]], eg[[2]]))
  expect_identical(uint_to_exp_golomb_bits(c(7, 5)), c(eg[[7]], eg[[5]]))
  expect_identical(uint_to_exp_golomb_bits(c(0, 7, 5)), c(eg0, eg[[7]], eg[[5]]))
  expect_identical(uint_to_exp_golomb_bits(c(7, 0, 5)), c(eg[[7]], eg0, eg[[5]]))
  expect_identical(uint_to_exp_golomb_bits(c(7, 5, 0)), c(eg[[7]], eg[[5]], eg0))
  
  expect_identical(exp_golomb_bits_to_uint(c(eg[[1]], eg[[2]]), n = 2), c(1L, 2L))
  expect_identical(exp_golomb_bits_to_uint(c(eg[[7]], eg[[5]]), n = 2), c(7L, 5L) )
  expect_identical(exp_golomb_bits_to_uint(c(eg0, eg[[7]], eg[[5]]), n = 3), c(0L, 7L, 5L) )
  expect_identical(exp_golomb_bits_to_uint(c(eg[[7]], eg0, eg[[5]]), n = 3), c(7L, 0L, 5L) )
  expect_identical(exp_golomb_bits_to_uint(c(eg[[7]], eg[[5]], eg0), n = 3), c(7L, 5L, 0L) )
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SIGNED Exp golomb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

seg <- list(
  "0"  = c(      T),
  "1"  = c(F, T, F),
  "-1" = c(F, T, T),
  "2"  = c(F, F, T, F, F),
  "-2" = c(F, F, T, F, T),
  "3"  = c(F, F, T, T, F),
  "-3" = c(F, F, T, T, T),
  "4"  = c(F, F, F, T, F, F, F),  
  "-4" = c(F, F, F, T, F, F, T) 
)



test_that("exp_golonb signed scalars work", {
  
  expect_identical(sint_to_exp_golomb_bits( 0), seg[[ "0"]])
  expect_identical(sint_to_exp_golomb_bits( 1), seg[[ "1"]])
  expect_identical(sint_to_exp_golomb_bits(-1), seg[["-1"]])
  expect_identical(sint_to_exp_golomb_bits( 2), seg[[ "2"]])
  expect_identical(sint_to_exp_golomb_bits(-2), seg[["-2"]])
  expect_identical(sint_to_exp_golomb_bits( 3), seg[[ "3"]])
  expect_identical(sint_to_exp_golomb_bits(-3), seg[["-3"]])
  expect_identical(sint_to_exp_golomb_bits( 4), seg[[ "4"]])
  expect_identical(sint_to_exp_golomb_bits(-4), seg[["-4"]])
  
  expect_identical(exp_golomb_bits_to_sint(seg[[ "0"]], n = 1),  0L)
  expect_identical(exp_golomb_bits_to_sint(seg[[ "1"]], n = 1),  1L)
  expect_identical(exp_golomb_bits_to_sint(seg[["-1"]], n = 1), -1L)
  expect_identical(exp_golomb_bits_to_sint(seg[[ "2"]], n = 1),  2L)
  expect_identical(exp_golomb_bits_to_sint(seg[["-2"]], n = 1), -2L)
  expect_identical(exp_golomb_bits_to_sint(seg[[ "3"]], n = 1),  3L)
  expect_identical(exp_golomb_bits_to_sint(seg[["-3"]], n = 1), -3L)
  expect_identical(exp_golomb_bits_to_sint(seg[[ "4"]], n = 1),  4L)
  expect_identical(exp_golomb_bits_to_sint(seg[["-4"]], n = 1), -4L)
  
})



test_that("exp_golonb signed vectors work", {
  expect_identical(sint_to_exp_golomb_bits(c(1, 2)), c(seg[["1"]], seg[["2"]]))
  expect_identical(sint_to_exp_golomb_bits(c(1, -2)), c(seg[["1"]], seg[["-2"]]))
  expect_identical(sint_to_exp_golomb_bits(c(0, -2, 1)), c(seg[["0"]], seg[["-2"]], seg[["1"]]))
  expect_identical(sint_to_exp_golomb_bits(c(-2, 0, 1)), c(seg[["-2"]], seg[["0"]], seg[["1"]]))
  expect_identical(sint_to_exp_golomb_bits(c(-2, 1, 0)), c(seg[["-2"]], seg[["1"]], seg[["0"]]))
  
  expect_identical(exp_golomb_bits_to_sint(c(seg[["1"]], seg[["2"]]), n = 2), c(1L, 2L))
  expect_identical(exp_golomb_bits_to_sint(c(seg[["1"]], seg[["-2"]]), n = 2), c(1L, -2L) )
  expect_identical(exp_golomb_bits_to_sint(c(seg[["0"]], seg[["-2"]], seg[["1"]]), n = 3), c(0L, -2L, 1L) )
  expect_identical(exp_golomb_bits_to_sint(c(seg[["-2"]], seg[["0"]], seg[["1"]]), n = 3), c(-2L, 0L, 1L) )
  expect_identical(exp_golomb_bits_to_sint(c(seg[["-2"]], seg[["1"]], seg[["0"]]), n = 3), c(-2L, 1L, 0L) )
})







