

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read/Write Exponential-Golomb encoded signed integers
#' 
#' @inheritParams bs_write_bit
#' @param x integer vector to write
#' @param n number of encoded integers to read
#' @return Reading returns a vector of integers. Writing returns
#'    the bitstream invisibly.
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_sint_exp_golomb(bs, c(0, 4, -21))
#' raw_vec <- bs_close(bs)
#' raw_vec
#' 
#' 
#' bs  <- bs_open(raw_vec, 'r')
#' bs_read_sint_exp_golomb(bs, 3)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_write_sint_exp_golomb <- function(bs, x) {
  bits <- sint_to_exp_golomb_bits(x)
  bs_write_bit(bs, bits)
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_write_sint_exp_golomb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_read_sint_exp_golomb <- function(bs, n = 1L) {

  # 32 bit integer is encoded as 63 bits in exp golomb
  int_vec <- vapply(seq(n), function(i) {
    bits <- bs_peek(bs, 64L)
    int <- exp_golomb_bits_to_sint(bits, n = 1)
    nbits_used <- length(sint_to_exp_golomb_bits(int))
    bs_advance(bs, nbits_used)
    int
  }, integer(1))
  
  int_vec
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between signed integers and Exponential-Golomb bit sequences
#' 
#' @param x integer vector with all values >= 0
#' @param bits logical vector of bit values
#' @param n number of values to decode. Default: 1. Set to 'Inf' to decode 
#'     all bits.  Will raise an error if there are extra bits at the end that
#'     are unused.
#' @return logical vector of bit values, or vector of signed integers
#' @examples
#' bits <- sint_to_exp_golomb_bits(c(0, 4, -21))
#' bits
#' exp_golomb_bits_to_sint(bits, n = 3)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sint_to_exp_golomb_bits <- function(x) {
  if (is.double(x)) {
    x <- as.integer(round(x))
  }
  stopifnot(!anyNA(x))
  
  x <- ifelse(x == 0, 0L, 
              ifelse(x < 0, -2L * x, 2L * x - 1L))
  
  uint_to_exp_golomb_bits(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sint_to_exp_golomb_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_golomb_bits_to_sint <- function(bits, n = 1) {
  stopifnot(is.logical(bits))
  
  x <- exp_golomb_bits_to_uint(bits, n = n)
 
  x <- ifelse(x == 0, 0L, 
                    ifelse(x %% 2 == 0L, -x/2L, (x + 1L)/2L))
  as.integer(round(x))
}


