

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read/Write Exponential-Golomb encoded non-negative integers
#' 
#' @inheritParams bs_write_bit
#' @param x integer vector to write
#' @param n number of encoded integers to read
#' @return Reading returns a vector of non-negative integers. Writing returns
#'    the bitstream invisibly.
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_uint_exp_golomb(bs, c(0, 4, 21))
#' bs_align(bs, 8)
#' raw_vec <- bs_close(bs)
#' raw_vec
#' 
#' 
#' bs  <- bs_open(raw_vec, 'r')
#' bs_read_uint_exp_golomb(bs, 3)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_write_uint_exp_golomb <- function(bs, x) {
  bits <- uint_to_exp_golomb_bits(x)
  bs_write_bit(bs, bits)
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_write_uint_exp_golomb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_read_uint_exp_golomb <- function(bs, n = 1L) {

  # 32 bit integer is encoded as 63 bits in exp golomb
  int_vec <- vapply(seq(n), function(i) {
    bits <- bs_peek(bs, 64L)
    int <- exp_golomb_bits_to_uint(bits, n = 1)
    nbits_used <- length(uint_to_exp_golomb_bits(int))
    bs_advance(bs, nbits_used)
    int
  }, integer(1))
  
  int_vec
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between non-negative integers and Exponential Golomb bit sequences
#' 
#' @param x integer vector with all values >= 0
#' @param bits logical vector of bit values
#' @param n number of values to decode. Default: 1. Set to 'Inf' to decode 
#'     all bits.  Will raise an error if there are extra bits at the end that
#'     are not properly encoded integers
#' @return logical vector of bit values, or vector of non-negative integers
#' @examples
#' bits <- uint_to_exp_golomb_bits(c(0, 4, 21))
#' bits
#' exp_golomb_bits_to_uint(bits, n = 3)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uint_to_exp_golomb_bits <- function(x) {
  if (is.double(x)) {
    x <- as.integer(round(x))
  }
  stopifnot(!anyNA(x))
  stopifnot(all(x >= 0))
  
  nbits <- ceiling(log2(x + 1.001))
  
  list_of_bits <- lapply(seq_along(x), \(i) {
    rhs <- intToBits(x[i] + 1)[seq(nbits[i])] |> 
      as.logical() |> 
      rev()
    
    lhs <- rep(F, nbits[[i]] - 1L)
    
    c(lhs, rhs)
  })
  
  unlist(list_of_bits, recursive = FALSE, use.names = FALSE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname uint_to_exp_golomb_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp_golomb_bits_to_uint <- function(bits, n = 1) {
  stopifnot(is.logical(bits))
  
  
  int_vec <- integer(0);
  while (length(bits) > 0 && n > 0L) {
    # Where is the first TRUE value?
    leading_zeros <- which.max(bits) - 1L
    
    # The number of leading zeros (plus 1) equals the number of bits 
    # used to encode the integer.
    # Extract just the encoding bits from all the bits
    int_start <- leading_zeros + 1L
    int_end   <- leading_zeros + leading_zeros + 1L
    if (int_end > length(bits)) {
      stop("exp golomb exceeds end of bits")
    }
    
    # The encoded integer is always (actual + 1), so subject
    # 1 to get the actual integer
    res <- bits_to_uint(bits[int_start:int_end]) - 1L
    int_vec <- c(int_vec, res)
    bits <- bits[-seq(int_end)]
    n <- n - 1L
  }
  
  int_vec
}


