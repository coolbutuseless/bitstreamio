

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read/Write unsigned integers
#' 
#' @inheritParams bs_write_bit
#' @param x integer vector to write
#' @param n number of integers to read
#' @param nbits the number of bits used for each integer
#' @return Reading returns a vector of non-negative integers. Writing returns
#'    the bitstream invisibly.
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_uint(bs, c(0, 4, 21), nbits = 5)
#' bs_align(bs, 8)
#' raw_vec <- bs_close(bs)
#' raw_vec
#' 
#' bs  <- bs_open(raw_vec, 'r')
#' bs_read_uint(bs, n = 3, nbits = 5)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_write_uint <- function(bs, x, nbits) {
  bits <- uint_to_bits(x, nbits = nbits)
  bs_write_bit(bs, bits)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_write_uint
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_read_uint <- function(bs, nbits, n = 1L) {
  total_bits <- nbits * n
  bits <- bs_read_bit(bs, total_bits)
  bits_to_uint(bits, nbits = nbits)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between bits and unsigned integers
#' 
#' @param x vector of unsigned integers 
#' @param bits logical vector of bit values in MSB first order
#' @param nbits number of bits per integer. If NULL, then \code{bits} is assumed
#'    to represent a single integer value.  If not NULL, then the number of 
#'    values in \code{bits} must be a multiple of \code{nbits}
#' @return logical vector of bit values of vector of unsigned integers
#' @examples
#' bits <- uint_to_bits(c(1, 2, 3), nbits = 3)
#' bits
#' bits_to_uint(bits, nbits = 3)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits_to_uint <- function(bits, nbits = NULL) {
  
  if (length(bits) == 0) {
    return(integer(0))
  }
  
  if (!is.null(nbits)) {
    if (length(bits) %% nbits != 0) {
      stop("length(bits) must be a multiple of 'nbits")
    }
    
    bit_chunks <- split(bits, ceiling(seq_along(bits) / nbits))
    
    res <- vapply(bit_chunks, bits_to_uint, integer(1), USE.NAMES = FALSE)
    return(res)
  }
  
  # multiply bit positions by their associated power-of-two, and then sum
  value <- 2L ^ seq(length(bits) - 1L, 0L)
  as.integer(sum(bits * value))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bits_to_uint
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uint_to_bits <- function(x, nbits) {
  
  stopifnot(all(x >= 0))
  
  res <- lapply(x, function(i) {
    (intToBits(i)[seq(nbits)]) |>
      as.logical() |>
      rev()
  })
  
  unlist(res, recursive = FALSE, use.names = FALSE)
}


