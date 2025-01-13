

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read/Write unaligned bytes with a bitstream
#' 
#' @inheritParams bs_write_bit
#' @param n number of bytes to read
#' @param x vector of bytes to write.  Integer vectors will be truncated to 8 bits
#'    before output.  Numeric vectors will be rounded to integers and then 
#'    truncated to 8 bits.
#' @return Reading returns a logical vector of bit values.  When writing, 
#' the \code{bs} 
#' bitstream connection is returned invisibly
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_bit(bs, c(TRUE, FALSE))
#' bs_write_byte(bs, c(1, 2, 3))
#' bs_align(bs)
#' raw_vec <- bs_close(bs)
#' 
#' bs  <- bs_open(raw_vec, 'r')
#' bs_read_bit(bs, 2)
#' bs_read_byte(bs, 3)
#' bs_close(bs)
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_write_byte <- function(bs, x) {
  assert_bs(bs)
  stopifnot(bs$mode == 'w')
  
  if (bs_is_aligned(bs, 8)) {
    # If bitstream is byte aligned, it is possible to rite the bytes 
    # directly to the stream
    # First: Flush all remaining buffered bits to stream
    #        Note: we already know that this a whole number of bytes
    bs_flush(bs)
    
    # Sanity check: Ensure our internal buffer is empty
    #               Note: this should never fail! 
    stopifnot(length(bs$buffer) == 0)
    
    # Convert dbl/integer to raw and sanity check values
    raw_vec <- num_to_raw(x)
    
    # Write to the connection
    if (bs$raw_con) {
      bs$con <- c(bs$con, raw_vec)
    } else {
      writeBin(raw_vec, bs$con)
    } 
    
    # Increase the bit_count for the bytes just written
    bs$bit_count <- bs$bit_count + length(x) * 8L
  } else {
    bits <- raw_to_bits(x, msb_first = bs$msb_first)
    bs_write_bit(bs, bits)
  }
  
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_write_byte
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_read_byte <- function(bs, n) {
  assert_bs(bs)
  stopifnot(n >= 0)
  if (n == 0) {
    return(raw(0))
  }
  
  if (bs_is_aligned(bs, 8) && (n * 8) > length(bs$buffer)) {
    # aligned read should be faster here
    # Read all possible bytes from the buffer
    nbits <- n * 8L
    
    if (length(bs$buffer) > 0) {
      buffer_bits  <- bs_read_bit(bs, length(bs$buffer))
      buffer_bytes <- bits_to_raw(buffer_bits, bs$msb_first)
    } else {
      buffer_bytes <- raw(0)
    }    
    
    # How many more bytes do we need?
    nbytes <- n - length(buffer_bytes)
    
    # Read new bytes directly from the connection
    new_bytes <- readBin(bs$con, 'raw', n = nbytes, size = 1)
    
    if (bs$raw_con) {
      # trim bytes from the front
      bs$con <- bs$con[-seq(nbytes)]
    }
    
    bytes <- c(buffer_bytes, new_bytes)
    
    # Switch bit order if not MSB
    if (!bs$msb_first) {
      bytes <- reverse_bit_lookup[as.integer(bytes) + 1L]
    }
    
    bytes
  } else {
    # unaligned read
    bits  <- bs_read_bit(bs, n * 8)
    bits_to_raw(bits, bs$msb_first)
  }
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between logical vector of bits and raw vector
#' 
#' @param bits Logical vector of bit values. Length must be a multiple of 8
#' @param x  Byte values. Integer vectors will be truncated to 8 bits
#'    before output.  Numeric vectors will be rounded to integers and then 
#'    truncated to 8 bits.  Raw vectors preferred.
#' @param msb_first MSB first? Default: TRUE
#' @return Logical vector of bit values or a raw vector.
#' @examples
#' bits <- raw_to_bits(c(0, 4, 21))
#' bits
#' bits_to_raw(bits) |> as.integer()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits_to_raw <- function(bits, msb_first = TRUE) {
  
  stopifnot(is.logical(bits))
  stopifnot(!anyNA(bits))
  
  if (length(bits) %% 8 != 0) {
    stop("bits_to_raw(): length(bits) must a multiple of 8")
  }
  
  # packbits is least-significant-bit first
  raw_vec <- packBits(bits, 'raw')
  
  if (msb_first) {
    raw_vec <- reverse_bit_lookup[as.integer(raw_vec) + 1L]
  }
  
  raw_vec
}



num_to_raw <- function(x) {
  if (is.numeric(x)) {
    x <- as.integer(round(x))
  }
  
  if (is.integer(x)) {
    stopifnot(!anyNA(x))
    stopifnot(all(x >= 0))
    x <- as.raw(x)
  }
  
  if (!is.raw(x)) {
    stop("type not understood: ", class(x))
  }
  
  x
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bits_to_raw
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw_to_bits <- function(x, msb_first = TRUE) {
  x <- num_to_raw(x)
  
  if (msb_first) {
    x <- reverse_bit_lookup[as.integer(x) + 1]
  }
  
  as.logical(rawToBits(x))
}



