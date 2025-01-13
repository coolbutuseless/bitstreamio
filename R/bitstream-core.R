

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Test if an object is a bitstream object
#' 
#' @param x object to test
#' @return logical. TRUE if object is a bitstream object
#' @examples
#' # Negative case
#' is_bs(NULL)
#' 
#' # Positive case
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' is_bs(bs)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_bs <- function(x) {
  inherits(x, 'bitstream') &&  (x$raw_con || isOpen(x$con))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Test if an object is a bitstream object and fail if it is not
#' @inheritParams is_bs
#' @return None
#' @examples
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' assert_bs(bs)
#' bs_close(bs)
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_bs <- function(x) {
  stopifnot(is_bs(x))
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Open/close a bitstream
#' 
#' @param con A vector of raw values or an R connection (e.g. \code{file()}, \code{url()}, etc)
#' @param bs Bistream connection object created with \code{bs_open()}
#' @param mode Bitstream mode set to read or write?  One of 'r', 'w', 'rb', 'wb'.
#' @param flush_threshold Threshold number of bits at which the buffered data
#'     will be automatically written to the connection. 
#'     Default: 8192 bits (1024 bytes).  Note: Use \code{bs_flush()} to 
#'     write out the buffer at any time. All bits are automatically written out 
#'     when \code{bs_close()} is called.
#' @param msb_first Should the output mode be Most Signficant Bit first?  
#'    Default: TRUE
#' @param verbosity Verbosity level. Default: 0
#' @return \code{bs_open()} returns a \code{bitstream} connection object. When the 
#'    connection is a raw vector and \code{mode = 'w'}, \code{bs_close()} returns 
#'    the final state of the raw vector; in all other cases \code{bs_close()}
#'    does not return a value.
#' @examples
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' assert_bs(bs)
#' bs_close(bs)
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_open <- function(con, mode, msb_first = TRUE, flush_threshold = 1024 * 8) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  flush_threshold <- as.integer(flush_threshold)
  stopifnot(exprs = {
    flush_threshold > 0
    length(mode) == 1
    mode %in% c('r', 'w', 'rb', 'wb')
  })
  
  # Are we responsible for closing this connection?
  # If the user passes in a connection, then NO
  # If we create a rawConnection() here, then yes.
  mode <- tolower(substr(mode, 1, 1))
  raw_con <- is.raw(con)
  
  stopifnot(exprs = {
    !is_bs(con)
    raw_con || isOpen(con)
  })
  
  
  bs             <- new.env()
  bs$con         <- con
  bs$buffer      <- logical(0)
  bs$bit_count   <- 0L
  bs$flush_threshold  <- flush_threshold
  bs$msb_first   <- isTRUE(msb_first)
  bs$mode        <- mode
  bs$raw_con     <- raw_con
  
  class(bs) <- 'bitstream'
  bs
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bs_open
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_close <- function(bs, verbosity = 0) {
  assert_bs(bs)
  
  # If mode = write, then flush all reamining bits to connection
  # pad out to byte-aligned stream if necessary
  if (bs$mode == 'w' && bs$bit_count > 0) {  
    if (!bs_is_aligned(bs, nbits = 8)) {
      if (verbosity > 0) {
        warning("bs_close(): Adding bits to byte-align the output")
      }
      bs_align(bs, nbits = 8L)
    }
    
    bs_flush(bs)
    stopifnot(length(bs$buffer) == 0)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we were given a raw vector as the initial con object in bs_open()
  # then extract the final raw vector and 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (bs$raw_con && bs$mode == 'w') {
    return(bs$con)
  }
  
  
  invisible()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Flush bits in the buffer
#'
#' This is called internally to flush bitstream buffers to the underlying
#' R connection.
#'
#' @inheritParams bs_open
#' @return \code{Bitstream} connection returned invisibly
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_bit(bs, c(TRUE, FALSE, TRUE))
#' bs_align(bs, nbits = 8)
#' bs_flush(bs)
#' output <- bs_close(bs)
#' output
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_flush <- function(bs) {
  
  stopifnot(exprs = {
    is_bs(bs)
    bs$mode == 'w'
  })
  
  # How many bytes are currently in the buffer?
  nbytes <- length(bs$buffer) %/% 8L
  
  # Write bytes to file
  if (nbytes > 0) {
    nbits   <- nbytes * 8L
    outbuf  <- bs$buffer[ seq(nbits)]
    raw_vec <- bits_to_raw(outbuf, msb_first = bs$msb_first)
    
    if (bs$raw_con) {
      bs$con <- c(bs$con, raw_vec)
    } else {
      writeBin(raw_vec, bs$con)
    }
    
    # Remove from the buffer the bits that have just been written to connection
    bs$buffer <- bs$buffer[-seq(nbits)]
  }
  
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is the current bit connection aligned at the given number of bits for reading/writing?
#' 
#' @inheritParams bs_open
#' @param nbits number of bits of alignment w.r.t start of bitstream. Default: 8
#' @return logical. TRUE if stream location is currently aligned to the 
#'    specified number of bits, otherwise FALSE
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_bit(bs, c(TRUE, FALSE, TRUE))
#' bs_is_aligned(bs, 8)
#' bs_align(bs, nbits = 8)
#' bs_is_aligned(bs, 8)
#' output <- bs_close(bs)
#' output
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_is_aligned <- function(bs, nbits = 8) {
  
  stopifnot(exprs = {
    is_bs(bs)
    nbits > 0
  })
  
  bs$bit_count %% nbits ==  0
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Align the bitstream to the given number of bits - relative to start of bitstream
#' @inheritParams bs_is_aligned
#' @param value bit fill value. Either TRUE or FALSE.  Default FALSE
#' @return \code{Bitstream} connection returned invisibly
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_bit(bs, c(TRUE, FALSE, TRUE))
#' bs_is_aligned(bs, 8)
#' bs_align(bs, nbits = 8)
#' bs_is_aligned(bs, 8)
#' output <- bs_close(bs)
#' output
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_align <- function(bs, nbits = 8L, value = FALSE) {
  
  stopifnot(exprs = {
    is_bs(bs)
    nbits > 0
  })
  
  if (!bs_is_aligned(bs, nbits = nbits)) {
    value <- isTRUE(value)
    n_filler_bits <- nbits - (bs$bit_count %% nbits)
    x <- rep(value, n_filler_bits)
    bs_write_bit(bs, x)
  } 
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Peek at bits from a bitstream i.e. examine bits without advancing bitstream
#'
#' @inheritParams bs_open
#' @param n number of bits to peek.
#' @return logical vector of bit values
#' @examples
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' bs_peek(bs, 4)
#' stopifnot(bs_is_aligned(bs))
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_peek <- function(bs, n) {
  
  stopifnot(exprs = {
    is_bs(bs)
    bs$mode == 'r'
    n >= 0
  })
  
  if (n == 0) {
    return(logical(0))
  }
  
  
  if (length(bs$buffer) < n) {
    # cat("Reservoir exhausted. replenishing...\n")
    nbytes <- ceiling(n / 8)
    bytes <- readBin(bs$con, 'raw', n = nbytes, size = 1L)
    
    if (bs$raw_con) {
      # swallow the bytes in the connectin, 
      # as they will now exist in the buffer
      bs$con <- bs$con[-seq_along(bytes)]
    }
    
    
    if (bs$msb_first) {
      bytes <- reverse_bit_lookup[as.integer(bytes) + 1L]
    }
    
    bits <- as.logical(rawToBits(bytes))
    
    bs$buffer <- c(bs$buffer, bits)
  }
  
  # Read as many bit as we can
  # its up to the caller to throw an error
  n <- min(length(bs$buffer), n)
  
  res <- bs$buffer[seq(n)]
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read bits from a bitstream
#'
#' @inheritParams bs_write_bit
#' @param n number of bits to read
#' @return logical vector of bit values
#' @examples
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' bs_read_bit(bs, 4)
#' bs_is_aligned(bs)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_read_bit <- function(bs, n) {

  stopifnot(exprs = {
    is_bs(bs)
    bs$mode == 'r'
  })
  
  res <- bs_peek(bs, n)
  if (length(res) != n) {
    stop("Incomplete read")
  }
  bs_advance(bs, n)

  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Advance bitstream
#' 
#' @inheritParams bs_write_bit
#' @param n number of bits to advance
#' @return \code{Bitstream} connection returned invisibly
#' @examples
#' raw_vec <- as.raw(1:3)
#' bs  <- bs_open(raw_vec, 'r')
#' bs_is_aligned(bs)
#' bs_advance(bs, 4)
#' bs_is_aligned(bs)
#' bs_read_bit(bs, 8)
#' bs_is_aligned(bs)
#' bs_close(bs)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_advance <- function(bs, n) {
  
  stopifnot(exprs = {
    is_bs(bs)
    bs$mode == 'r'
    n >= 0
  })
  
  # Ensure we have enough bits in the buffer
  bs_peek(bs, n)
  if (length(bs$buffer) < n) {
    stop("Reached EOF while advancing")
  }
  
  bs$buffer    <- bs$buffer[-seq(n)]
  bs$bit_count <- bs$bit_count + n
  invisible(bs)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write unaligned bits to a bitstream
#' 
#' @inheritParams bs_open
#' @param x Logical vector of bit values
#' @return \code{Bitstream} connection returned invisibly
#' @examples
#' bs  <- bs_open(raw(), 'w')
#' bs_write_bit(bs, c(TRUE, FALSE, TRUE))
#' bs_align(bs, nbits = 8)
#' bs_flush(bs) 
#' output <- bs_close(bs)
#' output
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs_write_bit <- function(bs, x) {
  stopifnot(exprs = {
    is_bs(bs)
    bs$mode == 'w'
    is.logical(x)
    !anyNA(x)
  })
  
  bs$buffer <- c(bs$buffer, x)
  bs$bit_count <- bs$bit_count + length(x)
  
  if (length(bs$buffer) > (bs$flush_threshold)) {
    bs_flush(bs)
  }
  
  invisible(bs)
}




