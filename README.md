
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitstreamio

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![CRAN](https://www.r-pkg.org/badges/version/bitstreamio)](https://CRAN.R-project.org/package=bitstreamio)
[![R-CMD-check](https://github.com/coolbutuseless/bitstreamio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/bitstreamio-dev/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`bitstreamio` is a package for reading bits from a connection or raw
vector.

This package has no requirement that reads/write operations be *byte
aligned*. This is useful for input/output with packed binary file
formats e.g. h264-compressed video, mp3 audio etc.

In addition to reading individual bits, this package also provides for:

- Reading/writing unaligned bytes as raw vectors
- Reading/writing unsigned integers (i.e. non-negative integers) at bit
  depths from 1 to 31
- Read/write signed/unsigned integers with [Exponential-Golomb
  coding](https://en.wikipedia.org/wiki/Exponential-Golomb_coding)

## What’s in the box

- Read write: bits, bytes, unsigned integers, integers coded with
  exponential golomb (signed and unsigned variants).
  - `bs_write_bit()`, `bs_read_bit()`
  - `bs_write_byte()`, `bs_read_byte()`
  - `bs_write_uint()`, `bs_read_uint()`
  - `bs_write_uint_exp_golomb()`, `bs_read_uint_exp_golomb()`
  - `bs_write_sint_exp_golomb()`, `bs_read_sint_exp_golomb()`
- Bitstream utilities
  - `bs_open()`, `bs_close()`
  - `bs_is_aligned()`, `bs_align()`
  - `bs_advance()`, `bs_peek()`

## Installation

This package can be installed from CRAN

``` r
install.packages('bitstreamio')
```

You can install the latest development version from
[GitHub](https://github.com/coolbutuseless/bitstreamio) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/bitstreamio')
```

Pre-built source/binary versions can also be installed from
[R-universe](https://r-universe.dev)

``` r
install.packages('bitstreamio', repos = c('https://coolbutuseless.r-universe.dev', 'https://cloud.r-project.org'))
```

## Read/Write with a raw vector

``` r
library(bitstreamio)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write bits to a raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw(), "w")
bs_write_bit(bs, c(TRUE , FALSE, FALSE, FALSE))
bs_write_byte(bs, c(0x01, 0x7f))  # write unaligned byte values
bs_write_bit(bs, c(FALSE, FALSE, FALSE, TRUE))

raw_vec <- bs_close(bs)
raw_vec
```

    #> [1] 80 17 f1

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read bits back from raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw_vec, mode = 'r')
bs_read_bit(bs, 4)
```

    #> [1]  TRUE FALSE FALSE FALSE

``` r
bs_read_byte(bs, 2) # unaligned byte read
```

    #> [1] 01 7f

``` r
bs_read_bit(bs, 4)
```

    #> [1] FALSE FALSE FALSE  TRUE

``` r
bs_close(bs)
```

## Reading/Write with a connection

``` r
filename <- tempfile()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write bits to a file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
con <- file(filename, "wb")
bs  <- bs_open(con, mode = 'w')

bs_write_bit(bs, c(TRUE , FALSE, FALSE, FALSE))
bs_write_byte(bs, c(1, 127))
bs_write_bit(bs, c(FALSE, FALSE, FALSE, TRUE))

bs_close(bs)
close(con)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the bits back in
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
con <- file(filename, "rb")
bs  <- bs_open(con, mode = 'r')

bs_read_bit(bs, 4)
```

    #> [1]  TRUE FALSE FALSE FALSE

``` r
bs_read_uint(bs, nbits = 8, n = 2)
```

    #> [1]   1 127

``` r
bs_read_bit(bs, 4)
```

    #> [1] FALSE FALSE FALSE  TRUE

``` r
bs_close(bs)
close(con)
```

## Read/write unsigned integers at varying bit depths

``` r
library(bitstreamio)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write bits to a raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw(), "w")
bs_write_uint(bs,  0:7 , nbits = 3) # write 8 * 3-bit integers
bs_write_uint(bs, 10:15, nbits = 4) # write 6 * 4-bit integers
raw_vec <- bs_close(bs)
raw_vec
```

    #> [1] 05 39 77 ab cd ef

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read bits back from raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw_vec, mode = 'r')
bs_read_uint(bs, nbits = 3, n = 8)
```

    #> [1] 0 1 2 3 4 5 6 7

``` r
bs_read_uint(bs, nbits = 4, n = 6)
```

    #> [1] 10 11 12 13 14 15

``` r
bs_close(bs)
```

## Read/write integers with Exponential-Golomb coding

[Exponential-Golomb
coding](https://en.wikipedia.org/wiki/Exponential-Golomb_coding) is a
way of encoding integers to bit sequences. These bit sequences are often
smaller than any standard integer type, and are often used to save space
when packing data into a stream when the size of the integer is not
known ahead of time.

``` r
# Converstion of unsigned integers to Exponential-Golomb coded bit sequences
uint_to_exp_golomb_bits(0)
```

    #> [1] TRUE

``` r
uint_to_exp_golomb_bits(1)
```

    #> [1] FALSE  TRUE FALSE

``` r
uint_to_exp_golomb_bits(2)
```

    #> [1] FALSE  TRUE  TRUE

``` r
uint_to_exp_golomb_bits(3)
```

    #> [1] FALSE FALSE  TRUE FALSE FALSE

``` r
# Converstion of signed integers to Exponential-Golomb coded bit sequences
sint_to_exp_golomb_bits(0)
```

    #> [1] TRUE

``` r
sint_to_exp_golomb_bits(-1)
```

    #> [1] FALSE  TRUE  TRUE

``` r
sint_to_exp_golomb_bits(-2)
```

    #> [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
sint_to_exp_golomb_bits(3)
```

    #> [1] FALSE FALSE  TRUE  TRUE FALSE

``` r
library(bitstreamio)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write bits to a raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw(), "w")
bs_write_uint_exp_golomb(bs,  0:9) 
raw_vec <- bs_close(bs)

# 10 integers encoded into 6 bytes
raw_vec
```

    #> [1] a6 42 98 e2 04 8a

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read bits back from raw vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bs <- bs_open(raw_vec, mode = 'r')
bs_read_uint_exp_golomb(bs, 10)
```

    #>  [1] 0 1 2 3 4 5 6 7 8 9

``` r
bs_close(bs)
```

## Related Software

- [ctypesio](https://cran.r-project.org/package=ctypesio) - for
  reading/writing standard C types with connections e.g. `uint16`,
  `float`, `double` etc.
