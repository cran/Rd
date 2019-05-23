#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `Rd_lines.R`')
#line 22 "R/Rd_lines.R"
test_that('Rd_lines', {#@testing
    l <- list( Rd_rcode("value \\%if\\% proposition")
             , Rd_rcode("proposition \\%otherwise\\% alternate")
             , Rd_rcode('')
             )
    exp <- Rd( Rd_rcode("value \\%if\\% proposition\n")
             , Rd_rcode("proposition \\%otherwise\\% alternate\n"))
    val <- Rd_lines(l)
    expect_identical(val, exp)
})
