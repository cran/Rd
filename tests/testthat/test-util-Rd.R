#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `util-Rd.R`')
#line 19 "R/util-Rd.R"
test_that('cleanup utilities.', {#@testing cleanup utilities.
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')
    expect_false(is.null(attr(txt, 'srcref')))
    expect_false(is.null(attr(txt[[1]], 'srcref')))

    txt <- Rd_rm_srcref(txt)
    expect_true(is.null(attr(txt, 'srcref')))
    expect_true(is.null(attr(txt[[1]], 'srcref')))

    lst <- Rd_unclass(txt)
    expect_is_exactly(lst, "list")
    expect_is_exactly(lst[[1]], "character")

    expect_equal(get_Rd_tag(txt[[1]]), "COMMENT")
    expect_null(get_Rd_tag(Rd_untag(txt[[1]])))
})
#line 47 "R/util-Rd.R"
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
#line 70 "R/util-Rd.R"
test_that('ensure_ends_with_newline', {#@testing
    expect_identical( ensure_ends_with_newline(Rd_text("testing"))
                    , Rd_text("testing\n"))
    expect_identical( ensure_ends_with_newline(Rd_rcode("testing"))
                    , Rd_rcode("testing\n"))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()")
                            , Rd_rcode("test_me()")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()\n")
                            , Rd_rcode("test_me()")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()\n")
                            , Rd_rcode("test_me()\n")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
})
