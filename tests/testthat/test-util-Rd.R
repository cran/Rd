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
#line 50 "R/util-Rd.R"
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
#line 122 "R/util-Rd.R"
test_that('Rd_compact', {#@testing
    l <- list( Rd_text('testing ')
             , Rd_code('Rd_compact()')
             , Rd(' with a list of Rd objects.')
             )
    expect_is_exactly(l[[1]], 'Rd_string')
    expect_is_exactly(l[[2]], 'Rd_tag')
    expect_is_exactly(l[[3]], 'Rd')
    val <- Rd_compact(l)
    expect_length(val, 3)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_string')
    expect_is_exactly(val[[2]], 'Rd_tag')
    expect_is_exactly(val[[3]], 'Rd_string')
})
