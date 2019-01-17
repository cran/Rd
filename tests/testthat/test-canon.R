#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `canon.R`')
#line 49 "C:/rdtf/Rd/R/canon.R"
test_that('Rd_canonize', {#@testing
    rd <- Rd_text("a\nb\nc\n")
    expect_is(rd, 'Rd_string')
    expect_true(is.character(rd))
    expect_length(rd, 1)

    expect_error(Rd_canonize_text(rd))

    val <- Rd_canonize_text(Rd(rd))
    expected <- Rd_unclass(.Rd( Rd_text('a\n')
                              , Rd_text('b\n')
                              , Rd_text('c\n')
                              ))
    expect_identical( Rd_unclass(val), expected)
    expect_identical(Rd_canonize(val), val)

    rd <- Rd_tag( "\\examples"
                , Rd_rcode("\n")
                , Rd_rcode("x<- rnorm(100)\n")
                , Rd_rcode("plot(x)\n"))
    expect_true(is_valid_Rd_list(rd))

    expect_identical(Rd_canonize_text(rd), rd)
    expect_identical(Rd_canonize_code(rd), rd)
    expect_identical(Rd_canonize(rd), rd)
    expect_identical(Rd_unclass(Rd_canonize(Rd_unclass(rd))), Rd_unclass(rd))

    expect_identical(Rd_canonize_code(Rd_tag('\\examples'
                                            , Rd_rcode("\nx<- rnorm(100)\nplot(x)\n")))
                    , rd)

    rd <- .Rd(Rd_text("use the \\backslash to escape.")
             , Rd_text("and '{}' to group.")
             )
    val <- Rd_canonize_text(rd)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_true(is_Rd_string(val[[1]], 'TEXT'))
    expect_length(val[[1]], 1L)
})
#line 89 "C:/rdtf/Rd/R/canon.R"
test_that('Rd_canonize with output from parse_Rd', {#@testing Rd_canonize with output from parse_Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(txt)
    expect_identical( Rd_unclass(val <- Rd_canonize_code(rd <- Rd_get_element(txt, '\\examples')))
                    , Rd_unclass(rd)
                    )

    desc <- Rd_get_element(txt, '\\description')
    canonical <- Rd_canonize_text(desc)
    expect_identical( as.character(desc)
                    , as.character(canonical)
                    )

    expect_identical(Rd_unclass(Rd_canonize_text(txt)), Rd_unclass(txt))
    expect_identical(Rd_unclass(Rd_canonize_code(txt)), Rd_unclass(txt))
    expect_identical(Rd_unclass(Rd_canonize     (txt)), Rd_unclass(txt))

    x <- Rd_text("test strings\nsecond line")
    val <- Rd_canonize(x)
    expected <- Rd(Rd_text("test strings\n"), Rd_text("second line"))
    expect_identical(val, expected)

    expect_identical(Rd_canonize_text(Rd.newline), Rd.newline)

    x <- .Rd( Rd_tag("\\item"), Rd_text(" "), Rd_text("content"))
    expect_identical(Rd_canonize_text(x)[[1]], Rd_tag('\\item'))
    expect_identical(Rd_canonize_text(x)[[2]], Rd_text(' content'))
})
#line 117 "C:/rdtf/Rd/R/canon.R"
test_that('Rd_canonize with unclassed arguments', {#@testing Rd_canonize with unclassed arguments
    x <- unclass(Rd_text('test'))
    expect_is(x, 'character')
    val <- Rd_canonize(x)
    expect_Rd_bare(val)
    expect_Rd_string(unclass(val)[[1]], 'TEXT')

    x <- unclass(Rd_tag('\\bold', 'text'))
    expect_is(x, 'list')
    expect_equal(get_Rd_tag(x), '\\bold')
    val <- Rd_canonize(x)
    expect_is(val, 'Rd_tag')
    expect_Rd_tag(val, '\\bold')
    expect_Rd_string(unclass(val)[[1]], 'TEXT')

    y <- unclass(Rd(x))
    y[[1]] <- unclass(y[[1]])
    expect_is(y, 'list')
    expect_is(y[[1]], 'list')
    val.y <- Rd_canonize(y)
    expect_is(val.y, 'Rd')
    expect_is(unclass(val.y)[[1]], 'Rd_tag')
    expect_Rd_bare(val.y)
    expect_Rd_tag(unclass(val.y)[[1]], '\\bold')
    expect_Rd_string(unclass(val.y)[[1]][[1]], 'TEXT')
})
#line 184 "C:/rdtf/Rd/R/canon.R"
test_that('Rd_canonize_text', {#@testing
    expect_error(Rd_canonize_text(Rd_text('\n')))
    expect_identical(Rd_canonize_text(Rd_text('\n'), .check=FALSE), Rd_text('\n'))

    x <- .Rd( Rd_text("    ")
            , Rd_text("hello")
            , Rd_text("\n    ")
            , Rd_text("world")
            )
    val <- Rd_canonize_text(x)
    expect_is(val, 'Rd')
    expect_length(val, 2)
    expect_identical(val, .Rd( Rd_text("    hello\n")
                             , Rd_text("    world")
                             ))

    reclaimed <- Rd_rm_srcref(s(tools::parse_Rd(textConnection(collapse0(x))), macros=NULL))
    val <- Rd_canonize_text(cl(c(x, .Rd(Rd_text('\n'))), 'Rd'))
    expect_identical(Rd_unclass(val), Rd_unclass(reclaimed))
})
#line 227 "C:/rdtf/Rd/R/canon.R"
test_that('Rd_canonize_code', {#@testing
    x <- Rd_tag( "\\usage"
               , Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition'), Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate'), Rd_rcode("\n")
               )
    expected <- Rd_tag( "\\usage"
               , Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition\n')
               , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
               )
    expect_identical( Rd_canonize_code(x), expected)
    expect_identical( Rd_canonize(x), expected)
    expect_identical( Rd_canonize_text(x), x)


    bad <- .Rd( Rd_text("\n")
              , Rd_rcode('value \\%if\\% proposition'), Rd_rcode("\n")
              , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate'), Rd_rcode("\n")
              )
    expect_error(Rd_canonize_code(bad)
                , "RCODE type strings may not appear in a  container with any other type\\.")
})
