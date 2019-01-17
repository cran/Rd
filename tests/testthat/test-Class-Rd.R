#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `Class-Rd.R`')
#line 45 "R/Class-Rd.R"
test_that('is_Rd_string', {#@testing
    expect_error(is_Rd_string('x', tags = "\\tag"))

    expect_identical(is_Rd_string(list(), strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(list(), strict=TRUE)
                    , s(FALSE, msg="not of class 'Rd_string'"))
    expect_identical(is_Rd_string(list(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(list(), strict=FALSE)
                    , s(FALSE, msg="not of mode character"))
    expect_identical(is_Rd_string(c('a','b'), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(c('a','b'), strict=FALSE)
                    , s(FALSE, msg="not a non-empty string"))
    expect_identical(is_Rd_string('a', strict=FALSE, tags='TEXT', reason=FALSE), FALSE)
    expect_identical(is_Rd_string('a', strict=FALSE)
                    , s(FALSE, msg="does not have an 'Rd_tag' attribute"))
    x <- s("test", Rd_tag = 'VERB')
    expect_true(is_Rd_string(x, strict=FALSE))
    expect_identical(is_Rd_string(x, strict=FALSE, tags='TEXT', reason=FALSE), FALSE)
    expect_identical(is_Rd_string(x, strict=FALSE, tags='TEXT')
                    , s(FALSE, msg="'Rd_tag' attribute is not in allowed tags"))

    class(x) <- 'Rd_string'
    expect_true(is_Rd_string(x, strict=TRUE))
})
#line 79 "R/Class-Rd.R"
test_that('are_Rd_strings', {#@testing
    x <- list( s(list(), Rd_tag='\\item')
             , s(list(), Rd_tag='\\dots', class='Rd_tag')
             , s('test', Rd_tag='VERB')
             , s('this', Rd_tag='TEXT', class='Rd_string')
             )
    expect_identical( are_Rd_strings(x)
                    , c(FALSE, FALSE, TRUE, TRUE)
                    )
    expect_identical( are_Rd_strings(x, tags="VERB")
                    , c(FALSE, FALSE, TRUE, FALSE)
                    )
    expect_identical( are_Rd_strings(x, strict=TRUE)
                    , c(FALSE, FALSE, FALSE, TRUE)
                    )
})
#line 120 "R/Class-Rd.R"
test_that('is_Rd_tag', {#@testing
    expect_error( is_Rd_tag(list(), tag='TEXT')
                , class = "Rd-error-assertion failure"
                )
    expect_identical(is_Rd_tag(list(), strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(list(), strict=TRUE)
                    , s(FALSE, msg="is not of class 'Rd_tag'"))
    expect_identical(is_Rd_tag(list(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(list(), strict=FALSE)
                    , s(FALSE, msg="does not have an 'Rd_tag' attribute"))
    expect_identical(is_Rd_tag(character(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(character(), strict=FALSE)
                    , s(FALSE, msg="is not a list"))
    x <- s(list(), Rd_tag = "item")
    expect_identical(is_Rd_tag(x, strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(x, strict=TRUE)
                    , s(FALSE, msg="is not of class 'Rd_tag'"))
    expect_identical( is_Rd_tag(x, strict=FALSE, reason=FALSE), FALSE)
    expect_identical( is_Rd_tag(x, strict=FALSE)
                    , s(FALSE, msg="'Rd_tag' attribute is invalid"))

    x <- s(list(), Rd_tag = "\\item")
    expect_true( is_Rd_tag(x) )
    expect_identical( is_Rd_tag(x, strict=FALSE, tag = c("\\details", "\\usage"), reason=FALSE), FALSE)
    expect_identical( is_Rd_tag(x, strict=FALSE, tag = c("\\details", "\\usage"))
                    , s(FALSE, msg="'Rd_tag' attribute is not in allowed tags"))
})
#line 157 "R/Class-Rd.R"
test_that('are_Rd_tags', {#@testing
    x <- list( s(list(), Rd_tag='\\item')
             , s(list(), Rd_tag='\\dots', class='Rd_tag')
             , s('test', Rd_tag='VERB')
             , s('this', Rd_tag='TEXT', class='Rd_string')
             )
    expect_identical( are_Rd_tags(x)
                    , c(TRUE, TRUE, FALSE, FALSE)
                    )
    expect_identical( are_Rd_tags(x, tags="\\dots")
                    , c(FALSE, TRUE, FALSE, FALSE)
                    )
    expect_identical( are_Rd_tags(x, strict=TRUE)
                    , c(FALSE, TRUE, FALSE, FALSE)
                    )
})
#line 187 "R/Class-Rd.R"
test_that('is_Rd', {#@testing
    x <- s(list( s("% comment", Rd_tag="COMMENT")
               , s(list( s("TEST", Rd_tag="VERB"))
                  , Rd_tag="\\name")
               ), class='Rd')
    expect_true(is_Rd(x))
    expect_true(is_Rd(unclass(x)))
    expect_true(is_Rd(x, strict=TRUE))
    expect_identical(is_Rd(list(), strict=TRUE)
                    , s(FALSE, msg="strict is TRUE but x does not inherit from class Rd"))
    expect_identical(is_Rd(unclass(x), strict=TRUE)
                    , s(FALSE, msg="strict is TRUE but x does not inherit from class Rd"))
    expect_identical(is_Rd(character(0), strict=FALSE)
                    , s(FALSE, msg="x is not a list"))
    expect_true(is_Rd(list()))

    expect_identical(is_Rd(unclass(x)[[2]])
                    , s(FALSE, msg='attr(x, "Rd_tag") is not NULL'))

})
#line 240 "R/Class-Rd.R"
test_that('is_valid_Rd_object', {#@testing
    expect_true(is_valid_Rd_object(list(), strict=FALSE))
    expect_false(is_valid_Rd_object(list(), strict=TRUE))

    x <- list( s("% comment", Rd_tag="COMMENT")
             , s(list( s("TEST", Rd_tag="VERB"))
                , Rd_tag="\\name")
             )
    expect_true(is_valid_Rd_object(x[[1]], strict=FALSE))
    expect_true(is_valid_Rd_object(x[[2]], strict=FALSE))
    expect_true(is_valid_Rd_object(x     , strict=FALSE))

    expect_false(is_valid_Rd_object(x[[1]], strict=TRUE))
    expect_false(is_valid_Rd_object(x[[2]], strict=TRUE))
    expect_false(is_valid_Rd_object(x     , strict=TRUE))

    expect_identical( validate_that(is_valid_Rd_object(c(x, TRUE), strict=FALSE))
                    , "Elements 3 of elements.are.valid are not true")
    expect_identical( validate_that(is_valid_Rd_object(c(x, TRUE), strict=NA))
                    , "Elements 1, 2, 3 of elements.are.valid are not true")
})
#line 261 "R/Class-Rd.R"
test_that('is_valid_Rd_object against parse_Rd results', {#@testing is_valid_Rd_object against parse_Rd results
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(is_valid_Rd_object(txt))
    expect_true(is_valid_Rd_object(txt, strict=FALSE, deep=TRUE))
})
#line 277 "R/Class-Rd.R"
test_that('is_Rd_newline', {#@testing
    expect_true(is_Rd_newline(Rd.newline))
    expect_true(is_Rd_newline(Rd.newline[[1]]))
    expect_false(is_Rd_newline(Rd.newline[[1]][[1]]))
    expect_true(is_Rd_newline(Rd.code.newline))
    expect_true(is_Rd_newline(Rd.code.newline[[1]]))
    expect_true(is_Rd_newline(Rd.code.newline[[1]]))
    expect_false(is_Rd_newline(.Rd(Rd.newline)))
    expect_false(is_Rd_newline(Rd_verb('\n')))
})
#line 291 "R/Class-Rd.R"
test_that('Rd_spans_multiple_lines', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(Rd_spans_multiple_lines(txt))
    expect_true(Rd_spans_multiple_lines(txt[['\\arguments']]))
    expect_false(Rd_spans_multiple_lines(txt[['\\arguments']][[3L]]))

    expect_false(Rd_spans_multiple_lines(Rd_text("hello world\n")))
    expect_true(Rd_spans_multiple_lines(Rd_text("hello\nworld\n")))

    x <- txt[[38]][2]
    expect_true(Rd_spans_multiple_lines(x))
    expect_false(Rd_spans_multiple_lines(unclass(x)))

    x <- Rd( Rd_rcode('\n')
           , Rd_rcode('value \\%if\\% proposition')
           , Rd_rcode('\n'))
    expect_true(Rd_spans_multiple_lines(x))
})
#line 314 "R/Class-Rd.R"
test_that('Rd_ends_with_newline', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(Rd_ends_with_newline(txt))

    x <- txt[[38]]

    expect_true(Rd_ends_with_newline(x))
    expect_false(Rd_ends_with_newline(x, TRUE))
})
#line 328 "R/Class-Rd.R"
test_that('Rd_starts_with_newline', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_false(Rd_starts_with_newline(txt))
    expect_true(Rd_starts_with_newline(txt[['\\arguments']]))
    expect_false(Rd_starts_with_newline(txt[['\\arguments']], TRUE))
})
