#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `construction.R`')
#line 16 "R/construction.R"
test_that('Rd_split', {#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))

    val <- Rd_split(txt)
    expect_is(val, 'list')
    expect_is_not(val, 'Rd')

    expect_all_inherit(val, 'Rd')
    expect_true(all_are_exactly(val, 'Rd'))

    x <- txt['\\examples'][[1]]
    y <- Rd_split(x)
    expect_is_not(y, 'Rd_tag')
    expect_is_not(y, 'Rd')
    expect_equal(get_Rd_tag(y), "\\examples")
    expect_true(attr(y, "split"))

    expect_is_exactly(y[[1]], 'Rd')
    expect_identical(y[[1]][[1]], x[[1]])
    expect_length(y, 29L)

    expect_equal( fwd(unlist(y, recursive = FALSE), x), x)
})
#line 105 "R/construction.R"
test_that('check_content(., .check=NA)', {#@testing check_content(., .check=NA)
    expect_identical(test_check_content(content=list()), .Rd())
    expect_message( val <- test_check_content("string")
                  , class = "Rd::test_check_content-message-class inference")
    expect_message( val <- test_check_content("string")
                  , regexp = "There was 1 character string at position 1" %<<%
                             "of content that was converted to an Rd_string/TEXT\\.")
    expect_identical(val, .Rd(Rd_text('string')))

    expect_message( val <- test_check_content("hello", "world")
                  , regexp = "There were 2 character strings in content" %<<%
                             "that were converted to Rd_string/TEXT\\.")

    expect_warning( val <- test_check_content(c("hello", "world"), verbose=FALSE)
                  , class = "Rd::test_check_content-warning-collapsing_character_vector")

    expect_message( val <- test_check_content(Rd_text("hello"), list(Rd_text("world")))
                  , "There was 1 list at position 2 of content that was converted to an Rd\\."
                  )
    expect_message( val <- test_check_content( list(Rd_text("hello"))
                                             , s(list(Rd_rcode("world")), Rd_tag="\\code")
                                             )
                  , "There were 2 lists in content that were converted to Rd objects\\."
                  )
    expect_identical(val, .Rd( .Rd(Rd_text("hello"))
                              , Rd_tag("\\code", Rd_rcode("world"))
                              ))

})
#line 134 "R/construction.R"
test_that('check_content(., .check=FALSE)', {#@testing check_content(., .check=FALSE)
    expect_identical(test_check_content(content=iris, .check=FALSE), iris)
})
#line 137 "R/construction.R"
test_that('check_content(., .check=TRUE)', {#@testing check_content(., .check=TRUE)
    expect_identical(test_check_content(content=list(), .check=TRUE), .Rd())
    expect_error( test_check_content("string", .check=TRUE)
                , "Elements 1 of elements.are.valid are not true")
    expect_error( test_check_content("string", .check=TRUE)
                , class = "Rd::test_check_content-error-assertion failure")

    expect_error( test_check_content("hello", "world", .check=TRUE)
                , "Elements 1, 2 of elements.are.valid are not true")
    expect_error( test_check_content("hello", "world", .check=TRUE)
                , class = "Rd::test_check_content-error-assertion failure")

    content <- list( list(Rd_text("hello"))
                   , s(list(Rd_rcode("world")), Rd_tag="\\code")
                   )
    expect_error( test_check_content( content =content
                                    , .check=TRUE)
                , "Elements 1, 2 of elements.are.valid are not true")
})
#line 209 "R/construction.R"
test_that('Rd_text', {#@testing
    val <- Rd_text('testing')
    expect_is_exactly(val, 'Rd_string')
    expect_is_not(val, 'Rd')
    expect_is_not(val, 'Rd_tag')
    expect_is_not(val, 'Rd_TEXT')

    expect_true(is.character(val))
    expect_false(is.list(val))

    val <- Rd_string('some(code)', 'RCODE')
    expect_equal(attr(val, 'Rd_tag'), 'RCODE')
    val <- Rd_string('some(code)', 'R')
    expect_is(val, 'Rd_string')
    expect_equal(attr(val, 'Rd_tag'), 'RCODE')

    x <- Rd_text(collapse(stringi::stri_rand_lipsum(3), '\n\n'))
    expect_is(x, 'Rd_string')
    expect_true(is_Rd_string(x, 'TEXT'))
    expect_is_not(x, 'Rd')
    expect_is_not(x, 'Rd_tag')
    expect_is_not(x, 'Rd_TEXT')

    expect_error(x <- Rd_text(c( 'hello', '\n', ' big', '\n', '  wide', '\n', '   world'))
                , class="Rd-error-assertion failure")

    x <- Rd_text("     hello world")
    expect_is(x, 'Rd_string')
    expect_length(x, 1L)
})
#line 248 "R/construction.R"
test_that('Rd_rcode', { #@testing
    val <- Rd_rcode('1+1==2')
    expect_identical(val, s('1+1==2', Rd_tag='RCODE', class='Rd_string'))
})
#line 265 "R/construction.R"
test_that('Rd_symb', { #@testing
    val <- Rd_symb('name')
    expect_identical(val, s('name', Rd_tag='VERB', class='Rd_string'))
})
#line 283 "R/construction.R"
test_that('Rd_rcode, Rd_symb, and Rd_comment', {#@testing Rd_rcode, Rd_symb, and Rd_comment
    expect_error(Rd_comment("testing"), "Ill-formed comment", class = "Rd-error-assertion failure")
    expect_true(is_Rd_string(Rd_comment("% comment"), "COMMENT", strict = TRUE))
})
#line 346 "R/construction.R"
test_that('Rd', {#@testing
    a <- "test"
    expect_message( b <- Rd(a, verbose=TRUE)
                  , "There was 1 character string at position 1" %<<%
                    "of content that was converted to an Rd_string/TEXT." )
    expect_is_exactly(b, 'Rd')
    expect_true(is_Rd_string(b[[1]], 'TEXT'))

    # a <- stringi::stri_rand_lipsum(3)
    # b <- Rd(collapse(a, '\n\n'), wrap.lines=TRUE)
    # expect_is_exactly(b, 'Rd')
    # expect_identical(mode(b), 'list')
    # expect_true(length(b) > 5)

    c <- Rd(Rd_text(a))
    expect_is_exactly(c, 'Rd')
    d <- Rd(c)
    expect_is_exactly(d, 'Rd')
    expect_identical(c, d)

    expect_error(Rd(NULL))

    expect_is(Rd(), 'Rd')
    expect_length(Rd(), 0L)

    expect_message(x <- Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n'), verbose=TRUE))
    expect_Rd_bare(x)
    expect_Rd_string(x[[1L]], 'TEXT')
    expect_true(all(are_Rd_strings(x, 'TEXT')))

    x <- Rd(Rd_text('text'))
    expect_Rd_bare(x)
    expect_Rd_string(x[[1]], 'TEXT')

    x <- Rd("Multiple ", "Character strings", " to convert")
    expect_Rd_bare(x)
    expect_Rd_string(x[[1]], "TEXT")
    expect_length(x, 1)
    expect_true(all(are_Rd_strings(x, 'TEXT')))
})
#line 392 "R/construction.R"
test_that('Class-Rd', {#@testing Class-Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')
    expect_true(is_valid_Rd_object(txt))
    expect_true(validObject(txt, TRUE, complete = FALSE))
})
#line 466 "R/construction.R"
test_that('Rd_tag', {#! @testing
    expect_error(Rd_tag(NULL, 'test'), "tag is not a string")
    expect_error(Rd_tag(c('a', 'b'), 'test'), "tag is not a string")
    expect_error(Rd_tag(1, 'test'), "tag is not a string")
    expect_warning( val <- Rd_tag('name', Rd_text('my name'))
                  , class = "Rd::Rd_tag-warning-invalid Rd tag"
                  )
    expect_warning( val <- Rd_tag('name', Rd_text('my name'))
                  , regexp = "`tag` is expected to start with a backslash\\." %<<%
                             "Converted to .{1,4}name.{1,4}\\."
                  )
    expect_is(val, "Rd_tag")
    expect_identical( Rd_tag('name', Rd_text('my name'), .check = FALSE)
                    , s( list(Rd_text("my name"))
                       , Rd_tag = "\\name"
                       , class  = 'Rd_tag'
                       ))

    expect_error(Rd_tag('name', Rd_text('my name'), .check = TRUE)
                , class="Rd-error-invalid Rd tag" )
})
#line 487 "R/construction.R"
test_that('Rd_tag', {#@testing
    x <- Rd_tag('\\item', Rd(Rd_text('arg')), Rd(Rd_text("an agrument")))
    expect_length(x, 2L)

    val <- Rd_tag('\\link', Rd_text('dest'), opt=Rd_text('pkg'))
    expect_Rd_string(attr(val, 'Rd_option'), 'TEXT')

    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[pkg]{dest}")
})
#line 497 "R/construction.R"
test_that('Rd_tag(indent=TRUE)', {#@testing Rd_tag(indent=TRUE)
    val <- Rd_tag('\\description'
                 , Rd_text('line 1\n')
                 , Rd_text('line 2\n')
                 , indent=TRUE
                 , indent.with='    '
                 )
    exp <- Rd_tag('\\description'
                 , Rd_text('\n')
                 , Rd_text('    line 1\n')
                 , Rd_text('    line 2\n')
                 , .check=FALSE
                 )
    expect_Rd_tag(val, '\\description')
    expect_length(val, 3L)
    expect_identical(val, exp)
    expect_identical( format(val)
                    , "\\description{" %\%
                      "    line 1" %\%
                      "    line 2" %\%
                      "}"
                    )
})
#line 527 "R/construction.R"
test_that('validObject(Rd_tag)', {#@testing validObject(Rd_tag)
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')

    desc <- Rd_get_element(txt, '\\description')
    expect_Rd_tag(desc, '\\description')
    expect_true(is_valid_Rd_object(desc))
    expect_true(validObject(desc, TRUE))
})
#line 536 "R/construction.R"
test_that('Rd_tag edge case all elements are Rd.', {#@testing Rd_tag edge case all elements are Rd.
    content <- collapse(strwrap(stringi::stri_rand_lipsum(1), 72), '\n')
    rd <- Rd_tag( "\\section"
                , content = list( Rd("Title")
                                , Rd(content)
                                )
                )
    expect_Rd_tag(rd, '\\section')
    expect_Rd_bare(rd[[1]])
    expect_Rd_bare(rd[[2]])
    expect_true(head(rd[[2]], 1) == '\n')
    expect_true(tail(rd[[2]], 1) != '\n')
})
