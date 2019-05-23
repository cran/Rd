#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `S3_extensions.R`')
#line 56 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('Rd_get_element by the numbers', {#@testing Rd_get_element by the numbers
    x <- .Rd( Rd_comment("% a comment")
            , Rd_text("\n")
            , Rd_tag("\\name", Rd_symb("testing"))
            , Rd_text("\n")
            , Rd_tag("\\item", .Rd(Rd_symb("name"))
                             , .Rd(Rd_text("A description")))
            )
    expect_is(Rd_get_element(Rd_unclass(x), 1), 'Rd_string')
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 1), 'COMMENT', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 2), 'TEXT', strict = TRUE))
    expect_true(is_Rd_tag(Rd_get_element(Rd_unclass(x), 3), '\\name', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), c(3,1)), 'VERB', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 4), 'TEXT', strict = TRUE))
    expect_true(is_Rd_tag(Rd_get_element(Rd_unclass(x), 5), '\\item', strict = TRUE))
    expect_true(is_Rd(Rd_get_element(Rd_unclass(x), c(5, 1)), strict = TRUE))
    expect_true(is_Rd(Rd_get_element(Rd_unclass(x), c(5, 2)), strict = TRUE))
    expect_true(is_Rd(Rd_get_element(2, x=
                      Rd_get_element(Rd_unclass(x), 5))
                     , strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), c(5,2,1)), strict = TRUE))
    expect_is_exactly(Rd_get_element(Rd_unclass(x), c(5,2,1,1)), 'character')
})
#line 79 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('Rd_get_element by the tag', {#@testing Rd_get_element by the tag
    test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)
    txt <- Rd_unclass(txt)

    expect_is_exactly(txt, 'list')

    expect_error( Rd_get_element(txt, 1,2,3)
                , class="Rd::Rd_get_element-error-invalid_subscripts")

    expect_Rd_tag(Rd_get_element(txt, '\\arguments'), '\\arguments')
    expect_Rd_tag(Rd_get_element(txt, '\\details'), '\\details')

    expect_error( Rd_get_element(txt, 'bibidy')
                , class = "Rd::Rd_get_element-error-not_found"
                )
    expect_error( Rd_get_element(txt, 'COMMENT')
                , class = "Rd::Rd_get_element-error-multiple_found"
                )
})
#line 100 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('Rd_get_element fringe cases', {#@testing Rd_get_element fringe cases
    bad.rd <- s(list( a <- Rd_comment("% This is not a valid Rd")
                    , b <- Rd_text('\n')
                    , c <- s(FALSE, Rd_tag='bad logical', class='flag')
                    ))
    expect_false(is_valid_Rd_list(bad.rd))
    expect_identical(Rd_get_element(bad.rd, 'COMMENT'), a)
    expect_identical(Rd_get_element(bad.rd, 2), b)
    expect_identical(Rd_get_element(bad.rd, 3), c)
    expect_identical(Rd_get_element(bad.rd, 'bad logical'), c)
})
#line 129 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('[[.Rd & [.Rd', {#@testing [[.Rd & [.Rd
    test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)

    expect_is_exactly(txt, 'Rd')

    i <- which(sapply(txt, get_Rd_tag) == '\\arguments' )

    expect_is_exactly(txt[[i]], 'Rd_tag')
    expect_Rd_tag(txt[[i]], '\\arguments')
    expect_Rd_string(txt[[i]][[1]], 'TEXT')
    expect_Rd_string(txt[[i]][[2]], 'TEXT')
    expect_Rd_tag(txt[[i]][[3]], '\\item')
    expect_Rd_bare(txt[[i]][[c(3,1)]])
    expect_Rd_string(txt[[i]][[c(3,1,1)]], 'TEXT')
    expect_Rd_string(txt[[i]][[c(3,2,1)]], 'TEXT')

    expect_Rd_tag(txt[[i]][[3L]], '\\item')
    expect_Rd_bare(txt[[i]][[3L]][[1L]])

    expect_Rd_string(txt[[2]], "TEXT")
    expect_Rd_string(txt[[c(48, 11)]], "TEXT")
})
#line 168 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('Rd_subset', {#@testing Rd_subset
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(is_valid_Rd_list(txt))
    expect_Rd_bare(txt)

    expect_Rd_bare(Rd_subset(txt, 1:6))
    expect_Rd_bare(Rd_subset(txt, 1))
    expect_Rd_bare(Rd_subset(txt, '\\arguments'))
    expect_length(Rd_subset(txt, 1:6), 6)
    expect_length(Rd_subset(txt, 1), 1)
    expect_length(Rd_subset(txt, '\\arguments'), 1)

    args <- Rd_get_element(txt, '\\arguments')
    expect_Rd_tag(args, '\\arguments')
    expect_Rd_tag(Rd_subset(args, 1:7), '\\arguments')
    expect_Rd_tag(Rd_subset(args, "\\item"), '\\arguments')
    expect_length(Rd_subset(args, "\\item"), 7)

    expect_true(all(purrr::map_chr(Rd_subset(args, "\\item"), get_Rd_tag) == '\\item'))
})
#line 192 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('`[.Rd` and `[.Rd_tag`', {#@testing `[.Rd` and `[.Rd_tag`
    txt <- Rd_rm_srcref(tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd')))
    expect_Rd_bare(txt)

    expect_error(txt[1,2,3], class="Rd::[.Rd-error-invalid_subscripts")

    expect_Rd_bare(val <- txt[1:2])
    expect_true(is_valid_Rd_list(val))

    expect_identical(txt['bibidy'], Rd())
    expect_identical(txt[logical(0)], Rd())
    expect_identical(txt[character(0)], Rd())
    expect_identical(txt[integer(0)], Rd())
})
#line 210 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('`[.Rd` and `[.Rd_tag`', {#@testing `[.Rd` and `[.Rd_tag`
    txt <- Rd_rm_srcref(tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd')))
    expect_Rd_bare(txt)

    i <- which(sapply(txt, get_Rd_tag) == '\\arguments' )
    args <- txt[[i]]
    expect_Rd_tag(args, '\\arguments')
    expect_Rd_tag(args[1:7], '\\arguments')
    expect_length(args[1:7], 7)

    expect_Rd_tag(args["\\item"], '\\arguments')
    expect_length(args["\\item"], 7)

    expect_true(all(purrr::map_chr(Rd_subset(args, "\\item"), get_Rd_tag) == '\\item'))

    expect_equal(args[    'bibidy'], Rd_tag("\\arguments"))
    expect_equal(args[  logical(0)], Rd_tag("\\arguments"))
    expect_equal(args[character(0)], Rd_tag("\\arguments"))
    expect_equal(args[  integer(0)], Rd_tag("\\arguments"))
})
#line 246 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('Rd_c', {#@testing
    x <- .Rd(Rd_text('testing'))

    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_alias('name'))
                    , Rd(Rd_text('  '), Rd_alias('name'))
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), 'name')
                    , .Rd(Rd_text('  '), 'name')
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_alias('name'), Rd_text('\n'))
                    , .Rd(Rd_text('  '), Rd_alias('name'), Rd_text('\n'))
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_text('name'), Rd_text('\n'))
                    , .Rd(Rd_text('  '), Rd_text('name'), Rd_text('\n'))
                    )
})
#line 271 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('c.Rd_string', {#@testing
    expect_true('c.Rd_string' %in% as.character(methods('c')))
    expect_identical(c(Rd_text("hello"), ' ', Rd_text("world"))
                    , Rd_text("hello world"))
    expect_identical(c(Rd_text("hello"), ' ', Rd_rcode("world"))
                    , c("hello", ' ', "world"))
})
#line 288 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('as.character, format, and print.', {#@testing as.character, format, and print.
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    desc <- Rd_get_element(txt, "\\description")

    expect_Rd_tag(desc, '\\description')
    expect_is_exactly(desc, 'Rd_tag')


    expect_identical( format(.Rd(desc))
                    , "\\description{" %\%
                      "  Density, distribution function, quantile function and random" %\%
                      "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                      "  and standard deviation equal to \\code{sd}." %\%
                      "}"
                    )
    expect_identical( format(desc)
                    , "\\description{" %\%
                      "  Density, distribution function, quantile function and random" %\%
                      "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                      "  and standard deviation equal to \\code{sd}." %\%
                      "}"
                    )

    expect_output(print(desc)
                 , "\\description{" %\%
                   "  Density, distribution function, quantile function and random" %\%
                   "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                   "  and standard deviation equal to \\code{sd}." %\%
                   "}"
                 , fixed=TRUE)
})
#line 336 "C:/rdtf/Rd/R/S3_extensions.R"
test_that('compare.Rd_tag', {#@testing
    constructed <- Rd_name("test")
    txt <- "\\name{test}"
    parsed <- tools::parse_Rd(textConnection(txt))[[1]]
    expect_true(is_Rd_tag(parsed, "\\name"))

    expect_identical( compare.Rd(constructed, parsed)
                    , cl(list(equal= TRUE, message= "TRUE"), 'comparison')
                    )
    val <- compare.Rd(constructed, parsed, ignore.srcref = FALSE)
    expect_false(val$equal)
    expect_equal(constructed, parsed)
})
