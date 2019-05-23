#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `indent.R`')
#line 59 "R/indent.R"
test_that('Rd_clean_indent expected errors', {#@testing Rd_clean_indent expected errors
    expect_error( Rd_clean_indent('a')
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent('  \n')
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(c(' ', ' '))
                , ".+indent\\.with.+ does not conform to a non-empty string"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(s(c(' ', ' '), Rd_tag='TEXT', class='Rd_string'))
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(Rd_verb('  '))
                , "'Rd_tag' attribute is not in allowed tags"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("  "), Rd_alias('test')))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_verb('  ')))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(.Rd(Rd_text("  "))))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("...")))
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("  \n")))
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(TRUE)
                , "bad indent"
                , class="Rd::Rd_clean_indent-error")
})
#line 93 "R/indent.R"
test_that('Rd_clean_indent warning', {#@testing Rd_clean_indent warning
    expect_warning( Rd_clean_indent(.Rd(Rd_text("\t")))
                  , "Tabs are discouraged from being used for indentation")
    expect_warning( Rd_clean_indent(.Rd(Rd_text("\t")))
                  , class="Rd::Rd_clean_indent-warning-guidelines_violation")
})
#line 99 "R/indent.R"
test_that('Rd_clean_indent expected output', {#@testing Rd_clean_indent expected output
    expect_identical(Rd_clean_indent('  '), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd_text('  ')), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd(Rd_text('  '))), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd(Rd_rcode('  '))), .Rd(Rd_rcode('  ')))
    expect_identical(Rd_clean_indent(Rd_rcode('  ')), .Rd(Rd_rcode('  ')))
})
#line 162 "R/indent.R"
test_that('Rd_indent for Rd_tag', {#@testing Rd_indent for Rd_tag
    rd <- Rd_alias('jane doe')
    expect_identical(Rd_indent(rd, .Rd.default.indent), rd)

    rd <- Rd_tag('\\description'
                , Rd_text("a description without\n")
                , Rd_text("a leading newline")
                , .check=FALSE
                )[2:3]
    expect_identical( format(Rd_indent(rd, '             ', indent.first = FALSE))
                    , "\\description{a description without" %\%
                      "             a leading newline" %\%
                      "}")
    expect_identical( Rd_indent(rd, .Rd.default.indent, no.first = FALSE)
                    , Rd_tag('\\description'
                            , Rd_text("  a description without\n")
                            , Rd_text("  a leading newline")
                            , .check=FALSE
                            )[2:3] )
})
#line 182 "R/indent.R"
test_that('Rd_indent errors', {#@testing Rd_indent errors
    expect_error(Rd_indent('hello'), class="Rd-error-assertion failure")
    expect_error(Rd_indent(Rd_text('hello')), class="Rd-error-assertion failure")
})
#line 186 "R/indent.R"
test_that('Rd_indent expected output.', {#@testing Rd_indent expected output.
    val <- Rd_indent(Rd(Rd_text('hello')), .Rd.default.indent)
    expect_identical(val, .Rd(Rd_text('  hello')))

    expect_warning( val <- Rd_indent(Rd(Rd_text('hello')), '\t')
                  , class = 'Rd-warning-guidelines_violation')
    expect_identical(val, .Rd(Rd_text('\thello')))

    x <- strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n')
                , width = 72)

    y <- collapse(x, '\n')
    val <- Rd_indent(Rd_tag('\\description', Rd_text(y)))
    expect_Rd_tag(val, '\\description')
    expect_true(length(val)> 5)
    expect_equal( tail(head(as.character(.Rd(val)), -1), -3)
                , paste0(ifelse(x == '', '', '  '), x, '\n'))
})
#line 204 "R/indent.R"
test_that('with parsed Rd', {#@testing with parsed Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(txt)
    x <- Rd_get_element(txt, '\\examples')

    val <- Rd_indent(x, indent.with = Rd_rcode("    "))
    expect_Rd_tag(val, '\\examples')
    expect_identical( stringi::stri_split_lines1(format(val))
                    , stringi::stri_split_lines1(format(x)) %>%
                        ifelse(. %in% c('\\examples{', '}', ''), ., paste0("    ", .))
                    )
    expect_equal(length(val), length(x))

    x <- Rd_get_element(txt, "\\arguments")[[9]]
    val <- Rd_indent(x, indent.with='  ')
    expect_Rd_tag(val, '\\item')
    exp <- s(.Rd( Rd(Rd_text('n'))
                , .Rd( x[[2]][[1]], x[[2]][[2]], x[[2]][[3]]
                     , Rd_text('  '), x[[2]][[4]]))
            , Rd_tag = "\\item", class='Rd_tag')
    exp <- Rd_canonize_text(exp)
    expect_identical(val, exp)

    x <- Rd_untag(Rd_get_element(txt, "\\arguments")[8:10])
    val <- Rd_indent(x, indent=TRUE, indent.with='  ', no.first=TRUE)
    expect_equal(format(val)
                , "  \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}\n" )

    x <- Rd_get_element(txt, "\\arguments")
    val <- Rd_indent(x, indent=TRUE, indent.with='  ')
    expect_equal( format(val)
                , "\\arguments{" %\%
                  "    \\item{x, q}{vector of quantiles.}" %\%
                  "    \\item{p}{vector of probabilities.}" %\%
                  "    \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}" %\%
                  "    \\item{mean}{vector of means.}" %\%
                  "    \\item{sd}{vector of standard deviations.}" %\%
                  "    \\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}" %\%
                  "    \\item{lower.tail}{logical; if TRUE (default), probabilities are" %\%
                  "      \\eqn{P[X \\le x]} otherwise, \\eqn{P[X > x]}.}" %\%
                  "}"
                )
})
