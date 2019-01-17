#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `convenience.R`')
#line 15 "R/convenience.R"
test_that('Rd_alias', {#@testing
    expect_Rd_tag(Rd_alias("x"), '\\alias')
    expect_identical(format(Rd_alias("x")), "\\alias{x}")
})
#line 25 "R/convenience.R"
test_that('Rd_aliases', {#@testing
    expect_Rd_bare(Rd_aliases(head(letters)))
    expect_identical(format(Rd_aliases(head(letters)))
                    , collapse(paste0("\\alias{", head(letters), "}"), '\n'))
})
#line 48 "R/convenience.R"
test_that('Rd_author', {#@testing
    expect_Rd_tag(Rd_author('joe blow'), "\\author")
    expect_identical(format(Rd_author('joe blow')), "\\author{joe blow}")

    suppress_messages(author <- Rd("My Name ", Rd_tag("\\email", "my.name@email")))
    expect_Rd_tag(. <- Rd_author(author), "\\author")
    expect_identical(format(.), "\\author{My Name \\email{my.name@email}}")

    expect_Rd_tag(. <- Rd_author(person("Given", "Family")), '\\author')
    expect_identical(format(.), "\\author{Given Family}")
})
#line 80 "R/convenience.R"
test_that('Rd_arguments', {#@testing
    items <- .Rd( Rd_item('a', 'first')
                , Rd_item('b', 'second'))
    expect_Rd_tag(Rd_arguments(items=items, indent=FALSE), '\\arguments')
    expect_length(Rd_arguments(items=items, indent=FALSE), 5L)
    expect_identical(format(Rd_arguments(items=items, indent=TRUE, indent.with='  '))
                    , "\\arguments{" %\%
                      "  \\item{a}{first}" %\%
                      "  \\item{b}{second}" %\%
                      "}")

})
#line 98 "R/convenience.R"
test_that('Rd_code', {#@testing
    expect_Rd_tag(Rd_code('code'), '\\code')
    expect_Rd_string(Rd_code('code')[[1]], 'RCODE')
    expect_identical( format(Rd_code('code'))
                    , "\\code{code}")

    expect_identical( Rd_code(Rd_text("hello_world"))
                    , Rd_tag("\\code", Rd_rcode("hello_world")))
})
#line 112 "R/convenience.R"
test_that('Rd_concept', {#@testing
    expect_Rd_tag(Rd_concept('testing'), '\\concept')
    expect_identical( format(Rd_concept('testing'))
                    , "\\concept{testing}")
    expect_Rd_string(Rd_concept('testing')[[1]], 'TEXT')
})
#line 127 "R/convenience.R"
test_that('Rd_concepts', {#@testing
    val <- Rd_concepts(c('test1', 'test2'))
    expect_Rd_bare(val)
    expect_length(val, 3L)
    expect_Rd_tag(val[[3]], '\\concept')
    expect_Rd_string(val[[2]], 'TEXT')
    expect_identical( format(val)
                    , "\\concept{test1}\n\\concept{test2}")
    expect_Rd_string(val[[1]][[1]], 'TEXT')

    expect_error(Rd_concepts(TRUE))
})
#line 145 "R/convenience.R"
test_that('Rd_description', {#@testing
    x <- strwrap(collapse(stringi::stri_rand_lipsum(3), '\n\n'), 72)
    val <- Rd_description(Rd_text(collapse(x, '\n')))
    expect_Rd_tag(val, '\\description')
    expect_true(length(val) > 5L)
    expect_true(is_Rd_newline(val[[1]]))
    expect_Rd_string(val[[2]], 'TEXT')
})
#line 162 "R/convenience.R"
test_that('Rd_examples', {#@testing
    expect_error(Rd_examples(Rd_text('example'))
                , class = "Rd-error-assertion failure")
    val <- Rd_examples( "Rd_alias('alias')\n"
                      , "Rd_concept('testing')"
                      )
    expect_Rd_tag(val, '\\examples')
    expect_length(val, 3L)
    expect_true(is_Rd_newline(val[[1]]))
    expect_Rd_string(val[[2]], 'RCODE')
})
#line 200 "R/convenience.R"
test_that('Rd_item', {#@testing
    expect_Rd_bare(Rd_item('an item'))
    expect_length(Rd_item('an item'), 2)
    expect_identical(format(Rd_item('an item')), "\\item an item")

    expect_Rd_tag(val <- Rd_item('a', 'the first letter of the alphabet'), '\\item')
    expect_identical(format(val), "\\item{a}{the first letter of the alphabet}")

    suppress_messages(item <- Rd(Rd_code('a'), ': the first letter of the alphabet'))
    expect_Rd_bare(val <- Rd_item(item))
    expect_Rd_tag(val[[1]], '\\item')
    expect_length(val[[1]], 0)
    expect_identical(format(val), "\\item \\code{a}: the first letter of the alphabet")

    val <- Rd_item( .Rd(Rd_code('a'), Rd_text(':'))
                  , Rd_text('the first letter of the alphabet')
                  )
    expect_Rd_tag(val, '\\item')
    expect_length(val, 2L)
    expect_Rd_bare(val[[1L]])
    expect_Rd_bare(val[[2L]])
    expect_identical(format(val), "\\item{\\code{a}:}{the first letter of the alphabet}")
})
#line 236 "R/convenience.R"
test_that('Rd_keyword', {#@testing
    expect_error(Rd_keyword(TRUE), class="Rd-error-assertion failure")
    expect_error(Rd_keyword("hibbidty"), class="Rd-error-assertion failure")

    expect_Rd_tag(Rd_keyword('documentation'), '\\keyword')
    expect_identical(format(Rd_keyword('documentation')), "\\keyword{documentation}")
})
#line 253 "R/convenience.R"
test_that('Rd_keywords', {#@testing
    expect_error(Rd_keywords(TRUE), class="Rd-error-assertion failure")
    expect_error(Rd_keywords("hibbidty"), class="Rd-error-assertion failure")

    expect_Rd_bare(. <-Rd_keywords(c('documentation', 'utilities')))
    expect_length(., 3)
    expect_Rd_tag(.[[1]], '\\keyword')
    expect_identical( format(.)
                    , "\\keyword{documentation}\n\\keyword{utilities}")
})
#line 271 "R/convenience.R"
test_that('Rd_name', {#@testing
    expect_Rd_tag(Rd_name('bob'), '\\name')
    expect_identical(format(Rd_name('bob')), '\\name{bob}')
})
#line 281 "R/convenience.R"
test_that('Rd_title', {#@testing
    expect_Rd_tag(Rd_title("A Title String"), '\\title')
    expect_identical( format(Rd_title("A Title String"))
                    , '\\title{A Title String}')
})
#line 299 "R/convenience.R"
test_that('Rd_usage', {#@testing
    . <- Rd_usage(Rd_rcode("Rd_usage(..., content=Rd(...))"))
    expect_Rd_tag(., '\\usage')
    expect_identical( format(.)
                    , '\\usage{Rd_usage(..., content=Rd(...))}')

    . <- Rd_usage( Rd_rcode("Rd_usage(..., content=Rd(...))")
                 , "Rd_alias(alias)"
                 )
    expect_Rd_tag(., '\\usage')
    expect_identical( format(.)
                    , '\\usage{' %\%
                      'Rd_usage(..., content=Rd(...))' %\%
                      'Rd_alias(alias)' %\%
                      '}')
})
#line 320 "R/convenience.R"
test_that('Rd_value', {#@testing
    expect_Rd_tag(. <- Rd_value(Rd(Rd_text("A strings describing the return value.")))
                 , '\\value')
    expect_identical( format(.)
                    , '\\value{A strings describing the return value.}')

    value <- Rd( Rd_text("A value ")
               , Rd_tag('\\link', Rd_text("tag"), opt=Rd_text('=Rd_tag'))
               , Rd_text("."))
    val <- Rd_value(value)
    expect_Rd_tag(val, '\\value')
    expect_identical(format(val), '\\value{A value \\link[=Rd_tag]{tag}.}')
})
