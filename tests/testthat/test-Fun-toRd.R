#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `Fun-toRd.R`')
#line 33 "R/Fun-toRd.R"
test_that('toRd with character return vectors', {#@testing toRd with character return vectors
    expect_Rd_string(val <- toRd('character'), NULL)
    expect_identical(val, Rd_text("character"))

    val <- toRd(c( "use the \\backslash to escape.\n"
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd(Rd_text("use the \\\\backslash to escape." %\%
                                 "and '\\{\\}' to group."
                                 )))
})
#line 46 "R/Fun-toRd.R"
test_that('INACTIVE', {#@testing INACTIVE
    toRd.test_class <- function(obj, ...)obj
    expect_error( toRd(cl(1L, 'test_class'))
                , class = "Rd-error")
})
#line 60 "R/Fun-toRd.R"
test_that('toRd.NULL', {#@testing
    expect_identical(toRd(NULL), Rd())
})
#line 85 "R/Fun-toRd.R"
test_that('toRd.list', {#@testing
    l <- list('\\hello ', '%world')
    expect_identical( toRd(l)
                    , .Rd( Rd_text("\\\\hello ")
                         , Rd_text("\\%world")
                         )
                    )

    l <- list( first  = Rd("first text")
             , second = Rd("second text")
             , third = NULL
             )
    val <- toRd(l)
    expect_is(val, 'Rd')
    expect_is(val[[1]], 'Rd_string')
    expect_length(val, 2)

    m <- list( first  = Rd("first text")
             , second = Rd_text("second text")
             , third = NULL
             )
    val2 <- toRd(m, unnest=TRUE)
    expect_identical(val, val2)

    val3 <- toRd(m)
    expect_is(val3, 'Rd')
    expect_is(val3[[1]], 'Rd')
    expect_is(val3[[1]][[1]], 'Rd_string')
    expect_length(val3, 3)
})
#line 130 "R/Fun-toRd.R"
test_that('toRd.(Rd|Rd_tag|Rd_string)', {#@testing toRd.(Rd|Rd_tag|Rd_string)
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)

    obj <- obj[[1]]
    expect_Rd_string(obj, 'TEXT')
    expect_identical(toRd(obj), obj)

    obj <- Rd_tag("\\rd")
    expect_Rd_tag(obj, '\\rd')
    expect_identical(toRd(obj), obj)
})
#line 173 "R/Fun-toRd.R"
test_that('toRd.person', {#@testing
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_Rd_string(val, 'TEXT')
    expect_length(val, 1L)

    expect_equal( as.character(val)
                , 'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
})
#line 191 "R/Fun-toRd.R"
test_that('toRd.name', {#@testing
    obj <- as.name('test.name')
    val <- toRd(obj)
    expect_Rd_string(val, 'VERB')
    expect_identical(toRd(obj), Rd_symb('test.name'))
})
