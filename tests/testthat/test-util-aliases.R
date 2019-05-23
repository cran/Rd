#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `util-aliases.R`')
#line 27 "R/util-aliases.R"
test_that('s', {#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
})
#line 44 "R/util-aliases.R"
test_that('cl', {#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
})
#line 58 "R/util-aliases.R"
test_that('undim', {#@testing
    x <- matrix(1:6, 2, 3)
    dimnames(x) <- list(rows = c('a', 'b'), cols = c('x', 'y', 'z'))

    expect_identical(undim(x), 1:6)
})
#line 76 "R/util-aliases.R"
test_that('named', {#@testing
    a <- 1L
    b <- TRUE

    val <- named(a,b)
    expect_identical(val, list(a=a, b=b))

    val <- named(a,b,c='hello')
    expect_identical(val, list(a=a, b=b, c='hello'))
})
#line 97 "R/util-aliases.R"
test_that('get_attr', {#@testing
    expect_identical(get_attr(s(list(), test='hello'), 'test'), 'hello')
    expect_null     (get_attr(s(list(), test='hello'), 'test2'))
    expect_identical(get_attr(s(list(), test='hello'), 'test3', 'world'), 'world')
})
#line 110 "R/util-aliases.R"
test_that('forward_attributes', {#@testing
    a <- s( list(Rd_symb("some"))
          , Rd_tag="\\keyword"
          , class=c("Rd_tag", 'Rd'))
    b <- forward_attributes(list(), a)
    expect_identical(attributes(a), attributes(b))

    a <- s( matrix(1:6, 2, 3)
          , class = 'rectangle'
          , another='shape'
          )
    b <- forward_attributes(list(), a)
    expect_true('dim' %in% names(attributes(a)))
    expect_false('dim' %in% names(attributes(b)))
    expect_identical( attributes(a)[names(attributes(b))]
                    , attributes(b)
                    )
})
#line 136 "R/util-aliases.R"
test_that('is_whitespace', {#@testing
    expect_true(is_whitespace(" "))
    expect_true(is_whitespace("\t"))
    expect_false(is_whitespace("t"))
    expect_false(is_whitespace(""))
})
