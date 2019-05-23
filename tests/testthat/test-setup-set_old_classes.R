#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `setup-set_old_classes.R`')
#line 24 "R/setup-set_old_classes.R"
test_that('Old classes', {#@testing Old classes
    expect_true(is(Rd_text('text'), 'Rd_string'))
    expect_true(is(Rd_text('text'), 'Rd_object'))

    expect_true(is(.Rd(Rd_text('text')), 'Rd'))
    expect_true(is(.Rd(Rd_text('text')), 'Rd_list'))
    expect_true(is(.Rd(Rd_text('text')), 'Rd_object'))

    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_tag'))
    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_list'))
    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_object'))

    expect_false(is(Rd_text('text'), 'character'))
    expect_false(is(.Rd(Rd_text('text')), 'Rd_tag'))
    expect_false(is(.Rd(Rd_text('text')), 'list'))
    expect_false(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd'))
    expect_false(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'list'))
})
