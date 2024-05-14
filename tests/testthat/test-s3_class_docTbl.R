test_that("`new_docTbl` works", {
  expect_no_error(new_docTbl(cols = c(a = 'A')))
})

test_that("`validate_docTbl` works", {

  test_obj <- new_docTbl(cols = c(a = 'A'))
  expect_no_error({validate_docTbl(obj = test_obj)})
  expect_true({validate_docTbl(obj = test_obj, bool_out = TRUE)})

})

test_that("`docTbl` works", {
  expect_no_error({docTbl(cols = c(a = 'A'))})
})

test_that("`validate_docTbl` catches class path errors", {

  msg <- "`obj` must inherit from 'docTbl'"
  expect_error({validate_docTbl(obj = 0)}, msg, fixed = TRUE)
  expect_equal({validate_docTbl(obj = 0, throw_err = FALSE)}, msg)

})
