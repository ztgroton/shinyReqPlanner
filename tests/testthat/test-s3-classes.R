
# docTbl ----
test_that("`new_docTbl` works", {
  test_data <- emptyDocTemplate(cols = c(a = 'A'))
  expect_no_error(new_docTbl(data = test_data))
})

test_that("`validate_docTbl` works", {

  test_data <- emptyDocTemplate(cols = c(a = 'A'))
  test_obj <- new_docTbl(data = test_data)

  expect_no_error({validate_docTbl(obj = test_obj)})
  expect_true({validate_docTbl(obj = test_obj, bool_out = TRUE)})

})

test_that("`docTbl` works", {
  test_data <- emptyDocTemplate(cols = c(a = 'A'))
  expect_no_error({docTbl(data = test_data)})
})

test_that("`validate_docTbl` catches attribute and class path errors", {

  msg <- c(
    "`obj` must be valid data.frame",
    "`obj` must inherit from 'docTbl'",
    "`attr(obj, 'colstrings')` was not found"
  )
  paste_msg <- paste0('\n', paste(msg, collapse = '\n'))

  expect_error({validate_docTbl(obj = 0)}, paste_msg, fixed = TRUE)
  expect_equal({validate_docTbl(obj = 0, throw_err = FALSE)}, msg)

})

# docUserStory ----
test_that("`new_docUserStory` works", {
  expect_no_error(new_docUserStory())
})

test_that("`validate_docUserStory` works", {

  test_obj <- new_docUserStory()
  expect_no_error({validate_docUserStory(obj = test_obj)})
  expect_true({validate_docUserStory(obj = test_obj, bool_out = TRUE)})

})

test_that("`docUserStory` works", {
  expect_no_error({docUserStory()})
})

# docFuncReq ----
test_that("`new_docFuncReq` works", {
  expect_no_error(new_docFuncReq())
})

test_that("`validate_docFuncReq` works", {

  test_obj <- new_docFuncReq()
  expect_no_error({validate_docFuncReq(obj = test_obj)})
  expect_true({validate_docFuncReq(obj = test_obj, bool_out = TRUE)})

})

test_that("`docFuncReq` works", {
  expect_no_error({docFuncReq()})
})

# docNonFuncReq ----
test_that("`new_docNonFuncReq` works", {
  expect_no_error(new_docNonFuncReq())
})

test_that("`validate_docNonFuncReq` works", {

  test_obj <- new_docNonFuncReq()
  expect_no_error({validate_docNonFuncReq(obj = test_obj)})
  expect_true({validate_docNonFuncReq(obj = test_obj, bool_out = TRUE)})

})

test_that("`docNonFuncReq` works", {
  expect_no_error({docNonFuncReq()})
})

# docTestCases ----
test_that("`new_docTestCases` works", {
  expect_no_error(new_docTestCases())
})

test_that("`validate_docTestCases` works", {

  test_obj <- new_docTestCases()
  expect_no_error({validate_docTestCases(obj = test_obj)})
  expect_true({validate_docTestCases(obj = test_obj, bool_out = TRUE)})

})

test_that("`docTestCases` works", {
  expect_no_error({docTestCases()})
})
