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
