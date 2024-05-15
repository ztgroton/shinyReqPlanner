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
