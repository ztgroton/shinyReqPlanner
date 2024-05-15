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
