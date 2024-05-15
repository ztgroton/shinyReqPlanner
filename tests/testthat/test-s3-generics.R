test_that("`exportExcel` works for 'docTbl'", {
  test_docTbl <- docTbl(cols = c(fdsofndaslkfjvdicovnlkrfnoidsjfv = 'A'))
  expect_no_error({exportExcel(obj = test_docTbl, file = tempfile())})
})

test_that("`exportExcel` works for 'docUserStory'", {
  test_docUserStory <- docUserStory()
  expect_no_error({exportExcel(obj = test_docUserStory, file = tempfile())})
})

test_that("`exportExcel` works for 'docFuncReq'", {
  test_docFuncReq <- docFuncReq()
  expect_no_error({exportExcel(obj = test_docFuncReq, file = tempfile())})
})

test_that("`exportExcel` works for 'docNonFuncReq'", {
  test_docNonFuncReq <- docNonFuncReq()
  expect_no_error({exportExcel(obj = test_docNonFuncReq, file = tempfile())})
})

test_that("`exportExcel` works for 'docTestCases'", {
  test_docTestCases <- docTestCases()
  expect_no_error({exportExcel(obj = test_docTestCases, file = tempfile())})
})
