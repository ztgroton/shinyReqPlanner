test_that("`exportExcel` works for 'docTbl'", {
  test_docTbl <- docTbl(cols = c(fdsofndaslkfjvdicovnlkrfnoidsjfv = 'A'))
  expect_no_error({exportExcel(obj = test_docTbl, file = tempfile())})
})
