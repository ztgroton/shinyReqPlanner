test_that("`cols2DocTbl` works", {
  expect_no_error({cols2DocTbl(cols = c(a = 'A'))})
})

test_that("`cols2DocTbl` catches missing 'names' attribute", {
  msg <- "`names(cols)` cannot be NULL"
  expect_error({cols2DocTbl(cols = c('t'))}, msg, fixed = TRUE)
})

test_that("`cols2DocTbl` handles whitespace without error", {
  expect_no_error({cols2DocTbl(cols = c(`a ` = ' A '))})
})

test_that("`cols2DocTbl` catches whitespace errors", {

  msg <- c(
    "`names(cols)` cannot contain blank values",
    "`cols` cannot contain blank values"
  )

  expect_error({cols2DocTbl(cols = c(` ` = 'A'))}, msg[1], fixed = TRUE)
  expect_error({cols2DocTbl(cols = c(a = ' '))}, msg[2], fixed = TRUE)

})

test_that("`cols2DocTbl` catches NA errors", {

  msg <- "`cols` cannot contain NA values"
  expect_error({cols2DocTbl(cols = c(a = NA))}, msg, fixed = TRUE)

})

test_that("`cols2DocTbl` catches non-alphanumeric errors" ,{

  msg <- "`names(cols)` cannot contain non-alphanumeric characters"

  expect_error({cols2DocTbl(cols = c(`a^` = 'A'))}, msg, fixed = TRUE)
  expect_no_error({cols2DocTbl(cols = c(`a` = 'A^'))})

})

test_that("`cols2DocTbl` output matches input", {

  input_cols <- c(a = 'A', b = 'B', c = 'C', d = 'D')
  expect_no_error({cols2DocTbl(cols = input_cols)})

  out <- cols2DocTbl(cols = input_cols)
  expect_true({is.data.frame(out)})
  expect_true({length(colnames(out)) == length(input_cols)})
  expect_setequal(colnames(out), names(input_cols))
  expect_mapequal(attr(out, 'colstrings'), input_cols)

})
