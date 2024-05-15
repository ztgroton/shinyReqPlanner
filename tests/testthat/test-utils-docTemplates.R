test_that("`emptyDocTemplate` works", {
  expect_no_error({emptyDocTemplate(cols = c(a = 'A'))})
})

test_that("`emptyDocTemplate` catches missing 'names' attribute", {
  msg <- "`names(cols)` cannot be NULL"
  expect_error({emptyDocTemplate(cols = c('t'))}, msg, fixed = TRUE)
})

test_that("`emptyDocTemplate` handles whitespace without error", {
  expect_no_error({emptyDocTemplate(cols = c(`a ` = ' A '))})
})

test_that("`emptyDocTemplate` catches whitespace errors", {

  msg <- c(
    "`names(cols)` cannot contain blank values",
    "`cols` cannot contain blank values"
  )

  expect_error({emptyDocTemplate(cols = c(` ` = 'A'))}, msg[1], fixed = TRUE)
  expect_error({emptyDocTemplate(cols = c(a = ' '))}, msg[2], fixed = TRUE)

})

test_that("`emptyDocTemplate` catches NA errors", {

  msg <- "`cols` cannot contain NA values"
  expect_error({emptyDocTemplate(cols = c(a = NA))}, msg, fixed = TRUE)

})

test_that("`emptyDocTemplate` catches non-alphanumeric errors" ,{

  msg <- "`names(cols)` cannot contain non-alphanumeric characters"

  expect_error({emptyDocTemplate(cols = c(`a^` = 'A'))}, msg, fixed = TRUE)
  expect_no_error({emptyDocTemplate(cols = c(`a` = 'A^'))})

})

test_that("`emptyDocTemplate` output matches input", {

  input_cols <- c(a = 'A', b = 'B', c = 'C', d = 'D')
  expect_no_error({emptyDocTemplate(cols = input_cols)})

  out <- emptyDocTemplate(cols = input_cols)
  expect_true({is.data.frame(out)})
  expect_true({length(colnames(out)) == length(input_cols)})
  expect_setequal(colnames(out), names(input_cols))
  expect_mapequal(attr(out, 'colstrings'), input_cols)

})
