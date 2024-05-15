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
