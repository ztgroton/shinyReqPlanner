test_that("`msg_setdiff` works", {

  expect_equal({msg_setdiff('a', 'a')}, "")
  expect_equal({msg_setdiff('a', 'b')}, "\n Difference in Vectors: \n  Missing: (a)\n  Unexpected: (b)")
  expect_equal({msg_setdiff(c('a'), c('a', 'b'))}, "\n Difference in Vectors: \n  Unexpected: (b)")
  expect_equal({msg_setdiff(c('a', 'b'), c('a'))}, "\n Difference in Vectors: \n  Missing: (b)")

})
