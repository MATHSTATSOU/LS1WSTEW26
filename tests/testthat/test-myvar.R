test_that("myvar equals var", {

  expect_equal(myvar(1:10), var(1:10))

})

test_that("myvar makes sense", {
  ones <- rep(1,10)
  expect_equal(myvar(ones), 0)

})
