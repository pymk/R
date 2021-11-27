testthat::test_that("x param: fail if enum is not recognized", {
  testthat::expect_error(
    get_data(x = "this_should_fail")
  )
})

testthat::test_that("return_as_tibble param: fail if non-boolean values are provided", {
  testthat::expect_error(
    get_data("characters", return_as_tibble = "Y")
  )
})


testthat::test_that("page param: fail if non-integers are provided", {
  testthat::expect_error(
    get_data("characters", page = "1")
  )
})

testthat::test_that("pageSize param: fail if non-integers are provided", {
  testthat::expect_error(
    get_data("characters", pageSize = "1")
  )
})

testthat::test_that("function works", {
  pass_check <- get_data("books", return_as_tibble = TRUE, page = 1, pageSize = 1)
  testthat::expect_length(pass_check, 11)
})
