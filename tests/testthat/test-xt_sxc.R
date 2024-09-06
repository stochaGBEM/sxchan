test_that("Cross sections can be created with xt_sxc.", {
  x <- xt_sxc(c(8, 7, 5, 6, 5, 8))
  expect_true(is_sxc(x))
  expect_identical(length(x), 6L)
  x <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2)) |>
    xt_sxc(crs = 3005)
  expect_true(is_sxc(x))
  expect_identical(length(x), 1L)
  expect_identical(sf::st_crs(x), sf::st_crs(3005))
})
