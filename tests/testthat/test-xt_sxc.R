test_that("Cross sections can be created with xt_sxc: numbers", {
  x <- xt_sxc(c(8, 7, 5, 6, 5, 8))
  expect_true(is_sxc(x))
  expect_identical(length(x), 6L)
})

test_that("Cross sections can be created with xt_sxc: line segs", {
  ## Linestring sfg
  l <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2))
  x <- xt_sxc(l, crs = 3005)
  expect_true(is_sxc(x))
  expect_identical(length(x), 1L)
  expect_identical(sf::st_crs(x), sf::st_crs(3005))
  ## Linestring sfc
  x <- xt_sxc(sf::st_sfc(l, crs = 3005))
  expect_true(is_sxc(x))
  expect_identical(length(x), 1L)
  expect_identical(sf::st_crs(x), sf::st_crs(3005))
  x <- xt_sxc(sf::st_sfc(l), crs = 3005)
  expect_identical(sf::st_crs(x), sf::st_crs(3005))
  ## multilinestring sfg
  l2 <- sf::st_multilinestring(list(
    matrix(c(-1, 1, 0, 1), ncol = 2),
    matrix(c(-2, 1, 0, 1), ncol = 2)
  ))
  x <- xt_sxc(l2)
  expect_true(is_sxc(x))
  expect_identical(length(x), 2L)
  ## sfc with linestring and multilinestring; flattens out to only linestrings
  sfc <- sf::st_sfc(l, l2)
  x <- xt_sxc(sfc)
  expect_true(is_sxc(x))
  expect_identical(length(x), 3L)
})

test_that("Cross sections can be created with xt_sxc: from sxc", {
  x <- xt_sxc(c(8, 7, 5, 6, 5, 8))
  expect_identical(xt_sxc(x), x)
  # Can be used to update spatial properties
  expect_identical(sf::st_crs(xt_sxc(x, crs = 3005)), sf::st_crs(3005))
})
