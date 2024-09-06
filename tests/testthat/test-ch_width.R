test_that("Channel width works", {
  w <- c(8, 7, 5, 6, 5, 8)
  x <- xt_sxc(w)
  expect_equal(xt_width(x), w)
  x <- sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2)) |>
    xt_sxc(crs = 3005)
  expect_equal(xt_width(x), units::set_units(sqrt(2), m))
})
