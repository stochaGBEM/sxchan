test_that("Centerline can be generated", {
  cl <- xt_generate_centerline(demo_bankline)
  expect_true(inherits(cl, "sfc"))
  # Has length
  expect_gt(sum(sf::st_length(cl)), units::set_units(0, m))
  # Does not intersect banks
  int <- sf::st_intersection(
    cl, sf::st_cast(demo_bankline, "MULTILINESTRING")
  )
  expect_equal(length(int), 0)
})
