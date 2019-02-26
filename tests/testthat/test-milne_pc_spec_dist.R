context("test-milne_pc_spec_dist")

test_that("examples", {
  approx_equal <- function(x, y, digits = 4) {
    all.equal(
      round(x, digits = digits),
      round(y, digits = digits)
    )
  }

  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(0, 4, 7)
      ),
      0
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(0, 3, 7)
      ),
      0.2597
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(0, 2, 7)
      ),
      0.2201
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(1, 5, 8)
      ),
      0.8527
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(2, 7, 11)
      ),
      0.4155
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(0, 4, 7),
        c(0, 5, 9)
      ),
      0.4155
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(2, 7, 8),
        c(3, 4, 8, 9)
      ),
      0.5619
    )
  )
  expect_true(
    approx_equal(
      milne_pc_spec_dist(
        c(2, 6, 8, 9),
        c(1, 4)
      ),
      0.5940
    )
  )
})

test_that("coercion", {
  expect_equal(
    milne_pc_spec_dist(c(0, 4, 7), c(0, 3, 7)),
    milne_pc_spec_dist(c(60, 64, 67), c(60, 63, 67))
  )

  expect_equal(
    milne_pc_spec_dist(c(0, 4, 7), c(0, 3, 7)),
    milne_pc_spec_dist(pi_chord(c(60, 64, 67)), pi_chord(c(60, 63, 67)))
  )
})
