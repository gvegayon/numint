context("Monte Carlo Inte")

test_that("First test", {
  ans0 <- num_int(dnorm, a = -1, b = 1, N = 1e5)
  ans1 <- 1 - pnorm(-1)*2

  expect_equal(ans0$val, ans1, tolerance  = 1e-2)
})

# expect_s3_class()
# expect_silent()
