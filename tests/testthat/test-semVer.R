test_that("Semver object is created when version is not supplied", {
  s <- semVer$new()
  expect_equal(s$version,"0.0.0")
})
test_that("Semver object is created when version is supplied", {
  s <- semVer$new("1.2.3")
  expect_equal(s$version, "1.2.3")
})
test_that("Semver object can test for higher versions", {
  s <- semVer$new("1.2.3")
  higher <- s$higherThanAll("1.2.2")
  expect_true(higher)
  higher <- s$higherThanAll("1.2.3")
  expect_false(higher)
  higher <- s$higherThanAll("1.2.4")
  expect_false(higher)
  higher <- s$higherThanAll("1.2.3-alpha")
  expect_true(higher)
  higher <- s$higherThanAll("1.2.3-alpha+build1")
  expect_true(higher)
  higher <- s$higherThanAll(c("10.0.1", "1.2.3-alpha+build1"))
  expect_false(higher)
  higher <- s$higherThanAll("5.2.2")
  expect_false(higher)
  higher <- s$higherThanAll("1.0.2")
  expect_true(higher)
  higher <- s$higherThanAll("0.2.2")
  expect_true(higher)
  higher <- s$higherThanAll("1.5.2")
  expect_false(higher)
  expect_warning(higher <- s$higherThanAll(c("10.0.1", "1.2.3.4-alpha+build1")))
  s$version <- "1.2.3-alpha+build1"
  higher <- s$higherThanAll("1.2.3-alpha+build1")
  expect_false(higher)
  higher <- s$higherThanAll("1.2.3-1al.pha+build1")
  expect_true(higher)

})
test_that("Semver object errors on bad semantic versions", {
  expect_warning(s <- semVer$new("1.2.3.hello?"))
  expect_false(s$version == "1.2.3.hello?")
})
test_that("Semver object parses correctly", {
  s <- semVer$new("1.2.3-alpha+build1")
  expect_equal(s$getMajor, "1")
  expect_equal(s$getMinor, "2")
  expect_equal(s$getPatch, "3")
  expect_equal(s$getPreRelease, "alpha")
  expect_equal(s$getBuild, "build1")
  expect_equal(s$version, "1.2.3-alpha+build1")
})



