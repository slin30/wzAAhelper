context("expand_argList")

## TODO: Rename context

#baseline list
my_baseList <- list(
  reportsuite.id = "myID",
  date.from = "2016-09-01",
  date.to = "2016-09-02",
  elements = "my_element",
  metrics = c("pageviews", "visits"),
  segment.id = c("segment1", "segment2"),
  date.granularity = "month",
  top = 10)

#expand list
my_expandList_1 <- list(
  seg1 = "a",
  seg2 = "b",
  seg3 = "c",
  seg4 = "d")

my_expandList_2 <- list(
  seg1 = "a",
  seg2 = "b",
  seg3 = NULL,
  seg4 = "d")


test_that("replace option outputs equal length with non-NULL replace", {
  ref <- length(my_expandList_1)
  tst <- expand_argList(my_baseList, "segment.id", my_expandList_1, how = "replace")
  tst <- lapply(tst, function(f) f[["segment.id"]])

  expect_equal(ref, length(unlist(tst)))
})

test_that("replace option works outputs unequal length with non-NULL replace", {
  ref <- length(my_expandList_1)
  tst <- expand_argList(my_baseList, "segment.id", my_expandList_2, how = "replace")
  tst <- lapply(tst, function(f) f[["segment.id"]])

  expect_length(unlist(tst), 3)
})
