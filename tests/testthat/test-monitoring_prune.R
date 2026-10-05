# Purge des caches de zones orphelines (brief coeur 0.209.0 §1.1)

test_that(".prune_zone_caches passes the project uuid to the core guard", {
  recu <- NULL
  local_mocked_bindings(
    prune_orphan_zone_caches = function(con, cache_root, ...) {
      recu <<- c(list(con = con, cache_root = cache_root), list(...))
      list(pruned = character(0))
    },
    .package = "nemeton"
  )
  res <- nemetonshiny:::.prune_zone_caches("CON", list(id = "p-uuid", path = "/x"))
  expect_identical(recu$con, "CON")
  expect_identical(recu$cache_root, file.path("/x", "cache", "layers"))
  expect_identical(recu$project_uuid, "p-uuid")
  expect_identical(res, list(pruned = character(0)))
})

test_that(".prune_zone_caches is best-effort", {
  local_mocked_bindings(
    prune_orphan_zone_caches = function(...) stop("base injoignable"),
    .package = "nemeton"
  )
  expect_null(suppressMessages(
    nemetonshiny:::.prune_zone_caches(NULL, list(id = "p", path = "/x"))))
})
