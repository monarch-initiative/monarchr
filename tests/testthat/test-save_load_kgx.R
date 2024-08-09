library(testthat)
library(assertthat)

test_that("save_kgx and load_kgx work as expected", {
	suppressWarnings(g <- monarch_engine() |>
		fetch_nodes(limit = 5) |>
		expand(limit = 20))

	save_kgx(g, "test.tar.gz")

	g2 <- load_kgx("test.tar.gz")

	expect_equal(nrow(nodes(g)), nrow(nodes(g2)))
	expect_equal(nrow(edges(g)), nrow(edges(g2)))

	expect_true(all(nodes(g)$id %in% nodes(g2)$id))
	expect_true(all(nodes(g2)$id %in% nodes(g)$id))

	expect_true("pcategory" %in% colnames(nodes(g2)))
	expect_true("category" %in% colnames(nodes(g2)))
	expect_true(is.list(nodes(g2)$category))

	file.remove("test.tar.gz")
})
