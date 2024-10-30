library(testthat)
library(assertthat)

test_that("example_graph for file engine", {
	#testthat::skip("temporary skip")
	options(width = 150)

	filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
	e <- file_engine(filename)

	sample <- example_graph(e)

	# check some expected categories
	expect_true(any("biolink:Disease" %in_list% nodes(sample)$category))
	expect_true(any("biolink:GenomicEntity" %in_list% nodes(sample)$category))
	expect_true(any("biolink:GeneOrGeneProduct" %in_list% nodes(sample)$category))
	expect_true(any("biolink:SequenceVariant" %in_list% nodes(sample)$category))
	expect_true(any("biolink:OntologyClass" %in_list% nodes(sample)$category))
	expect_true(any("biolink:PhysicalEssence" %in_list% nodes(sample)$category))

	# check some expected predicates
	expect_true(any("biolink:causes" %in_list% edges(sample)$predicate))
	expect_true(any("biolink:subclass_of" %in_list% edges(sample)$predicate))
	expect_true(any("biolink:associated_with_increased_likelihood_of" %in_list% edges(sample)$predicate))
	expect_true(any("biolink:treats_or_applied_or_studied_to_treat" %in_list% edges(sample)$predicate))
	expect_true(any("biolink:genetically_associated_with" %in_list% edges(sample)$predicate))
})
