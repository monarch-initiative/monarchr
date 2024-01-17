# test_utils.R

library(testthat)


# Test for null_to_na
test_that("null_to_na correctly converts NULL to NA", {
    expect_equal(null_to_na(NULL), NA_character_)
    expect_equal(null_to_na(NULL, as.integer), as.integer(NA))
    expect_equal(null_to_na("Not null"), "Not null")
})

# Test for get_association_all
test_that("get_association_all retrieves associations correctly", {
    skip("Requires live API interaction, thus skipping in automatic test.")
    #vcr no worky, why not? (empty cassette deleted)
    #vcr::use_cassette("get_association_all", {
        monarch_kg <- get_association_all("biolink:CorrelatedGeneToDiseaseAssociation", "HGNC:1884", 3, 1)
        expect_s3_class(monarch_kg, "monarch_kg") # Update "monarch_kg" to the actual class of the returned object if it's not correct.
    #})
})

# Test for get_entity
test_that("get_entity retrieves entity data correctly", {
    skip("Requires live API interaction, thus skipping in automatic test.")
    entity <- get_entity("MONDO:0005148")
    expect_s3_class(entity, "data.frame")
    expect_equal(nrow(entity), 1)
    expect_true("MONDO:0005148" %in% entity$id)
})

test_that("get_entity returns an empty data frame for NULL or unknown entity", {
    expect_equal(nrow(get_entity()), 0)
    expect_equal(ncol(get_entity()), 9)
    suppressWarnings(expect_warning(get_entity("unknown_entity")))
    suppressWarnings(expect_equal(nrow(get_entity("unknown_entity")), 0))
})

# Test for seed_graph
test_that("seed_graph creates a graph correctly", {
    skip("Requires live API interaction, thus skipping in automatic test.")
    monarch_kg <- seed_graph("MONDO:0009061")
    expect_s3_class(monarch_kg, "monarch_kg") # Update "monarch_kg" to the actual class of the returned object if it's not correct.
})

test_that("seed_graph returns an empty graph for NULL or unknown entity", {
    expect_s3_class(seed_graph(), "tbl_graph")
})

