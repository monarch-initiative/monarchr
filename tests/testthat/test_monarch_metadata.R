library(testthat)

pkgname <- "monarchr"

test_that("get_association_categories works as expected", {
    cats <- get_association_categories("Gene", "Disease")
    expect_true("biolink:CorrelatedGeneToDiseaseAssociation" %in% cats)
    expect_true("biolink:CausalGeneToDiseaseAssociation" %in% cats)

    # check reversals
    cats <- get_association_categories("Disease", "Gene")
    expect_true("biolink:CorrelatedGeneToDiseaseAssociation" %in% cats)
    expect_true("biolink:CausalGeneToDiseaseAssociation" %in% cats)

    # couple more
    cats <- get_association_categories("Gene", "GeneticInheritance")
    expect_true("biolink:GeneToPhenotypicFeatureAssociation" %in% cats)
    expect_true("biolink:DiseaseOrPhenotypicFeatureToGeneticInheritanceAssociation" %in% cats)

    # check reversals
    cats <- get_association_categories("GeneticInheritance", "Gene")
    expect_true("biolink:GeneToPhenotypicFeatureAssociation" %in% cats)
    expect_true("biolink:DiseaseOrPhenotypicFeatureToGeneticInheritanceAssociation" %in% cats)
    })


test_that("get_entity_types works as expected", {
  types <- get_entity_types()
  expect_equal(all(c("Disease", "Gene", "GeneticInheritance", "PhenotypicQuality") %in% types), TRUE)
})