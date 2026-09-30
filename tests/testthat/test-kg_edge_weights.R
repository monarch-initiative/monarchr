library(testthat)
library(assertthat)

data(eds_marfan_kg)

# NOTE: this only checks the shape of the output; the computed weights
# themselves are not yet validated (see notes on normalisation and encodings
# in kg_edge_weights())
test_that("kg_edge_weights adds a numeric weight per edge", {
    g <- eds_marfan_kg |>
        fetch_nodes(query_ids = "MONDO:0007525") |>
        expand(predicates = "biolink:has_phenotype", categories = "biolink:PhenotypicFeature")

    weighted <- kg_edge_weights(g)
    expect_type(edges(weighted)$weight, "double")
    expect_length(edges(weighted)$weight, nrow(edges(g)))

    unnormalised <- kg_edge_weights(g, normalise = FALSE)
    expect_length(edges(unnormalised)$weight, nrow(edges(g)))
})

test_that("monarch_edge_weight_encodings returns named encodings", {
    enc <- monarch_edge_weight_encodings()
    expect_type(enc, "list")
    expect_true(all(c("knowledge_level", "frequency_qualifier", "negated") %in% names(enc)))
    expect_equal(enc$frequency_qualifier[["HP:0040281"]], 1)
})
