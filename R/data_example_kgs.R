#' Example Ehlers-Danlos and Marfan Syndrome Knowledge Graph Engine
#'
#' A small `file_engine()` Knowledge Graph (KG) containing Monarch Initiative
#' data for Ehlers-Danlos Syndrome and Marfan Syndrome, including their subtypes,
#' all entities connected to those diseases or subtypes, and all ancestors (supertypes)
#' of all those diseases and entities. Generated 8/15/2024 via:
#'
#' ```
#' monarch_engine() |>
#' 	fetch_nodes(query_ids = c("MONDO:0020066", "MONDO:0007947")) |>
#' 	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |>
#' 	expand() |>
#' 	expand(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE)
#' ```
#'
#' This example engine may also be loaded from file via
#'
#' ```
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' eds_marfan_kg <- file_engine(filename)
#' ```
#'
#' @docType data
#' @usage data(eds_marfan_kg)
#' @format An object of class \code{file_engine} for use with `fetch_nodes()`, `expand()`, etc.
#' @keywords datasets
#' @examples
#' data(eds_marfan_kg)
#' phenos <- eds_marfan_kg |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
"eds_marfan_kg"



