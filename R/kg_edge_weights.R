#' Knowledge Graph edge weights
#'
#' Compute edge weights for the given \link{tbl_kgx} graph using
#' several pieces of categorical, ordinal, and continuous metadata.
#' @param fun Function to compute edge weights with across the
#' numerically encoded attributes. Default is \link{rowSums}.
#' @param normalise Normalise each encoding from 0-1 by dividing by the
#' maximum value. Default is \code{TRUE}.
#' @param encodings A list of named lists of encoding values for
#' different edge attributes.
#' @inheritParams nodes
#' @import dplyr
#' @export
#' @examples
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' g2 <- kg_edge_weights(g)
#' edges(g2)$weight
kg_edge_weights <- function(graph,
														normalise=TRUE,
														encodings=list(
															"knowledge_level"=list(
																"knowledge_assertion"=1,
																"logical_entailment"=1,
																"not_provided"=0
															),
															"frequency_qualifier"=list(
																"HP:0040281"=1, # "Very frequent"
																	"HP:0040282"=.75, # "Frequent"
																	"HP:0040283"=.5, # "Occasional"
																	"HP:0040284"=.25 # "Very rare"
															),
															"negated"=list(
																`TRUE`=-1,
																`FALSE`=0
															),
															"has_total"=NULL,
															"has_quotient"=numeric(),
															"has_count"=NULL,
															"has_percentage"=NULL,
															"has_evidence"=NULL,
															"onset_qualifier"=NULL,
															"publications"=function(x){length(unique(x))}
														),
														fun=function(x){rowSums(x, na.rm = TRUE)}
														){
	# knowledge_level *
	# frequency_qualifier **
	# negated ?
	# has_total
	# has_quotient
	# has_count
	# has_percentage *
	# has_evidence *
	# has_percentage
	# onset_qualifier
	# publications **
	encoded_cols <- c()
	for(key in names(encodings)){
		nm_encoded <- paste0(key,"_encoded")
		val <- encodings[[key]]
		if(is.null(val)){
			next
		}
		if(is.numeric(val)){
			# message(key,": numeric")
			encoded_cols <- c(encoded_cols, nm_encoded)
			graph <- graph|>
				activate(edges)|>
				mutate(!!nm_encoded:=edges(graph)[[key]])
			next
		}
		if(is.function(val)){
			# message(key,": function")
			encoded_cols <- c(encoded_cols, nm_encoded)
			graph <- graph|>
				activate(edges)|>
				mutate(!!nm_encoded:=val(!!key))

			next
		}
		if(is.list(val)){
			# message(key,": list")
			encoded_cols <- c(encoded_cols, nm_encoded)
			graph <- graph|>
				activate(edges)|>
				mutate(!!nm_encoded:=ifelse(!!key %in% names(val), val[!!key], 0))
			next
		}
	}
	## normalise within each col
	if(normalise){
		graph <- graph|>
			activate(edges)|>
			mutate(
				across(all_of(encoded_cols),
							 ~(min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE))
				))
	}
	igraph::E(graph)$weight <- fun(edges(graph)[,unique(encoded_cols)])
	return(graph)
}
