## ----setup, include=FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidygraph)
library(monarchr)

options(width = 300)
knitr::opts_chunk$set(echo = TRUE)

`%+%` <- function(a, b) {
	paste0(a, b)
}

wrap <- function(labels, size = 20) {
	wrapped <- labels %>% lapply(strwrap, size)
	reformatted <- wrapped %>% lapply(paste, collapse = "\n")
	return(as.character(reformatted))
}

knitr::opts_chunk$set(eval = FALSE)

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nodes_kbl

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edges_kbl

## ----welcome, eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
monarch_engine() |>
  get_nodes(query_ids = c("HGNC:1884")) |>
  expand(predicates = "biolink:causes")

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nodes_kbl

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edges_kbl

## ----monarch_engine, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g <- monarch_engine() |>
	search_nodes("cystic fibrosis", limit = 5)

g

## ----neo4j_engine, eval=FALSE, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # not run:
#  g <- neo4j_engine("http://localhost:7687", username = "user", password = "pass") |>
#  	search_nodes("cystic fibrosis", limit = 5)
#  
#  g

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nodes_kbl

## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edges_kbl

## ----file_engine, eval=TRUE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
# filename <- "https://kghub.io/kg-obo/mondo/2024-03-04/mondo_kgx_tsv.tar.gz"

print(filename)
	
g <- file_engine(filename) |>
	search_nodes("cystic fibrosis", limit = 5)

g

## ----library, message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(monarchr)
#  library(dplyr)
#  library(tidygraph)
#  #library(kableExtra) # for kbl() table output formatting
#  #library(ggraph)
#  #library(ggiraph)

## ----search-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # id lookup
#  eds_hits <- query_ids(c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021"))
#  # or search
#  eds_hits <- monarch_search("Ehlers-danlos syndrome", limit = 3)
#  
#  
#  nodes(eds_hits)
#  edges(eds_hits)

## ----manipulate-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_hits <- eds_hits %>%
#  	activate(nodes) %>%
#  	mutate(source = "search") %>%
#  	select(id, source, everything()) # move new source col to 2nd position
#  
#  nodes(eds_hits)

## ----pcategory--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  options("kg_prefs")

## ----neighborhood-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  library(knitr)
#  summarize_neighborhood(eds_hits, summarize = "nodes")
#  summarize_neighborhood(eds_hits, summarize = "edges")

## ----fetch-edges------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_with_genes <- eds_hits |>
#  	fetch_edges(result_categories = "biolink:Gene")
#  
#  eds_with_genes

## ----fetch-deges-2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_with_genes <- eds_hits |>
#  	fetch_edges(result_categories = "biolink:Gene") |>
#  	activate(nodes) |>
#  	mutate(source = replace_na(source, "genelinks"))
#  
#  eds_with_genes
#  # summarize the outgoing edges connected to the graph so far
#  # summarize_neighborhood(eds_with_genes, direction = "out", summarize = "edges")

## ----fetch-edges-3----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_with_genes_phenos_expanded <- eds_with_genes |>
#  	fetch_edges(direction = "out",
#  							predicates = c("biolink:has_phenotype", "biolink:interacts_with"),
#  							result_categories = c("biolink:Gene", "biolink:PhenotypicFeature"))
#  
#  eds_with_genes_phenos_expanded

## ----visualization----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  ggraph(eds_with_genes_phenos_expanded, layout = "fr") +
#  	geom_edge_link(aes(color = predicate)) +
#  	geom_node_point(aes(color = pcategory))

## ----graph-joins------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  fa <- monarch_search("Fanconi anemia", limit = 1) %>%
#  	fetch_edges(result_categories = "biolink:Gene") %>%
#  	fetch_edges(direction = "out", predicates = "biolink:has_phenotype")
#  
#  cf <- monarch_search("Cystic Fibrosis", limit = 1) %>%
#  	fetch_edges(result_categories = "biolink:Gene") %>%
#  	fetch_edges(direction = "out", predicates = "biolink:has_phenotype")
#  
#  shared_nodes <- inner_join(nodes(fa), nodes(cf))
#  shared_nodes

## ----graph-joins-2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  merged <- fa %>%
#  	graph_join(cf) %>%
#  	activate(nodes) %>%
#  	filter(pcategory %in% c("biolink:Gene", "biolink:Disease") | id %in% shared_nodes$id)
#  
#  ggraph(merged, layout = "fr") +
#  	geom_edge_link(aes(color = predicate)) +
#  	geom_node_point(aes(color = pcategory))

## ----transitivity-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_with_subtypes <- monarch_search("Ehlers-danlos syndrome", limit = 1) |>
#  	fetch_edges(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE) |>
#  	fetch_edges(result_categories = "biolink:Gene")
#  
#  # plot
#  ggraph(eds_with_subtypes, layout = "sugiyama") +
#  	geom_edge_link(aes(color = predicate)) +
#  	geom_node_point(aes(color = pcategory)) +
#  	coord_flip()

## ----ancestors-descendants--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_search <- monarch_search("Ehlers-danlos syndrome", limit = 5) %>%
#  	activate(nodes) %>%
#  	mutate(source = "search")
#  
#  eds_search_ancestors <- eds_search |>
#  	fetch_edges(direction = "out", predicates = "biolink:subclass_of", transitive = TRUE) %>%
#  	activate(nodes) %>%
#  	mutate(source = replace_na(source, "ancestors"))
#  
#  eds_search_descendants <- eds_search |>
#  	fetch_edges(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE) %>%
#  	activate(nodes) %>%
#  	mutate(source = replace_na(source, "descendants"))
#  
#  ggraph(eds_search_ancestors, layout = "sugiyama") +
#  		geom_edge_link(aes(color = predicate)) +
#  		geom_node_point(aes(color = source)) +
#  		coord_flip()
#  
#  ggraph(eds_search_descendants, layout = "sugiyama") +
#  		geom_edge_link(aes(color = predicate)) +
#  		geom_node_point(aes(color = source)) +
#  		coord_flip()

## ----ancestors-descendants-2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  merged <- eds_search_ancestors |>
#  	graph_join(eds_search_descendants)
#  
#  ggraph(merged, layout = "sugiyama") +
#  		geom_edge_link(aes(color = predicate)) +
#  		geom_node_point(aes(color = source)) +
#  		coord_flip()
#  
#  inner <- eds_search_ancestors %>%
#  	activate(nodes) %>%
#  	inner_join(nodes(eds_search_descendants))
#  
#  ggraph(inner, layout = "sugiyama") +
#  		geom_edge_link(aes(color = predicate)) +
#  		geom_node_point(aes(color = source)) +
#  		coord_flip()

## ----list-of-graphs---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  eds_search %>%                                                 # starting with a graph with 5 disease nodes
#  	explode() %>%                                                # split into a list of 5 single-node graphs
#  	lapply(fetch_edges) %>%                                      # fetch neighborhood edges for each graph
#  	lapply(nodes) %>%                                            # extract nodes df from each graph
#  	lapply(function(nodes_df) {filter(nodes_df, is.na(source))}) # remove original searched nodes from each df

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  utils::sessionInfo()

