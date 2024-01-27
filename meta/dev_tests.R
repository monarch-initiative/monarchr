library(monarchr)
library(tidygraph)

monarch_search("syndrome", limit = 5) %>%
    activate(nodes) %>%
    filter(label == "EEC syndrome and related syndrome")

monarch_search("syndrome", limit = 30)
monarch_search("syndrome", limit = 20)


library(neo2R)
graph <- startGraph(
	"http://24.144.94.219:7474",
	#"http://neo4j-bolt.monarchinitiative.org:7474"
	#username="neo4j", password="donttrustusers",
	#importPath="~/neo4j_home/neo4jImport",
	#.opts = list(ssl_verifypeer=0)

)

system.time({
	df <- cypher(
		graph,
		prepCql(
			'MATCH (n)-[r]-(p) return n,r,p limit 1'
		),
		result = "graph"
	)
})




library(tidygraph)

get_neighborhood <- function(id = "MONDO:0009061") {
	res <- neo2R::cypher(graph,
											prepCql('MATCH (n) return n limit 3'),
											#parameters = list(id = id),
											result = "graph")


	# Assuming 'your_list' is the list structure you provided
	nodes <- res$nodes
	nodes_df <- data.frame(id = lapply(nodes, function(node) { node$properties$id }))
	nodes_df$category <- lapply(nodes, function(node) { node$ })

}

x <- get_neighborhood()

library(tidygraph)
rstat_nodes <- data.frame(name = c("Hadley", "David", "Romain", "Julia"))
rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
													to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
x <- tbl_graph(nodes = rstat_nodes, edges = rstat_edges)

rstat_nodes <- data.frame(name = c("Jim", "David", "John", "Rex"), age = c(23, 24, 25, 26))
rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
													to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
y <- tbl_graph(nodes = rstat_nodes, edges = rstat_edges)

z <- tidygraph::graph_join(x, y)




# calls tidygraph::as_tbl_graph on the input, ensures the output conforms to kgx format,
# and sets required tidygraph "from" and "to" columns from "subject" and "object"
as_tbl_kgx <- function(...) {
	g <- tidygraph::as_tbl_graph(..., node_key = "id")
	#str(g)
	print(g)
}

node_df <- data.frame(id = c("MONDO:1", "MONDO:2", "MONDO:3"))
node_df$category <- list(c(1, 2, 3), c(3, 4), c(5, 6, 7))
edge_df <- data.frame(subject = c("MONDO:1", "MONDO:2", "MONDO:3"),
											predicate = c("likes", "loves", "hates"),
											object = c("MONDO:3", "MONDO:1", "MONDO:2"))



tbl_kgx <- function(nodes = NULL, edges = NULL, ...) {
	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'id' column.") }
	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'category' column.") }
	# we do allow graphs with no edges
	if(!is.null(edges)) {
		if(is.null(edges$subject)) { stop("Error: tbl_kgx edges must have an 'subject' column.") }
		if(is.null(edges$predicate)) { stop("Error: tbl_kgx edges must have an 'predicate' column.") }
		if(is.null(edges$object)) { stop("Error: tbl_kgx edges must have an 'object' column.") }

		# set canonical to and from columns
		edges$from <- edges$subject
		edges$to <- edges$object
		}

	g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "id")
	class(g) <- c("tbl_kgx", class(g))
	return(g)
}

z <- tbl_kgx(nodes = node_df, edges = edge_df)

library(ggraph)

p <- ggraph::autograph(z)


##################

library(neo2R)
graph <- startGraph("http://24.144.94.219:7474")
prefs <- yaml::read_yaml("meta/kg_prefs.yaml")



# given a list of vectors of categories, and an ordered preference list over
# categories, selects the most preferred from each vector,
# or the first if no preferred are included
normalize_categories <- function(cats_list, cats_prefs) {
	normed <- unlist(lapply(cats_list, function(categories) {
		positions <- match(categories, cats_prefs)

		# If all matches are NO, there is no preference match, so use the first label;
		# otherwise, use the label with the minimum position in the preferred list
		most_preferred_label <- if (all(is.na(positions))) {
			categories[1]
		} else {
			categories[which.min(positions)]
		}

		most_preferred_label
	}))

	return(normed)
}


query_node_ids <- function(ids) {
	res <- neo2R::cypher(graph,
											 query = "MATCH (n) WHERE n.id IN $ids RETURN n",
											 parameters = list(ids = ids),
											 result = "graph")

	#nodes_df <- data.frame
	node_ids <- unlist(lapply(res$nodes, function(node) {
		node$properties$id
	}))

	node_categories <- lapply(res$nodes, function(node) {
		node$properties$category
	})

	nodes_df <- tibble::tibble(id = node_ids, category = node_categories)

	nodes_df$pcategory <- normalize_categories(nodes_df$category, prefs$monarch_kg$category_priority)
	nodes_df$selected <- FALSE


	node_prop_names <- unname(unique(unlist(lapply(res$nodes, function(node){
		names(node$properties)
	}))))
	node_prop_names <- node_prop_names[!node_prop_names %in% c("id", "category")]

	for(prop_name in node_prop_names) {
		# sapply!
		nodes_df[[prop_name]] <- sapply(res$nodes, function(node) {
			node$properties[[prop_name]]
		})
	}

	g <- tbl_kgx(nodes_df)
	return(g)
}
r <- query_node_ids(c("MONDO:0009061", "MONDO:0005413"))

count_labels <- function() {
	query <- "
MATCH (n)
WITH LABELS(n) AS labels
UNWIND labels AS label
WITH label, COUNT(*) AS usageCount
RETURN label, usageCount
ORDER BY usageCount DESC
		"
	res <- neo2R::cypher(graph, query = query)
	print(res)
}
count_labels()



# system.time({
# 	df <- cypher(
# 		graph,
# 		prepCql(
# 			'MATCH (n)-[r]-(p) return n,r,p limit 1'
# 		),
# 		result = "graph"
# 	)
# })
#









