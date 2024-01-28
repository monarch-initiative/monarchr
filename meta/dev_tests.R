library(tidygraph)
library(neo2R)
library(dotenv)

load_dot_env()

##################


## connect to graph db
graph <- startGraph(Sys.getenv("NEO_4J_HTTP")) # e.g. "http://localhost:7474"
## load preferences (primarily how to define each node's "primary category";
## e.g. from [biolink:NamedThing, biolink:BiologicalEntity, biolink:Gene, biolink:PhysicalEssenceOrOccurrent], prefer biolink:Gene)
prefs <- yaml::read_yaml("meta/kg_prefs.yaml")

## a tbl_kgx is a tidygraph::tbl_graph, conforming to the kgx specification, https://github.com/biolink/kgx/blob/master/specification/kgx-format.md
## the from and to information defining edge endpoints is set from edge subject and object
## and the node_key is assumed to be id
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

## Neo4j array results come back as lists of length-1 character vectors;
## these need to be stitched together to length-N character vectors.
## We leave these in a list element to distinguish sets of size 1 from
## scalar strings
stitch_vectors <- function(x) {
	# Check if the element is a list
	if (is.list(x)) {
		# Check if all elements of the list are single-character vectors
		if (all(sapply(x, function(y) is.character(y) && length(y) == 1))) {
			# Concatenate all single-character vectors and put them in a list
			return(list(unlist(x)))
		} else {
			# Recursively apply the function to each element of the list
			return(lapply(x, stitch_vectors))
		}
	} else {
		# Return the element as is if it's not a list
		return(x)
	}
}

## Given a cypher query, returns a tbl_kgx graph.
query_graph <- function(graph, query = "MATCH (s) -[p]- (o) return s, p, o LIMIT 2", parameters = NULL) {
	res <- neo2R::cypher(graph, query= query, parameters = parameters, result = "graph")
	res <- stitch_vectors(res)

	#str(res)
	## node info
	node_ids <- unlist(lapply(res$nodes, function(node) {
		node$properties$id
	}))

	node_categories <- lapply(res$nodes, function(node) {
		node$properties$category[[1]] ## pull these out of the list container to get a simple list of vecs
	})

	nodes_df <- tibble::tibble(id = node_ids, category = node_categories)

	## compute a pcategory, or priority category, based on a preference list
	nodes_df$pcategory <- normalize_categories(node_categories, prefs$monarch_kg$category_priority)
	nodes_df$selected <- FALSE

	## add all other node properties as columns
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

	if(is.null(res$relationships[[1]])) {
		return(tbl_kgx(nodes_df))
	}

	## edge info
	edge_subjects <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$subject
	}))

	edge_predicates <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$predicate
	}))

	edge_objects <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$object
	}))

	edges_df <- data.frame(subject = edge_subjects,
												 predicate = edge_predicates,
												 object = edge_objects)

	# add all other edge properties as columns
	edge_prop_names <- unname(unique(unlist(lapply(res$relationships, function(edge){
		names(edge$properties)
	}))))
	edge_prop_names <- edge_prop_names[!edge_prop_names %in% c("subject", "predicate", "object")]

	for(prop_name in edge_prop_names) {
		# sapply!
		edges_df[[prop_name]] <- sapply(res$relationships, function(edge) {
			edge$properties[[prop_name]]
		})
	}

	# set from and to info for graph
	edges_df$from <- edge_subjects
	edges_df$to <- edge_objects

	g <- tbl_kgx(nodes_df, edges_df)
	return(g)
}
query_graph(graph, query = "MATCH (s) -[r]- (o) return s LIMIT 1")
query_graph(graph, query = "MATCH (s) -[r]- (o) return r LIMIT 1")
query_graph(graph, query = "MATCH (s) -[r]- (o) return s, o LIMIT 1")

query_graph(graph, query = "MATCH path = (startNode)-[*3..3]-(endNode), path2 = (startNode)-[*3..3]-(endNode) return path, path2 LIMIT 1")


query_node_ids <- function(graph, ids) {
	return(query_graph(graph,
										 query = "MATCH (n) WHERE n.id IN $ids RETURN n",
										 parameters = list(ids = ids))
				 )
}
query_node_ids(graph, c("MONDO:0009061", "MONDO:0005413"))




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











