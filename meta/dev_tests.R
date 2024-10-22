devtools::load_all()
f <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
gs <- f |>
  fetch_nodes(name %~% "Marfan") |>
  expand()

g <- gs |>
  expand()

# The `expand` function will fetch edges edges between nodes in the query as well; for example, if we want to see if any of the top 5 hits for Fanconi anemia are directly connected, we can begin by getting all of the edges associated with them, storing the result in a separate graph, and taking the intersection of their node sets to filter back down to keep just the original nodes but all the edges between them.

# g <- monarch_search("Ehlers-danlos syndrome", limit = 5)

# expanded <- g |>
# 	expand()

# joined <- expanded |>
# 	activate(nodes) |>
# 	inner_join(nodes(g))

# joined

# Let's do something more complicated: let's fetch all the genes related to FA and EDS, and all the phenotypes associated with either of those diseases directly or via those genes.

# fa <- monarch_search("Fanconi anemia", limit = 1) %>%
# 	expand(result_)


# Let's visualize that:

# ggraph(joined, layout = "fr") +
# 	geom_edge_link() +
#   geom_node_point(aes(color = pcategory, label = wrap(name))) +
# 	theme_graph() +
#   theme(legend.position = 'bottom')


# # Let's visualize that:

# joined <- joined |>
# 	mutate(tooltip = paste0(name, "\n\n", paste(strwrap(description, 80), collapse = "\n"))) |>
# 	mutate(tooltip = paste0(name, "\n\n", pcategory, "\n", iri ))

#

# Load required libraries
library(digest)
library(RColorBrewer)
library(dplyr)
library(tidygraph)

# Generates a palette with num_colors entries, mapping
# inputs pseudorandomly to them. if levels_only = FALSE,
# a vector of RGB values the same length as input is returned.
# if levels_only = TRUE, a named vector is returned mapping input
# levels to RGB values
color_cats <- function(input, num_colors = 16, levels_only = TRUE) {
	# Ensure the input is treated as a factor
	factors <- factor(input)

	palette <- grDevices::hcl.colors(num_colors, palette = "Set3")


	# Hash function to convert factor levels to numeric values consistently
	hashes <- lapply(levels(factors), function(x) digest::digest(x, algo = "crc32", serialize = FALSE))
	hash_integers <- sapply(hashes, function(x) strtoi(substr(x, 1, 5), base=16))

	# Map hashes to indices in the color palette
	# Use modulo to wrap around if there are more factors than colors
	color_indices <- (hash_integers %% length(palette)) + 1

	color_map <- setNames(palette[color_indices], levels(factors))
	if(levels_only) {
		return(color_map)
	} else {
		# Return the colors corresponding to the input
		return(unname(color_map[as.character(input)]))
	}
}
# g |> nodes() |> pull(pcategory) |> factor() |> levels() |> head(n = 4) |> color_cats()
#
#
# # # Example usage
# # factors <- c("Apple", "Banana", "Cherry", "Date", "Apple", "Banana")
# # colors <- map_factors_to_colors(factors)
#
# # # Print the mapping
# # print(colors)
#
#
#
# # jexp <- joined |>
# # 	expand(categories = "biolink:PhenotypicFeature")
#
#
# library(visNetwork)
# visNetwork(nodes(g) %>%
# 						 mutate(kg_id = id) %>%
# 						 mutate(id = 1:nrow(nodes(g))) %>%
# 						 mutate(color = pal),
# 					 edges(g)) %>%
# 	visEdges(shadow = TRUE)
#
#
# program <- "document.addEventListener('mousemove', function(e) {
#   let x = document.getElementsByClassName('infobox')[3];
#   x.style['background'] = '#222222';
#   x.style['border-radius'] = '5px';
#   x.style['color'] = '#222';
#   x.style['font-family'] = 'sans-serif';
#   x.style['position'] = 'absolute';
#   x.style['top'] = e.pageY + 'px';
#   x.style['left'] = e.pageX + 'px';
# })"
#
#
# library(rthreejs)
# graphjs(gs,
# 				vertex.color = as.character(pal),
# 				vertex.size = 0.5, vertex.label = nodes(g)$name)
#
#
# library(RedeR)
# startRedeR()
# addGraphToRedeR(g=gs)


cytoscape <- function(g) {
	RCy3::cytoscapePing()

	nodes_df <- nodes(g)
	nodes_df$desc_wrapped <- stringr::str_wrap(nodes_df$description, 50)

	edges_df <- edges(g)

	RCy3::createNetworkFromDataFrames(nodes_df,
																		edges_df,
																		title = "KG Nodes",
																		collection = "monarchr Graphs",
																		source.id.list = 'subject',
																		target.id.list = 'object')
	RCy3::layoutNetwork('kamada-kawai')

	pal <- color_cats(nodes(g)$pcategory, levels_only = TRUE)
	RCy3::setNodeColorMapping('pcategory', table.column.values = names(pal), colors = pal, mapping.type = 'd')

	RCy3::setNodeTooltipMapping(table.column = 'desc_wrapped')
}

cytoscape(g)



library(ggraph)
library(ggiraph)

z <- ggraph(g, layout = 'fr') + # fr
	geom_edge_link(aes(alpha = after_stat(index),
										 color = predicate)) +
	# geom_node_point(aes(color = pcategory)) +
	geom_point_interactive(
		mapping = aes(x = x,
									y = y,
									color = pcategory,
									data_id = id,
									tooltip = name)
	) +
	scale_edge_alpha('Edge direction', guide = 'edge_direction') +
	theme_graph() +
	theme(legend.position = 'bottom')

girafe(ggobj = z, height_svg = 3, width_svg = 5,
			 options = list(
			 	opts_toolbar(position = "top"),
			 	opts_tooltip(css = "font-family: sans-serif; background: #333333; padding: 10px; font-size: small")

			 ))
# str(z)
#
# subtypes <- g %>%
# 	expand(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE)
#
# with_phenos <- subtypes %>%
# 	expand(predicates = "biolink:has_phenotype")
#
# # print(with_phenos)
#
# with_genes <- subtypes %>%
# 	expand(categories = "biolink:Gene")
#
# # print(with_genes)
#
# neighbors <- graph_join(with_phenos, with_genes)
# neighbors

library(visNetwork)
library(igraph)
library(ggiraph)
library(ggraph)

# x <- visNetwork(g %N>% as_tibble(), g %E>% as_tibble(), height = "400px") %>%
# 	visNodes(color = list(background = ""))
# g <- monarch_search("Fanconi anemia", limit = 1) %>%
# 	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) %>%
# 	expand(categories = "biolink:PhenotypicFeature")
#
# g <- g %>%
# 	activate(nodes) %>%
# 	mutate(tooltip = paste0(name, "\n", description))

z <- ggraph(g, layout = 'fr') + # fr
	# geom_edge_link(aes(alpha = after_stat(index),
	# 									 color = predicate)) +
	# geom_line_interactive(aes(x = x, y = y)) +
	geom_edge_point() +
	# geom_node_point(aes(color = pcategory)) +
	geom_point_interactive(
		mapping = aes(x = x,
									y = y,
									color = pcategory,
									data_id = id,
									tooltip = name)
	) +
	scale_edge_alpha('Edge direction', guide = 'edge_direction') +
	theme_graph() +
	theme(legend.position = 'bottom')

girafe(ggobj = z, width_svg = 5, height_svg = 5,
			 options = list(
			 	opts_toolbar(position = "top"),
			 	opts_zoom(max = 5)
			 ))



#############################

devtools::load_all()
# counts by sets - formatted for use with UpSetR
res <- monarch_engine() |>
	cypher_query_df("MATCH (n) WITH labels(n) AS LabelList, COUNT(n) AS Count WITH LabelList, Count, REDUCE(s = HEAD(LabelList), x IN TAIL(LabelList) | s + '&' + x) AS LabelCombination RETURN LabelCombination, Count", parameters = list(nodes = list("NCBIGene:2876645")))

res

# counts by sets - catories returned a list col
res <- monarch_engine() |>
	cypher_query_df("MATCH (n) WITH labels(n) AS LabelList, COUNT(n) AS Count RETURN LabelList, Count")

res


input <- res$Count
names(input) <- res$LabelCombination

library(UpSetR)

# Create the UpSet plot
upset(fromExpression(input),
			order.by = "freq",
			nsets = 20,
			nintersects = NA)


# node category counts
cat_counts <- monarch_engine() |>
	cypher_query_df("MATCH (n) UNWIND labels(n) AS Label WITH Label, COUNT(n) AS Count RETURN Label, Count ORDER BY Count DESC")

cat_counts

# relationship type counts

pred_counts <- monarch_engine() |>
	cypher_query_df("MATCH ()-[r]->() RETURN type(r) AS RelationshipType, COUNT(*) AS RelationshipCount ORDER BY RelationshipCount DESC")

pred_counts


# category set counts for a single relationship type
cat_set_rel_counts <- monarch_engine() |>
	cypher_query_df("MATCH (source)-[rel:`biolink:has_phenotype`]->(target) WITH labels(source) AS SourceLabelList, type(rel) AS Relationship, labels(target) AS DestLabelList, COUNT(*) AS Count RETURN SourceLabelList, Relationship, DestLabelList, Count ORDER BY Count DESC")

cat_set_rel_counts



# property counts on nodes - random examples
# MATCH (n:`biolink:Disease`)
# WITH n, keys(n) AS properties
# UNWIND properties AS property
# WITH property, COUNT(n[property]) AS count, collect([n[property], n.id]) AS examples
# WITH property, count, examples, rand() AS random
# RETURN property, count, examples[TOINTEGER(random * SIZE(examples))][0] AS example, examples[TOINTEGER(random * SIZE(examples))][1] AS example_id
# ORDER BY count DESC

# property counts on edges - random examples
# MATCH ()-[r:`biolink:causes`]->()
# WITH r, keys(r) AS properties
# UNWIND properties AS property
# WITH property, COUNT(r[property]) AS count, collect([r[property], startNode(r).id, endNode(r).id]) AS examples
# WITH property, count, examples, rand() AS random
# WITH property, count, examples[TOINTEGER(random * SIZE(examples))] AS example_data
# RETURN property, count, example_data[0] AS example, example_data[1] AS source_id, example_data[2] AS target_id
# ORDER BY count DESC


#
query <- "
// Directly refer known primary categories
WITH $categories AS KnownCategories
UNWIND KnownCategories AS CategoryA
UNWIND KnownCategories AS CategoryB
WITH DISTINCT CategoryA, CategoryB

// Match and count relationships using primary known categories
MATCH (a:CategoryA)-[r:`biolink:interacts_with`]->(b:CategoryB)
RETURN CategoryA, type(r) AS RelationshipType, CategoryB, COUNT(*) AS RelationshipCount
ORDER BY RelationshipCount DESC
"

interacts_counts <- cypher_query_df(monarch_engine(), query, parameters = list(categories = as.list(cat_counts$Label)))
interacts_counts


opts <- list(category = as.list(res$Label))
names(opts$category) <- res$Label

# property counts for nodes of type biolink:GeneOrGeneProduct
res <- monarch_engine() |>
	cypher_query_df("MATCH (n)
										WHERE $label IN n.category
										WITH n, keys(n) AS properties
										UNWIND properties AS property
										WITH property, COUNT(n[property]) AS count, collect([n[property], n.id])[0] AS example_data
										RETURN property, count, example_data[0] AS example, example_data[1] AS example_id
										ORDER BY count DESC",
									parameters = list(label = "biolink:GeneOrGeneProduct"))


res


# schema graph - not compatible with KGX
res <- monarch_engine() |>
	cypher_query("CALL db.schema.visualization() YIELD nodes, relationships RETURN [node IN nodes | node {.*, id: id(node)}] AS nodes, relationships")


queries <- paste0("MATCH (n:`",res$Label,"`) RETURN n LIMIT 1")

monarch <- monarch_engine()

fetched <- cypher_query(monarch, queries = queries)


###### diversity sampling...

# get the available relationship types

e <- neo4j_engine("http://neo4j.monarchinitiative.org:7474")

pred_types_query <- "CALL db.schema.visualization() YIELD relationships
UNWIND relationships AS rel
RETURN DISTINCT type(rel) AS predicate"

pred_types <- cypher_query_df(e, pred_types_query)

# sample one of each
sample_preds_query <- paste0("MATCH (a)-[r:`", pred_types$predicate, "`]->(b) RETURN a, b, r LIMIT 1")

sample_preds_graph <- cypher_query(e, query = sample_preds_query)

# debug plot - turn off colors, too many category and predicate types
plot(sample_preds_graph, node_color = NA, edge_color = NA)

# compute the categories that are used thus far
used_categories <- sample_preds_graph |>
	activate(nodes) |>
	as.data.frame() |>
	pull(category) |>
	unlist() |>
	unique()

used_categories

# get the available categories
categories_query <- "CALL db.labels() YIELD label RETURN DISTINCT label"

all_node_categories <- cypher_query_df(e, categories_query)$label
all_node_categories

# compute the node categories that are still needed
needed_categories <- setdiff(all_node_categories, used_categories)

# sample nodes of those categories
sample_cats_query <- paste0("MATCH (a:`", needed_categories, "`) RETURN a LIMIT 1")

sample_new_cats <- cypher_query(e, query = sample_cats_query)

# debug plot - turn off colors
plot(sample_new_cats, node_color = NA, edge_color = NA)

# join the two samples
full_sample <- kg_join(sample_preds_graph, sample_new_cats)


full_sample
plot(full_sample, node_color = NA, edge_color = NA)



##### another try at category counts
cat_counts_query <- paste0("MATCH (a:`", all_node_categories, "`) WITH count(*) as count, '", all_node_categories ,"' as category RETURN category, count")
cat_counts <- cypher_query_df(e, cat_counts_query)
cat_counts_df <- do.call(rbind, cat_counts) |> arrange(desc(count))



