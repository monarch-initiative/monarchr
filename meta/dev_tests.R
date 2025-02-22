devtools::load_all()
f <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
gs <- f |>
  fetch_nodes(name %~% "Ehlers") |>
  expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |>
	expand(categories = "biolink:Gene")

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



#cytoscape(gs)



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




s <- monarch_engine() |> example_graph.neo4j_engine()

cytoscape(s)


f <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))


s2 <- f |> example_graph.file_engine()
s2 |> cytoscape()

