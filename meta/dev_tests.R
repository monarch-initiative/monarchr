

The `expand` function will fetch edges edges between nodes in the query as well; for example, if we want to see if any of the top 5 hits for Fanconi anemia are directly connected, we can begin by getting all of the edges associated with them, storing the result in a separate graph, and taking the intersection of their node sets to filter back down to keep just the original nodes but all the edges between them.

```{r}
g <- monarch_search("Ehlers-danlos syndrome", limit = 5)

expanded <- g |>
	expand()

joined <- expanded |>
	activate(nodes) |>
	inner_join(nodes(g))

joined
```

Let's do something more complicated: let's fetch all the genes related to FA and EDS, and all the phenotypes associated with either of those diseases directly or via those genes.

```{r}
fa <- monarch_search("Fanconi anemia", limit = 1) %>%
	expand(result_)
```

Let's visualize that:

```{r fig.height=5, fig.width=7}
ggraph(joined, layout = "fr") +
	geom_edge_link() +
  geom_node_point(aes(color = pcategory, label = wrap(name))) +
	theme_graph() +
  theme(legend.position = 'bottom')

```

Let's visualize that:

	```{r eval=FALSE}
joined <- joined |>
	mutate(tooltip = paste0(name, "\n\n", paste(strwrap(description, 80), collapse = "\n"))) |>
	mutate(tooltip = paste0(name, "\n\n", pcategory, "\n", iri ))



# Load required libraries
library(digest)
library(RColorBrewer)

# Function to map factor levels or character vector to colors
color_cats <- function(factors) {
	# Ensure the input is treated as a factor
	factors <- factor(factors)

	# Choose a color palette (e.g., Set3) with a reasonably large number of distinct colors
	palette_name <- "Set3"
	palette <- brewer.pal(name = palette_name, n = min(length(unique(factors)), brewer.pal.info[palette_name, "max"]))

	# Hash function to convert factor levels to numeric values consistently
	hashes <- lapply(levels(factors), function(x) digest(x, algo = "crc32", serialize = FALSE))

	hash_integers <- sapply(hashes, function(x) strtoi(substr(x, 1, 5), base=16))

	# Map hashes to indices in the color palette
	# Use modulo to wrap around if there are more factors than colors
	color_indices <- (hash_integers %% length(palette)) + 1
	color_map <- setNames(palette[color_indices], levels(factors))

	# Return the colors corresponding to the input factors
	return(color_map[as.character(factors)])
}

# Example usage
factors <- c("Apple", "Banana", "Cherry", "Date", "Apple", "Banana")
colors <- map_factors_to_colors(factors)

# Print the mapping
print(colors)



jexp <- joined |>
	expand(categories = "biolink:PhenotypicFeature")

library(visNetwork)
visNetwork(nodes(joined) #%>%
					 #mutate(kg_id = id)# %>%
					 #mutate(id = 1:nrow(nodes(jexp))) #%>%
					 # mutate(color = color_cats(pcategory))
					 ,
					 edges(joined)) %>%
	visEdges(shadow = TRUE)

z <- ggraph(joined, layout = 'fr') + # fr
	geom_edge_link(aes(alpha = after_stat(index),
										 color = predicate)) +
	# geom_node_point(aes(color = pcategory)) +
	geom_point_interactive(
		mapping = aes(x = x,
									y = y,
									color = pcategory,
									data_id = id,
									tooltip = tooltip)
	) +
	scale_edge_alpha('Edge direction', guide = 'edge_direction') +
	theme_graph() +
	theme(legend.position = 'bottom')

girafe(ggobj = z, height_svg = 3, width_svg = 5,
			 options = list(
			 	opts_toolbar(position = "top"),
			 	opts_tooltip(css = "font-family: sans-serif; background: #333333; padding: 10px; font-size: small")

			 ))
str(z)
```

```{r eval=FALSE}
subtypes <- g %>%
	expand(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE)

with_phenos <- subtypes %>%
	expand(predicates = "biolink:has_phenotype")

# print(with_phenos)

with_genes <- subtypes %>%
	expand(categories = "biolink:Gene")

# print(with_genes)

neighbors <- graph_join(with_phenos, with_genes)
neighbors
```

```{r eval=FALSE}
library(ggraph)

neighbors <- neighbors %>%
	activate(nodes)

p <- ggraph(neighbors, layout = 'fr') +
	geom_edge_link() +
	geom_node_point(aes(color = pcategory)) +
	theme(legend.position = 'bottom')

plot(p)
```

```{r eval=FALSE}
library(ggraph)

eds_phenos <- monarch_search("Ehlers-danlos syndrome", limit = 1) %>%
	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) %>%
	expand(categories = "biolink:PhenotypicFeature")

# eds_phenos

fanconi_phenos <- monarch_search("Fanconi anemia", limit = 1) %>%
	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) %>%
	expand(categories = "biolink:PhenotypicFeature")

# fanconi_phenos

inner <- inner_join(activate(eds_phenos, nodes), activate(fanconi_phenos, nodes) %>% as_tibble())
print(inner)

p <- ggraph(inner, layout = 'fr') +
	geom_edge_link() +
	geom_node_point(aes(color = pcategory)) +
	theme(legend.position = 'bottom')

plot(p)

both <- graph_join(eds_phenos, fanconi_phenos)


p <- ggraph(both, layout = 'fr') +
	geom_edge_link() +
	geom_node_point(aes(color = pcategory)) +
	theme(legend.position = 'bottom')


plot(p)

```

```{r eval=FALSE}
library(visNetwork)
library(igraph)
library(ggiraph)
library(ggraph)

# x <- visNetwork(g %N>% as_tibble(), g %E>% as_tibble(), height = "400px") %>%
# 	visNodes(color = list(background = ""))
g <- monarch_search("Fanconi anemia", limit = 1) %>%
	expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) %>%
	expand(categories = "biolink:PhenotypicFeature")

g <- g %>%
	activate(nodes) %>%
	mutate(tooltip = paste0(name, "\n", description))

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
									tooltip = tooltip)
	) +
	scale_edge_alpha('Edge direction', guide = 'edge_direction') +
	theme_graph() +
	theme(legend.position = 'bottom')

girafe(ggobj = z, width_svg = 5, height_svg = 5,
			 options = list(
			 	opts_toolbar(position = "top"),
			 	opts_zoom(max = 5)
			 ))

```
