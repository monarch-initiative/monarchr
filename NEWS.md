# monarchr 1.6.2

## Minor Changes

* `plot()` now uses a more basic but KG-flexible `ggraph`, and by default shows node namespace and edge knowledge source if available
* Tooltips in Cytoscape plots show node namespace and edge primary knowledge source if available

# monarchr 1.6.1

## Bug fixes

* Supported using non-default base URLs for `cytoscape()` plotting
* Fixed a bug resulting in duplicate entries when joining graphs from different data contexts

# monarchr 1.6.0

## New features

* New utility function:
	- `graph_semsim`
	- `graph_centrality`
	- `graph_sparsity`
	
* Updated plotting functions
	-	Overhauled `plot.tbl_kgx`
	- New function for getting consistent Monarch palettes: `monarch_palettes`.
	- New function to apply monarch theme to existing `ggraph` plots: `theme_monarch`

* `kg_edge_weights`: New function to compute quantitative edge weights from 
discrete and continuous edge metadata.

* `expand_n`: New function to expand a graph to a specified number of nodes.

## Bug fixes

* Improved the URL try order for the Monarch Neo4j API.


# monarchr 1.5.0

## New features

* Exporting to `cytoscape()` for visualization.
* Added caching for `neo4j_engine` queries (enabled by default, length of R session only)

## Bug fixes

* Fixed a method signature inconsistency for `summary()`.

# monarchr 1.4.0

## New features

* `example_graph()` function for engines
* Engine `summary()` now returns named lists of available node categories and edge predicates for convenient auto-completion

# monarchr 1.3.0

## New features

* `summary()` function for engines

## Bugfixs

* fix backend bug in Neo4j table queries not handling default params properly
* add batch queries for Neo4j backend engine
* added summary() for KG engines to summarize total node and edge count information

# monarchr 1.2.2

## Bugfixs

* Add fallback URLs for `monarch_engine()`

# monarchr 1.2.1

## New features

* New `save_kgx()` and `load_kgx()` for saving/loading graphs.

## Bugfixes

* Reduced size of package with smaller example data
* Fixed a bug in `expand()` where the engine was not carried through for file-engine backed graphs
* Unit tests dependent on live-hosted Monarch KG now allow for minor changes in result counts

# monarchr 1.1.0

## New features

* New `kg_join()` function

## Bug fixes

* Fix `expand()` to include new edges when multiple nodes with the same `id` are part of the query.

1.0.1: Fix missing self loops in plot

# monarchr 1.0.0

1.0.0 is a major update to `monarchr` functionality and APIs.

* Engines
  - `monarchr` now supports the Monarch cloud-hosted KG, other Neo4j-hosted KGs, and file-based KG access from [kghub.org](https://kghub.org)
  - Queries to hosted databases are now paged for efficiency
* API updates and new features
  - `fetch_edges()` is now `expand()`
  - `fetch_nodes()` now supports bulk fetching by provided ID list or node properties
  - `limit` parameter added to `expand()` and `fetch_nodes()` for exploratory queries
  - `monarch_semsim()` provides semantic-similarity matching via the Monarch cloud-hosted API
  - Basic `plot()` added based on `ggraph`
  - Improved display of graphs as node and edge tables in R markdown docs

# monarchr 0.2.2

## New features

* README:
	- Create to README.Rmd to enable autofilling. 
	- Add badges
	- Add link to docs site
* DESCRIPTION:
	- Add URL/BugReports fields
* vignettes
	- Change Basics.Rmd --> monarchr.Rmd to match conventions.
	- Name chunks in vignette
	- Add session info to end of vignette

# monarchr 0.2.1

* Add rworkflows GHA for automated checking, site building, and Docker container creation.
* Update *.Rbuildignore*.
* Add NEWS file.
* Add Title, Description, and Authors to *DESCRIPTION*.
	- Hope you don't mind me adding our names here @oneilsh, I anticipate making lots of contributions!
* Add `tidyr` Import to *DESCRIPTION*.
* Add vignettes header info.
* Fix `edges` docs by adding title.
* Add `VignetteBuilder: knitr` to *DESCRIPTION*.
* Fix `cypher_query` example by adding `ids`
* Fix `cypher_query_df` example by adding `ids`
* Export `cypher_query_df`
* Get `monarchR` closer to passing rcmdcheck (methods for edges/nodes not yet being recognized)
