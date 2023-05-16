# monarchr Example Uses

R users will appreciate a `monarchr` package as a convenient and usable data source, abstracting away ontology details as much as possible.

We envision the user building small, in-memory knowledge graphs (minikgs?), possibly of a simpler structure than the base knowledge graph. To facilitate working with them we'll allow users to select subsets of nodes (and edges?) to perform operations on. 

A user might for example start with a minikg with one node and no edges as a result from a call like below (`c("disease")` in R is more or less equivalent to `["disease"]` in Python, ie we can search multiple categories).

```
c19 <- monarch_search("COVID-19", categories = c("disease"), max_hits = 1)
```

And then enrich it by finding all causal genes that cause or contribute to that node, and adding them and the edges to the minikg. Note that R is functional, it generally does not mutate data in place:

```
c19_w_genes <- add_nodes(c19, 
                         categories = c("gene"), 
                         relationships = c("causes", "contributes_to"))
```

I'm imaging that newly added nodes will be automatically selected by default, and that `add_nodes()` works with the selected set by default, so we can further add phenotypes related to those genes:

```
c19_w_genes_phenotypes <- add_nodes(c19_w_genes,
                                    categories = c("phenotype"))
```

In R it's quite common to use an operator, `%>%`, to send the its left side as the first argument to its right side; for example `-1 %>% abs()` is the same as `abs(-1)`. So we can chain these calls, a very R thing to do. Here's a similar thing for MERS:

```
mers_w_genes_phenotypes <- monarch_search("MERS", categories = c("disease"), max_hits = 1) %>%
                           add_nodes(categories = c("gene"), 
                                     relationships = c("causes", "contributes_to")) %>%
                           add_nodes(categories = c("phenotype"))
```

Now, maybe we want to union these two minigraphs (this isn't a great example since COVID and MERS phenotypes
are so broad) on the nodes they have in common (de-duplicating any duplicate edges obviously). Maybe from there we'll extract a new minigraph of the intersected phenotypes only:

```
common_phenotypes <- union(c19_w_genes_phenotypes, 
                           mers_w_genes_phenotypes,
                           select = "intersection") %>%
                     filter(selected = TRUE & category = "phenotype")
```

From this set of common phenotypes we can re-expand into connected diseases. From there we might select the edges with experimental evidence, and extract the publications list as a custom-formatted data frame.

```
nearby_diseases <- add_nodes(common_phenotypes, 
                             categories = c("diseases"))
                             
pubs <- nearby_diseases %>%
          select_edges(evidence_types = c("experimental_evidence")) %>%
          get_publications()
```


## Other features


The minikg grow/select/filter/union/intersect kit seems like a powerful data-oriented paradigm and fits well with the rest of the R ecosystem.

Obviously, users will want to create cool plots of their minikgs, and there are some great packages to do so (even interactive ones). Utility helpers may be created to count connected components, get stats on selected nodes or edges, or other tasks. There are also many libraries for graph data science (for e.g. pagerank, graph embeddings, etc) and if we use standard libraries to back the minikgs they should all be compatible.


## Proposed minikg data model

We should probably start with a simple data model; we can always add features later so long as we don't paint ourselves in any corners. This is shown in a nodes/edges tabular version, though R can easily convert between this and igraph-based internal structures:

```
nodes columns:
	id: primary identifier, e.g. MONDO:0138501
	taxon: e.g. "Human"
	name: e.g. "Hyperchondriaphobia" or "Leukocytokine inhibitor 3"
	category: e.g. "Disease" or "Gene"
	description: "Fear of developing hypochondria. Isn't it ironic." or "A gene I made up"
	synonyms: alternative identifiers e.g. ORPHA:136134, MONDO:135135 [array column]
	          NOTE: I don't know if it's feasible to identify a "primary" ID and synonymous 
	                secondary IDs, where edges only reference the primary ID. that would be the ideal world...
	selected: boolean
	                
edges columns:
	subject: primary identifier, e.g. MONDO:0138501
	relationship: e.g. contributes_to, active_in, etc.
	object: primary identifier, e.g. HGNC:136146
	evidence: a complex array column, with each entry representing a publication and some details about it
	          e.g. for a single row [{"title": "Hyperchondriaphobia: a case study", 
	                                  "url": "https://pubmed...",
	                                  "evidence_type": "experimental_evidence"}, 
	                                 {"title": "Other paper",
	                                  "url": "https://pubmed...",
	                                  "evidence_type": "guesswork"}]
  selected: boolean
```
	
In the future we may want to add additional complex columns for other less commonly used metadata or data that varies depending on the node or edge type.


