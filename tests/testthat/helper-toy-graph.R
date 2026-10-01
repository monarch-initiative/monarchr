# A small hand-built graph for fast, network-free tests with exact expected
# values:
#
#   A --subclass_of--> B --subclass_of--> C
#   G --causes--> A
toy_graph <- function() {
    nodes <- data.frame(
        id = c("A", "B", "C", "G"),
        name = c("a", "b", "c", "g"),
        count = c(1, 2, 4, 8),
        namespace = c("HP", "HP", "HP", "HGNC"),
        pcategory = c(rep("biolink:PhenotypicFeature", 3), "biolink:Gene")
    )
    nodes$category <- list(
        "biolink:PhenotypicFeature",
        "biolink:PhenotypicFeature",
        "biolink:PhenotypicFeature",
        "biolink:Gene"
    )
    edges <- data.frame(
        subject = c("A", "B", "G"),
        predicate = c("biolink:subclass_of", "biolink:subclass_of", "biolink:causes"),
        object = c("B", "C", "A"),
        primary_knowledge_source = "test"
    )
    tbl_kgx(nodes = nodes, edges = edges)
}

# edges as sorted "subject predicate object" strings, for easy comparison
edge_keys <- function(g) {
    e <- edges(g)
    sort(paste(e$subject, e$predicate, e$object))
}
