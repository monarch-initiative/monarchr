# Column names used unquoted in dplyr/tidygraph/ggraph code (tidy evaluation).
# Declaring them here tells R CMD check they are not undefined globals; this
# has no effect at runtime.
utils::globalVariables(c(
    ".",
    "category",
    "depth",
    "downstream_nodes",
    "edge_idx",
    "edge_key",
    "from",
    "id",
    "idx",
    "index",
    "name",
    "namespace",
    "object",
    "pcategory",
    "plot_name",
    "predicate",
    "primary_knowledge_source",
    "query_category",
    "query_pcategory",
    "result_category",
    "result_pcategory",
    "subject",
    "to"
))
