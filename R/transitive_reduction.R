#' Compute transitive reduction over a predicate.
#'
#' Computes the transitive reduction of a graph, treating the specified
#' predicate as transitive.
#'
#' @return Graph with redundant (transitively implied) edges of the given
#'         predicate removed.
#' @seealso [transitive_closure()], [roll_up()], [transfer()], [descendants()],
#'          [ancestors()]
#' @param g The `tbl_kgx` graph to compute on.
#' @param predicate The edge predicate to reduce over.
#'
#' @examples
#' library(dplyr)
#' library(tidygraph)
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'     fetch_nodes(name == "Tall stature") |>
#'     expand_n(predicates = "biolink:subclass_of", direction = "out", n = 3) |>
#'     bind_edges(data.frame(
#'         from = 2,
#'         to = 9,
#'         predicate = "biolink:subclass_of",
#'         primary_knowledge_source = "hand_annotated"
#'     ))
#'
#' plot(g, edge_color = primary_knowledge_source)
#'
#' g_closed <- g |>
#'     transitive_closure(predicate = "biolink:subclass_of")
#'
#' plot(g_closed, edge_color = primary_knowledge_source)
#'
#' g_reduced <- g_closed |>
#'     transitive_reduction()
#'
#' plot(g_reduced, edge_color = primary_knowledge_source)
#' @import tidygraph
#' @import dplyr
#' @export
transitive_reduction <- function(g, predicate = "biolink:subclass_of") {
    if (!requireNamespace("sets", quietly = TRUE)) {
        stop(
            "The 'sets' package is required to use transitive_reduction() ",
            "but is not installed. Please install it with:\n",
            "  install.packages('sets')",
            call. = FALSE
        )
    }

    if (!requireNamespace("relations", quietly = TRUE)) {
        stop(
            "The 'relations' package is required to use transitive_reduction() ",
            "but is not installed. Please install it with:\n",
            "  install.packages('relations')",
            call. = FALSE
        )
    }

    # note: inside filter(), `predicate` refers to the edge column, so we use
    # a differently named local variable for the argument
    p <- predicate

    # if there are no edges to reduce, return the input
    if (nrow(edges(g) |> filter(predicate == p)) == 0) {
        return(g)
    }

    # first we make a copy
    active_tbl <- active(g)
    g2 <- g

    # in the original, remove the predicate edges
    g <- g |>
        activate(edges) |>
        filter(predicate != p)

    # compute the reduction over just the predicate edges, using node ids as
    # the domain so that the incidence matrix is labeled by id
    df <- g2 |>
        activate(edges) |>
        filter(predicate == p) |>
        as.data.frame()
    r <- relations::endorelation(
        domain = lapply(unique(c(df$subject, df$object)), sets::as.set),
        graph = df[c("subject", "object")]
    )
    mat <- relations::relation_incidence(relations::transitive_reduction(r))

    keep_idx <- which(mat == 1, arr.ind = TRUE)
    keep_edges <- data.frame(
        subject = rownames(mat)[keep_idx[, "row"]],
        object = colnames(mat)[keep_idx[, "col"]]
    )

    g_reduced <- g2 |>
        activate(edges) |>
        filter(predicate == p) |>
        semi_join(keep_edges, by = c("subject", "object"))

    # merge the original w g_reduced, adding back just the reduction edges
    suppressMessages(g <- kg_join(g, g_reduced), classes = "message") # suppress joining info
    g <- g |> activate(!!rlang::sym(active_tbl))

    return(g)
}
