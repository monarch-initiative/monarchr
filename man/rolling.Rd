% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rollup.R
\name{rolling}
\alias{rolling}
\alias{roll_up}
\alias{roll_down}
\title{Roll node data up (along) or down (against) transitive edges. Use with \code{mutate()}}
\usage{
roll_up(
  column = NULL,
  fun = c,
  include_self = TRUE,
  predicates = "biolink:subclass_of",
  ...
)

roll_down(
  column = NULL,
  fun = c,
  include_self = TRUE,
  predicates = "biolink:subclass_of",
  ...
)
}
\arguments{
\item{column}{The node column to draw rollup or rolldown information from.}

\item{fun}{The aggregation function to use when rolling up or down. Default is \code{c}}

\item{include_self}{Whether to include each nodes' value in \code{column} in the rollup/rolldown for that node.}

\item{predicates}{A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the rollup/rolldown. Should be transitive; default \code{biolink:subclass_of}}

\item{...}{Other parameters (unused)}
}
\value{
Vector or list, with one entry per node.
}
\description{
This function computes, for each node, an aggregated set of data from all
descendant (for roll-ups) or ancestor (for roll-downs) nodes defined by
specified edge predicates. Designed for use with \code{mutate()}
on node data, for each node N, the specified \code{fun} is called
on the node table \code{column} filtered to nodes that can reach (be reached by) N over
\code{predicates} edges. If \code{include_self} is true, N itself is included.
}
\details{
Note that path counts and order are not considered; rollups (rolldowns)
collect information from all descendant (ancestor) nodes as a set.

The return value will be either a list, or if the result would be a list
with all length-1 or length-0 elements, a vector with 0-length elements
replaced by NA. Practically, this results in a list when necessary and a vector
otherwise.
}
\examples{
engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))

engine |> fetch_nodes(name == "Tall stature" | name == "Short stature") |>
  expand_n(predicates = "biolink:subclass_of", direction = "out", n = 2) |>  # get 2 levels of ancestors
  activate(nodes) |>
  mutate(count = rpois(graph_order(), 1.5)) |>                               # random count value per node
  mutate(sum_count = roll_up(count, fun = sum, include_self = TRUE)) |>      # apply sum to descendant (and self) values
  plot(node_label = paste(name, " count: ", count, "sum_count: ", sum_count))

}
\seealso{
\code{\link[=roll_down]{roll_down()}}, \code{\link[=descendants]{descendants()}}, [ancestors(), \code{\link[=transfer]{transfer()}}, \code{\link[=transitive_closure]{transitive_closure()}}]
}
