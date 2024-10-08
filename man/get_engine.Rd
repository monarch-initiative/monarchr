% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_engine.R
\name{get_engine}
\alias{get_engine}
\title{Get most recent engine from a graph.}
\usage{
get_engine(g, fail_if_missing = TRUE)
}
\arguments{
\item{g}{A tbl_kgx graph.}

\item{fail_if_missing}{If TRUE, fail if there is no engine associated with the graph.}
}
\value{
A graph engine object.
}
\description{
Given a tbl_kgx graph, retrieve the last-used engine.
}
\examples{
# Using example KGX file packaged with monarchr
filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")

g <- file_engine(filename) |>
  fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))

print(get_engine(g))

}
