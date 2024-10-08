% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_kgx.R
\name{save_kgx}
\alias{save_kgx}
\title{Save a graph as a KGX-formatted .tar.gz file.}
\usage{
save_kgx(graph, filename = "saved_kgx_graph.tar.gz", ...)
}
\arguments{
\item{graph}{A \code{tbl_kgx} graph to save.}

\item{filename}{File to save the graph to. Must end in .tar.gz.}

\item{...}{Other parameters (unused)}
}
\value{
The input graph (invisibly).
}
\description{
Given a graph, saves it using the tabular KGX format
(see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md) for later
use with \code{load_kgx()} (or even backing an engine with \code{file_engine()}). Note that if
any engine is associated with the graph it is not saved.
}
\examples{
\dontshow{if (monarch_engine_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
phenos <- monarch_engine() |>
          fetch_nodes(query_ids = "MONDO:0007525") |>
          expand(predicates = "biolink:has_phenotype",
                 categories = "biolink:PhenotypicFeature")

save_kgx(phenos, "phenos.tar.gz")

# when loading the graph, we can optionally attach an engine
loaded_phenos <- load_kgx("phenos.tar.gz", attach_engine = monarch_engine())

loaded_phenos

# cleanup saved file
file.remove("phenos.tar.gz")
\dontshow{\}) # examplesIf}
}
