% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monarch_search.R
\name{monarch_search}
\alias{monarch_search}
\title{Search for KG nodes using the Monarch Initiative search API}
\usage{
monarch_search(query, category = NULL, limit = 10, ...)
}
\arguments{
\item{query}{Search query string, e.g. "Cystic fibrosis"}

\item{category}{A set of node category labels to limit the search to, e.g. c("biolink:Disease", "biolink:Gene")}

\item{limit}{Maximum number of nodes to return. Default 10.}

\item{...}{Parameters passed to monarch_engine().}
}
\value{
A local tbl_kgx graph with no edges.
}
\description{
This function is a wrapper around the Monarch-hosted
\href{https://api.monarchinitiative.org/v3/docs#/search/search_v3_api_search_get}{search API}.
It returns nodes (no edges) from the Monarch KG, fetched via an instance of \code{monarch_engine()}.
}
\examples{
\dontshow{if (monarch_engine_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
cf_hits <- monarch_search("Cystic fibrosis", category = "biolink:Disease", limit = 5)
print(cf_hits)
\dontshow{\}) # examplesIf}
}
