% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cypher_query.R
\name{cypher_query}
\alias{cypher_query}
\title{Execute a Cypher Query}
\usage{
cypher_query(engine, query, parameters = NULL, ...)
}
\arguments{
\item{engine}{A neo4j KG engine}

\item{query}{A string representing the Cypher query. Multiple queries may be passed as a vector; if so, Neo2R::multicypher if used and the result is returned as a single joined graph.}

\item{parameters}{A list of parameters for the Cypher query. Default is an empty list.}

\item{...}{Additional arguments passed to the function.}
}
\value{
The result of the Cypher query as a tbl_kgx graph.
}
\description{
This function takes a Cypher query and parameters, executes the query using the given engine, and returns the result as a tbl_kgx graph.
}
\examples{
\dontshow{if (monarch_engine_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
engine <- monarch_engine()

query <- "MATCH (n) WHERE n.id IN $ids RETURN n LIMIT 10"
ids <- c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021")
parameters <- list(ids = ids)

result <- cypher_query(engine, query, parameters)
print(result)
\dontshow{\}) # examplesIf}
}
