% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neo4j_engine.R
\name{neo4j_engine}
\alias{neo4j_engine}
\title{Create a knowledge graph engine object for a neo4j database}
\usage{
neo4j_engine(
  url,
  username = NA,
  password = NA,
  preferences = NULL,
  timeout = 1,
  cache = TRUE,
  ...
)
}
\arguments{
\item{url}{A character string indicating the URL of the neo4j database. If given a vector, each will be tried in sequence; if a URL times out (see timeout) or fails, the next is tried.}

\item{username}{A character string indicating the username for the neo4j database (if needed).}

\item{password}{A character string indicating the password for the neo4j database (if needed).}

\item{preferences}{A named list of preferences for the engine.}

\item{timeout}{Number of sections to wait before trying the next url.}

\item{cache}{Whether to cache query results in memory for the length of the R session.}

\item{...}{Additional arguments passed to \code{neo2R::startGraph()}.}
}
\value{
An object of class \code{neo4j_engine}
}
\description{
Creates a knowledge graph engine backed by a neo4j database, from a URL and optional username and password. Knowledge graph "engines"
are objects that store information about how to connect to a (potentially large) knowledge graph, and can be used to fetch nodes and edges from the database as local
graph objects.
}
\details{
Engines store preference information specifying how data are fetched and manipulated; for example,
while node \code{category} is multi-valued (nodes may have multiple categories, for example "biolink:Gene" and "biolink:NamedThing"),
typically a single category is used to represent the node in a graph, and is returned as the nodes' \code{pcategory}. A preference list of categories to use for \code{pcategory} is
stored in the engine's preferences. A default set of preferences is stored in the package for use with KGX (BioLink-compatible) graphs (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md),
but these can be overridden by the user.

For \code{neo4j_engine()}s, preferences are also used to set the node properties to search when using \code{search_nodes()}, defaulting to regex-based searches on id, name, and description. (The \code{monarch_engine()} is a type
of \code{neo4j_engine()} with the URL set to the Monarch Neo4j instance, and overrides \code{search_nodes()} to use the Monarch search API, see \code{monarch_engine()} for details).
}
\examples{
\dontshow{if (neo4j_engine_check("https://neo4j.monarchinitiative.org")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(tidygraph)
library(dplyr)

engine <- neo4j_engine(url = "https://neo4j.monarchinitiative.org")
res <- engine |> fetch_nodes(query_ids = c("MONDO:0007522", "MONDO:0007947"))
print(res)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{file_engine()}, \code{monarch_engine()}
}
