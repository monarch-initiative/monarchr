% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cypher_query.neo4j_engine.R
\name{unname_cols}
\alias{unname_cols}
\title{Remove names from columns}
\usage{
unname_cols(df)
}
\arguments{
\item{df}{Input data frame}
}
\value{
The input, with unnamed columns
}
\description{
Given a data-frame like object, runs each column through unname()
}
