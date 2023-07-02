#' Expand a monarch_kg graph.
#' 
#' @description Expand a monarch_kg graph by adding nodes linked to the selected node set of the given types. 
#' All selected nodes will be expanded from, even if they are of different types. For example,
#' if the selected nodes include a disease and a gene, and the types include "gene", then all
#' genes linked to the selected disease and all genes linked to the selected gene will be added
#' to the graph.
#' 
#' @param graph A monarch_kg graph object.
#' 