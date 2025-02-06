#' Generate Default Color Palettes for Monarch Graph Visualization
#'
#' This function returns a list of default color palettes and shape settings
#' for nodes and edges used in Monarch knowledge graph visualizations.
#' Users can run this function to inspect the default settings and
#' override them with custom palettes if desired.
#'
#' @return A list containing color and shape information for nodes and edges:
#' \describe{
#'   \item{continuous}{A vector of hex color codes for continuous color gradients.}
#'   \item{nodes}{A list with the following elements:
#'     \describe{
#'       \item{discrete}{A named vector of hex color codes for discrete node types:
#'         \itemize{
#'           \item \code{"biolink:Disease"}: \code{"#d52473"}
#'           \item \code{"biolink:PhenotypicFeature"}: \code{"#2f9869"}
#'           \item \code{"biolink:Gene"}: \code{"#2c81c6"}
#'         }
#'       }
#'       \item{continuous}{A named vector of hex color codes for continuous node gradients:
#'         \itemize{
#'           \item \code{"biolink:Disease"}: first color in \code{continuous}
#'           \item \code{"biolink:PhenotypicFeature"}: second color in \code{continuous}
#'           \item \code{"biolink:Gene"}: third color in \code{continuous}
#'         }
#'       }
#'       \item{shape}{A named vector of shapes for node types:
#'         \itemize{
#'           \item \code{"biolink:Disease"}: square (\code{15})
#'           \item \code{"biolink:PhenotypicFeature"}: circle (\code{16})
#'           \item \code{"biolink:Gene"}: triangle (\code{18})
#'         }
#'       }
#'     }
#'   }
#'   \item{edges}{A list with the following elements:
#'     \describe{
#'       \item{discrete}{A named vector of hex color codes for discrete edge types:
#'         \itemize{
#'           \item \code{"biolink:causes"}: \code{"black"}
#'           \item \code{"biolink:gene_associated_with_condition"}: \code{"#2c81c6"}
#'           \item \code{"biolink:has_phenotype"}: \code{"#2f9869"}
#'           \item \code{"biolink:subclass_of"}: \code{"darkgrey"}
#'         }
#'       }
#'       \item{continuous}{A named vector of hex color codes for continuous edge gradients:
#'         \itemize{
#'           \item \code{"biolink:causes"}: first color in \code{continuous}
#'           \item \code{"biolink:gene_associated_with_condition"}: second color in \code{continuous}
#'           \item \code{"biolink:has_phenotype"}: third color in \code{continuous}
#'           \item \code{"biolink:subclass_of"}: fourth color in \code{continuous}
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' @examples
#' # View the default palettes
#' monarch_palettes()
#'
#' # Override the palette with custom colors
#' custom_palettes <- monarch_palettes()
#' custom_palettes$nodes$discrete["biolink:Disease"] <- "#ff0000"
#'
#' @export
monarch_palettes <- function() {
	continuous <- c("#2c8c9a","#79e4e7","#a7ecf3","#fafafa","white")
	list(
		continuous=continuous,
		nodes=list(
			discrete = c("biolink:Disease"="#d52473",
									 "biolink:PhenotypicFeature"="#2f9869",
									 "biolink:Gene"="#2c81c6"),
			continuous = c("biolink:Disease"=continuous[1],
										 "biolink:PhenotypicFeature"=continuous[2],
										 "biolink:Gene"=continuous[3]),
			shape=c("biolink:Disease"=15,
							"biolink:PhenotypicFeature"=16,
							"biolink:Gene"=18)
		),
		edges=list(
			discrete = c("biolink:causes"="black",
									 "biolink:gene_associated_with_condition"="#2c81c6",
									 "biolink:has_phenotype"="#2f9869",
									 "biolink:subclass_of"="darkgrey"),
			continuous = c("biolink:causes"=continuous[1],
										 "biolink:gene_associated_with_condition"=continuous[2],
										 "biolink:has_phenotype"=continuous[3],
										 "biolink:subclass_of"=continuous[4])
		)
	)
}
