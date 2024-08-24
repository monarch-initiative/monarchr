monarch_palettes <- function(alphas=list(main=1,
																				 pcategory=1,
																				 predicate=1)){
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
