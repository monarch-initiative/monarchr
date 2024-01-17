.onLoad <- function(libname, pkgname) {
    options("monarchr.base_api_url" = "https://api-v3.monarchinitiative.org/v3/api")

    assoc_path <- system.file("edge_categories.tsv", package = pkgname)
    association_categories_df <- read.table(assoc_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    association_categories_df <- association_categories_df[association_categories_df$association_category != "biolink:Association", ]

    # print(head(assoc_df))) 
    #   association_category          subject_category           predicate           object_category
    # 1  biolink:Association   biolink:MolecularEntity  biolink:related_to   biolink:MolecularEntity
    # 2  biolink:Association   biolink:MolecularEntity biolink:subclass_of   biolink:MolecularEntity
    # 3  biolink:Association biolink:PhenotypicQuality  biolink:related_to   biolink:MolecularEntity
    # 4  biolink:Association   biolink:MolecularEntity  biolink:related_to     biolink:SmallMolecule
    # 5  biolink:Association   biolink:MolecularEntity  biolink:related_to biolink:PhenotypicQuality
    # 6  biolink:Association biolink:PhenotypicQuality  biolink:related_to           biolink:Protein

    pkg_env <- asNamespace(pkgname)
    assign("association_categories_df", association_categories_df, envir = pkg_env)


    # message("Thank you for choosing Monarch for your data integration needs.")
}
