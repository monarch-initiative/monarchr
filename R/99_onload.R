.onLoad <- function(libname, pkgname) {
    options("monarchr.base_api_url" = "https://api-v3.monarchinitiative.org/v3/api")

    assoc_path <- system.file("association_type_mappings.yaml", package = pkgname)
    assoc_map <- yaml::yaml.load_file(assoc_path)

    pkg_env <- asNamespace(pkgname)
    assign("association_type_mappings", assoc_map, envir = pkg_env)


    #message("Thank you for choosing Monarch for your data integration needs.")
}
