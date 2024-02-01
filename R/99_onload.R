.onLoad <- function(libname, pkgname) {
	options(monarch_neo4j_http = "http://24.144.94.219:7474")
	options(monarch_base_api_url = "https://api-v3.monarchinitiative.org/v3/api")

	pref_path <- system.file("kg_prefs.yaml", package = pkgname)

	kg_prefs <- yaml::read_yaml(pref_path)
	monarch_graph_conn <- neo2R::startGraph(getOption("monarch_neo4j_http"))

	pkg_env <- asNamespace(pkgname)

	assign("kg_prefs", kg_prefs, envir = pkg_env)
	assign("graph_connections", list(monarch = monarch_graph_conn), envir = pkg_env)

	invisible()
}
