.onLoad <- function(libname, pkgname) {
	pref_path <- system.file("kg_prefs.yaml", package = pkgname)
	kg_prefs <- yaml::read_yaml(pref_path)
	options("default_prefs" = kg_prefs)
	invisible()
}
