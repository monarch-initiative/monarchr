monarchr
================
[![License: MIT + file
LICENSE](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-blue.svg)](https://cran.r-project.org/web/licenses/MIT%20+%20file%20LICENSE)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.14553217-blue.svg)](https://doi.org/10.5281/zenodo.14553217)
[![](https://img.shields.io/badge/devel%20version-2.99.0-black.svg)](https://github.com/monarch-initiative/monarchr)
[![](https://img.shields.io/github/languages/code-size/monarch-initiative/monarchr.svg)](https://github.com/monarch-initiative/monarchr)
[![](https://img.shields.io/github/last-commit/monarch-initiative/monarchr.svg)](https://github.com/monarch-initiative/monarchr/commits/main)
<br> [![R build
status](https://github.com/monarch-initiative/monarchr/workflows/rworkflows/badge.svg)](https://github.com/monarch-initiative/monarchr/actions)
[![](https://codecov.io/gh/monarch-initiative/monarchr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/monarch-initiative/monarchr)
<br>\
<h4>\
Authors: <i>Shawn O’Neil, Brian Schilder</i>\
</h4>
README updated: <i>Oct-01-2026</i>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## monarchr: Monarch Knowledge Graph Queries

monarchr provides a tidy interface for querying and analyzing biomedical
knowledge graphs (KGs), including the Monarch Initiative KG, which
integrates data on biological entities such as genes, disease, and
phenotypes, and the relationships between them within and across
species. The same set of functions queries the Monarch-hosted Neo4j
database, other Neo4j databases, or local files in the KGX format (such
as those available from KG-Hub), fetching nodes by identifier or
property and expanding along edges by category and predicate. Results
are tidygraph objects that can be manipulated with dplyr verbs and
joined with other graphs. The package also provides transitive closure
and reduction, aggregation over ontology hierarchies, and graph
visualization.

- [Website](https://monarch-initiative.github.io/monarchr/)
- [Get
  started](https://monarch-initiative.github.io/monarchr/articles/monarchr.html)
- [Monarch Initiative](https://monarchinitiative.org)
- [KG-Hub](https://kghub.org/) (more knowledge graphs in KGX format)

<!-- If you use `monarchr`, please cite:  -->

<!-- Modify this by editing the file: inst/CITATION  -->

<!-- >  -->

Installation:

``` r
if(!require("BiocManager")) install.packages("BiocManager")

BiocManager::install("monarch-initiative/monarchr", update=FALSE)
library(monarchr)
```
