monarchr
================
[![License: MIT + file
LICENSE](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-blue.svg)](https://cran.r-project.org/web/licenses/MIT%20+%20file%20LICENSE)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.14553217-blue.svg)](https://doi.org/10.5281/zenodo.14553217)
[![](https://img.shields.io/badge/devel%20version-1.6.2-black.svg)](https://github.com/monarch-initiative/monarchr)
[![](https://img.shields.io/github/languages/code-size/monarch-initiative/monarchr.svg)](https://github.com/monarch-initiative/monarchr)
[![](https://img.shields.io/github/last-commit/monarch-initiative/monarchr.svg)](https://github.com/monarch-initiative/monarchr/commits/main)
<br> [![R build
status](https://github.com/monarch-initiative/monarchr/workflows/rworkflows/badge.svg)](https://github.com/monarch-initiative/monarchr/actions)
[![](https://codecov.io/gh/monarch-initiative/monarchr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/monarch-initiative/monarchr)
<br>  
<h4>  
Authors: <i>Shawn O’Neil, Brian Schilder</i>  
</h4>
README updated: <i>Mar-09-2026</i>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `monarchr`: Monarch Knowledge Graph Queries

### R package for easy access, manipulation, and analysis of Monarch KG data Resources.

`monarchr` provides a tidy interface to data hosted at
<https://monarchinitiative.org>, and other knowledge graphs in KGX
format (e.g. those at [KGHub](https://kghub.org/)).

- [Website](https://monarch-initiative.github.io/monarchr/)
- [Get
  started](https://monarch-initiative.github.io/monarchr/articles/monarchr.html)

<!-- If you use `monarchr`, please cite:  -->

<!-- Modify this by editing the file: inst/CITATION  -->

<!-- >  -->

Installation:

``` r
if(!require("BiocManager")) install.packages("BiocManager")

BiocManager::install("monarch-initiative/monarchr", update=FALSE)
library(monarchr)
```
