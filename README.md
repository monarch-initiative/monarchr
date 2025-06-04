monarchr
================
[![License: MIT + file
LICENSE](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-blue.svg)](https://cran.r-project.org/web/licenses/MIT%20+%20file%20LICENSE)
[![](https://img.shields.io/badge/devel%20version-1.5.0-black.svg)](https://github.com/monarch-initiative/monarchr)
<br> [![R build
status](https://github.com/monarch-initiative/monarchr/workflows/rworkflows/badge.svg)](https://github.com/monarch-initiative/monarchr/actions)
[![](https://codecov.io/gh/monarch-initiative/monarchr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/monarch-initiative/monarchr)
[![DOI](https://zenodo.org/badge/639616520.svg)](https://doi.org/10.5281/zenodo.14553217)
<br>  
<h4>  
Authors: <i>Shawn O’Neil, Brian Schilder</i>  
</h4>
<h4>  
README updated: <i>Jul-20-2024</i>  
</h4>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `monarchr`: Monarch Knowledge Graph Queries

### R package for easy access, manipulation, and analysis of knowledge graphs.

`monarchr` provides a tidy interface to data hosted at
<https://monarchinitiative.org>, and other knowledge graphs in KGX format (e.g. those at [KGHub](https://kghub.org/)).

- [Website](https://monarch-initiative.github.io/monarchr)
- [Get
  started](https://monarch-initiative.github.io/monarchr/articles/monarchr)

<!-- If you use `monarchr`, please cite:  -->
<!-- Modify this by editing the file: inst/CITATION  -->
<!-- >  -->

Installation:

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("https://github.com/monarch-initiative/monarchr")
library(monarchr)
```
