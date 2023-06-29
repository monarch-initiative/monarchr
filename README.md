
# monarchr

<!-- badges: start -->
<!-- badges: end -->

MonarchR provides a tidy interface to data hosted at https://monarchinitiative.org. The package is currently in development and subject to significant change.

## Installation

You can install the development version of monarchr like so:

``` r
install.packages("devtools")
devtools::install_github("monarch-initiative/monarchr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(monarchr)

graph <- monarch_search("Cystic Fibrosis", limit = 10)
graph
```

Output: 

```
# A tbl_graph: 10 nodes and 0 edges
#
# A rooted forest with 10 trees
#
# A tibble: 10 × 10
  id            categories label                                                   description              symbol synonyms taxon inheritance_id inheritance_name selected
  <chr>         <list>     <chr>                                                   <chr>                    <chr>  <list>   <chr> <chr>          <chr>            <lgl>   
1 MONDO:0009061 <chr [1]>  cystic fibrosis                                         Cystic fibrosis (CF) is… NA     <chr>    NA    HP:0000007     HP:0000007       TRUE    
2 MONDO:0005413 <chr [1]>  cystic fibrosis associated meconium ileus               Cystic fibrosis associa… NA     <chr>    NA    NA             NA               TRUE    
3 MONDO:0009062 <chr [1]>  cystic fibrosis-gastritis-megaloblastic anemia syndrome A rare genetic disease … NA     <chr>    NA    NA             NA               TRUE    
4 HGNC:51351    <chr [1]>  CFTR pseudogene 2                                       NA                       CFTRP2 <chr>    NA    NA             NA               TRUE    
5 HGNC:51352    <chr [1]>  CFTR pseudogene 3                                       NA                       CFTRP3 <chr>    NA    NA             NA               TRUE    
6 HGNC:16182    <chr [1]>  CFTR pseudogene 1                                       NA                       CFTRP1 <chr>    NA    NA             NA               TRUE    
# ℹ 4 more rows
# ℹ Use `print(n = ...)` to see more rows
#
# A tibble: 0 × 2
# ℹ 2 variables: from <int>, to <int>
```
