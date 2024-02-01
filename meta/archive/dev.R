# # bolt://24.144.94.219:7687

# # Load libraries
# library(tidyverse)
# library(neo4r)

# library(neo4r)
# con <- neo4j_api$new(
#   url = "http://24.144.94.219:7474",
#   user = "",
#   password = ""
# )

# # Test the endpoint, that will not work :
# con$ping()

# x <- con$get_labels()
# x

# # get node with id: MONDO:0011476 and it's neighbors
# z <- "MATCH (n)-[r]-(m) WHERE n.id = 'MONDO:0011476' RETURN n" %>%
#   call_neo4j(con)

# #########################

# library(neo2R)
# graph <- startGraph("bolt://24.144.94.219:7687")

