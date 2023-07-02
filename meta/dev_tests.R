library(monarchr)
library(tidygraph)

monarch_search("syndrome", limit = 5) %>%
    activate(nodes) %>%
    filter(label == "EEC syndrome and related syndrome")

monarch_search("syndrome", limit = 30)
monarch_search("syndrome", limit = 20)
