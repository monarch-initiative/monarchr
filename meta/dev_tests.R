library(monarchr)
library(tidygraph)

monarch_search("syndrome", limit = 5) %>%
    activate(nodes) %>%
    filter(label == "EEC syndrome and related syndrome")