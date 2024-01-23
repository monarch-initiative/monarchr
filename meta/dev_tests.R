library(monarchr)
library(tidygraph)

monarch_search("syndrome", limit = 5) %>%
    activate(nodes) %>%
    filter(label == "EEC syndrome and related syndrome")

monarch_search("syndrome", limit = 30)
monarch_search("syndrome", limit = 20)


library(neo2R)
graph <- startGraph(
	"http://24.144.94.219:7474",
	#"http://neo4j-bolt.monarchinitiative.org:7474"
	#username="neo4j", password="donttrustusers",
	#importPath="~/neo4j_home/neo4jImport",
	#.opts = list(ssl_verifypeer=0)

)

df <- cypher(
	graph,
	prepCql(
		'MATCH (n)-[r]-(p) return n,r,p limit 2000'
	),
	result = "graph"
)




