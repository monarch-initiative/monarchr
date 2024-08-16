The `eds_marfan_kg.tar.gz` file in this directory was created with the following on Aug 15, 2024,
to represent Ehlers-Danlos syndrome (MONDO:0020066), Marfan syndrome (MONDO:0007947), all their subtypes,
all entities connected to those diseases or their subtypes, and finally all parents (supertypes) of all
those diseases and entities.

```
monarch_engine() |> 
  fetch_nodes(query_ids = c("MONDO:0020066", "MONDO:0007947")) |> 
  expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |> 
  expand() |> 
  expand(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE) |> 
  save_kgx("eds_marfan_kg.tar.gz")
```
