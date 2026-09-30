library(testthat)
library(assertthat)

data(eds_marfan_kg)

test_that("ancestors follows subclass_of transitively upward", {
    g <- eds_marfan_kg |> fetch_nodes(name == "Tall stature")
    anc <- suppressMessages(ancestors(g))

    # compare to repeated one-step expansion
    one_step <- suppressMessages(g |> expand(predicates = "biolink:subclass_of", direction = "out"))
    expect_gt(nrow(nodes(anc)), nrow(nodes(one_step)))
    expect_true(all(nodes(one_step)$id %in% nodes(anc)$id))
    expect_true(all(edges(anc)$predicate == "biolink:subclass_of"))

    # every non-query node is reached going out from the query
    expect_true(all(setdiff(nodes(anc)$id, nodes(g)$id) %in% edges(anc)$object))
})

test_that("descendants follows subclass_of transitively downward", {
    g <- eds_marfan_kg |> fetch_nodes(name == "Abnormality of body height")
    desc <- suppressMessages(descendants(g))

    expect_true(all(edges(desc)$predicate == "biolink:subclass_of"))
    expect_true("Tall stature" %in% nodes(desc)$name)

    # every non-query node is reached going in toward the query
    expect_true(all(setdiff(nodes(desc)$id, nodes(g)$id) %in% edges(desc)$subject))
})
