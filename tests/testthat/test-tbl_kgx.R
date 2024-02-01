library(testthat)
library(assertthat)


test_that("tbl_kgx constructor follows the rules", {
    # first up, properly formatted node and edge dfs should work
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"))
    edges_df <- data.frame(subject = c("a"), object = c("b"), predicate = c("baz"))
    g <- tbl_kgx(nodes_df, edges_df)
    expect_s3_class(g, "tbl_kgx")

    # try one with a selected column
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"), selected = c(TRUE, FALSE))
    edges_df <- data.frame(subject = c("a"), object = c("b"), predicate = c("baz"))
    g <- tbl_kgx(nodes_df, edges_df)
    expect_s3_class(g, "tbl_kgx")

    # if the node df is missing the id column, it should fail
    nodes_df <- data.frame(category = c("foo", "bar"))
    edges_df <- data.frame(subject = c("a"), object = c("b"), predicate = c("baz"))
    expect_error(tbl_kgx(nodes_df, edges_df))

    # if the edge df is missing the subject column, it should fail
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"))
    edges_df <- data.frame(object = c("b"), predicate = c("baz"))
    expect_error(tbl_kgx(nodes_df, edges_df))

    # if the edge df is missing the object column, it should fail
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"))
    edges_df <- data.frame(subject = c("a"), predicate = c("baz"))
    expect_error(tbl_kgx(nodes_df, edges_df))

    # if the edge df is missing the predicate column, it should fail
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"))
    edges_df <- data.frame(subject = c("a"), object = c("b"))
    expect_error(tbl_kgx(nodes_df, edges_df))

    # if a selected column is present, but is not logical, it should fail
    nodes_df <- data.frame(id = c("a", "b"), category = c("foo", "bar"), selected = c("foo", "bar"))
    edges_df <- data.frame(subject = c("a"), object = c("b"), predicate = c("baz"))
    expect_error(tbl_kgx(nodes_df, edges_df))
})