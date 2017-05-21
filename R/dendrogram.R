#' @describeIn tbl_graph Method for dendrogram objects
#' @importFrom dplyr bind_rows
#' @export
as_tbl_graph.dendrogram <- function(x, directed = TRUE, mode = 'out', ...) {
  x <- identify_nodes(x)

  nodes <- get_nodes(x)
  extraPar <- bind_rows(lapply(nodes$nodePar, as.data.frame, stringsAsFactors = FALSE))
  nodes$nodePar <- NULL
  nodes <- cbind(nodes, extraPar)
  nodes <- nodes[order(nodes$.tidygraph_id), ]
  nodes$.tidygraph_id <- NULL
  if (all(nodes$label == '')) nodes$label <- NULL

  edges <- get_edges(x)
  extraPar <- bind_rows(lapply(edges$edgePar, as.data.frame, stringsAsFactors = FALSE))
  edges$edgePar <- NULL
  edges <- cbind(edges, extraPar)
  if (all(edges$label == '')) edges$label <- NULL
  if (directed && mode == 'in') {
    edges[, c('from', 'to')] <- edges[, c('to', 'from')]
  }
  as_tbl_graph(list(nodes = nodes, edges = edges), directed = directed)
}
#' @importFrom stats is.leaf
identify_nodes <- function(den, start = 1) {
  if (is.leaf(den)) {
    attr(den, '.tidygraph_id') <- start
  } else {
    den[[1]] <- identify_nodes(den[[1]], start)
    den[[2]] <- identify_nodes(den[[2]], attr(den[[1]], '.tidygraph_id') + 1)
    attr(den, '.tidygraph_id') <- attr(den[[2]], '.tidygraph_id') + 1
  }
  den
}
#' @importFrom stats is.leaf
get_nodes <- function(den) {
  id <- attr(den, '.tidygraph_id')
  label <- attr(den, 'label')
  if (is.null(label)) label <- ''
  members <- attr(den, 'members')
  nodePar <- attr(den, 'nodePar')
  if (is.null(nodePar)) nodePar <- data.frame(row.names = 1)
  if (is.leaf(den)) {
    list(
      height = attr(den, 'height'),
      .tidygraph_id = id,
      leaf = TRUE,
      label = label,
      members = members,
      nodePar = list(nodePar)
    )
  } else {
    coord1 <- get_nodes(den[[1]])
    coord2 <- get_nodes(den[[2]])
    list(
      height = c(coord1$height, coord2$height, attr(den, 'height')),
      .tidygraph_id = c(coord1$.tidygraph_id, coord2$.tidygraph_id, id),
      leaf = c(coord1$leaf, coord2$leaf, FALSE),
      label = c(coord1$label, coord2$label, label),
      members = c(coord1$members, coord2$members, members),
      nodePar = c(coord1$nodePar, coord2$nodePar, list(nodePar))
    )
  }
}
#' @importFrom stats is.leaf
get_edges <- function(den) {
  id <- attr(den, '.tidygraph_id')
  if (is.leaf(den)) {
    data.frame(row.names = 1)
  } else {
    conn1 <- get_edges(den[[1]])
    conn2 <- get_edges(den[[2]])
    list(
      from = c(conn1$from, conn2$from, rep(id, 2)),
      to = c(conn1$to, conn2$to, unlist(lapply(den, attr, which = '.tidygraph_id'))),
      label = c(conn1$label, conn2$label, unlist(lapply(den, function(subden) {
        lab <- attr(subden, 'edgetext')
        if (is.null(lab)) '' else lab
      }))),
      edgePar = c(conn1$edgePar, conn2$edgePar, lapply(den, function(subden) {
        par <- attr(subden, 'edgePar')
        if (is.null(par)) data.frame(row.names = 1) else par
      }))
    )
  }
}
