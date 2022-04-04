#' Apply a function to nodes in the order of a breath first search
#'
#' These functions allow you to map over the nodes in a graph, by first
#' performing a breath first search on the graph and then mapping over each
#' node in the order they are visited. The mapping function will have access to
#' the result and search statistics for all the nodes between itself and the
#' root in the search. To map over the nodes in the reverse direction use
#' [map_bfs_back()].
#'
#' @details
#' The function provided to `.f` will be called with the following arguments in
#' addition to those supplied through `...`:
#'
#' * `graph`: The full `tbl_graph` object
#' * `node`: The index of the node currently mapped over
#' * `rank`: The rank of the node in the search
#' * `parent`: The index of the node that led to the current node
#' * `before`: The index of the node that was visited before the current node
#' * `after`: The index of the node that was visited after the current node.
#' * `dist`: The distance of the current node from the root
#' * `path`: A table containing `node`, `rank`, `parent`, `before`, `after`,
#'   `dist`, and `result` columns giving the values for each node leading to the
#'   current node. The `result` column will contain the result of the mapping
#'   of each node in a list.
#'
#' Instead of spelling out all of these in the function it is possible to simply
#' name the ones needed and use `...` to catch the rest.
#'
#' @param root The node to start the search from
#'
#' @param mode How should edges be followed? `'out'` only follows outbound
#' edges, `'in'` only follows inbound edges, and `'all'` follows all edges. This
#' parameter is ignored for undirected graphs.
#'
#' @param unreachable Should the search jump to an unvisited node if the search
#' is completed without visiting all nodes.
#'
#' @param .f A function to map over all nodes. See Details
#'
#' @param ... Additional parameters to pass to `.f`
#'
#' @return `map_bfs()` returns a list of the same length as the number of nodes
#' in the graph, in the order matching the node order in the graph (that is, not
#' in the order they are called). `map_bfs_*()` tries to coerce its result into
#' a vector of the classes `logical` (`map_bfs_lgl`), `character`
#' (`map_bfs_chr`), `integer` (`map_bfs_int`), or `double` (`map_bfs_dbl`).
#' These functions will throw an error if they are unsuccesful, so they are type
#' safe.
#'
#' @family node map functions
#'
#' @export
#' @importFrom igraph gorder
#'
#' @examples
#' # Accumulate values along a search
#' create_tree(40, children = 3, directed = TRUE) %>%
#'   mutate(value = round(runif(40)*100)) %>%
#'   mutate(value_acc = map_bfs_dbl(node_is_root(), .f = function(node, path, ...) {
#'     sum(.N()$value[c(node, path$node)])
#'   }))
map_bfs <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  dot_params <- list(...)
  search_df <- bfs_df(graph, root, mode, unreachable)
  paths <- get_paths(as.integer(search_df$parent))
  call_nodes(graph, .f, search_df, paths, dot_params)
}
#' @rdname map_bfs
#' @export
map_bfs_lgl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = logical(1))
}
#' @rdname map_bfs
#' @export
map_bfs_chr <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = character(1))
}
#' @rdname map_bfs
#' @export
map_bfs_int <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = integer(1))
}
#' @rdname map_bfs
#' @export
map_bfs_dbl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = double(1))
}
#' Apply a function to nodes in the reverse order of a breath first search
#'
#' These functions allow you to map over the nodes in a graph, by first
#' performing a breath first search on the graph and then mapping over each
#' node in the reverse order they are visited. The mapping function will have
#' access to the result and search statistics for all the nodes following itself
#' in the search. To map over the nodes in the original direction use
#' [map_bfs()].
#'
#' @details
#' The function provided to `.f` will be called with the following arguments in
#' addition to those supplied through `...`:
#'
#' * `graph`: The full `tbl_graph` object
#' * `node`: The index of the node currently mapped over
#' * `rank`: The rank of the node in the search
#' * `parent`: The index of the node that led to the current node
#' * `before`: The index of the node that was visited before the current node
#' * `after`: The index of the node that was visited after the current node.
#' * `dist`: The distance of the current node from the root
#' * `path`: A table containing `node`, `rank`, `parent`, `before`, `after`,
#'   `dist`, and `result` columns giving the values for each node reached from
#'   the current node. The `result` column will contain the result of the mapping
#'   of each node in a list.
#'
#' Instead of spelling out all of these in the function it is possible to simply
#' name the ones needed and use `...` to catch the rest.
#'
#' @inheritParams map_bfs
#'
#' @return `map_bfs_back()` returns a list of the same length as the number of
#' nodes in the graph, in the order matching the node order in the graph (that
#' is, not in the order they are called). `map_bfs_back_*()` tries to coerce
#' its result into a vector of the classes `logical` (`map_bfs_back_lgl`),
#' `character` (`map_bfs_back_chr`), `integer` (`map_bfs_back_int`), or `double`
#' (`map_bfs_back_dbl`). These functions will throw an error if they are
#' unsuccesful, so they are type safe.
#'
#' @family node map functions
#'
#' @export
#' @importFrom igraph gorder
#'
#' @examples
#' # Collect values from children
#' create_tree(40, children = 3, directed = TRUE) %>%
#'   mutate(value = round(runif(40)*100)) %>%
#'   mutate(child_acc = map_bfs_back_dbl(node_is_root(), .f = function(node, path, ...) {
#'     if (nrow(path) == 0) .N()$value[node]
#'     else {
#'       sum(unlist(path$result[path$parent == node]))
#'     }
#'   }))
map_bfs_back <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  dot_params <- list(...)
  search_df <- bfs_df(graph, root, mode, unreachable)
  offspring <- get_offspring(as.integer(search_df$parent), order(search_df$rank))
  call_nodes(graph, .f, search_df, offspring, dot_params, reverse = TRUE)
}
#' @rdname map_bfs_back
#' @export
map_bfs_back_lgl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = logical(1))
}
#' @rdname map_bfs_back
#' @export
map_bfs_back_chr <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = character(1))
}
#' @rdname map_bfs_back
#' @export
map_bfs_back_int <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = integer(1))
}
#' @rdname map_bfs_back
#' @export
map_bfs_back_dbl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_bfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = double(1))
}
#' Apply a function to nodes in the order of a depth first search
#'
#' These functions allow you to map over the nodes in a graph, by first
#' performing a depth first search on the graph and then mapping over each
#' node in the order they are visited. The mapping function will have access to
#' the result and search statistics for all the nodes between itself and the
#' root in the search. To map over the nodes in the reverse direction use
#' [map_dfs_back()].
#'
#' @details
#' The function provided to `.f` will be called with the following arguments in
#' addition to those supplied through `...`:
#'
#' * `graph`: The full `tbl_graph` object
#' * `node`: The index of the node currently mapped over
#' * `rank`: The rank of the node in the search
#' * `rank_out`: The rank of the completion of the nodes subtree
#' * `parent`: The index of the node that led to the current node
#' * `dist`: The distance of the current node from the root
#' * `path`: A table containing `node`, `rank`, `rank_out`, `parent`, dist`, and
#'   `result` columns giving the values for each node leading to the
#'   current node. The `result` column will contain the result of the mapping
#'   of each node in a list.
#'
#' Instead of spelling out all of these in the function it is possible to simply
#' name the ones needed and use `...` to catch the rest.
#'
#' @inheritParams map_bfs
#'
#' @return `map_dfs()` returns a list of the same length as the number of nodes
#' in the graph, in the order matching the node order in the graph (that is, not
#' in the order they are called). `map_dfs_*()` tries to coerce its result into
#' a vector of the classes `logical` (`map_dfs_lgl`), `character`
#' (`map_dfs_chr`), `integer` (`map_dfs_int`), or `double` (`map_dfs_dbl`).
#' These functions will throw an error if they are unsuccesful, so they are type
#' safe.
#'
#' @family node map functions
#'
#' @export
#' @importFrom igraph gorder
#'
#' @examples
#' # Add a random integer to the last value along a search
#' create_tree(40, children = 3, directed = TRUE) %>%
#'   mutate(child_acc = map_dfs_int(node_is_root(), .f = function(node, path, ...) {
#'     last_val <- if (nrow(path) == 0) 0L else tail(unlist(path$result), 1)
#'     last_val + sample(1:10, 1)
#'   }))
map_dfs <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  dot_params <- list(...)
  search_df <- dfs_df(graph, root, mode, unreachable)
  paths <- get_paths(as.integer(search_df$parent))
  call_nodes(graph, .f, search_df, paths, dot_params)
}
#' @rdname map_dfs
#' @export
map_dfs_lgl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = logical(1))
}
#' @rdname map_dfs
#' @export
map_dfs_chr <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = character(1))
}
#' @rdname map_dfs
#' @export
map_dfs_int <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = integer(1))
}
#' @rdname map_dfs
#' @export
map_dfs_dbl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = double(1))
}
#' Apply a function to nodes in the reverse order of a depth first search
#'
#' These functions allow you to map over the nodes in a graph, by first
#' performing a depth first search on the graph and then mapping over each
#' node in the reverse order they are visited. The mapping function will have
#' access to the result and search statistics for all the nodes following itself
#' in the search. To map over the nodes in the original direction use
#' [map_dfs()].
#'
#' @details
#' The function provided to `.f` will be called with the following arguments in
#' addition to those supplied through `...`:
#'
#' * `graph`: The full `tbl_graph` object
#' * `node`: The index of the node currently mapped over
#' * `rank`: The rank of the node in the search
#' * `rank_out`: The rank of the completion of the nodes subtree
#' * `parent`: The index of the node that led to the current node
#' * `dist`: The distance of the current node from the root
#' * `path`: A table containing `node`, `rank`, `rank_out`, `parent`, dist`, and
#'   `result` columns giving the values for each node reached from
#'   the current node. The `result` column will contain the result of the mapping
#'   of each node in a list.
#'
#' Instead of spelling out all of these in the function it is possible to simply
#' name the ones needed and use `...` to catch the rest.
#'
#' @inheritParams map_bfs
#'
#' @return `map_dfs_back()` returns a list of the same length as the number of
#' nodes in the graph, in the order matching the node order in the graph (that
#' is, not in the order they are called). `map_dfs_back_*()` tries to coerce
#' its result into a vector of the classes `logical` (`map_dfs_back_lgl`),
#' `character` (`map_dfs_back_chr`), `integer` (`map_dfs_back_int`), or `double`
#' (`map_dfs_back_dbl`). These functions will throw an error if they are
#' unsuccesful, so they are type safe.
#'
#' @family node map functions
#'
#' @export
#' @importFrom igraph gorder
#'
#' @examples
#' # Collect values from the 2 closest layers of children in a dfs search
#' create_tree(40, children = 3, directed = TRUE) %>%
#'   mutate(value = round(runif(40)*100)) %>%
#'   mutate(child_acc = map_dfs_back(node_is_root(), .f = function(node, path, dist, ...) {
#'     if (nrow(path) == 0) .N()$value[node]
#'     else {
#'       unlist(path$result[path$dist - dist <= 2])
#'     }
#'   }))
map_dfs_back <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  dot_params <- list(...)
  search_df <- dfs_df(graph, root, mode, unreachable)
  offspring <- get_offspring(as.integer(search_df$parent), order(search_df$rank))
  call_nodes(graph, .f, search_df, offspring, dot_params, reverse = TRUE)
}
#' @rdname map_dfs_back
#' @export
map_dfs_back_lgl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = logical(1))
}
#' @rdname map_dfs_back
#' @export
map_dfs_back_chr <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = character(1))
}
#' @rdname map_dfs_back
#' @export
map_dfs_back_int <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = integer(1))
}
#' @rdname map_dfs_back
#' @export
map_dfs_back_dbl <- function(root, mode = 'out', unreachable = FALSE, .f, ...) {
  res <- map_dfs_back(root = root, mode = mode, unreachable = unreachable, .f = .f, ...)
  as_vector(res, .type = double(1))
}
#' Map a function over a graph representing the neighborhood of each node
#'
#' This function extracts the neighborhood of each node as a graph and maps over
#' each of these neighborhood graphs. Conceptually it is similar to
#' [igraph::local_scan()], but it borrows the type safe versions available in
#' [map_bfs()] and [map_dfs()].
#'
#' @details
#' The function provided to `.f` will be called with the following arguments in
#' addition to those supplied through `...`:
#'
#' * `neighborhood`: The neighborhood graph of the node
#' * `graph`: The full `tbl_graph` object
#' * `node`: The index of the node currently mapped over
#'
#' The `neighborhood` graph will contain an extra node attribute called
#' `.central_node`, which will be `TRUE` for the node that the neighborhood is
#' expanded from and `FALSE` for everything else.
#'
#' @inheritParams igraph::ego
#' @inheritParams map_bfs
#'
#' @return `map_local()` returns a list of the same length as the number of
#' nodes in the graph, in the order matching the node order in the graph.
#' `map_local_*()` tries to coerce its result into a vector of the classes
#' `logical` (`map_local_lgl`), `character` (`map_local_chr`), `integer`
#' (`map_local_int`), or `double` (`map_local_dbl`). These functions will throw
#' an error if they are unsuccesful, so they are type safe.
#'
#' @importFrom igraph gorder make_ego_graph V<-
#' @export
#'
#' @examples
#' # Smooth out values over a neighborhood
#' create_notable('meredith') %>%
#'   mutate(value = rpois(graph_order(), 5)) %>%
#'   mutate(value_smooth = map_local_dbl(order = 2, .f = function(neighborhood, ...) {
#'     mean(as_tibble(neighborhood, active = 'nodes')$value)
#'   }))
map_local <- function(order = 1, mode = 'all', mindist = 0, .f, ...) {
  expect_nodes()
  graph <- .G()
  V(graph)$.central_node <- FALSE
  res <- lapply(seq_len(gorder(graph)), function(i) {
    V(graph)$.central_node[i] <- TRUE
    ego_graph <- make_ego_graph(graph, order = order, nodes = i, mode = mode, mindist = mindist)[[1]]
    .f(neighborhood = as_tbl_graph(ego_graph), graph = graph, node = i, ...)
  })
}
#' @rdname map_local
#' @export
map_local_lgl <- function(order = 1, mode = 'all', mindist = 0, .f, ...) {
  res <- map_local(order = order, mode = mode, mindist = mindist, .f = .f, ...)
  as_vector(res, .type = logical(1))
}
#' @rdname map_local
#' @export
map_local_chr <- function(order = 1, mode = 'all', mindist = 0, .f, ...) {
  res <- map_local(order = order, mode = mode, mindist = mindist, .f = .f, ...)
  as_vector(res, .type = character(1))
}
#' @rdname map_local
#' @export
map_local_int <- function(order = 1, mode = 'all', mindist = 0, .f, ...) {
  res <- map_local(order = order, mode = mode, mindist = mindist, .f = .f, ...)
  as_vector(res, .type = integer(1))
}
#' @rdname map_local
#' @export
map_local_dbl <- function(order = 1, mode = 'all', mindist = 0, .f, ...) {
  res <- map_local(order = order, mode = mode, mindist = mindist, .f = .f, ...)
  as_vector(res, .type = double(1))
}

# Helpers -----------------------------------------------------------------

#' @importFrom igraph bfs
#' @importFrom tibble tibble
bfs_df <- function(graph, root, mode, unreachable) {
  search <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
                order = TRUE, rank = TRUE, father = TRUE, pred = TRUE,
                succ = TRUE, dist = TRUE)
  nodes <- seq_along(search$order)
  tibble(
    node = nodes,
    rank = as.integer(search$rank),
    parent = as.integer(search$father),
    before = as.integer(search$pred),
    after = as.integer(search$succ),
    dist = as.integer(search$dist),
    result = rep(list(NULL), length(nodes))
  )
}
#' @importFrom igraph dfs
#' @importFrom tibble tibble
dfs_df <- function(graph, root, mode, unreachable) {
  search <- dfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
                order = TRUE, order.out = TRUE, father = TRUE, dist = TRUE)
  nodes <- seq_along(search$order)
  tibble(
    node = nodes,
    rank = match(nodes, as.integer(search$order)),
    rank_out = match(nodes, as.integer(search$order.out)),
    parent = as.integer(search$father),
    dist = as.integer(search$dist),
    result = rep(list(NULL), length(nodes))
  )
}
call_nodes <- function(graph, .f, search, connections, dot_params, reverse = FALSE) {
  not_results <- which(names(search) != 'result')
  call_order <- order(search$rank)
  if (reverse) call_order <- rev(call_order)
  for (i in call_order) {
    if (is.na(i)) break

    conn <- connections[[i]]
    search$result[[i]] <- do.call(
      .f,
      c(list(graph = graph),
        as.list(search[i, not_results]),
        list(path = search[conn, , drop = FALSE]),
        dot_params)
    )
  }
  search$result
}
get_offspring <- function(parent, order) {
  offspring <- rep(list(integer(0)), length(parent))
  direct_offspring <- split(seq_along(parent), parent)
  offspring[as.integer(names(direct_offspring))] <- direct_offspring
  offspring <- collect_offspring(offspring, as.integer(rev(order)))
  lapply(offspring, function(x) x[order(match(x, order))])
}

# Avoid importing full purrr for as_vector fun
can_simplify <- function(x, type = NULL) {
  is_atomic <- vapply(x, is.atomic, logical(1))
  if (!all(is_atomic))
    return(FALSE)
  mode <- unique(vapply(x, typeof, character(1)))
  if (length(mode) > 1 && !all(c("double", "integer") %in%
                               mode)) {
    return(FALSE)
  }
  is.null(type) || can_coerce(x, type)
}
can_coerce <- function(x, type) {
  actual <- typeof(x[[1]])
  if (is_mold(type)) {
    lengths <- unique(lengths(x))
    if (length(lengths) > 1 || !(lengths == length(type))) {
      return(FALSE)
    }
    else {
      type <- typeof(type)
    }
  }
  if (actual == "integer" && type %in% c("integer", "double",
                                         "numeric")) {
    return(TRUE)
  }
  if (actual %in% c("integer", "double") && type == "numeric") {
    return(TRUE)
  }
  actual == type
}
is_mold <- function (type) {
  modes <- c("numeric", "logical", "integer", "double", "complex",
             "character", "raw")
  length(type) > 1 || (!type %in% modes)
}
as_vector <- function(.x, .type = NULL){
  null_elem <- sapply(.x, is.null)
  if (any(null_elem)) {
    na <- rep(NA, length(.type))
    class(na) <- class(.type)
    .x[null_elem] <- na
  }
  if (can_simplify(.x, .type)) {
    unlist(.x)
  }
  else {
    type <- deparse(substitute(.type))
    cli::cli_abort("Cannot coerce values to {.cls {type}}")
  }
}
