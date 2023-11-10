#' Fortify a tbl_graph for ggplot2 plotting
#'
#' In general `tbl_graph` objects are intended to be plotted by network
#' visualisation libraries such as `ggraph`. However, if you do wish to plot
#' either the node or edge data directly with `ggplot2` you can simply add the
#' `tbl_graph` object as either the global or layer data and the currently
#' active data is passed on as a regular data frame.
#'
#' @keywords internal
#'
fortify.tbl_graph <- function(model, data, ...) {
  as_tibble(model)
}

rlang::on_load(register_s3_method('ggplot2', 'fortify', 'tbl_graph'))
