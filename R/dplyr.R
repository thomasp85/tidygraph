# Adapted from purrrlyr dplyr.R

has_recent_dplyr <- FALSE
delayedAssign("has_recent_dplyr", utils::packageVersion("dplyr") >= "0.7.99")

group_data <- function(...) {
  stop("tilt")
}

.onLoad <- function(...) {
  if (has_recent_dplyr) {
    group_data <<- asNamespace("dplyr")$group_data
  }
}

group_indices <- function(data) {
  if (has_recent_dplyr) {
    group_data(data)$.rows
  } else {
    lapply(attr(data, "indices"), `+`, 1L)
  }
}

group_labels <- function(data) {
  if (has_recent_dplyr) {
    labels <- group_data(data)
    dplyr::select(labels, -".rows")
  } else {
    tibble::as_tibble(attr(data, "labels"))
  }
}
