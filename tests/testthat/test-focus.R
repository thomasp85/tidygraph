test_that("focusing and unfocusing behaves", {
  gr <- create_notable('meredith')
  expect_false(is.focused_tbl_graph(focus(gr, TRUE)))
  expect_false(is.focused_tbl_graph(focus(gr, FALSE)))

  gr_focus <- focus(gr, dplyr::row_number() %in% 6:10)
  expect_s3_class(gr_focus, 'focused_tbl_graph')

  expect_equal(focus_ind(gr_focus), 6:10)
  expect_equal(focus_ind(gr), seq_len(70))

  expect_false(is.focused_tbl_graph(activate(gr_focus, edges)))
  expect_false(is.focused_tbl_graph(group_by(gr_focus, dplyr::row_number() %% 2 == 0)))
  expect_false(is.focused_tbl_graph(convert(gr_focus, to_complement)))
  expect_false(is.focused_tbl_graph(unfocus(gr_focus)))

  gr_morph_focus <- morph(gr, to_complement) |> focus(dplyr::row_number() == 1)
  expect_s3_class((crystallise(gr_morph_focus) |> pull(graph))[[1]], 'focused_tbl_graph')
  expect_false(is.focused_tbl_graph((crystallise(unfocus(gr_morph_focus)) |> pull(graph))[[1]]))
  expect_false(is.focused_tbl_graph(unmorph(gr_morph_focus)))

  gr_group_focus <- group_by(gr, dplyr::row_number() %% 2 == 0) |>
    focus(dplyr::row_number() %in% 1:6)
  expect_s3_class(gr_group_focus, 'focused_tbl_graph')
  expect_false(is.focused_tbl_graph(ungroup(gr_group_focus)))

  expect_false(is.focused_tbl_graph(arrange(gr_focus, rev(dplyr::row_number()))))
  expect_false(is.focused_tbl_graph(bind_nodes(gr_focus, mtcars)))
  expect_false(is.focused_tbl_graph(bind_edges(gr_focus, data.frame(from = 1:3, to = 4:6))))
  expect_false(is.focused_tbl_graph(bind_graphs(gr_focus, create_notable('bull'))))
  expect_false(is.focused_tbl_graph(distinct(gr_focus, dplyr::row_number())))
  expect_false(is.focused_tbl_graph(filter(gr_focus, dplyr::row_number() < 10)))
  expect_false(is.focused_tbl_graph(slice(gr_focus, 1:4)))

  join_tbl <- data.frame(from = 1:70, to = 70:1, info = as.character(1:70))
  gr_focus <- activate(gr_focus, edges) |> focus(dplyr::row_number() %in% 6:10)
  expect_s3_class(left_join(gr_focus, join_tbl), 'focused_tbl_graph')
  expect_false(is.focused_tbl_graph(right_join(gr_focus, join_tbl)))
  expect_false(is.focused_tbl_graph(full_join(gr_focus, join_tbl)))
  expect_false(is.focused_tbl_graph(inner_join(gr_focus, join_tbl)))
  expect_false(is.focused_tbl_graph(semi_join(gr_focus, join_tbl)))
  expect_false(is.focused_tbl_graph(anti_join(gr_focus, join_tbl)))
})

test_that("modifying a focused graph only affects the focus", {
  gr <- create_notable('meredith') |>
    mutate(col1 = 1) |>
    focus(dplyr::row_number() %in% 6:10) |>
    mutate(col1 = 2, col2 = 2) |>
    unfocus()

  expect_equal(pull(gr, col1), c(rep(1, 5), rep(2, 5), rep(1, 60)))
  expect_equal(pull(gr, col2), c(rep(NA, 5), rep(2, 5), rep(NA, 60)))
})

test_empty_context()
