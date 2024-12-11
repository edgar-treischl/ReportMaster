# tmp.var <- stringr::str_split(tmp.meta,"#") |> unlist()
# tmp.rprtpckg <- tmp.var[1]
# tmp.plotid <- tmp.var[2]
#
# plots_report <- sub(".*#(.*)", "\\1", tmp.meta)
#
# tmp.plotid <- plots_report[2]
#
# tmp.tab <- plotGetData(data = tmp.data,
#                        plotid = tmp.plotid,
#                        rprtpckg = tmp.rprtpckg,
#                        report = tmp.report,
#                        audience  = tmp.audience)
#
# #Get set
# tmp.set <- tmp.tab |>
#   dplyr::group_by(set) |>
#   dplyr::summarise(anz = dplyr::n()) |>
#   dplyr::select(set) |>
#   unlist()
#
# tmp.set <-tmp.tab |>
#   dplyr::pull(set) |>
#   unique()
#
#
# #Labels
# # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
# #                                       sheet = 'sets')
#
# tmp.item.labels <- sets |> dplyr::filter(
#   set == tmp.set
# ) |>
#   dplyr::arrange(
#     dplyr::desc(sort)
#   )
#
#
# data <- tmp.tab
#
#
# tmp.var_plot <- length(unique(data$vars))
#
#
# #Manual adjustments for filter questions
# data <- data |> dplyr::filter(vals != "k. A.")
#
#
#
# las_theme <- ggplot2::theme(
#   #axis.title.x = ggplot2::element_blank(),
#   axis.text.x = ggplot2::element_text(size = 11),
#   axis.title.y = ggplot2::element_blank(),
#   axis.text.y = ggplot2::element_text(size = 12),
#   plot.margin = ggplot2::margin(t = 10,  # Top margin
#                                 r = 0,  # Right margin
#                                 b = 10,  # Bottom margin
#                                 l = 0)) # Left
#
#
#
#
# data$newlable <- paste0(data$vars, ": ", data$label_short)
# data$newlable <- as.factor(data$newlable)
#
# data$newlable <- stringr::str_trim(data$newlable)
#
#
# text <- stringr::str_wrap(data$newlable, width = 40)
#
#
# data$newlable
#
# bold_labels <- sapply(data$newlable, function(x) {
#
#   # Wrap the entire label to fit within the axis (Markdown formatting still intact)
#   x <- stringr::str_wrap(x, width = 40)
#
#   # Replace Markdown newlines (\n) with HTML <br> tags for ggtext::element_markdown
#   x <- stringr::str_replace_all(x, "\n", "<br>")
#
#   # Extract the part before the colon (e.g., "B223c (sus):")
#   bold_part <- stringr::str_extract(x, "^[^:]+:")  # Match everything before the first colon and include the colon
#
#   # Add bold formatting using Markdown syntax
#   bold_part <- paste0("**", bold_part, "**")
#
#   # Extract the part after the colon
#   rest_part <- stringr::str_remove(x, "^[^:]+:")  # Remove everything before and including the colon
#
#   # Combine the bold part and the rest of the string
#   new_label <- paste0(bold_part, rest_part)
# })
# # Test the output
# bold_labels
#
#
# data
#
# tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
#   ggplot2::geom_bar(
#     stat = 'identity',
#     position = ggplot2::position_stack(),
#     width = 0.5
#   ) +
#   ggplot2::geom_label(
#     ggplot2::aes(label = ifelse(p > 3,  paste0(label_n, "\n", "(", anz, ")"), "*"), group = factor(vals)),
#     position = ggplot2::position_stack(vjust = 0.5),
#     size = 2.8,
#     fill = "white",
#     colour = "black"
#   ) +
#   ggplot2::scale_fill_manual(
#     breaks = rev(tmp.item.labels$labels),
#     values = rev(tmp.item.labels$colors),
#     drop = TRUE,
#     labels = function(x) stringr::str_wrap(x, width = 7)
#   ) +
#   ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
#                             labels = bold_labels,
#                             limits = rev(levels(data$newlable))) +
#   ggplot2::coord_flip() +
#   ggplot2::theme_minimal(base_size = 12) +
#   ggplot2::theme(
#     legend.position = "bottom",
#     #legend.box.margin = ggplot2::margin(10, 10, 10, 10),
#     legend.spacing.y = ggplot2::unit(0.5, "cm"),
#     legend.key.size = ggplot2::unit(0.5, "lines"),
#     legend.text = ggplot2::element_text(size = 9, lineheight = 0.8),
#     #axis.text = ggplot2::element_text(size = 9),
#     axis.text = ggtext::element_markdown(size = 9),
#     axis.text.y = ggplot2::element_text(hjust = 0))+
#     ggplot2::labs(x = '', y = 'Prozent', fill = "", caption = "*: Numerische Werte kleiner 3 Prozent werden aufgrund verbesserter Lesbarkeit nicht grafisch dargestellt.") +
#     ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
#
# tmp.p
# #
# #
# #
# #
# #
# #
# #
# # #UBB#################
# # data$newlable <- data$label_short
# # data$newlable <- as.factor(data$newlable)
# # data$newlable <- stringr::str_replace(data$newlable, " ", ": ")
# #
# # data |>
# #   dplyr::filter(vars == "A114 (ubb)")
# #
# #
# # tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x = newlable)) +
# #   ggplot2::geom_bar(
# #     stat = 'identity',
# #     position = ggplot2::position_stack(),
# #     width = 0.5
# #   ) +
# #   ggplot2::geom_label(
# #     ggplot2::aes(label =  ifelse(anz > 1, paste(as.character(anz), "\n", label_n), "*"), group = factor(vals)),
# #     position = ggplot2::position_stack(vjust = 0.5),
# #     size = 2.8,
# #     fill = "white",
# #     colour = "black"
# #   ) +
# #   ggplot2::scale_fill_manual(
# #     breaks = rev(tmp.item.labels$labels),
# #     values = rev(tmp.item.labels$colors),
# #     drop = TRUE,
# #     labels = function(x) stringr::str_wrap(x, width = 12)  # Wrap legend text
# #   ) +
# #   ggplot2::scale_x_discrete(
# #     guide = ggplot2::guide_axis(n.dodge = 1),
# #     labels = function(x) stringr::str_wrap(x, width = 40),
# #     limits = rev(levels(data$newlable))
# #   ) +
# #   ggplot2::scale_y_continuous(
# #     breaks = function(x) scales::pretty_breaks()(x) |> round(),  # Apply rounding to the breaks
# #     labels = scales::number_format(accuracy = 1)  # Format labels as integers
# #   )+
# #   # ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
# #   ggplot2::coord_flip() +
# #   ggplot2::theme_minimal(base_size = 12) +
# #   ggplot2::theme(
# #     legend.position = "bottom",
# #     #legend.box.margin = ggplot2::margin(10, 10, 10, 10),
# #     legend.spacing.y = ggplot2::unit(0.5, "cm"),
# #     legend.key.size = ggplot2::unit(0.5, "lines"),
# #     legend.text = ggplot2::element_text(size = 9),
# #     axis.text = ggplot2::element_text(size = 9),
# #     axis.text.y = ggplot2::element_text(hjust = 0)
# #   ) +
# #   ggplot2::labs(x = '', y = 'Anzahl', fill = "")
# #
# # tmp.p
#
