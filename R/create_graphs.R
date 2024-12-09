#' Export a plot
#'
#' @description Function creates and export a plot.
#' @param meta Meta data
#' @param snr School number
#' @param audience Audience
#' @param report Report template
#' @param data Data
#' @param ubb UBB
#' @param export Export
#' @export

export_plot = function (meta,
                        snr,
                        audience,
                        report,
                        data,
                        ubb,
                        export = TRUE) {

  #Load fonts
  font_name <- "Noto Sans"
  font_path <- "NotoSans-Regular.ttf"

  available_fonts <- sysfonts::font_files()

  if (font_path %in% available_fonts$file) {
    sysfonts::font_add("Noto Sans", "NotoSans-Regular.ttf")
    showtext::showtext_auto()
  }else {
    font_name <- "sans"
  }






  #Split meta list
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]


  if (tmp.plotid == "A3a") {
    tmp.var_plot <- 5
    tmp.p <- createWordCloud(data = tmp.data)

  }else {
    #Get data
    tmp.tab <- plotGetData(data = data,
                           plotid = tmp.plotid,
                           rprtpckg = tmp.rprtpckg,
                           report = report,
                           audience  = audience)

    #Get set
    tmp.set <- tmp.tab |>
      dplyr::group_by(set) |>
      dplyr::summarise(anz = dplyr::n()) |>
      dplyr::select(set) |>
      unlist()

    #Labels
    # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
    #                                       sheet = 'sets')

    tmp.item.labels <- sets |> dplyr::filter(
      set == tmp.set
    ) |>
      dplyr::arrange(
        dplyr::desc(sort)
      )


    data <- tmp.tab


    tmp.var_plot <- length(unique(data$vars))


    #Manual adjustments for filter questions
    #filterlist <- c("W2a", "W2leh", "w2use", "w33a")
    #filterlist <- get_filtervars()

    data <- data |> dplyr::filter(vals != "k. A.")

    # if ((tmp.plotid %in% filterlist) == TRUE) {
    #   #data <- data |> dplyr::filter(vals != "k. A.")
    #   data <- data |> dplyr::filter(vals != " ")
    # }

    # if (tmp.plotid == "A3b" & ubb == TRUE) {
    #   data <- data |> dplyr::filter(vals != "NA")
    # }
    #
    # if (tmp.plotid == "W2b" & ubb == TRUE) {
    #   data <- tidyr::drop_na(data)
    # }


    #data$txtlabel <- paste0(data$label_n, "\n (n: ", data$anz, ")")

    # las_theme <- ggplot2::theme(
    #   #axis.title.x = ggplot2::element_blank(),
    #   legend.position = "none",
    #   axis.text.x = ggplot2::element_text(size = 11),
    #   axis.title.y = ggplot2::element_blank(),
    #   axis.text.y = ggplot2::element_text(size = 12),
    #   plot.margin = ggplot2::margin(t = 10,  # Top margin
    #                                 r = 0,  # Right margin
    #                                 b = 10,  # Bottom margin
    #                                 l = 0)) # Left margin


    #Plots for UBB (absolute values) vs. survey (relative values) label_n
    if (ubb == FALSE) {

      data$newlable <- paste0(data$vars, ": ", data$label_short)
      data$newlable <- as.factor(data$newlable)


      bold_labels <- sapply(data$newlable, function(x) {

        # Wrap the entire label to fit within the axis (Markdown formatting still intact)
        x <- stringr::str_wrap(x, width = 45)

        # Replace Markdown newlines (\n) with HTML <br> tags for ggtext::element_markdown
        x <- stringr::str_replace_all(x, "\n", "<br>")

        # Extract the part before the colon (e.g., "B223c (sus):")
        bold_part <- stringr::str_extract(x, "^[^:]+:")  # Match everything before the first colon and include the colon

        # Add bold formatting using Markdown syntax
        bold_part <- paste0("**", bold_part, "**")

        # Extract the part after the colon
        rest_part <- stringr::str_remove(x, "^[^:]+:")  # Remove everything before and including the colon

        # Combine the bold part and the rest of the string
        new_label <- paste0(bold_part, rest_part)
      })

      tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
        ggplot2::geom_bar(
          stat = 'identity',
          position = ggplot2::position_stack(),
          width = 0.5
        ) +
        ggplot2::geom_label(
          ggplot2::aes(label = paste0(label_n, "\n", "(", anz, ")"), group = factor(vals)),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 2.8,
          fill = "white",
          colour = "black"
        ) +
        ggplot2::scale_fill_manual(
          breaks = rev(tmp.item.labels$labels),
          values = rev(tmp.item.labels$colors),
          drop = TRUE,
          labels = function(x) stringr::str_wrap(x, width = 7)
        ) +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
                                  labels = bold_labels,
                                  limits = rev(levels(data$newlable))) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          #legend.box.margin = ggplot2::margin(10, 10, 10, 10),
          legend.spacing.y = ggplot2::unit(0.5, "cm"),
          legend.key.size = ggplot2::unit(0.5, "lines"),
          legend.text = ggplot2::element_text(size = 10, lineheight = 0.8),
          #axis.text = ggplot2::element_text(size = 9),
          axis.text = ggtext::element_markdown(size = 11),
          axis.text.y = ggplot2::element_text(hjust = 0))+
        ggplot2::labs(x = '', y = 'Prozent', fill = "") +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))




      # tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
      #   ggplot2::geom_bar(
      #     stat = 'identity',
      #     position = ggplot2::position_stack(),
      #     width = 0.5
      #   ) +
      #   ggplot2::geom_label(
      #     ggplot2::aes(label = label_n, group = factor(vals)),
      #     position = ggplot2::position_stack(vjust = 0.5),
      #     size = 2.8,
      #     fill = "white",
      #     colour = "black"
      #   ) +
      #   ggplot2::scale_fill_manual(
      #     breaks = rev(tmp.item.labels$labels),
      #     values = rev(tmp.item.labels$colors),
      #     drop = TRUE
      #   ) +
      #   ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
      #                             labels = function(x)
      #                               stringr::str_wrap(x, width = 40),
      #                             limits = rev(levels(data$newlable))) +
      #   ggplot2::coord_flip() +
      #   ggplot2::theme_minimal(base_size = 12) +
      #   ggplot2::theme(
      #     legend.position = "bottom",
      #     axis.text = ggplot2::element_text(size = 10),
      #     #legend.text = ggplot2::element_text(size=8),
      #     axis.text.y = ggplot2::element_text(hjust = 0)
      #   ) +
      #   ggplot2::labs(x = '', y = 'Prozent', fill = "") +
      #   ggplot2::guides(fill  =  ggplot2::guide_legend(nrow = 2))+
      #   las_theme

    }

    if (ubb == TRUE) {
      data$newlable <- data$label_short
      data$newlable <- as.factor(data$newlable)
      data$newlable <- stringr::str_replace(data$newlable, " ", ": ")

      bold_labels <- sapply(data$newlable, function(x) {

        # Wrap the entire label to fit within the axis (Markdown formatting still intact)
        x <- stringr::str_wrap(x, width = 40)

        # Replace Markdown newlines (\n) with HTML <br> tags for ggtext::element_markdown
        x <- stringr::str_replace_all(x, "\n", "<br>")

        # Extract the part before the colon (e.g., "B223c (sus):")
        bold_part <- stringr::str_extract(x, "^[^:]+:")  # Match everything before the first colon and include the colon

        # Add bold formatting using Markdown syntax
        bold_part <- paste0("**", bold_part, "**")

        # Extract the part after the colon
        rest_part <- stringr::str_remove(x, "^[^:]+:")  # Remove everything before and including the colon

        # Combine the bold part and the rest of the string
        new_label <- paste0(bold_part, rest_part)
      })

      # tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x =
      #                                               newlable)) +
      #   ggplot2::geom_bar(
      #     stat = 'identity',
      #     position = ggplot2::position_stack(),
      #     width = 0.5
      #   ) +
      #   ggplot2::geom_label(
      #     ggplot2::aes(label = paste(as.character(anz),"\n", label_n), group = factor(vals)),
      #     #ggplot2::aes(label = as.character(anz), group = factor(vals)),
      #     position = ggplot2::position_stack(vjust = 0.5),
      #     size = 2.8,
      #     fill = "white",
      #     colour = "black"
      #   )+
      #   ggplot2::scale_fill_manual(
      #     breaks = rev(tmp.item.labels$labels),
      #     values = rev(tmp.item.labels$colors),
      #     drop = TRUE
      #   ) +
      #   ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
      #                             labels = function(x)
      #                               stringr::str_wrap(x, width = 40),
      #                             limits = rev(levels(data$newlable))) +
      #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks())+
      #   ggplot2::coord_flip() +
      #   ggplot2::theme_minimal(base_size = 12) +
      #   ggplot2::theme(
      #     legend.position = "bottom",
      #     axis.text = ggplot2::element_text(size = 8),
      #     legend.box.margin = ggplot2::margin(0, 0, 0, 0) ,
      #     legend.text = ggplot2::element_text(size=8),
      #     legend.spacing.y = ggplot2::unit(0.5, "cm"),
      #     legend.key.size = ggplot2::unit(0.5, "lines"),
      #     axis.text.y = ggplot2::element_text(hjust = 0)
      #   ) +
      #   ggplot2::labs(x = '', y = 'Anzahl', fill = "")

      tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x = newlable)) +
        ggplot2::geom_bar(
          stat = 'identity',
          position = ggplot2::position_stack(),
          width = 0.5
        ) +
        ggplot2::geom_label(
          ggplot2::aes(label = paste(as.character(anz), "\n", label_n), group = factor(vals)),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 3.5,
          fill = "white",
          colour = "black"
        ) +
        ggplot2::scale_fill_manual(
          breaks = rev(tmp.item.labels$labels),
          values = rev(tmp.item.labels$colors),
          drop = TRUE,
          labels = function(x) stringr::str_wrap(x, width = 12)  # Wrap legend text
        ) +
        ggplot2::scale_x_discrete(
          guide = ggplot2::guide_axis(n.dodge = 1),
          labels = bold_labels,
          limits = rev(levels(data$newlable))
        ) +
        ggplot2::scale_y_continuous(
          breaks = function(x) scales::pretty_breaks()(x) |> round(),  # Apply rounding to the breaks
          labels = scales::number_format(accuracy = 1)  # Format labels as integers
        )+
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.box.margin = ggplot2::margin(10, 10, 10, 10),
          legend.spacing.y = ggplot2::unit(0.5, "cm"),
          legend.key.size = ggplot2::unit(.75, "lines"),
          legend.text = ggplot2::element_text(size = 12),
          #axis.text = ggplot2::element_text(size = 9),
          axis.text = ggtext::element_markdown(size = 16),
          axis.text.y = ggplot2::element_text(hjust = 0)
        ) +
        ggplot2::labs(x = '', y = 'Anzahl', fill = "")





    }
  }



  #height_plot <- (148/4)*tmp.var_plot


  if (export == TRUE) {

    # height_plot <- 8.27
    #
    # if (tmp.var_plot == 2) {
    #   height_plot <- 100
    # }
    #
    # if (tmp.var_plot == 1) {
    #   height_plot <- 80
    # }

    min_height <- 4  # Minimum height in inches (for tmp.var_plot == 1)
    max_height <- 8.27  # Maximum height in inches (for tmp.var_plot == 5)

    # Scale height based on the number of bars (tmp.var_plot)
    # Scale height based on the number of bars (tmp.var_plot)
    # If tmp.var_plot is greater than 5, just set height to max_height
    if (tmp.var_plot > 5) {
      height_plot <- max_height  # If tmp.var_plot > 5, directly assign max_height
    } else {
      # Otherwise, calculate height based on the number of bars
      height_plot <- min_height + (max_height - min_height) * (tmp.var_plot - 1) / 4
    }


    #tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
    #tmp.dir_res <- here::here()

    tmp.dir <- get_directory(snr = snr)

    # ggplot2::ggsave(paste0(tmp.plotid,'_plot.pdf'),
    #                 path =  paste0(tmp.dir, "/plots"),
    #                 plot = tmp.p,
    #                 width = 210,
    #                 height = height_plot,
    #                 dpi = 300,
    #                 units = "mm"
    # )

    ggplot2::ggsave(paste0(tmp.plotid, '_plot.pdf'),
                    path = paste0(tmp.dir, "/plots"),
                    plot = tmp.p,
                    width = 11.69,  #
                    height = height_plot,
                    dpi = 300,
                    units = "in")

    usethis::ui_done("Export plot: {usethis::ui_value(tmp.plotid)}")
  }

  return(tmp.p)

}




#' Create all report plots at once simplified
#' @description Create all report plots at once simplified (direct export)
#' @param meta Meta data
#' @param snr School number
#' @param audience Audience
#' @param report Report template
#' @param data Data
#' @param ubb UBB
#' @export

create_allplots2 = function (meta,
                             snr,
                             audience,
                             report,
                             data,
                             ubb) {

  #Check if meta data is available
  if (length(meta) == 0) {
    cli::cli_abort("Plotid or report template not found in meta list.")
  }

  #Split meta data and run export_plot
  tmp.var <- stringr::str_split(meta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #NEW export_plot without saving RDS
  invisible(lapply(meta, export_plot,
                   audience = audience,
                   ubb = ubb,
                   data = data,
                   snr = snr,
                   report = report))

  #What will be exported
  tmp.dir <- get_directory(snr = snr)
  all_files <- list.files(path = here::here(tmp.dir, "plots"), pattern = "_plot.pdf")
  count_png <- length(all_files)

  #Inform what was exported
  if (count_png == 0) {
    usethis::ui_warn("Error. No exported png files.")
  } else {
    usethis::ui_done("Exported {usethis::ui_value(count_png)} graphs.")
  }

}


#' Export a plot
#'
#' @description Function creates and export a plot.
#' @param data Meta data
#' @param export Export
#' @export

createWordCloud <- function(data) {
  freitext <- tmp.data |> dplyr::filter(vars == "A311ub")

  df <- tibble::tibble(Angabe = freitext$vals)


  # word_data <- df |>
  #   tidytext::unnest_tokens(word, txt, token = "lines")

  word_count <- df |>
    dplyr::count(Angabe, sort = TRUE)


  font_name <- "Gloria Hallelujah"
  font_path <- "GloriaHallelujah-Regular.ttf"

  available_fonts <- sysfonts::font_files()

  if (font_path %in% available_fonts$file) {
    sysfonts::font_add("Gloria Hallelujah", "GloriaHallelujah-Regular.ttf")
    showtext::showtext_auto()
  }else {
    font_name <- "sans"
  }

  #sysfonts::font_add("Gloria Hallelujah", "GloriaHallelujah-Regular.ttf")
  #showtext::showtext_auto()

  tmp.p <- ggplot2::ggplot(word_count,
                           ggplot2::aes(label = Angabe,
                                        size = n)) +
    #ggwordcloud::geom_text_wordcloud() +
    ggwordcloud::geom_text_wordcloud_area(family=font_name,
                                          color = "black",
                                          rm_outside = FALSE) +
    ggplot2::scale_size_area(max_size = 36, trans = ggwordcloud::power_trans(1/.7)) +
    ggplot2::theme_minimal()

  return(tmp.p)
}



