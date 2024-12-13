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

export_plot <- function (meta,
                         snr,
                         audience,
                         report,
                         data,
                         ubb,
                         export = TRUE) {
  #Load fonts
  font_name <- "Noto Sans"
  font_path <- "NotoSans-Regular.ttf"
  #Check if font is available
  available_fonts <- sysfonts::font_files()

  #Add font if available
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

  #Get data
  plot.df <- plotGetData(data = data,
                         plotid = tmp.plotid,
                         rprtpckg = tmp.rprtpckg,
                         report = report,
                         audience  = audience)


  if (nrow(plot.df) == 0) {

    #Just in case no data is available
    tmp.var_plot <- 6
    tmp.p <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "Data is not available."),
                         size = 12) +
      ggplot2::theme_void()

    #Create a wordplot if plotid is A3a
    if (tmp.plotid == "A3a") {
      tmp.var_plot <- 6
      tmp.p <- createWordCloud(data = data)

    }


  }else {
    #Get set
    tmp.set <- plot.df |>
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


    #Check how many variables (for export)
    tmp.var_plot <- length(unique(plot.df$vars))


    #Manual adjustments for filter questions
    plot.df <- plot.df |> dplyr::filter(vals != "k. A.")

    #Create plot
    tmp.p <- create_ggplot(data = plot.df,
                           ubb = ubb,
                           labels = tmp.item.labels)
  }


  if (export == TRUE) {
    # Scale height based on the number of bars (tmp.var_plot)
    min_height <- 4
    max_height <- 8.27

    if (tmp.var_plot > 5) {
      height_plot <- max_height
    } else {
      height_plot <- min_height + (max_height - min_height) * (tmp.var_plot - 1) / 4
    }


    #Get directory
    tmp.dir <- get_directory(snr = snr)

    #Export plot
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





#' Create a ggplot based on prepare data
#'
#' @description Function creates and export a plot.
#' @param data Data
#' @param ubb UBB

create_ggplot <- function(data,
                          ubb,
                          labels) {

  #Create plot for UBB or not
  if (ubb) {
    data$newlable <- data$label_short
    data$newlable <- as.factor(data$newlable)
    data$newlable <- stringr::str_replace(data$newlable, " ", ": ")

    bold_labels <- sapply(data$newlable, function(x) {

      # Wrap the entire label to fit within the axis (Markdown formatting still intact)
      x <- stringr::str_wrap(x, width = 35)
      # Replace Markdown newlines (\n) with HTML <br> tags
      x <- stringr::str_replace_all(x, "\n", "<br>")
      # Extract the part before the colon (e.g., "B223c (sus):")
      bold_part <- stringr::str_extract(x, "^[^:]+:")
      # Add bold formatting using Markdown syntax
      bold_part <- paste0("**", bold_part, "**")
      # Extract the part after the colon
      rest_part <- stringr::str_remove(x, "^[^:]+:")
      # Combine the bold part and the rest of the string
      new_label <- paste0(bold_part, rest_part)
    })

    tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = anz, x = forcats::fct_rev(data$newlable))) +
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
        breaks = rev(labels$labels),
        values = rev(labels$colors),
        drop = TRUE,
        labels = function(x) stringr::str_wrap(x, width = 12)  # Wrap legend text
      ) +
      ggplot2::scale_x_discrete(
        guide = ggplot2::guide_axis(n.dodge = 1),
        labels = bold_labels,
        limits = levels(forcats::fct_rev(data$newlable))
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
  }else {
    data$newlable <- paste0(data$vars, ": ", data$label_short)
    data$newlable <- as.factor(data$newlable)


    bold_labels <- sapply(data$newlable, function(x) {

      # Wrap the entire label to fit within the axis
      x <- stringr::str_wrap(x, width = 45)
      # Replace Markdown newlines (\n) with HTML <br> tags
      x <- stringr::str_replace_all(x, "\n", "<br>")
      # Extract the part before the colon (e.g., "B223c (sus):")
      bold_part <- stringr::str_extract(x, "^[^:]+:")
      # Add bold formatting using Markdown syntax
      bold_part <- paste0("**", bold_part, "**")
      # Extract the part after the colon
      rest_part <- stringr::str_remove(x, "^[^:]+:")
      # Combine the bold part and the rest of the string
      new_label <- paste0(bold_part, rest_part)
    })

    tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = forcats::fct_rev(data$newlable))) +
      ggplot2::geom_bar(
        stat = 'identity',
        position = ggplot2::position_stack(),
        width = 0.5
      ) +
      ggplot2::geom_label(
        ggplot2::aes(label = ifelse(p > 3,  paste0(label_n, "\n", "(", anz, ")"), "*"), group = factor(vals)),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 2.8,
        fill = "white",
        colour = "black"
      ) +
      ggplot2::scale_fill_manual(
        breaks = rev(labels$labels),
        values = rev(labels$colors),
        drop = TRUE,
        labels = function(x) stringr::str_wrap(x, width = 7)
      ) +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
                                labels = bold_labels,
                                limits = levels(forcats::fct_rev(data$newlable))
      ) +
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
      ggplot2::labs(x = '', y = 'Prozent', fill = "", caption = "*: Numerische Werte kleiner 3 Prozent werden aufgrund verbesserter Lesbarkeit nicht grafisch dargestellt.") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
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

export_AllPlots <- function (meta,
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
  freitext <- data |> dplyr::filter(vars == "A311ub")

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
    ggwordcloud::geom_text_wordcloud_area(family = font_name,
                                          color = "black",
                                          rm_outside = FALSE) +
    ggplot2::scale_size_area(max_size = 36, trans = ggwordcloud::power_trans(1/.7)) +
    ggplot2::theme_minimal()

  return(tmp.p)
}


#' Create a Plot for the Web
#'
#' @description Function creates and export a plot.
#' @param meta Meta data
#' @param snr School number
#' @param audience Audience
#' @param report Report template
#' @param data Data
#' @param ubb UBB
#' @export

create_PlotWeb <- function (meta, snr, audience, report, data, ubb) {
  #Load fonts: newlable
  font_name <- "Noto Sans"
  font_path <- "NotoSans-Regular.ttf"
  #Check if font is available
  available_fonts <- sysfonts::font_files()

  #Add font if available
  if (font_path %in% available_fonts$file) {
    sysfonts::font_add("Noto Sans", "NotoSans-Regular.ttf")
    showtext::showtext_auto()
  } else {
    font_name <- "sans"
  }


  #Split meta list
  tmp.var <- stringr::str_split(meta, "#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  plot.df <- plotGetData(
    data = data,
    plotid = tmp.plotid,
    rprtpckg = tmp.rprtpckg,
    report = report,
    audience  = audience
  )


  if (nrow(plot.df) == 0) {
    #Just in case no data is available
    tmp.var_plot <- 6
    tmp.p <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "Data is not available."),
                         size = 12) +
      ggplot2::theme_void()

    #Create a wordplot if plotid is A3a
    if (tmp.plotid == "A3a") {
      tmp.var_plot <- 6
      tmp.p <- createWordCloud(data = data)

    }


  } else {
    #Get set
    tmp.set <- plot.df |>
      dplyr::group_by(set) |>
      dplyr::summarise(anz = dplyr::n()) |>
      dplyr::select(set) |>
      unlist()

    #Labels
    # tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
    #                                       sheet = 'sets')

    tmp.item.labels <- sets |> dplyr::filter(set == tmp.set) |>
      dplyr::arrange(dplyr::desc(sort))


    #Check how many variables (for export)
    tmp.var_plot <- length(unique(plot.df$vars))


    #Manual adjustments for filter questions
    plot.df <- plot.df |> dplyr::filter(vals != "k. A.")
    data <- plot.df
    #Create plot
    if (ubb) {
      data$newlable <- data$label_short
      data$newlable <- as.factor(data$newlable)
      data$newlable <- stringr::str_replace(data$newlable, " ", ": ")

      tmp.p <- ggplot2::ggplot(data,
                               ggplot2::aes(
                                 fill = vals,
                                 y = anz,
                                 x = forcats::fct_rev(data$newlable)
                               )) +
        ggplot2::geom_bar(
          stat = 'identity',
          position = ggplot2::position_stack(),
          width = 0.5
        ) +
        ggplot2::geom_label(
          ggplot2::aes(
            label = paste(as.character(anz), "\n", label_n),
            group = factor(vals)
          ),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 6,
          fill = "white",
          colour = "black"
        ) +
        ggplot2::scale_fill_manual(
          breaks = rev(tmp.item.labels$labels),
          values = rev(tmp.item.labels$colors),
          drop = TRUE,
          labels = function(x)
            stringr::str_wrap(x, width = 20)  # Wrap legend text
        ) +
        ggplot2::scale_x_discrete(
          guide = ggplot2::guide_axis(n.dodge = 1),
          labels = stringr::str_wrap(data$newlable, width = 40),
          limits = levels(forcats::fct_rev(data$newlable))
        ) +
        ggplot2::scale_y_continuous(
          breaks = function(x)
            scales::pretty_breaks()(x) |> round(),
          # Apply rounding to the breaks
          labels = scales::number_format(accuracy = 1)  # Format labels as integers
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 32) +
        ggplot2::theme(
          legend.position = "bottom",
          #legend.box.margin = ggplot2::margin(10, 10, 10, 10),
          #legend.spacing.y = ggplot2::unit(1, "cm"),
          #legend.key.size = ggplot2::unit(1, "lines"),
          legend.text = ggplot2::element_text(size = 26),
          #axis.text = ggplot2::element_text(size = 9),
          #axis.text = ggtext::element_markdown(size = 22),
          axis.text.y = ggplot2::element_text(hjust = 0)
        ) +
        ggplot2::labs(x = '', y = 'Anzahl', fill = "")
    } else {
      data$newlable <- paste0(data$vars, ": ", data$label_short)
      data$newlable <- as.factor(data$newlable)

      tmp.p <- ggplot2::ggplot(data, ggplot2::aes(
        fill = vals,
        y = p,
        x = forcats::fct_rev(data$newlable)
      )) +
        ggplot2::geom_bar(
          stat = 'identity',
          position = ggplot2::position_stack(),
          width = 0.5
        ) +
        ggplot2::geom_label(
          ggplot2::aes(
            label = ifelse(p > 3, paste0(label_n, "\n", "(", anz, ")"), "*"),
            group = factor(vals)
          ),
          position = ggplot2::position_stack(vjust = 0.5),
          size = 6,
          fill = "white",
          colour = "black"
        ) +
        ggplot2::scale_fill_manual(
          breaks = rev(tmp.item.labels$labels),
          values = rev(tmp.item.labels$colors),
          drop = TRUE,
          labels = function(x)
            stringr::str_wrap(x, width = 9)
        ) +
        ggplot2::scale_x_discrete(
          guide = ggplot2::guide_axis(n.dodge = 1),
          labels = stringr::str_wrap(data$newlable, width = 40),
          limits = levels(forcats::fct_rev(data$newlable))
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 32) +
        ggplot2::theme(
          legend.position = "bottom",
          #legend.box.margin = ggplot2::margin(10, 10, 10, 10),
          #legend.spacing.y = ggplot2::unit(1, "cm"),
          #legend.key.size = ggplot2::unit(1, "lines"),
          legend.text = ggplot2::element_text(size = 26, lineheight = 0.8),
          #axis.text = ggplot2::element_text(size = 9),
          #axis.text = ggtext::element_markdown(size = 18),
          axis.text.y = ggplot2::element_text(hjust = 0)
        ) +
        ggplot2::labs(
          x = '',
          y = 'Prozent',
          fill = "",
          caption = "*: Numerische Werte kleiner 3 Prozent werden aufgrund verbesserter Lesbarkeit nicht grafisch dargestellt."
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
    }
  }

  return(tmp.p)

}

# library(bslib)
#
# library(pdftools)
# library(png)
# library(purrr)
#
#
# convert_pdf_to_png <- function(pdf_file) {
#   # Extract the base name (without extension) from the PDF file
#   base_name <- tools::file_path_sans_ext(base::basename(pdf_file))
#
#   # Render the single page of the PDF (300 dpi)
#   img <- pdftools::pdf_render_page(pdf_file, page = 1, dpi = 300)
#
#   # Define the output PNG file name using the base name
#   # Define the output PNG file name
#   tmp.dir <- get_directory(snr = tmp.snr)
#   tmp.dir <- file.path(tmp.dir, "plots")
#
#   output_file <- file.path(tmp.dir, paste0(base_name, ".png"))
#
#   # Save the image as PNG
#   png::writePNG(img, output_file)
#
#   # Return the output file path
#   return(output_file)
# }
#
# # Call the function to convert a PDF to PNG
#
# tmp.dir <- get_directory(snr = tmp.snr)
# tmp.dir <- file.path(tmp.dir, "plots")
# #List all pdf files here
# pdffiles <- list.files(tmp.dir, full.names = TRUE, pattern = ".pdf")
#
#
# convert_pdf_to_png(pdffiles[2])
#
#
# num_cores <- parallel::detectCores()
# workers <- max(1, num_cores - 1)  #
#
#
# #plan(multicore, workers = 4)  # Adjust workers based on your CPU cores
# future::plan(future::multisession, workers = num_cores)  #
#
# furrr::future_map(pdffiles, ~convert_pdf_to_png(.x))
#

#' Create PDFs (for serial runs only)
#'
#' @description Create a PDF report based on exported plots and data.
#' @param snr School number
#' @param audience Audience
#' @param name Name
#' @param ubb UBB
#' @param n Number of observations
#' @param results String for reporting group
#' @param d Duration (UBB only)
#' @param drop Drop the temporary files
#'
#' @return PDF file

create_pdfs <- function (snr,
                         audience,
                         name,
                         ubb,
                         n,
                         results,
                         d = NULL,
                         drop = TRUE) {

  year <- format(Sys.Date(), "%Y")
  tmp.dir_res <- get_directory_res(snr = snr, audience = audience)
  tmp.dir <- get_directory(snr = snr)

  #Create report for UBB or survey
  if (ubb == TRUE) {
    #UBB has open comments (tmp.freitext) to include in report
    results <- tmp.freitext

    rmarkdown::render(
      input = paste0(tmp.dir, "/plots/template.Rmd"),
      output_file = paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf"),
      quiet = TRUE,
      params = list(
        snr = snr,
        name = name,
        t = year,
        n = n,
        d = d,
        fb = results
      ))
  }

  if (ubb == FALSE) {
    rmarkdown::render(
      input = here::here(tmp.dir, "plots/template.Rmd"),
      output_file = paste0(here::here(tmp.dir), "/", snr, "_results_", audience, ".pdf"),
      quiet = TRUE,
      params = list(
        snr = snr,
        name = name,
        t = year,
        n = n,
        fb = results
      ))
  }

  if (drop == TRUE) {
    unlink(tmp.dir_res, recursive=TRUE)
  }

  #Report via CLI if results are available:
  x <- paste0(here::here(tmp.dir), "/", snr, "_results_", audience, ".pdf")

  if (file.exists(x) == TRUE) {
    usethis::ui_done("Exported PDF file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
  }

  if (interactive() == TRUE) {
    invisible(system(paste0('open ', x)))
  }

}


