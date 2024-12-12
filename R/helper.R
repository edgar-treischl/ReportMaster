#' Get the directory
#' @description Returns the main directory of the results
#' @param snr School number
#' @return String
#' @export

get_directory <- function(snr) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir <- paste0("res/", snr,"_", year)
  tmp.dir <- here::here(tmp.dir)

  return(tmp.dir)
}

#' Get the directory of the results
#' @description Get the sub-directory of the results
#' @param snr School number
#' @param audience Report audience
#' @return String
#' @export

get_directory_res <- function(snr, audience) {
  year <- format(Sys.Date(), "%Y")
  tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)
  tmp.dir_res <- here::here(tmp.dir_res)

  return(tmp.dir_res)
}


#' Get school name
#' @description Get school name based on their school number
#' @param snr School number
#' @export
get_sname = function (snr) {

  tmp.name <- schools_names |> dplyr::filter(SNR == snr)
  tmp.name <- tmp.name$SNAME

  #Check if more than one name is found
  if (length(tmp.name) > 1) {
    cli::cli_abort("Error in get_sname(): More than one school name found.")
  }

  # if no name is found?
  if (length(tmp.name) == 0) {
    tmp.name <- "School name not available."
  }

  return(tmp.name)
}

#' Get Rmd Code for the Report
#' @description Depending on the number of plots and tables, the function
#'  creates the Rmd chunks for the report
#' @param x_seq Sequence
#' @return Character
#' @export

get_rmd <- function(x_seq) {
  # Initialize a list to hold the chunks
  rmd_chunks <- c()

  # Loop over the sequence and generate the RMarkdown content for each value of `x_seq`
  for (x in x_seq) {
    chunk1 <- paste0("```{r, results='asis'}\n", "cat(paste0('## ', header_report$header1[", x, "]))\n", "```")
    chunk2 <- paste0("```{r}\n", "plot_list[[", x, "]]\n", "```")
    chunk3 <- paste0("```{r}\n", "table_list[[", x, "]]\n", "```")

    # Add vertical fill to the third chunk
    #chunk3_vfill <- "\\vfill"
    chunk3_vfill <- '\\vspace*{1cm}'

    # Add each chunk to the list
    rmd_chunks <- c(rmd_chunks, chunk1, chunk2, chunk3_vfill, chunk3)

    # Add a LaTeX page break after each set of chunks
    rmd_chunks <- c(rmd_chunks, "\\newpage")
  }

  # Combine all the chunks into a single string, separated by newlines
  rmd_content <- paste(rmd_chunks, collapse = "\n\n")

  # Return the full RMarkdown content as a single string
  return(rmd_content)
}


#' Get Rmd Code for the Report
#' @description Depending on the number of plots and tables, the function
#'  creates the Rmd chunks for the report
#' @param x_seq Sequence
#' @return Character
#' @export

get_rmd3 <- function(meta, num_bars) {

  # Step 3: Initialize a list to store the RMarkdown chunks
  rmd_chunks <- list()

  # Standard plot height
  standard_height <- 5

  # Step 4: Loop through the sequence and generate RMarkdown content
  for (x in 1:length(meta)) {
    # Access the correct num_bars for the current index
    num_bars_x <- num_bars[x]

    # Handle cases where num_bars_x might be NULL or NA
    if (is.null(num_bars_x) || is.na(num_bars_x)) {
      warning(paste("Invalid number of bars for plot", x, ". Using default height."))
      num_bars_x <- 3  # Default value for num_bars_x
    }

    # Adjust fig.height based on the number of bars
    if (num_bars_x == 3) {
      fig_height <- standard_height - 1  # Decrease by 1 for 3 bars
    } else if (num_bars_x == 2) {
      fig_height <- standard_height - 2  # Decrease by 2 for 2 bars
    } else if (num_bars_x == 1) {
      fig_height <- standard_height - 3  # Decrease by 3 for 1 bar
    } else {
      fig_height <- standard_height  # Keep standard height for more than 3 bars
    }

    # Ensure fig_height doesn't go below a minimum value (e.g., 3)
    fig_height <- max(fig_height, 3)

    # Create RMarkdown chunks
    chunk1 <- sprintf("```{r, results='asis'}\ncat(paste0('## ', header_report$header1[%d]))\n```", x)
    chunk2 <- sprintf("```{r, fig.height=%d}\nplot_list[[%d]]\n```", fig_height, x)
    chunk3 <- sprintf("```{r}\ntable_list[[%d]]\n```", x)
    chunk3_vfill <- '\\vspace*{1cm}'  # Vertical space for chunk 3
    #"\\newpage"

    # Append chunks to the list
    #rmd_chunks <- c(rmd_chunks, chunk1, chunk2)
    rmd_chunks <- c(rmd_chunks, chunk1, chunk2, chunk3_vfill, chunk3)
  }

  # Step 5: Combine all chunks into a single string, separated by newlines
  rmd_content <- paste(rmd_chunks, collapse = "\n\n")

  # Return the full RMarkdown content
  return(rmd_content)
}


#' Get Rmd Code for the Report
#' @description Depending on the number of plots and tables, the function
#'  creates the Rmd chunks for the report
#' @param x_seq Sequence
#' @return Character
#' @export
#'
get_rmdX <- function(meta,
                     num_bars,
                     header) {

  # Step 1: Ensure that meta and num_bars have the same length
  if (length(meta) != length(num_bars)) {
    stop("The length of 'meta' and 'num_bars' must be the same.")
  }

  meta <- sub(".*#(.*)", "\\1", meta)
  # Step 2: Initialize a list to store the RMarkdown chunks
  rmd_chunks <- list()

  # Standard plot height
  standard_height <- 4

  # Step 3: Loop through the sequence and generate RMarkdown content
  for (x in seq_along(meta)) {
    # Access the correct num_bars for the current index
    plot_name <- meta[x]
    num_bars_x <- num_bars[x]
    headerx <- header[x]

    # Handle cases where num_bars_x might be NULL or NA
    if (is.null(num_bars_x) || is.na(num_bars_x)) {
      warning(paste("Invalid number of bars for plot", x, ". Using default height."))
      num_bars_x <- 3  # Default value for num_bars_x
    }

    # Adjust fig.height based on the number of bars
    if (num_bars_x == 3) {
      fig_height <- standard_height - 1  # Decrease by 1 for 3 bars
    } else if (num_bars_x == 2) {
      fig_height <- standard_height - 2  # Decrease by 2 for 2 bars
    } else if (num_bars_x == 1) {
      fig_height <- standard_height - 3  # Decrease by 3 for 1 bar
    } else {
      fig_height <- standard_height  # Keep standard height for more than 3 bars
    }

    # Ensure fig_height doesn't go below a minimum value (e.g., 3)
    fig_height <- max(fig_height, 3)

    # Create RMarkdown chunks
    chunk1 <- headerx

    # chunk1 <- paste0("```{r, results='asis'}\n",
    #                  "cat(paste0('## ', '", header1_x, "'[", x, "]))\n",
    #                  "```")

    chunk2 <- paste0("```{r, fig.height=", fig_height, "}\n",
                     "knitr::include_graphics('plots/", plot_name, "_plot.pdf')\n",
                     "```")

    # chunk3 <- paste0("```{r}\n",
    #                  "table_list[['", plot_name, "']]\n",
    #                  "```")
    #
    # chunk3_vfill <- '\\vspace*{1cm}'  # Vertical space for chunk 3

    # Append chunks to the list
    #rmd_chunks <- append(rmd_chunks, list(chunk1, chunk2, chunk3_vfill, chunk3))
    rmd_chunks <- append(rmd_chunks, list(chunk1, chunk2))
  }

  # Step 4: Combine all chunks into a single string, separated by newlines
  rmd_content <- paste(unlist(rmd_chunks), collapse = "\n\n")

  # Return the full RMarkdown content
  return(rmd_content)
}




#' Generate the Rmd file for the report
#' @description Functions combines the template and adds the Rmd content
#'  from the get_rmd function
#' @param x_seq Sequence
#' @param ubb UBB
#' @param export Export
#' @param file_name File name
#' @return Returns a plot
#' @export


generate_rmd <- function(meta,
                         num_bars,
                         ubb,
                         header,
                         export = TRUE,
                         file_name = "generated_document.Rmd") {
  # Read the template file content for the YAML header and any other template content
  #yaml_header <- "---\ntitle: \"Untitled\"\noutput: html_document\ndate: \"2024-11-26\"\n---\n"

  syspath <- system.file(package = "ReportMaster")
  package_path <- paste0(syspath, "/templates/")

  if (ubb) {
    yaml_header <- readLines(paste0(package_path, "template_ubb_min.Rmd"))
  }else {
    yaml_header <- readLines(paste0(package_path, "template_min.Rmd"))
  }



  # Combine the YAML header content into a single string with appropriate newlines
  yaml_header <- paste(yaml_header, collapse = "\n")

  # Get the RMarkdown content from the get_rmd function
  #rmd_content <- get_rmd(x_seq)

  # Example usage:
  #plots_report <- sub(".*#(.*)", "\\1", tmp.meta)
  rmd_content <- get_rmdX(meta = meta, num_bars = num_bars,
                          header = header)

  # Combine the YAML header with the RMarkdown content
  full_rmd <- paste(yaml_header, rmd_content, sep = "\n\n")

  # Write the full RMarkdown content to the specified file
  if (export == TRUE) {
    writeLines(full_rmd, file_name)
  }else {
    return(full_rmd)
  }


  # Optionally, print a message to confirm the file is created
  #message("RMarkdown content has been written to ", file_name)
}



#generate_rmd(1:44, ubb = TRUE)

#' Create directories
#' @description Create directories for the report
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB
#'
#' @export
create_directories <- function (snr, audience, ubb) {

  #Create if not already exists
  if(!dir.exists("res")){
    dir.create("res")
  }

  year <- format(Sys.Date(), "%Y")

  tmp.dir <- here::here("res", paste0(snr, "_", year))

  #Create if not already exists
  if(!dir.exists(tmp.dir)){
    dir.create(tmp.dir)
  }


  if(!dir.exists(here::here(tmp.dir, "plots"))){
    dir.create(here::here(tmp.dir, "plots"))
  }

  if(!dir.exists(here::here(tmp.dir, "plots/p/"))){
    dir.create(here::here(tmp.dir, "plots/p/"))
  }

  # tmp.dir_res <- here::here("res", paste0(snr, "_", year, "/", audience, "/", "plots"))
  #
  # if(!dir.exists(tmp.dir_res)){
  #   dir.create(tmp.dir_res)
  # }

  syspath <- system.file(package = "ReportMaster")
  package_path <- paste0(syspath, "/templates/")

  if (ubb == TRUE) {

    file.copy(
      from = paste0(package_path, "graphic_title_ubb.png"),
      to = paste0(tmp.dir, "/graphic_title_ubb.png")
    )
  }else {
    file.copy(
      from = paste0(package_path, "graphic-title_bfr.png"),
      to = paste0(tmp.dir, "/graphic-title_bfr.png")
    )

  }

  file.copy(
    from = paste0(package_path, "header_eva_las.png"),
    to = paste0(tmp.dir, "/header_eva_las.png")
  )

  # file.copy(
  #   from = paste0(package_path, "NotoSans-Regular.ttf"),
  #   to = paste0(tmp.dir, "/NotoSans-Regular.ttf")
  # )
  #
  # file.copy(
  #   from = paste0(package_path, "GloriaHallelujah-Regular.ttf"),
  #   to = paste0(tmp.dir, "/GloriaHallelujah-Regular.ttf")
  # )


}




#' Create report
#' @description Run all functions to create a report
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB
#' @param results Results: Text string for Audience
#' @param ganztag Ganztagsschule
#' @param stype School type
#' @examples
#' \dontrun{
#' purrr::pwalk(mylist, run_aslist)
#' }
#' @export
create_report <- function(snr,
                          audience,
                          ubb,
                          results,
                          ganztag,
                          stype) {


  tmp.server <- config::get("tmp.server")
  tmp.user <- config::get("tmp.user")
  tmp.credential <- config::get("tmp.credential")

  #Create directories under res
  year <- format(Sys.Date(), "%Y")
  # create_directories(snr = snr,
  #                    audience = audience,
  #                    ubb = ubb)

  tmp.dir <- paste0("/res/", snr,"_", year)
  path <- paste0(here::here(), tmp.dir)

  #Create if not already exists
  if(!dir.exists(path)){
    dir.create(path)
  }

  #Create path for subfolders (e.g. sus)
  #tmp.dir_res <- paste0("res/", snr,"_", year, "/", audience)

  #Create folder if not exist
  # if(!dir.exists(tmp.dir_res)){
  #   dir.create(here::here(tmp.dir_res))
  # }

  if(!dir.exists(here::here(path, "plots"))){
    dir.create(here::here(path, "plots"))
  }

  if(!dir.exists(here::here(path, "plots/p/"))){
    dir.create(here::here(path, "plots/p/"))
  }

  syspath <- system.file(package = "ReportMaster")
  package_path <- paste0(syspath, "/templates/")

  if (ubb == TRUE) {
    file.copy(paste0(package_path, "template_ubb.Rmd"), here::here(path, "plots"))
    file.copy(paste0(package_path, "graphic_title_ubb.png"), here::here(path, "plots"))
    file.rename(from = here::here(path, "plots/", "template_ubb.Rmd"),
                to = here::here(path, "plots/", "template.Rmd"))
  }

  if (ubb == FALSE) {
    file.copy(paste0(package_path, "template_generale.Rmd"), here::here(path, "plots"))
    file.copy(paste0(package_path, "graphic-title_bfr.png"), here::here(path, "plots"))

    file.rename(from = here::here(path, "plots", "template_generale.Rmd"),
                to = here::here(path, "plots", "template.Rmd"))
  }


  #for header plot
  file.copy(paste0(package_path, "header_eva_las.png"), here::here(path,"plots/p"))
  file.copy(paste0(package_path, "NotoSans-Regular.ttf"), here::here(path,"plots"))

  #Get name of school
  tmp.name <- get_sname(snr)
  #assign("tmp.name", value = tmp.name, envir=globalenv())
  cli::cli_alert_info("Get parameters for: {tmp.name}")



  #Adjust directory for UBB
  # if (ubb == TRUE) {
  #   tmp.dir_res <- paste0("res/", snr,"_", year, "/", "ubb")
  # }

  #Single steps
  tmp.session <- surveyConnectLs(user = tmp.user,
                                 credential = tmp.credential,
                                 server = tmp.server)

  #Get data and meta data
  tmp.sids.df <- surveyGetSurveyIds(snr, year, ubb)
  #assign("tmp.sids.df", value = tmp.sids.df, envir=globalenv())

  #Get report package
  tmp.sids <- tmp.sids.df$sid

  if (length(tmp.sids) == 0) {
    cli::cli_abort("Error in surveyGetSurveyIds")
  }

  #Get report package, survey and report template
  rprtpckg_list <- get_rprtpckg(report = audience,
                                school = stype,
                                ubbs = ubb,
                                allday = ganztag)


  tmp.rprtpckg <- rprtpckg_list[[1]]
  tmp.survey <- rprtpckg_list[[2]]


  tmp.report <- rprtpckg_list[[3]]
  assign("tmp.report", value = tmp.report, envir=globalenv())



  #Error if data is not available:
  tmp.data <- surveyGetDataLongformat(ids = tmp.sids,
                                      ubb = ubb)
  assign("tmp.data", value = tmp.data, envir=globalenv())


  #CLI: Downloaded?
  if (exists("tmp.data") == TRUE) {
    cli::cli_alert_success("Downloaded data from LimeSurvey.")
  }else {
    cli::cli_alert_warning("Cannot download data from LimeSurvey.")
  }


  #N to print in report
  tmp.n <- get_n(audience, data = tmp.sids.df)


  #Get meta data
  tmp.meta <- plotGetMetaData(rprtpckg = tmp.rprtpckg,
                              audience = audience,
                              report = tmp.report,
                              ganztag = ganztag)

  #Create a unique plot list in case of serval templates
  meta_split <- stringr::str_split_fixed(tmp.meta, pattern = "#", n = 2)
  template <- meta_split[1:1]
  plotnames <- meta_split[,2]
  plotnames <- unique(plotnames)
  meta_combined <- paste0(template, "#", plotnames)
  tmp.meta <- meta_combined

  assign("tmp.meta", value = tmp.meta, envir=globalenv())

  #Duration (tmp.dauer) or UBB only, otherwise NULL
  assign("tmp.dauer", value = NULL, envir=globalenv())

  #Further adjustments for UBB
  if (ubb == TRUE) {
    tmp.meta <- unique(tmp.meta)
    drop_meta1 <- stringr::str_which(tmp.meta, "\\#NA")
    #Freitext del?
    drop_meta2 <- stringr::str_which(tmp.meta, "\\#A3a")

    drop_meta <- c(drop_meta1, drop_meta2)
    #tmp.meta <- as.vector(tmp.meta$plotdata)
    tmp.meta <- tmp.meta[-c(drop_meta)]

    #freitext <- tmp.data |> dplyr::filter(vars == "A311UBB")
    freitext <- tmp.data |> dplyr::filter(vars == "A311ub")
    tmp.freitext <- freitext$vals
    #tmp.freitext <- unique(text)
    assign("tmp.freitext", value = tmp.freitext, envir=globalenv())

    #Dauer of UBB
    tmp.dauer <- tmp.data |> dplyr::filter(vars == "Dauer") |>
      dplyr::pull(vals) |> unique()

    if (length(tmp.dauer) == 1) {
      if (tmp.dauer == "1") {
        tmp.dauer <- "20 Minuten"
      }

      if (tmp.dauer == "2") {
        tmp.dauer <- "45 Minuten"
      }
    }

    #tmp.dauer <- "45 Minuten"
    assign("tmp.dauer", value = tmp.dauer, envir=globalenv())

    #tmp.meta <- tmp.meta[-1]
    assign("tmp.meta", value = tmp.meta, envir=globalenv())
  }


  cli::cli_progress_step("Create data and plots:", spinner = TRUE)
  create_allplots2(meta = tmp.meta,
                   audience = audience,
                   data = tmp.data,
                   report = tmp.report,
                   snr = snr,
                   ubb = ubb)

  cli::cli_progress_step("Export tables:", spinner = TRUE)
  export_tables(meta = tmp.meta,
                data = tmp.data,
                snr = snr,
                report = tmp.report,
                audience = audience,
                ubb = ubb)

  #Render results
  cli::cli_progress_update();
  cli::cli_progress_step("Render results", spinner = TRUE)

  create_pdfs(snr = snr,
              audience = audience,
              name = tmp.name,
              ubb = ubb,
              n = tmp.n,
              d = tmp.dauer,
              results = results,
              drop = FALSE)

}




#' Check if a survey on Lime Survey has valid response
#'
#' @description Connect to Lime Survey and checks if a survey has valid response
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB

check_response <- function (snr,
                            audience,
                            ubb) {
  #Connect
  tmp.server <- config::get("tmp.server")
  tmp.user <- config::get("tmp.user")
  tmp.credential <- config::get("tmp.credential")

  tmp.session <- surveyConnectLs(user = tmp.user,
                                 server = tmp.server,
                                 credential = tmp.credential)

  #GET IDs
  #survey_list <- surveyGetSurveyIds(snr, ubb)
  snr <- as.character(snr)

  #Get survey list and filter by SNR
  tmp.surveys <- call_limer(method = "list_surveys") |>
    dplyr::mutate(snr = stringr::str_sub(surveyls_title, 1, 4)) |>
    dplyr::filter(
      stringr::str_detect(snr, "[0-9][0-9][0-9][0-9]") &
        stringr::str_detect(surveyls_title, "ubb") == ubb
    ) |>
    dplyr::mutate(str = !!snr) |>
    dplyr::filter(snr == !!snr)


  if (nrow(tmp.surveys) == 0) {
    cli::cli_abort("Error in check_response: SNR not found in Limesurvey.")
  }

  #Help fun to count cases:
  #del full_responses
  get_n = function (id) {
    tmp <- call_limer(method = "get_summary",
                      params = list(iSurveyID = id)) |>
      as.data.frame() |>
      dplyr::mutate(sid = id) |>
      dplyr::select(sid, completed_responses)
  }
  #Apply to all elemets and rbind them
  tmp.resp <- lapply(tmp.surveys$sid, get_n)
  tmp.stat <- do.call("rbind", tmp.resp)

  release_session_key()

  #Join tmp.stat to tmp.surveys
  survey_list <- tmp.surveys |>
    dplyr::left_join(tmp.stat)

  #Response check for all
  if (audience == "all") {
    survey_list <- survey_list |> dplyr::filter(completed_responses > 0)

    if (nrow(survey_list) == 0) {
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      df <- tibble::tibble(snr = snr,
                           check = FALSE)

      return(df)
    } else {
      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      df <- tibble::tibble(snr = snr,
                           check = TRUE)

      #usethis::ui_info("Response check for: {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
      return(df)
    }


  }


  #Response check for other reports audiences
  audience_list <- c("sus", "elt", "leh", "ubb", "aus", "ubb")
  #Filter main groups
  if (audience %in% audience_list == TRUE) {

    #Which ID for group
    results_audience <- stringr::str_which(survey_list$surveyls_title, audience)


    completed_responses <- survey_list[results_audience, ]$completed_responses

    if (any(completed_responses == 0)) {
      df <- tibble::tibble(snr = snr,
                           check = FALSE)

      return(df)
    }else {
      df <- tibble::tibble(snr = snr,
                           check = TRUE)
      return(df)
    }

  }



}












#' Run in Parallel Mode
#'
#' @description Run all functions to create a report in parallel
#' @param snr School number
#' @param audience Audience
#' @param ubb UBB
#' @param results Results: Text string for Audience
#' @param ganztag Ganztagsschule
#' @param stype School type
#' @return Returns PDF file
#'
#' @export

run_Parallel <- function(snr,
                        audience,
                        ubb,
                        results,
                        ganztag,
                        stype) {

  assign("ubb", value = ubb, envir=globalenv())

  cli::cli_progress_step("Create data and plots", spinner = TRUE)
  get_parameter(snr = snr,
                audience = audience,
                ubb = ubb,
                ganztag = ganztag,
                stype = stype)


  cli::cli_progress_update();
  cli::cli_progress_step("Get report infos", spinner = TRUE)
  # plots_report <- reports |>
  #   dplyr::filter(report == tmp.report) |>
  #   dplyr::arrange(plot) |>
  #   dplyr::pull(plot) |>
  #   unique()

  plots_report <- sub(".*#(.*)", "\\1", tmp.meta)

  if (ubb) {
    header_report <- plots_headers_ubb
    header_report <- header_report |> dplyr::filter(plot %in% plots_report)
  }else {
    header_report <- plots_headers
    header_report <- header_report |> dplyr::filter(plot %in% plots_report)
  }

  #we need to count how many vars in each plot
  plots_count <- reports |>
    dplyr::filter(report == tmp.report) |>
    dplyr::group_by(plot) |>
    dplyr::summarise(vars_count = dplyr::n_distinct(vars))

  if (ubb) {
    #Give the word cloud room to breathe
    plots_count$vars_count[plots_count$plot == "A3a"] <- 6
  }



  header_report <- header_report |> dplyr::left_join(plots_count, by = "plot")

  tmp.meta <- paste0("XX#", header_report$plot)
  tmp.count <- header_report$vars_count

  if (!ubb) {
    #Give the word cloud room to breathe
    #headertxt <- paste0("## ", header_report$header1, "\n", "### ", header_report$header2, "\n\n")
    headertxt <- paste0("## ", header_report$header1, ": ", header_report$header2, "\n\n")

  }else {
    headertxt <- paste0("## ", header_report$header1, "\n\n")
  }

  num_cores <- parallel::detectCores()
  workers <- max(1, num_cores - 1)  #


  #plan(multicore, workers = 4)  # Adjust workers based on your CPU cores
  future::plan(future::multisession, workers = num_cores)  #

  cli::cli_progress_update();
  cli::cli_progress_step("Create plots", spinner = TRUE)

  # Generate the list of plots in parallel
  plot_list <- furrr::future_map(tmp.meta, ~ export_plot(
    meta = .x,
    snr = snr,
    audience = audience,
    report = tmp.report,
    data = tmp.data,
    ubb = ubb,
    export = TRUE
  ), .progress = TRUE,
  .options = furrr::furrr_options(
    packages = c("ggtext", "showtext", "stringr", "ggwordcloud", "dplyr")  # Ensure packages are loaded in each worker
  ))

  # cli::cli_progress_update();
  # cli::cli_progress_step("Create tables", spinner = TRUE)
  # # Generate the list of tables in parallel
  # table_list <- furrr::future_map(tmp.meta, ~ get_table(
  #   meta = .x,
  #   data = tmp.data,
  #   audience = audience,
  #   ubb = ubb,
  #   report = tmp.report,
  #   snr = snr,
  #   export = FALSE
  # ), .progress = TRUE)


  #cli::cli_progress_update();
  cli::cli_progress_step("Create PDF", spinner = TRUE)
  tmp.dir <- get_directory(snr = snr)

  # Create a template Rmd file
  generate_rmd(meta = tmp.meta,
               num_bars = tmp.count,
               ubb = ubb,
               header = headertxt,
               file_name = paste0(tmp.dir, "/", "template.Rmd"))

  rmarkdown::render(
    input = paste0(tmp.dir, "/", "template.Rmd"),
    output_file = paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf"),
    quiet = TRUE,
    clean = FALSE,
    params = list(
      snr = snr,
      name = tmp.name,
      n = tmp.n,
      d = tmp.dauer,
      fb = results
    ))

  # List all files in the directory (without full paths)
  all_files <- list.files(tmp.dir, full.names = TRUE, recursive = TRUE)
  # Filter out the PDF files
  files_to_delete <- all_files[!grepl("\\.pdf$", all_files, ignore.case = TRUE)]
  # Delete the non-PDF files
  file.remove(files_to_delete)
  dirs_to_delete <- here::here(tmp.dir, "plots")
  unlink(dirs_to_delete, recursive = TRUE)

  #Report via CLI if results are available:
  x <- paste0(tmp.dir, "/", snr, "_results_", audience, ".pdf")

  if (file.exists(x) == TRUE) {
    usethis::ui_done("Exported PDF file for school {usethis::ui_value(snr)} and group {usethis::ui_value(audience)}")
  }

  if (interactive() == TRUE) {
    invisible(system(paste0('open ', x)))
  }


}






#' Test Export Function
#' @description Functions runs export_plots function for testing a single plot
#' @param testmeta Test meta data
#' @return Returns a plot
#'

test_ExportFun <- function(testmeta) {

  #Split meta list
  tmp.var <- stringr::str_split(testmeta,"#") |> unlist()
  tmp.rprtpckg <- tmp.var[1]
  tmp.plotid <- tmp.var[2]

  #Get data
  tmp.tab <- plotGetData(data = tmp.data,
                         plotid = tmp.plotid,
                         rprtpckg = tmp.rprtpckg,
                         report = tmp.report,
                         audience  = tmp.audience)

  #Get set
  tmp.set <- tmp.tab |>
    dplyr::group_by(set) |>
    dplyr::summarise(anz = dplyr::n()) |>
    dplyr::select(set) |>
    unlist()

  #Labels
  tmp.item.labels <- readxl::read_excel(here::here("orig/report_meta_dev.xlsx"),
                                        sheet = 'sets') |>
    dplyr::filter(
      set == tmp.set
    ) |>
    dplyr::arrange(
      dplyr::desc(sort)
    )


  data <- tmp.tab


  tmp.var_plot <- length(unique(data$vars))



  #Manual adjustments for filter questions
  data <- data |> dplyr::filter(vals != "k. A.")


  las_theme <- ggplot2::theme(
    #axis.title.x = ggplot2::element_blank(),
    legend.position = "none",
    axis.text.x = ggplot2::element_text(size = 11),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 12),
    plot.margin = ggplot2::margin(t = 10,  # Top margin
                                  r = 0,  # Right margin
                                  b = 10,  # Bottom margin
                                  l = 0)) # Left margin



  data$newlable <- paste0(data$vars, ": ", data$label_short)
  data$newlable <- as.factor(data$newlable)

  tmp.p <- ggplot2::ggplot(data, ggplot2::aes(fill = vals, y = p, x = newlable)) +
    ggplot2::geom_bar(
      stat = 'identity',
      position = ggplot2::position_stack(),
      width = 0.5
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_n, group = factor(vals)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 2.8,
      fill = "white",
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(
      breaks = rev(tmp.item.labels$labels),
      values = rev(tmp.item.labels$colors),
      drop = TRUE
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 1),
                              labels = function(x)
                                stringr::str_wrap(x, width = 40),
                              limits = rev(levels(data$newlable))) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 10),
      #legend.text = ggplot2::element_text(size=8),
      axis.text.y = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::labs(x = '', y = 'Prozent', fill = "") +
    ggplot2::guides(fill  =  ggplot2::guide_legend(nrow = 2))+
    las_theme

  return(tmp.p)

}




#' Export table headers (Superseded by Parallelization)
#' @description Returns text (headers) for the reports
#' @param meta Meta data
#' @param ubb UBB
#'
#' @return data
#' @export
export_headers <- function (meta,
                            ubb) {

  #Which headers
  if (ubb == TRUE) {
    headers <- plots_headers_ubb
  }else {
    headers <- plots_headers
  }

  #Match survey items (plotsnames) with headers
  plotnames <- stringr::str_split_fixed(meta, pattern = "#", n = 2)
  plotnames <- plotnames[,2]
  plotnames <- unique(plotnames)

  headers_df <- headers |>
    dplyr::filter(plot %in% plotnames) |>
    dplyr::arrange(sort)

  return(headers_df)
}


#' Get Rid of Blankspace
#' @description Helper function to get rid of blankspace. Run steps manually.
#'
#' @return Alert.

get_rid_of_blankspace <- function() {
  cli::cli_alert("Run this function manually, supposed to update the header data.")

  # header_reports$header1 <- stringr::str_trim(header_reports$header1)
  # header_reports$header1 <- stringr::str_replace_all(header_reports$header1,
  #                                                    pattern = "  ", " ")
  #
  # header_reports$header2 <- stringr::str_trim(header_reports$header2)
  # header_reports$header2 <- stringr::str_replace_all(header_reports$header2,
  #                                                    pattern = "  ", " ")
  #
  # usethis::use_data(header_reports, overwrite = TRUE)
}



utils::globalVariables(c("plots_headers",
                         "schools_names",
                         "sets",
                         "plots_headers_ubb",
                         "reports", "report")
                       )








