# tmp.server <- config::get("tmp.server")
# tmp.user <- config::get("tmp.user")
# tmp.credential <- config::get("tmp.credential")
#
# #Create directories under res
# year <- format(Sys.Date(), "%Y")
#
#
#
# #Get name of school
# tmp.name <- get_sname(tmp.snr)
# assign("tmp.name", value = tmp.name, envir=globalenv())
# cli::cli_alert_info("Get parameters for: {tmp.name}")
#
#
#
#
#
#
# #Single steps
# tmp.session <- surveyConnectLs(user = tmp.user,
#                                credential = tmp.credential,
#                                server = tmp.server)
#
# #Get data and meta data
# tmp.sids.df <- surveyGetSurveyIds(tmp.snr, year, tmp.ubb)
# #assign("tmp.sids.df", value = tmp.sids.df, envir=globalenv())
#
# #Get report package
# tmp.sids <- tmp.sids.df$sid
#
#
# which_n <- stringr::str_which(tmp.sids.df$surveyls_title, tmp.audience)
#
# tmp.sids.df
#
#
# tmp.n <- tmp.sids.df$completed_responses[which_n]
# return(tmp.n)
#
# tmp.sids.df$sus <- stringr::str_detect(tmp.sids.df$surveyls_title, tmp.audience)
#
# tmp.sids.df <- tmp.sids.df |>
#   dplyr::filter(sus == TRUE) |>
#   dplyr::arrange(surveyls_title) |>
#   dplyr::select(surveyls_title, completed_responses) |>
#   dplyr::mutate(n_group = c("GS", "GM", "GS Ganztag", "GM Ganztag"))
#
# paste0(tmp.sids.df$completed_responses, " (", tmp.sids.df$n_group, ")")
#
# master_to_template <- MetaMaster::DB_Table("master_to_template")
# View(master_to_template)
#
#
#
