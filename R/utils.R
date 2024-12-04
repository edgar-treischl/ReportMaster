# # #Helper to Create prod
# # sessioninfo::session_info(to_file = "session.log")
# # renv::activate()
# # renv::snapshot()
# # renv::deactivate()
#
#
# # Example string with ellipsis character
# # text <- "This is an example… with an ellipsis."
# #
# # # Replace the ellipsis character (U+2026) with three dots
# # reports$label <- gsub("…", "...", reports$label)
# # reports$label_short <- gsub("…", "...", reports$label_short)
#
#
# #usethis::use_data(reports, overwrite = TRUE)
#
#
# library(ReportMaster)
# Sys.setenv(R_CONFIG_ACTIVE = "default")
#
# #Parameters######################################################
# tmp.snr <- "0001"
# tmp.ubb <- TRUE
# #Report for: audience == sus, leh, elt, all, ubb, aus
# tmp.audience <- "ubb"
#
# tmp.results <- "ubb"
# tmp.stype <- "gy"
# tmp.ganztag <- FALSE
# #Run###########################################################
#
# run_Parallel(
#   snr = tmp.snr,
#   audience = tmp.audience,
#   stype = tmp.stype,
#   ubb = tmp.ubb,
#   ganztag = tmp.ganztag,
#   results = tmp.results
# )
#
#
#
# get_parameter(snr = tmp.snr,
#               audience = tmp.audience,
#               ubb = tmp.ubb,
#               ganztag = tmp.ganztag,
#               stype = tmp.stype)
#
#
#
# export_plot(meta = tmp.meta[8],
#             snr = tmp.snr,
#             audience = tmp.audience,
#             report = tmp.report,
#             ubb = tmp.ubb,
#             data = tmp.data,
#             export = FALSE)
#
#
# tmp.freitext
#
# freitext <- tmp.data |> dplyr::filter(vars == "A311ub")
#
# df <- tibble::tibble(txt = freitext$vals)
#
# # word_data <- df |>
# #   tidytext::unnest_tokens(word, txt)
#
# word_count <- df |>
#   dplyr::count(txt, sort = TRUE)
#
#
#
# set.seed(123)
# ggplot2::ggplot(word_count, ggplot2::aes(label = txt, size = n)) +
#   ggwordcloud::geom_text_wordcloud() +
#   ggplot2::scale_size_area(max_size = 14)+
#   ggplot2::theme_void()
#
#
#
#
#
#
#
