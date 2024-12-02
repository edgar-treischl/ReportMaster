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
# tmp.ubb <- FALSE
# #Report for: audience == sus, leh, elt, all, ubb, aus
# tmp.audience <- "sus"
#
# tmp.results <- "Lehrer"
# tmp.stype <- "gy"
# tmp.ganztag <- FALSE
#
#
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
