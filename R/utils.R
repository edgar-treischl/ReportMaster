# # #Helper to Create prod
# # sessioninfo::session_info(to_file = "session.log")
# # renv::activate()
# # renv::snapshot()
# # renv::deactivate()
#
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
# tmp.stype <- "leh"
# tmp.ganztag <- FALSE
#
#
# #Run###########################################################
#
# runParallel(
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
