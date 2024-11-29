# #01 Load source code############################################################
# library(ReportMaster)
# Sys.setenv(R_CONFIG_ACTIVE = "default")
#
#
# #Provide buggers##############################################################
# tmp.snr <- "0001"
# tmp.ubb <- FALSE
# tmp.ganztag <- FALSE
# tmp.audience <- "leh"
# tmp.stype <- "gy"
# tmp.results <- "Eltern"
# ############################################################################

#' Debug the Code
#' @description Function runs the code and returns error messages.
#' @param tmp.snr The snr
#' @param tmp.ubb The ubb
#' @param tmp.ganztag The ganztag
#' @param tmp.audience The audience
#' @param tmp.stype The stype
#' @param tmp.results The results string
#' @return Error message
#' @export

debugme <- function(tmp.snr,
                  tmp.ubb,
                  tmp.ganztag,
                  tmp.audience,
                  tmp.stype,
                  tmp.results) {
  #Make a list
  mylist <- list(snr = tmp.snr,
                 ubb = tmp.ubb,
                 ganztag = tmp.ganztag,
                 audience = tmp.audience,
                 stype = tmp.stype,
                 results = tmp.results)

  #Create reports based on snr list
  results <- purrr::pmap(mylist, purrr::safely(create_reports))


  error_messages <- purrr::map_chr(results, function(res) {
    # Check if there's an error and capture it
    if (!is.null(res$error)) {
      return(res$error$message)  # Return the error message
    } else {
      return(NA)  # No error
    }
  })

  return(error_messages)

}

#debugme(tmp.snr, tmp.ubb, tmp.ganztag, tmp.audience, tmp.stype, tmp.results)

