#' Sets data
#'
#' This data defines sets, lables, colors, etc.
#'
#' @format A data frame with 54 rows and 7 variables:
#' \describe{
#'   \item{set}{The set name}
#'   \item{code}{The numerical value code}
#'   \item{labels}{The labels}
#'   \item{sort}{The sorting}
#'   \item{colors}{The colors}
#'   \item{text_color}{The text color}
#'   \item{timestamp}{Timestamp of creation}
#'   ...
#' }
#' @name sets
#' @docType data
NULL


#' The plot headers data
#'
#' This data which headers are displayed by each plot of the survey.
#'
#' @format A data frame with 112 rows and 5 variables:
#' \describe{
#'   \item{sort}{The sorting}
#'   \item{plot}{The plot name}
#'   \item{header1}{The first header}
#'   \item{header2}{The second header}
#'   \item{timestamp}{Timestamp of creation}
#'   ...
#' }
#' @name plots_headers
#' @docType data
NULL

#' The UBB plot headers data
#'
#' This data which headers are displayed by each plot of the UBB.
#'
#' @format A data frame with 112 rows and 5 variables:
#' \describe{
#'   \item{sort}{The sorting}
#'   \item{plot}{The plot name}
#'   \item{header1}{The first header}
#'   \item{timestamp}{Timestamp of creation}
#'   ...
#' }
#' @name plots_headers_ubb
#' @docType data
NULL


#' The headers reports data with all headers
#'
#' This data which headers are displayed by each plot of the survey.
#'
#' @format A data frame with 141 rows and 6 variables:
#' \describe{
#'   \item{sort}{The sorting}
#'   \item{plot}{The plot name}
#'   \item{header1}{The first header}
#'   \item{header2}{The second header}
#'   \item{timestamp}{Timestamp of creation}
#'   \item{report}{Report type}
#'   ...
#' }
#' @name header_reports
#' @docType data
NULL

#' The Report Data
#'
#' This data all report templates.
#'
#' @format A data frame with 112 rows and 5 variables:
#' \describe{
#'   \item{report}{The Report name}
#'   \item{vars}{The variable name}
#'   \item{plot}{The plot name}
#'   \item{label}{The label}
#'   \item{label_short}{The label short}
#'   \item{sets}{The sets}
#'   \item{type}{The type}
#'   \item{timestamp}{Timestamp of creation}
#'   ...
#' }
#' @name reports
#' @docType data
NULL


#' The Templates data
#'
#' This data defines muster, survey, and report template.
#'
#' @format A data frame with 112 rows and 5 variables:
#' \describe{
#'   \item{stype}{The school type}
#'   \item{ganztag}{Ganztag}
#'   \item{type}{Survey type}
#'   \item{surveys}{Survey template}
#'   \item{rprtpckg}{Report package}
#'   \item{ubb}{UBB}
#'   \item{report_tmpl}{Report template}
#'   \item{timestamp}{Timestamp of creation}
#'   ...
#' }
#' @name templates
#' @docType data
NULL

#' The school names data.
#'
#' This data contains the school names.
#'
#' @format A data frame with 54 rows and 7 variables:
#' \describe{
#'   \item{SNR}{The school number}
#'   \item{SNAME}{School name}
#'   ...
#' }
#' @name schools_names
#' @docType data
NULL

#' The Master to Templates data
#'
#' This data contains the school names.
#'
#' @format A data frame with 104 rows and 9 variables:
#' \describe{
#'   \item{sart}{Schulart}
#'   \item{pckg}{Packet}
#'   \item{survey}{Indicator for survey or not}
#'   \item{surveyls_title}{Survey title in Lime Survey}
#'   \item{template}{Template name}
#'   \item{rpt}{Report}
#'   \item{rpt_overall}{Report Overall}
#'   \item{surveyID}{Survey ID Lime Survey}
#'   \item{bfr_grp}{Befragtengruppe}
#'   \item{timestamp}{Timestamp}
#'   ...
#' }
#' @name master_to_template
#' @docType data
NULL
