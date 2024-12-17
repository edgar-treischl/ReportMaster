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
