#' Extract text from PDF and create line-by-line dataframe
#'
#' @param pdf_path Path to the PDF file
#' @return A dataframe with columns: page, line, text
#' @export
extract_pdf_text <- function(pdf_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  # Extract text from PDF
  pdf_content <- pdftools::pdf_text(pdf_path)
  text_lines <- unlist(strsplit(pdf_content, "\n"))

  # Create dataframe with page, line, and text information
  df_text <- data.frame(
    page = rep(1:length(pdf_content), sapply(strsplit(pdf_content, "\n"), length)),
    line = 1:length(text_lines),
    text = trimws(text_lines) # remove extra spaces
  ) %>%
    dplyr::filter(text != "") # remove empty lines

  return(df_text)
}

#' Simple wrapper to extract just the text lines
#'
#' @param pdf_path Path to the PDF file
#' @return Character vector with text lines
#' @export
extract_text_lines <- function(pdf_path) {
  df <- extract_pdf_text(pdf_path)
  return(df$text)
}
