#' Save processed text lines to file
#'
#' @param lines Character vector of text lines
#' @param filename Output filename (default: "processed_text.txt")
#' @return NULL (invisible)
#' @export
save_processed_text <- function(lines, filename = "processed_text.txt") {
  if (length(lines) == 0) {
    cat("No lines to save.\n")
    return(invisible())
  }

  writeLines(lines, filename)
  cat("✓ Text saved to:", filename, "\n")
  cat("  - Lines saved:", length(lines), "\n")
  return(invisible())
}

#' Load processed text lines from file
#'
#' @param filename Input filename (default: "processed_text.txt")
#' @return Character vector of text lines, or NULL if file doesn't exist
#' @export
load_processed_text <- function(filename = "processed_text.txt") {
  if (!file.exists(filename)) {
    cat("⚠ File not found:", filename, "\n")
    return(NULL)
  }

  lines <- readLines(filename)
  cat("✓ Text loaded from:", filename, "\n")
  cat("  - Lines loaded:", length(lines), "\n")
  return(lines)
}
