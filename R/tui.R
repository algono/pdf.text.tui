#' Main Text User Interface for PDF text processing
#'
#' Launch the interactive TUI for complete PDF text processing workflow.
#' Includes extraction, filtering, navigation, and storage functionality.
#'
#' @return NULL (invisible)
#' @export
pdf_text_tui <- function() {
  cat("========================================\n")
  cat("     PDF Text Interactive Processor     \n")
  cat("========================================\n\n")

  processed_lines <- NULL

  while (TRUE) {
    cat("MAIN MENU:\n")
    cat("1. Extract text from PDF\n")
    cat("2. Load previously processed text\n")
    if (!is.null(processed_lines)) {
      cat("3. Interactive filter (current:", length(processed_lines), "lines)\n")
      cat("4. Navigate and copy lines\n")
      cat("5. Save processed text\n")
    }
    cat("q. Quit\n\n")
    cat("Choose an option: ")

    option <- trimws(readline())

    if (option == "1") {
      cat("Enter PDF file path: ")
      pdf_path <- trimws(readline())

      if (pdf_path == "") {
        cat("⚠ No file path provided\n")
        next
      }

      tryCatch({
        df <- extract_pdf_text(pdf_path)
        processed_lines <- df$text
        cat("✓ Extracted", length(processed_lines), "lines from PDF\n\n")
      }, error = function(e) {
        cat("⚠ Error extracting PDF:", e$message, "\n\n")
      })

    } else if (option == "2") {
      cat("Enter filename (or press ENTER for 'processed_text.txt'): ")
      filename <- trimws(readline())
      if (filename == "") filename <- "processed_text.txt"

      loaded_lines <- load_processed_text(filename)
      if (!is.null(loaded_lines)) {
        processed_lines <- loaded_lines
      }
      cat("\n")

    } else if (option == "3" && !is.null(processed_lines)) {
      cat("Starting interactive filter...\n")
      Sys.sleep(1)
      processed_lines <- interactive_filter(processed_lines)
      cat("✓ Interactive filtering completed\n\n")

    } else if (option == "4" && !is.null(processed_lines)) {
      cat("Starting navigation mode...\n")
      Sys.sleep(1)
      copy_navigator(processed_lines)
      cat("✓ Navigation completed\n\n")

    } else if (option == "5" && !is.null(processed_lines)) {
      cat("Enter filename (or press ENTER for 'processed_text.txt'): ")
      filename <- trimws(readline())
      if (filename == "") filename <- "processed_text.txt"

      save_processed_text(processed_lines, filename)
      cat("\n")

    } else if (option == "q") {
      cat("Goodbye! 👋\n")
      break

    } else {
      if (is.null(processed_lines) && option %in% c("3", "4", "5")) {
        cat("⚠ No text loaded. Please extract from PDF or load from file first.\n\n")
      } else {
        cat("⚠ Invalid option\n\n")
      }
    }
  }

  return(invisible())
}
