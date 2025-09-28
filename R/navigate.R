#' Interactive text navigator with clipboard copying
#'
#' Navigate through processed text lines with sequential copying capabilities.
#' Allows copying lines to clipboard with navigation controls.
#'
#' @param lines Character vector of text lines to navigate
#' @return NULL (invisible)
#' @export
copy_navigator <- function(lines) {
  if (length(lines) == 0) {
    cat("No lines to navigate.\n")
    return(invisible())
  }

  while (TRUE) {
    # Show all lines numbered
    cat("\014")  # clear console
    cat("========================================\n")
    cat("PROCESSED LINES:\n")
    cat("========================================\n")
    for (i in 1:length(lines)) {
      cat(i, ": ", lines[i], "\n\n", sep="")
    }

    cat("Enter line number to start copying (or 'q' to quit): ")
    option <- readline()

    if (option == "q") {
      cat("Goodbye!\n")
      break
    }

    num <- as.numeric(option)
    if (is.na(num) || num < 1 || num > length(lines)) {
      cat("⚠ Invalid number\n")
      Sys.sleep(1)
      next
    }

    # Sequential mode: copy from chosen line
    current_line <- num
    while (current_line <= length(lines)) {
      # Copy to clipboard
      .copy_to_clipboard(lines[current_line])

      cat("✓ Line", current_line, "copied:\n")
      cat("\"", lines[current_line], "\"\n\n", sep="")
      cat("ENTER = next | r = copy again | b = back to menu | q = quit: ")

      action <- trimws(readline())

      if (action == "q") {
        cat("Goodbye!\n")
        return(invisible())
      } else if (action == "b") {
        break  # back to main menu
      } else if (action == "r") {
        cat("↻ Recopying line", current_line, "...\n")
        next  # repeat current line copy
      } else {
        current_line <- current_line + 1
      }
    }

    # If we reached the end
    if (current_line > length(lines)) {
      cat("You've reached the end of the lines!\n")
      cat("Press ENTER to return to menu...")
      readline()
    }
  }
}

#' Internal function to copy text to clipboard
#' @param text Text to copy
#' @keywords internal
.copy_to_clipboard <- function(text) {
  if (Sys.info()["sysname"] == "Windows") {
    writeClipboard(text)
  } else if (Sys.info()["sysname"] == "Darwin") {
    system(paste0("echo '", text, "' | pbcopy"))
  } else {
    # Linux - try xclip first, then xsel
    if (system("which xclip", ignore.stdout = TRUE) == 0) {
      system(paste0("echo '", text, "' | xclip -selection clipboard"))
    } else if (system("which xsel", ignore.stdout = TRUE) == 0) {
      system(paste0("echo '", text, "' | xsel --clipboard --input"))
    } else {
      warning("Clipboard functionality not available. Install xclip or xsel.")
    }
  }
}
