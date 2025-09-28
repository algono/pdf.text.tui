#' Interactive line filtering with undo functionality
#'
#' Process text lines interactively, allowing inclusion, exclusion,
#' appending to previous line, and unlimited undo operations.
#'
#' @param lines Character vector of text lines to process
#' @return Character vector of processed lines
#' @export
interactive_filter <- function(lines) {
  if (length(lines) == 0) {
    cat("No lines to process.\n")
    return(character(0))
  }

  result <- c()
  history <- list()  # for undo: each element is a state of 'result'
  i <- 1

  while (i <= length(lines)) {
    # Clear console and show context
    cat("\014")
    cat("========================================\n")
    cat("Line", i, "of", length(lines), "\n")
    cat("========================================\n\n")

    # Show previous line if exists
    if (length(result) > 0) {
      cat("PREVIOUS: \"", result[length(result)], "\"\n\n", sep="")
    }

    # Show current line
    cat("CURRENT:  \"", lines[i], "\"\n\n", sep="")

    # Show next line if exists
    if (i < length(lines)) {
      cat("NEXT:     \"", lines[i+1], "\"\n\n", sep="")
    }

    cat("========================================\n")
    cat("Actions on CURRENT line\n")
    undo_text <- if (length(history) > 0) " | u = undo" else ""
    cat("i = include | e = exclude | a = append to previous", undo_text, " | q = quit\n", sep="")
    cat("Option: ")

    option <- tolower(trimws(readline()))

    if (option == "i") {
      history <- append(history, list(result))  # save state
      result <- c(result, lines[i])

    } else if (option == "e") {
      history <- append(history, list(result))  # save state
      # do nothing (exclude = don't add)

    } else if (option == "a") {
      history <- append(history, list(result))  # save state
      if (length(result) > 0) {
        result[length(result)] <- paste(result[length(result)], lines[i])
      } else {
        result <- c(result, lines[i])
      }

    } else if (option == "u" && length(history) > 0) {
      result <- history[[length(history)]]  # restore state
      history <- history[-length(history)]   # remove from history
      i <- i - 1  # go back to previous line
      next

    } else if (option == "q") {
      break

    } else {
      cat("⚠ Invalid option\n")
      Sys.sleep(1)
      next  # don't advance line
    }

    i <- i + 1
  }

  return(result)
}
