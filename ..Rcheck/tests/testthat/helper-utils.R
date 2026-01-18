# Test helper functions

#' Skip test if function is not available
#' @param function_name Name of function to check for
skip_if_not_available <- function(function_name) {
  if (!exists(function_name, mode = "function")) {
    testthat::skip(paste("Function", function_name, "not available"))
  }
}
