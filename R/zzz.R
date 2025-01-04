.onLoad <- function(...) {
  tryCatch(
    {
      S7::methods_register()
    },
    error = function(e) {
      message("Error registering S7 methods: ", conditionMessage(e))
    }
  )
}
