.check_paths <- function(bd_path, q_path) {

  if (!fs::dir_exists(bd_path)) {
    stop(
      paste("The blogdown directory", bd_path, "doesn't exist."),
      call. = FALSE
    )
  }

  if (!fs::dir_exists(q_path)) {
    stop(
      paste("The Quarto blog directory", q_path, "doesn't exist."),
      call. = FALSE
    )
  }

}
