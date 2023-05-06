.check_bd_path <- function(
    bd_path,
    bd_hallmarks = c("index.Rmd", "config.toml")
) {

  dir_exists <- fs::dir_exists(bd_path)

  if (!dir_exists) {
    cli::cli_abort(
      paste(
        "The directory {.file {bd_path}} doesn't exist.",
        "You must provide a path to the source for an existing 'blogdown' blog."
      )
    )
  }

  hallmarks_exist <- fs::path(bd_path, bd_hallmarks) |>
    fs::file_exists() |>
    all()

  if (!hallmarks_exist) {
    cli::cli_warn(
      paste(
        "The directory {bd_path} doesn't look like it contains the source for",
        "a 'blogdown' blog. It doesn't contain one of these typical files:",
        "{toString(bd_hallmarks)}."
      )
    )
  }

}

.check_q_path <- function(
    q_path,
    q_hallmarks = "_quarto.yml"
) {

  dir_exists <- fs::dir_exists(q_path)

  if (!dir_exists) {
    cli::cli_abort(
      paste(
        "The directory {q_path} doesn't exist. Follow instructions at",
        "{.url https://quarto.org/docs/websites/website-blog.html} for setup."
      )
    )
  }

  hallmarks_exist <- fs::path(q_path, q_hallmarks) |>
    fs::file_exists() |>
    all()

  if (!hallmarks_exist) {
    cli::cli_warn(
      paste(
        "The directory {q_path} doesn't look like the source for a Quarto blog.",
        "It doesn't contain one of these files: {toString(q_hallmarks)}."
      )
    )
  }

}

.check_arg_char <- function(content, allow_null = FALSE) {

  # Capture name of object passed to content (to be used in the abort message)
  arg_name <- as.list(match.call()[-1])[["content"]]

  abort_msg <- c(
    "You must supply a character vector to the argument '{arg_name}'.",
    "i" = "You supplied an object of type '{typeof(content)}'."
  )

  if (!allow_null & !is.character(content)) {
      cli::cli_abort(abort_msg)
  }

  if (allow_null & !(is.character(content) | is.null(content))) {
    cli::cli_abort(abort_msg)
  }

}
