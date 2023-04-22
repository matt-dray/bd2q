.check_bd_path <- function(
    bd_path,
    bd_hallmarks = c("index.Rmd", "config.toml")
) {

  dir_exists <- fs::dir_exists(bd_path)

  if (!dir_exists) {
    stop(
      paste(
        "The directory", bd_path, "doesn't exist.",
        "You must provide a path to the source for an existing 'blogdown' blog."
      ),
      call. = FALSE
    )
  }

  hallmarks_exist <- fs::path(bd_path, bd_hallmarks) |>
    fs::file_exists() |>
    all()

  if (!hallmarks_exist) {
    warning(
      paste0(
        "The directory ", bd_path,
        " doesn't look like the source for a 'blogdown' blog. ",
        "It doesn't contain one of these files: ", toString(bd_hallmarks), "."
      ),
      call. = FALSE
    )
  }

}

.check_q_path <- function(
    q_path,
    q_hallmarks = "_quarto.yml"
) {

  dir_exists <- fs::dir_exists(q_path)

  if (!dir_exists) {
    stop(
      paste(
        "The Quarto blog directory", q_path, "doesn't exist.",
        "Follow the instructions at",
        "<https://quarto.org/docs/websites/website-blog.html> for setup."
      ),
      call. = FALSE
    )
  }

  hallmarks_exist <- fs::path(q_path, q_hallmarks) |>
    fs::file_exists() |>
    all()

  if (!hallmarks_exist) {
    warning(
      paste0(
        "The directory ", q_path,
        " doesn't look like the source for a Quarto blog. ",
        "It doesn't contain one of these files: ", toString(q_hallmarks), "."
      ),
      call. = FALSE
    )
  }

}
