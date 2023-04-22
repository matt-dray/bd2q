
#' Remove 'Draft' Status from Quarto YAML
#'
#' Search for the YAML line in each post's index.qmd that might contain the
#' string 'draft:', remove them and overwrite the original file.
#'
#' @param q_path Character. Path to directory containing the Quarto blog.
#'
#' @details A qmd with 'demo: no' in the YAML will render, but will be skipped
#'     when it comes to the site's index.qmd being rendered; these posts simply
#'     won't appear on the homepage. We can read in the qmd, remove the line
#'     with 'draft:' if it exists, and resave it.
#'
#' @return Nothing. Quarto files are overwritten if they contained a draft
#'     status in their YAML header.
#'
#' @export
#'
#' @examples \dontrun{delete_draft_line("~/Documents/matt-dray/rostrum-blog-2")}
delete_draft_line <- function(q_path) {

  .check_q_path(q_path)

  # Paths to all old Rmd files
  q_qmds <- fs::path(q_path, "posts") |>
    fs::dir_ls(regexp = "index.qmd$", recurse = TRUE)

  # Read the qmd, remove lines starting 'draft:', overwrite original
  purrr::walk(
    q_qmds,
    function(post) {

      post_lines <- readr::read_lines(post)
      line_to_remove <- which(stringr::str_detect(post_lines, "^draft:"))

      if (length(line_to_remove) > 0) {
        post_lines_updated <- post_lines[-line_to_remove]
        readr::write_lines(post)
      }

    }
  )

}
