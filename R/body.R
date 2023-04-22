#' Update Resource Paths
#'
#' Find lines from each post that contain paths to resources and update them
#' from 'blogdown' to Quarto structure.
#'
#' @param q_path Character. Path to directory containing the Quarto blog.
#' @param resources_dir Character. The name of the folder that contains
#'     resources, such as images, that are used in posts.
#' @param resource_rx Character. A regular expression that will capture a line
#'     from a qmd file that contains resource paths to be edited.
#'
#' @details Assumes that the resources for a 'blogdown' post are at the path
#'    /static/post/YYYY-MM-DD-post-name_files/, while for Quarto there's a
#'    subdirectory for resources (defaults to 'resources/) in
#'    /posts/YYYY-MM-DD-post-name/ alongside that post's index.qmd file.
#'
#' @return Nothing. New directory structure and files are created at the path
#'     given by q_path, in the subdirectory given by resources_dir.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_resource_paths(
#'   q_path = "~/Documents/matt-dray/rostrum-blog-2",
#'   resources_dir = "resources",
#'   resource_rx = "<img src="  # lines that contain images
#' )
#' }
update_resource_paths <- function(
    q_path,
    resources_dir = "resources",
    resource_rx = "<img src="
) {

  .check_q_path(q_path)

  qmds_to_fix <- q_path |>
    fs::path("posts") |>
    fs::dir_ls(recurse = TRUE, regexp = ".qmd$")

  purrr::walk(
    qmds_to_fix,
    function(file) {

      qmd_lines <- readr::read_lines(file)
      n_lines <- length(qmd_lines)

      lines_with_img_i <- which(stringr::str_detect(qmd_lines, resource_rx))  # index of img lines
      lines_with_img <- qmd_lines[stringr::str_detect(qmd_lines, resource_rx)]  # string for img line

      lines_replacement <- lines_with_img |>
        stringr::str_replace("/post/", "resources/") |>
        stringr::str_remove("\\d{4}-\\d{2}-\\d{2}-.*_files/") |>
        stats::setNames(lines_with_img_i)  # name replacement string with qmd line index

      for (i in seq_along(lines_replacement)) {
        qmd_lines[as.numeric(names(lines_replacement[i]))] <- lines_replacement[i]
      }

      readr::write_lines(qmd_lines, file)

    }
  )

}
