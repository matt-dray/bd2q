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
#'   q_path = "~/Documents/new-quarto-project/",
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

  cli::cli_alert_info("Updating posts.")

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
        readr::write_lines(qmd_lines, file)
      }

    }
  )

  cli::cli_alert_success("{length(qmds_to_fix)} posts updated.")

}

#' Remove a Line from a Quarto Post
#'
#' Search each Quarto blog post's index.qmd file for lines that contain a
#' supplied regular expression, delete them and overwrite the original file.
#'
#' @param q_path Character. Path to directory containing the Quarto blog.
#' @param detect_rx Character. A regular expression that captures text within a
#'     line that you want to delete from each post's index.qmd file.
#'
#' @return Nothing. Quarto files are overwritten if they contain matching lines.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' remove_line(
#'   q_path = "~/Documents/new-quarto-project/",
#'   detect_rx = "^draft:"  # remove YAML line that specifies draft status
#' )
#' }
remove_line <- function(q_path, detect_rx) {

  .check_q_path(q_path)

  if (!is.character(detect_rx) | length(detect_rx) > 1) {
    cli::cli_abort(
      c(
        "You must supply a single regular expression string to detect_rx.",
        "i" = "You supplied an object of type '{typeof(detect_rx)}'."
      )
    )
  }

  # Paths to all qmd files
  q_qmds <- fs::path(q_path, "posts") |>
    fs::dir_ls(regexp = "index.qmd$", recurse = TRUE)

  # Read the qmd, remove lines containing the regex, overwrite original

  cli::cli_alert_info("Making corrections.")

  count_posts <- 0

  purrr::walk(
    q_qmds,
    function(post) {

      post_lines <- readr::read_lines(post)
      line_to_remove <- which(stringr::str_detect(post_lines, detect_rx))

      if (length(line_to_remove) > 0) {
        post_lines_updated <- post_lines[-line_to_remove]
        readr::write_lines(post_lines_updated, post)
        count_posts <<- count_posts + 1
      }

    }
  )

  cli::cli_alert_success(
    paste(
      "Removed lines matching the regular expression '{detect_rx}' from",
      "{count_posts} out of {length(q_qmds)} posts."
    )
  )

}

#' Replace or Remove Lines from a Post
#'
#' @param q_path Character. Path to directory containing the Quarto blog.
#' @param match_str Character. A vector of strings that match exactly to
#'     consecutive lines in a blog post that you want to replace (with
#'     'replacement_str') or remove (when 'replacement_str' is `NULL`).
#' @param replacement_str Character. A vector of consecutive strings to replace
#'     the strings proviede in 'match_str'. Specify `NULL` (default) if you want
#'     to remove 'match_str' without replacement.
#' @param collapse_str Character. The function works by collapsing a post's
#'     lines to a single string, with elements separated by some unique string.
#'
#' @return Nothing. Quarto files are overwritten if they contain matching lines.
#'
#' @export
#'
#' @examples \dontrun{
#' replace_lines(
#'   q_path = "~/Documents/new-quarto-project/",
#'   match_str = c("This is an example", "These are consecutive lines to match"),
#'   replacement_str = c("Replace with this line", "And this one")
#' )
#' }
replace_lines <- function(
    q_path,
    match_str,
    replacement_str,
    collapse_str = "///"
) {

  .check_q_path(q_path)
  .check_arg_char(match_str)
  .check_arg_char(collapse_str)

  # Paths to all qmd files
  q_qmds <- fs::path(q_path, "posts") |>
    fs::dir_ls(regexp = "index.qmd$", recurse = TRUE)

  # Prepare lines as single strings
  match_collapse <- paste(match_str, collapse = collapse_str)
  match_collapse_rx <-  # convert the match string to regular expression form
    stringr::str_replace_all(match_collapse, "(\\W)", "\\\\\\1")
  replacement_collapse <- paste(replacement_str, collapse = collapse_str)

  # Read the qmd, remove specified lines, overwrite original

  cli::cli_alert_info("Making corrections.")

  count_posts <- 0

  purrr::walk(
    q_qmds,
    function(post) {

      post_lines <- readr::read_lines(post)
      post_collapse <- paste(post_lines, collapse = collapse_str)

      if (!is.null(replacement_str)) {
        post_collapse_updated <- post_collapse |>
          stringr::str_replace_all(
            pattern = paste0(match_collapse_rx, collapse_str),
            replacement = paste0(replacement_collapse, collapse_str)
          )
      }

      if (is.null(replacement_str)) {
        post_collapse_updated <- post_collapse |>
          stringr::str_remove(paste0(match_collapse_rx, collapse_str))
      }

      lines_updated <- stringr::str_split_1(post_collapse_updated, collapse_str)

      if (post_collapse != post_collapse_updated) {
        readr::write_lines(lines_updated, post)
        count_posts <<- count_posts + 1
      }

    }
  )

  cli::cli_alert_success(
    paste(
      "Removed lines matching the provided string vector from",
      "{count_posts} out of {length(q_qmds)} posts."
    )
  )

}
