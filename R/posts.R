#' Build Quarto Directory Structure and Add Posts
#'
#' Convert the directory structure for existing 'blogdown' posts to that needed
#' for a newly-created Quarto blog.
#'
#' @param bd_path Character. Path to directory containing the 'blogdown' blog.
#' @param q_path Character. Path to directory containing the Quarto blog.
#' @param ... Passed to [fs::file_copy], with the intention that you can supply
#'     `overwrite = TRUE` if you need to overwrite any existing files.
#'
#' @details Assumes that Rmd posts in the 'blogdown' blog are at the path
#'    /content/post/YYYY-MM-DD-post-name.Rmd. For Quarto, the path must be
#'    /posts/YYYY-MM-DD-post-name/ and the file containing each post must be
#'    named 'index.qmd'. (This directory will also hold a folder of associated
#'    resources, which can be generated using [transfer_resources].)
#'
#' @return Nothing. New directory structure and files are created at the path
#'     given by q_path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' transfer_posts(
#'     bd_path = "~/Documents/matt-dray/rostrum-blog",
#'     q_path  = "~/Documents/matt-dray/rostrum-blog-2",
#'     overwrite = TRUE
#' )
#' }
transfer_posts <- function(bd_path, q_path, ...) {

  .check_bd_path(bd_path)
  .check_q_path(q_path)

  # Paths to all old Rmd files
  bd_rmds <- fs::path(bd_path, "content/post") |>
    fs::dir_ls(regexp = "*.Rmd$")

  # Extract old Rmd names to use as new folder names
  q_folder_names <- basename(bd_rmds) |>
    stringr::str_remove(".Rmd") |>
    stringr::str_replace_all("_", "-")

  cli::cli_alert_success("Created directory structure.")

  # Create new folder structure for posts to go in
  fs::dir_create(fs::path(q_path, "posts", q_folder_names))

  cli::cli_alert_info("Copying posts.")

  # Fill new folders with the old Rmd files, but save as 'index.qmd'
  purrr::walk(
    bd_rmds,
    function(rmd_path) {
      dir_name <- stringr::str_remove(basename(rmd_path), ".Rmd")
      q_post_path <- fs::path(q_path, "posts", dir_name, "index.qmd")
      fs::file_copy(rmd_path, q_post_path, ...)
    }
  )

  cli::cli_alert_success(
    "{length(q_folder_names)} posts copied to {fs::path(q_path, 'posts')}."
  )

}

#' Add Resources to Quarto Post Directories
#'
#' Copy resources (images, etc) from the directory structure of a 'blogdown'
#' blog to that required by Quarto. Assumes you've already set up the directory
#' structure for each post using [transfer_posts].
#'
#' @param bd_path Character. Path to directory containing the 'blogdown' blog.
#' @param q_path Character. Path to directory containing the Quarto blog.
#' @param resources_dir Character. The name of the folder that contains
#'     resources, such as images, that are used in posts.
#' @param exts_keep Character. A vector of extensions for files to keep from the
#'     blogdown blog.
#' @param ... Passed to [fs::file_copy], with the intention that you can supply
#'     `overwrite = TRUE` if you need to overwrite any existing files.
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
#' transfer_resources(
#'     bd_path = "~/Documents/matt-dray/rostrum-blog",
#'     q_path  = "~/Documents/matt-dray/rostrum-blog-2",
#'     resources_dir = "resources",
#'     exts_keep = c("gif", "jpg", "jpeg", "png", "svg", "wav"),
#'     overwrite = TRUE
#' )
#' }
transfer_resources <- function(
    bd_path,
    q_path,
    resources_dir = "resources",
    exts_keep = c("gif", "jpg", "jpeg", "png", "svg", "wav"),
    ...
) {

  .check_bd_path(bd_path)
  .check_q_path(q_path)

  # Paths to folder of each blogdown post's resources
  bd_resources <- bd_path |>
    fs::path("static/post") |>
    fs::dir_ls(recurse = TRUE, type = "file", regexp = "")

  # Paths to resources to be retained
  bd_resources_keep <-
    bd_resources[tolower(tools::file_ext(bd_resources)) %in% tolower(exts_keep)]

  cli::cli_alert_info("Copying resources.")

  # Create resource folders and fill
  purrr::walk(
    bd_resources_keep,
    function(bd_resource_path) {

      # Extract the Quarto post folder name from the blogdown resource path,
      # i.e. extract YYYY-MM-DD-post-name_files and convert to YYYY-MM-DD-post-name
      post_dir <- bd_resource_path |>
        stringr::str_extract("\\d{4}-\\d{2}-\\d{2}-.*_files") |>
        stringr::str_remove("_files") |>
        stringr::str_replace_all("_", "-")

      # Create 'resources' folder for each Quarto post
      q_resources_path <- fs::path(q_path, "posts", post_dir, resources_dir)
      fs::dir_create(q_resources_path)

      # Fill new folder with files for that post
      q_resource_path <- fs::path(q_resources_path, basename(bd_resource_path))
      fs::file_copy(bd_resource_path, q_resource_path, ...)

    }
  )

  cli::cli_alert_success(
    "{length(bd_resources_keep)} resources copied to {fs::path(q_path, 'posts')}."
  )

}
