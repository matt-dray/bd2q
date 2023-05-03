#' Transfer a 'blogdown' Blog to Quarto Structure
#'
#' A convenience function that creates a Quarto blog template and transfers the
#' structure and content of posts and resources from a 'blogdown' blog project.
#'
#' @param bd_path Character. Path to directory containing a 'blogdown' blog from
#'     where you want to copy posts
#' @param q_path Character. Path to directory containing a Quarto blog where you
#'     want to copy posts to.
#' @param rproj Logical. Include an R Project (.Rproj) file? Defaults to
#'     `TRUE`.
#' @param resources_dir Character. The name of the folder that contains
#'     resources, such as images, that are used in posts.
#' @param exts_keep Character. A vector of extensions for files to keep from the
#'     'blogdown' blog.
#' @param overwrite Logical. Overwrite the existing directory and all posts and
#'     resource files at the provided q_path? Defaults to `FALSE`.
#'
#' @details
#' Executes [create_template], [transfer_posts] and [transfer_resources] to
#' set up the basic Quarto blog template and generate and fill the posts/
#' directory based on the posts found in the provided 'blogdown' blog project
#'
#' @return Nothing. New directory structure and files are created at the path
#'     given by q_path.
#'
#' @export
#'
#' @examples \dontrun{create_and_transfer}
create_and_transfer <- function(
    bd_path,
    q_path,
    rproj = TRUE,
    resources_dir = "resources",
    exts_keep = c("gif", "jpg", "jpeg", "png", "svg", "wav"),
    overwrite = FALSE
) {

  create_template(q_path, rproj, overwrite)

  if (!overwrite) {
    transfer_posts(bd_path, q_path)
    transfer_resources(bd_path, q_path, resources_dir, exts_keep)
  }

  if (overwrite) {
    transfer_posts(bd_path, q_path, overwrite = TRUE)
    transfer_resources(bd_path, q_path, resources_dir, exts_keep, overwrite = TRUE)
  }

}

#' Generate a Quarto Blog Template
#'
#' Create template structure and files for a basic Quarto blog.
#'
#' @param path Character. Path to a directory where you want to create a Quarto
#'     blog.
#' @param rproj Logical. Include an R Project (.Rproj) file? Defaults to
#'     `TRUE`.
#' @param overwrite Logical. Overwrite an existing directory at the provided
#'     path? Defaults to `FALSE`.
#'
#' @details Creates a blog directory at the user-specified path, with the files
#'     _quarto.yml, about.qmd, index.qmd, posts/_metadata.yml, styles.css and
#'     an optional .Rproj file. This is similar to the starter blog created in
#'     [the Quarto quickstart guide](https://quarto.org/docs/websites/website-blog.html),
#'     but with some opinionated differences.
#'
#' @return Nothing. New directory structure and files are created at the path
#'     provided by the user.
#'
#' @export
#'
#' @examples \dontrun{create_template("~/Documents/new-quarto-project")}
create_template <- function(path, rproj = TRUE, overwrite = FALSE) {

  if(!is.logical(rproj) | !is.logical(overwrite)) {
    cli::cli_abort(
      c(
        "Arguments 'rproj' and 'overwrite' must be logical",
        "i" = "Set these arguments to either TRUE or FALSE."
      )
    )
  }

  path <- fs::path(path)
  path_exists <- fs::dir_exists(path)

  if (path_exists & !overwrite) {
    cli::cli_abort(
      c(
        "The directory {path} already exists.",
        "i" = "Set overwrite = TRUE to replace the existing directory."
      )
    )
  }

  if (path_exists & overwrite) {
    fs::dir_delete(path)
  }

  fs::dir_create(path)
  fs::dir_create(fs::path(path, "posts"))

  # _quarto.yml
  readr::write_lines(
    c(
      'project:',
      '  type: website',
      '',
      'website:',
      '  title: "Blog Title"',
      '  navbar:',
      '    right:',
      '      - about.qmd',
      '      - icon: github',
      '        href: https://github.com/',
      '      - icon: mastodon',
      '        href: https://fosstodon.org',
      'format:',
      '  html:',
      '    theme: cosmo',
      '    css: styles.css'
    ),
    fs::path(path, "_quarto.yml")
  )

  # about.qmd
  readr::write_lines(
    c(
      '---',
      'title: "About"',
      '---',
      '',
      '## Section',
      '',
      'Body text.'
    ),
    fs::path(path, "about.qmd")
  )

  # index.qmd
  readr::write_lines(
    c(
      '---',
      'listing:',
      '  contents:',
      '    - "posts/*/index.qmd"',
      '  fields: [date, title]',
      '  feed: true',
      '  sort: "date desc"',
      '  type: grid',
      '  grid-columns: 3',
      '  page-size: 200',
      '  date-format: iso',
      '  categories: false',
      '  sort-ui: false',
      '  filter-ui: false',
      'page-layout: full',
      'title-block-banner: false',
      '---',
      '',
      '<!-- See https://quarto.org/docs/websites/website-listings.html -->'
    ),
    fs::path(path, "index.qmd")
  )

  # styles.css
  readr::write_lines(
    c(
      '/* css styles */',
      ''
    ),
    fs::path(path, "styles.css")
  )

  # posts/_metadata.yml
  readr::write_lines(
    c(
      '# options specified here will apply to all posts in this folder',
      '',
      '# freeze computational output',
      '# (see https://quarto.org/docs/projects/code-execution.html#freeze)',
      'freeze: true',
      '',
      '# Enable banner style title blocks',
      'title-block-banner: true',
      '',
      '# Enable CC licence appendix',
      'license: CC BY-NC-SA 4.0',
      '',
      '# Author name of all blog posts',
      'author:',
      '  - name: Your Name'
    ),
    fs::path(path, "posts", "_metadata.yml")
  )

  if (rproj) {

    proj_path_split <- stringr::str_split(path, "/")[[1]]
    proj_name <- proj_path_split[length(proj_path_split)]

    readr::write_lines(
      c('Version: 1.0'),
      fs::path(path, paste0(proj_name, ".Rproj"))
    )

  }

  cli::cli_alert_success("Created template Quarto blog at {path}.")

}

#' Transfer Posts from 'blogdown' to Quarto
#'
#' Generate the folder structure required to move R Markdown (.Rmd) posts from a
#' 'blogdown' project to a Quarto project and then copy them over, naming each
#' one 'index.qmd' in its own folder.
#'
#' @param bd_path Character. Path to directory containing a 'blogdown' blog from
#'     where you want to copy posts
#' @param q_path Character. Path to directory containing a Quarto blog where you
#'     want to copy posts to.
#' @param ... Passed to [fs::file_copy], with the intention that you can supply
#'     `overwrite = TRUE` if you need to overwrite any existing files.
#'
#' @details Assumes that Rmd posts in the 'blogdown' blog are at the path
#'    /content/post/YYYY-MM-DD-post-name.Rmd. For Quarto, the corresponding path
#'    will be /posts/YYYY-MM-DD-post-name/ and the file containing each post
#'    will be named index.qmd. (Each of these individual post directories can
#'    also hold a folder of  associated resources for each post, which can be
#'    generated and copied from a 'blogdown' blog project using the
#'    [transfer_resources] function.)
#'
#' @return Nothing. New directory structure and files are created at the path
#'     given by q_path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' transfer_posts(
#'     bd_path = "~/Documents/old-blogdown-project/",
#'     q_path  = "~/Documents/new-quarto-project/",
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

  # Create new folder structure for posts to go in
  fs::dir_create(fs::path(q_path, "posts", q_folder_names))
  cli::cli_alert_success("Created posts/ directory structure.")

  # Fill new folders with the old Rmd files, but save as 'index.qmd'

  cli::cli_alert_info("Copying posts.")

  purrr::walk(
    bd_rmds,
    function(rmd_path) {
      dir_name <- stringr::str_remove(basename(rmd_path), ".Rmd")
      q_post_path <- fs::path(q_path, "posts", dir_name, "index.qmd")
      fs::file_copy(rmd_path, q_post_path, ...)
    }
  )

  cli::cli_alert_success(
    "Copied {length(q_folder_names)} posts to {fs::path(q_path, 'posts')}."
  )

}

#' Transfer Resources from 'blogdown' to Quarto
#'
#' Copy resources (images, audio files, etc) from the directory structure of a
#' 'blogdown' blog to that required by Quarto. Assumes you've already set up the
#' directory structure for each post using [transfer_posts].
#'
#' @param bd_path Character. Path to directory containing a 'blogdown' blog from
#'     where you want to copy resources.
#' @param q_path Character. Path to directory containing a Quarto blog where you
#'     want to copy resources to.
#' @param resources_dir Character. The name of the folder that contains
#'     resources, such as images, that are used in posts.
#' @param exts_keep Character. A vector of extensions for files to keep from the
#'     'blogdown' blog.
#' @param ... Passed to [fs::file_copy], with the intention that you can supply
#'     `overwrite = TRUE` if you need to overwrite any existing files.
#'
#' @details Assumes that the resources for a given 'blogdown' post are at the
#'    path /static/post/YYYY-MM-DD-post-name_files/, and the that the
#'    corresponding Quarto project will have a subdirectory for resources, which
#'    defaults to the name 'resources/', in the folder
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
#'     bd_path = "~/Documents/old-blogdown-project/",
#'     q_path  = "~/Documents/new-quarto-project/",
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

  # Create resource folders and fill

  cli::cli_alert_info("Copying resources.")

  purrr::walk(
    bd_resources_keep,
    function(bd_resource_path) {

      # Extract the Quarto post folder name from the blogdown resource path,
      # i.e. extract YYYY-MM-DD-post-name_files and convert to YYYY-MM-DD-post-name
      post_dir <- bd_resource_path |>
        stringr::str_extract("\\d{4}-\\d{2}-\\d{2}-.*_files") |>
        stringr::str_remove("_files") |>
        stringr::str_replace_all("_", "-")  # ensure hyphens only

      # Create resources folder within each Quarto post's folder
      q_resources_path <- fs::path(q_path, "posts", post_dir, resources_dir)
      fs::dir_create(q_resources_path)

      # Fill new folder with files for that post
      q_resource_path <- fs::path(q_resources_path, basename(bd_resource_path))
      fs::file_copy(bd_resource_path, q_resource_path, ...)

    }
  )

  cli::cli_alert_success(
    paste(
      "Copied {length(bd_resources_keep)} resources to each post's",
      "{resources_dir}/ folder in {fs::path(q_path, 'posts')}."
    )
  )

}
