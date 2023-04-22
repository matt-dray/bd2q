
#' Create a Quarto Blog
#'
#' Create template structure and files for a basic Quarto blog.
#'
#' @param path Character. Path to directory where you want to create a Quarto
#'     blog.
#' @param rproj Logical. Include an R Project (.Rproj) file? Defaults to
#'     `TRUE`.
#' @param overwrite Logical. Overwrite an existing directory at the provided
#'     path? Defaults to `FALSE`.
#'
#' @details Creates a blog directory at the user-specified path, with the files
#'     _quarto.yml, about.qmd, index.qmd, posts/_metadata.yml, styles.css and
#'     an optional .Rroj file. This is similar to the starter blog created in
#'     [the quickstart guide](https://quarto.org/docs/websites/website-blog.html),
#'     but with some opinionated differences.
#'
#' @return Nothing. New directory structure and files are created at the path
#'     provided by the user.
#'
#' @export
#'
#' @examples \dontrun{create_quarto_blog("~/Documents/matt-dray/rostrum-blog-2")}
create_quarto_blog <- function(path, rproj = TRUE, overwrite = FALSE) {

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
        "A directory at {path} already exists.",
        "i" = "Set overwrite = TRUE to overwrite the existing directory."
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
      '<!-- https://quarto.org/docs/websites/website-listings.html -->'
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
      '  - name: Matt Dray'
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

  cli::cli_alert_info("Created template Quarto blog at {path}.")

}
