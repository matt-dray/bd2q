% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust-qmd-files.R
\name{update_resource_paths}
\alias{update_resource_paths}
\title{Update Resource Paths}
\usage{
update_resource_paths(
  q_path,
  resources_dir = "resources",
  resource_rx = "<img src="
)
}
\arguments{
\item{q_path}{Character. Path to directory containing the Quarto blog.}

\item{resources_dir}{Character. The name of the folder that contains
resources, such as images, that are used in posts.}

\item{resource_rx}{Character. A regular expression that will capture a line
from a qmd file that contains resource paths to be edited.}
}
\value{
Nothing. New directory structure and files are created at the path
given by q_path, in the subdirectory given by resources_dir.
}
\description{
Find lines from each post that contain paths to resources and update them
from 'blogdown' to Quarto structure.
}
\details{
Assumes that the resources for a 'blogdown' post are at the path
/static/post/YYYY-MM-DD-post-name_files/, while for Quarto there's a
subdirectory for resources (defaults to 'resources/) in
/posts/YYYY-MM-DD-post-name/ alongside that post's index.qmd file.
}
\examples{
\dontrun{
update_resource_paths(
  q_path = "~/Documents/new-quarto-project/",
  resources_dir = "resources",
  resource_rx = "<img src="  # lines that contain images
)
}
}
