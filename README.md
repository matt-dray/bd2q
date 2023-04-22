
# bd2q

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Helpers for porting [a {blogdown} blog](https://pkgs.rstudio.com/blogdown/) to [a Quarto blog](https://quarto.org/docs/websites/website-blog.html). Very much a work in progress.

'bd2q' = '{blogdown} to Quarto'.

# Context

The aim is to help convert my live {blogdown} blog, [rostrum.blog](https://ww.rostrum.blog), to Quarto. It would be tedious to do this manually because there's nearly 150 posts.

The original source code is at [matt-dray/rostrum-blog](https://github.com/matt-dray/rostrum-blog) and the Quarto version will be at [matt-dray/rostrum-blog-2](https://github.com/matt-dray/rostrum-blog-2). 

The package is for my own use, but should be pretty generalisable to other {blogdown}-to-Quarto transitions. Your mileage may vary.

# Scope

Two things are in scope for now: organising the directory structure for posts and amending the content of the qmd file for each post.

This includes functions that:

* create a directory containing a Quarto blog template (like in [the quickstart guidance](https://quarto.org/docs/websites/website-blog.html))
* set up a new posts/ directory and fill with posts (converted from Rmd to qmd)
* retrieve resources (images, etc) and put them in a 'resources' subdirectory in each post's directory
* adjust YAML content of each post (e.g. remove 'draft' lines, consolidate categories/tags, etc)
* adjust inline code (e.g. my 'update' divs can become Quarto callouts, session info details blocks can be removed and added to appendix, etc)

I'll still have to go through certain posts manually to correct for out-of-date packages and things like linkrot.
