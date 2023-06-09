
# bd2q

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![Blog post](https://img.shields.io/badge/rostrum.blog-post-008900?labelColor=000000&logo=data%3Aimage%2Fgif%3Bbase64%2CR0lGODlhEAAQAPEAAAAAABWCBAAAAAAAACH5BAlkAAIAIf8LTkVUU0NBUEUyLjADAQAAACwAAAAAEAAQAAAC55QkISIiEoQQQgghRBBCiCAIgiAIgiAIQiAIgSAIgiAIQiAIgRAEQiAQBAQCgUAQEAQEgYAgIAgIBAKBQBAQCAKBQEAgCAgEAoFAIAgEBAKBIBAQCAQCgUAgEAgCgUBAICAgICAgIBAgEBAgEBAgEBAgECAgICAgECAQIBAQIBAgECAgICAgICAgECAQECAQICAgICAgICAgEBAgEBAgEBAgICAgICAgECAQIBAQIBAgECAgICAgIBAgECAQECAQIBAgICAgIBAgIBAgEBAgECAgECAgICAgICAgECAgECAgQIAAAQIKAAAh%2BQQJZAACACwAAAAAEAAQAAAC55QkIiESIoQQQgghhAhCBCEIgiAIgiAIQiAIgSAIgiAIQiAIgRAEQiAQBAQCgUAQEAQEgYAgIAgIBAKBQBAQCAKBQEAgCAgEAoFAIAgEBAKBIBAQCAQCgUAgEAgCgUBAICAgICAgIBAgEBAgEBAgEBAgECAgICAgECAQIBAQIBAgECAgICAgICAgECAQECAQICAgICAgICAgEBAgEBAgEBAgICAgICAgECAQIBAQIBAgECAgICAgIBAgECAQECAQIBAgICAgIBAgIBAgEBAgECAgECAgICAgICAgECAgECAgQIAAAQIKAAA7)](https://www.rostrum.blog/2023/05/07/bd2q/)
<!-- badges: end -->

Helpers for porting [a {blogdown} blog](https://pkgs.rstudio.com/blogdown/) to [a Quarto blog](https://quarto.org/docs/websites/website-blog.html). Read more [in the blogpost](https://www.rostrum.blog/2023/05/07/bd2q/).

'bd2q' = '{blogdown} to Quarto'.

# Context

The aim is to help convert my live {blogdown} blog, [rostrum.blog](https://ww.rostrum.blog), to Quarto. It would be tedious to do this manually because there's nearly 150 posts.

The original source code is at [matt-dray/rostrum-blog](https://github.com/matt-dray/rostrum-blog) and the Quarto version will be at [matt-dray/rostrum-blog-2](https://github.com/matt-dray/rostrum-blog-2). 
The package is for my own use, but might be useful to other {blogdown}-to-Quarto transitions. Your mileage may vary.

# Scope

Three things are in scope for now: to create a Quarto-blog template repository; to organise the directory structure for posts and copy them over; to amend the content of the index.qmd file for each post.

To transfer from {blogdown} to Quarto:

* `create_template()` to generate a Quarto blog template
* `transfer_posts()` to copy across posts from a {blogdown} blog with the correct structure
* `transfer_resources()` to copy across resources from a {blogdown} blog with the correct structure
* `create_and_transfer()` a convenience function that runs the above three functions in one go (used mostly by me for testing)

To adjust the index.qmd file for each post:

* `update_resource_paths()` to correct resource file paths in each post
* `remove_line()` to delete a single line from a post based on a provided regular expression
* `replace_lines()` to either remove a set of consecutive lines from a post matched to a provided character vector, or replace with a provided vector of consecutive lines.

Plenty of stuff is out of scope, like generating CSS styles, amending Quarto meta files, addressing linkrot, etc. The intent of the package is to automate the easier stuff. You'll probably have to do a lot of manual adjustment once you try to re-render the entire blog from scratch. Good luck to all of us.
