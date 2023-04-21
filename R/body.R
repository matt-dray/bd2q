#
#
# # Adjust old resource paths -----------------------------------------------
#
#
# qmds_to_fix <- dir_ls(path(new_dir, "posts"), recurse = TRUE, regexp = ".qmd$")
#
# walk(
#   qmds_to_fix,
#   \(file) {
#
#     qmd_lines <- read_lines(file)
#     n_lines <- length(qmd_lines)
#
#     lines_with_img_i <- which(str_detect(qmd_lines, "<img src="))  # index of img lines
#     lines_with_img <- qmd_lines[str_detect(qmd_lines, "<img src=")]  # string for img line
#     lines_replacement <- lines_with_img |>
#       str_replace("/post/", "resources/") |>
#       str_remove("\\d{4}-\\d{2}-\\d{2}-.*_files/") |>
#       setNames(lines_with_img_i)  # name replacement string with qmd line index
#
#     for (i in seq_along(lines_replacement)) {
#       qmd_lines[as.numeric(names(lines_replacement[i]))] <- lines_replacement[i]
#     }
#
#     write_lines(qmd_lines, file)
#
#   }
# )
