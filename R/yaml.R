# For whatever reason, it seems that a qmd with 'demo: no' in the YAML will
# render, but will be skipped when it comes to site's index.qmd being rendered;
# these posts simply won't appear on the homepage. We can read in the qmd,
# remove 'draft: no' if it exists, and resave it.
