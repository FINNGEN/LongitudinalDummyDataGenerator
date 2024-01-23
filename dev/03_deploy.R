# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()

#
# options(rmarkdown.html_vignette.check_title = FALSE)
# knitr::knit("vignettes/create_dummy_data.Rmd.orig", output = "vignettes/create_dummy_data.Rmd")
# devtools::build_rmd("vignettes/create_dummy_data.Rmd")
# browseURL("vignettes/create_dummy_data.html")


# increase version
pkgdown::build_site()

#rhub::check_for_cran()
usethis::use_version()

gert::git_push()
