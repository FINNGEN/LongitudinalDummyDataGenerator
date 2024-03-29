########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
usethis::use_description(
  fields = list(
    Title = "Longitudinal Dummy Data Generator",
    Description = "Creates Longitudinal Dummy data from summary statiscics",
    `Authors@R` = 'person("Javier", "Gracia-Tabuenca", email = "javier.graciatabuenca@tuni.fi",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "YOUR-ORCID-ID"))',
    License = "none",
    Language =  "en"
  )
)


usethis::use_readme_md()


usethis::use_git()


renv::init()


