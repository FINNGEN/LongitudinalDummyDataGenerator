

#' @title generate_dummy_birth_mother_data
#' @description FUNCTION_DESCRIPTION
#' @param birth_mother_level_data_version PARAM_DESCRIPTION, Default: 'R10v3'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Birth Mother dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param birth_mother PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr rename
generate_dummy_mothers_birth_register_data <- function(
    birth_mother_level_data_version="R10v3",
    n_patients_minimum = 100,
    seed=13,
    birth_mother_data = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # Person level information to match for SEX variable in FUTURE. IT IS NEEDED NOW
  #summary_tables <- summary_data_versions_list[[person_level_data_version]]
  # covariates has almost all the info, hence we use it to produces the minimum extended
  #covariates_data <- scanReportToTibble(summary_tables$covariates$ScanReport_covaraites, n_patients_minimum)

  # process birth_mother data
  birth_tables <- summary_data_versions_list[[birth_mother_level_data_version]]
  birth_mother_data <- scanReportToTibble(birth_tables$birth_mother$ScanReport_birth_mother, n_patients_minimum)

  # Change the column from FINNGENID to MOTHER_FINNGENID
  birth_mother_data <- birth_mother_data |> dplyr::rename("MOTHER_FINNGENID" = "FINNGENID")

  return(birth_mother_data)

}




