

#' @title generate_dummy_birth_mother_data
#' @description FUNCTION_DESCRIPTION
#' @param birth_mother_level_data_version PARAM_DESCRIPTION, Default: 'R10v3'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Birth Mother dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param service_sector_data PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_mothers_birth_register_data <- function(
    birth_mother_level_data_version="R10v3",
    n_patients_minimum = 100,
    seed=13,
    service_sector_data = NULL
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

  # APPROX_BIRTH_DATE column gives out the information when mother gave birth
  # All of them are NA. To fix this we use service sector data
  # From service sector data, we calculate bday using APPROX_EVENT_DAY and EVENT_AGE
  # Using this bday column from service sector data, we can calculate APPROX_BIRTH_DATE as bday + MOTHER_AGE
  if(!is.null(service_sector_data)){

    # summary from service_sector_data
    ss_summary <- service_sector_data |>
      dplyr::arrange(dplyr::desc(APPROX_EVENT_DAY)) |>
      dplyr::distinct(FINNGENID, .keep_all = T) |>
      dplyr::transmute(
        FINNGENID = FINNGENID,
        bday = lubridate::as_date(APPROX_EVENT_DAY - lubridate::dyears(EVENT_AGE))
      )

    # APPROX_BIRTH_DATE of child is calculated based on MOTHER birth date from service sector data and MOTHER_AGE
    # If MOTHER birth date from service sector is NULL then decimal value of MOTHER_AGE is converted to days and added to BIRTH_YEAR
    # BIRTH_YEAR of child is recalculated from calculated APPROX_BIRTH_DATE of the child
    birth_mother_data <- birth_mother_data |>
      dplyr::left_join(ss_summary, by="FINNGENID") |>
      dplyr::mutate(
        APPROX_DELIVERY_DATE = dplyr::if_else(is.na(bday), lubridate::make_date(as.numeric(DELIVERY_YEAR),1,1) + lubridate::days(round(abs(as.numeric(MOTHER_AGE) - floor(as.numeric(MOTHER_AGE))) * 365.24)), lubridate::as_date(bday + lubridate::dyears(as.numeric(MOTHER_AGE)))),
        DELIVERY_YEAR = lubridate::year(APPROX_DELIVERY_DATE)
      ) |>
      dplyr::select(-bday)
  }

  # Change the column from FINNGENID to MOTHER_FINNGENID
  birth_mother_data <- birth_mother_data |>
    dplyr::rename("MOTHER_FINNGENID" = "FINNGENID")

  return(birth_mother_data)

}




