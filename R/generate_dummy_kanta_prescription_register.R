
#' @title generate_dummy_kanta_prescription_data
#' @description FUNCTION_DESCRIPTION
#' @param kanta_prescription_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Vision dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param kanta_medication_delivery PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_kanta_prescription_data <- function(
    kanta_prescription_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13,
    kanta_medication_delivery = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # process kanta prescription data
  kanta_prescription_tables <- summary_data_versions_list[[kanta_prescription_level_data_version]]
  kanta_prescription_data <- scanReportToTibble(kanta_prescription_tables$kanta_prescription$ScanReport_kanta_prescription, n_patients_minimum)

  # scanReportTibble creates a single entry for each person but we need to more events.
  # To create multiple events, we will use the minimum extended table to get birth information and duplicate the rows
  # We will fix EVENT_AGE, APPROX_EVENT_DAY, ATC_CODE, PRODUCT_CODE
  # We will also add some dummy CDA_ID_SALATTU codes generated for each event per person

  if(!is.null(kanta_medication_delivery)){

    # Get PURCH events from service sector data and fill out the prescription
    # The idea is to have majority of prescription events mapping to PURCH events
    # There will be some prescription events that are only present in prescription but are not present in PURCH events

    vision_data <- vision_data |>
      dplyr::left_join(minimum_extended |> dplyr::select(FINNGENID, DEATH_FU_AGE), by = "FINNGENID") |>
      dplyr::rowwise() |>
      dplyr::mutate(EVENT_AGE = sprintf("%.2f", runif(1, min = 0.01, max = as.double(DEATH_FU_AGE)))) |>
      dplyr::select(-DEATH_FU_AGE)

  }

  return(vision_data)

}
