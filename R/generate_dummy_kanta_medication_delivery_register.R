
#' @title generate_dummy_kanta_medication_delivery_data
#' @description FUNCTION_DESCRIPTION
#' @param kanta_medication_delivery_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Vision dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param service_sector_data PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_kanta_medication_delivery_data <- function(
    kanta_medication_delivery_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13,
    service_sector_data = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # Steps of processing kanta medication delivery data
  # Important idea here is to create many of the kanta medication delivery events present in service sector PURCH events and 100 events
  # exclusively present in kanta medication delivery.
  # 1.   Load the Scan report
  # 2.   Use service sector data PURCH events to fill out medication delivery for 100 samples
  # 2.1  It is simply copying the EVENT_AGE, APPROX_EVENT_DAY, CODE1 and CODE3 columns into respective columns in the medication delivery data
  # 2.2  For some 100 events create events with simply copying EVENT_AGE, APPROX_EVENT_DAY, CODE3 but no mapping CODE1 (ATC code is different)

  # 1. process kanta medication delivery data
  kanta_medication_delivery_tables <- summary_data_versions_list[[kanta_medication_delivery_level_data_version]]
  kanta_medication_delivery_data <- scanReportToTibble(kanta_medication_delivery_tables$kanta_medication_delivery$ScanReport_kanta_medication_delivery, n_patients_minimum)

  # 2. Use service sector data PURCH events to fill out medication delivery for 100 samples
  if(!is.null(service_sector_data)){

    # Collect 100 random IDs from service sector data
    random_ids <- service_sector_data |>
      dplyr::distinct(FINNGENID) |>
      dplyr::sample_n(n_patients_minimum)

    # Filter for PURCH events for 100 IDs
    service_sector_data <- service_sector_data |>
      dplyr::filter(SOURCE == "PURCH" & FINNGENID %in% random_ids$FINNGENID)

    # Filter out 100 random events from filtered PURCH events
    medication_only_data <- service_sector_data |>
      dplyr::distinct(CODE1, .keep_all = TRUE) |>
      dplyr::sample_n(100)

    # Remove 100 random events from filtered PURCH events
    service_sector_data <- service_sector_data |>
      dplyr::anti_join(medication_only_data, by = c("FINNGENID","EVENT_AGE","APPROX_EVENT_DAY","CODE1","CODE3"))

    # Change the CODE1 and CODE3 values for the 100 random events
    medication_only_data <- medication_only_data |>
      dplyr::mutate(CODE1 = sample(medication_only_data$CODE1),
                    CODE3 = sample(medication_only_data$CODE3))

    # Combine the mutated CODE1 for medication only data and service sector data
    service_sector_final <- dplyr::bind_rows(service_sector_data, medication_only_data)

    # Generate the DOC_GROUP_SALATTU and CDA_ID_SALATTU columns
    service_sector_final <- service_sector_final |>
      dplyr::mutate(DOC_GROUP_SALATTU = stringi::stri_rand_strings(n(), 30, pattern = "[A-Z0-9]"),
                    CDA_ID_SALATTU = stringi::stri_rand_strings(n(), 30, pattern = "[A-Z0-9]")
                   ) |>
      dplyr::transmute(FINNGENID,
                       EVENT_AGE,
                       APPROX_EVENT_DAY,
                       CDA_ID_SALATTU,
                       DOC_GROUP_SALATTU,
                       ATC_CODE = CODE1,
                       PRODUCT_CODE1 = CODE3)

    # Select the kanta medication delivery data and recycle the rows of rest of the columns
    kanta_medication_delivery_data_expanded <- kanta_medication_delivery_data |>
      dplyr::select(RESEPTISTATUS,DIS_AMOUNT_CALC_TXT,DIS_AMT_VALUE, DIS_AMOUNT_TXT,DIS_AMT_UNIT,
                    DELIVERY_FEE, MED_EXCHANGED,
                    DOSE_DISTRIBUTION, PREPARATION_TYPE_CODE, PRODUCT_CODE2, PRODUCT_CODE3,
                    LAAKEMUOTOKOODI, DRUG_NAME_C, DOSE_QUANTITY_TEXT) |>
      dplyr::slice(rep(1:n(), length.out = nrow(service_sector_final)))

    # Combine the service sector data and kanta medication delivery data
    kanta_medication_delivery_data <- dplyr::bind_cols(service_sector_final, kanta_medication_delivery_data_expanded) |>
      dplyr::transmute(FINNGENID,
                       EVENT_AGE,
                       APPROX_EVENT_DAY,
                       CDA_ID_SALATTU,
                       DOC_GROUP_SALATTU,
                       RESEPTISTATUS,
                       DIS_AMOUNT_CALC_TXT,
                       DIS_AMT_VALUE,
                       DIS_AMOUNT_TXT,
                       DIS_AMT_UNIT,
                       DELIVERY_FEE,
                       MED_EXCHANGED,
                       DOSE_DISTRIBUTION,
                       ATC_CODE,
                       PREPARATION_TYPE_CODE,
                       PRODUCT_CODE1,
                       PRODUCT_CODE2,
                       PRODUCT_CODE3,
                       LAAKEMUOTOKOODI,
                       DRUG_NAME_C,
                       DOSE_QUANTITY_TEXT)
  }

  return(kanta_medication_delivery_data)

}
