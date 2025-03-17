
#' @title generate_dummy_kanta_prescription_data
#' @description FUNCTION_DESCRIPTION
#' @param kanta_prescription_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: Vision dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param kanta_medication_delivery PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename stringi
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
  # To fix this, we will use kanta medication delivery data
  # samples from kanta medication delivery data will have the same information in kanta prescription
  # New events will be created that are present only in kanta prescription data

  if(!is.null(kanta_medication_delivery)){

    # Filter out 100 extra events for prescription only using kanta medication delivery events and change the ATC_CODE and PRODUCT_CODE1
    filtered_events <- kanta_medication_delivery |>
      dplyr::sample_n(100)

    kanta_medication_delivery_filtered <- kanta_medication_delivery |>
      dplyr::anti_join(filtered_events, by = c("FINNGENID", "EVENT_AGE", "APPROX_EVENT_DAY", "CDA_ID_SALATTU", "DOC_GROUP_SALATTU", "ATC_CODE", "PRODUCT_CODE1"))

    # Change the ATC_CODE and PRODUCT_CODE1
    filtered_events <- filtered_events |>
      dplyr::mutate(ATC_CODE = sample(filtered_events$ATC_CODE),
                    PRODUCT_CODE1 = sample(filtered_events$PRODUCT_CODE1))

    # combine filtered data with rest of filtered kanta medication delivery
    kanta_medication_delivery <- dplyr::bind_rows(kanta_medication_delivery_filtered, filtered_events)

    # Get kanta medication delivery data and transform data
    kanta_medication_delivery <- kanta_medication_delivery |>
      dplyr::transmute(FINNGENID, EVENT_AGE, APPROX_EVENT_DAY,
                       CDA_ID_SALATTU = DOC_GROUP_SALATTU,
                       DOC_GROUP_SALATTU = stringi::stri_rand_strings(n(), 25, pattern = "[A-Z0-9]"),
                       ATC_CODE,
                       PRODUCT_CODE = PRODUCT_CODE1)

    # Select the kanta prescription data and recycle the rows of rest of the columns
    kanta_prescription_data_expanded <- kanta_prescription_data |>
      dplyr::select(DOC_TYPE_CODE,DOC_VERSION,
                    TYPE_1_AMOUNT, TYPE_1_SIZE,TYPE_2_AMOUNT,TYPE_2_SIZE_UNIT,TYPE_3_TIME,TYPE_3_UNIT,
                    PREPARATION_TYPE_CODE, RESEPTISTATUS,
                    LAAKEMUOTOKOODI, DRUG_NAME_C, DOSE_QUANTITY_TEXT,
                    RENEWAL_BAN,MED_EXCHANGE_BAN,DOSE_DISTRIBUTION,ORGANIZATION_OID_SALATTU,
                    ERIKOISALA_CODE) |>
      dplyr::slice(rep(1:n(), length.out = nrow(kanta_medication_delivery)))

    # Combine the filtered data and kanta prescription data expanded
    kanta_prescription_data <- dplyr::bind_cols(kanta_medication_delivery, kanta_prescription_data_expanded) |>
      dplyr::transmute(FINNGENID,
                       EVENT_AGE,
                       APPROX_EVENT_DAY,
                       CDA_ID_SALATTU,
                       DOC_GROUP_SALATTU,
                       DOC_TYPE_CODE,
                       DOC_VERSION,
                       TYPE_1_AMOUNT,
                       TYPE_1_SIZE,
                       TYPE_2_AMOUNT,
                       TYPE_2_SIZE_UNIT,
                       TYPE_3_TIME,
                       TYPE_3_UNIT,
                       ATC_CODE,
                       PREPARATION_TYPE_CODE,
                       PRODUCT_CODE,
                       RESEPTISTATUS,
                       LAAKEMUOTOKOODI,
                       DRUG_NAME_C,
                       DOSE_QUANTITY_TEXT,
                       RENEWAL_BAN,
                       MED_EXCHANGE_BAN,
                       DOSE_DISTRIBUTION,
                       ORGANIZATION_OID_SALATTU,
                       ERIKOISALA_CODE)

  }

  return(kanta_prescription_data)

}
