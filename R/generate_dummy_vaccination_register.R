
#' @title generate_dummy_vaccination_data
#' @description FUNCTION_DESCRIPTION
#' @param vaccination_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: vaccination dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param service_sector_data PARAM_DESCRIPTION, Default: NULL
#' @param kanta_medication_delivery_data PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_vaccination_data <- function(
    vaccination_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13,
    service_sector_data = NULL,
    kanta_medication_delivery_data = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # Steps of processing vaccination data
  # Important idea here is to create the following
  # - Most of the events will be vaccination exclusive
  # - 10 events will match exact date and ATC with respect to Kanta medication delivery for same FINNGENID
  # - 5 events will match the Kanta medication delivery date with gap < 14 days but same ATC for same FINNGENID
  # - 1 event will match kanta medication delivery and kela service sector data with exact date and ATC for same FINNGENID

  # process vaccination data
  vaccination_tables <- summary_data_versions_list[[vaccination_level_data_version]]
  vaccination_data <- scanReportToTibble(vaccination_tables$vaccination$ScanReport_vaccination, n_patients_minimum)

  if(!is.null(service_sector_data) & !is.null(kanta_medication_delivery_data)){

    # Filter for IDs in vaccination to that of service sector and kanta medication delivery data
    kanta_medication_delivery <- kanta_medication_delivery_data |>
      dplyr::filter(FINNGENID %in% vaccination_data$FINNGENID) |>
      dplyr::mutate(APPROX_EVENT_DAY = as.character(APPROX_EVENT_DAY))
    service_sector <- service_sector_data |>
      dplyr::filter(SOURCE == "PURCH" & FINNGENID %in% kanta_medication_delivery$FINNGENID) |>
      dplyr::mutate(APPROX_EVENT_DAY = as.character(APPROX_EVENT_DAY))

    # Add 10 random events into vaccination data from Kanta medication delivery so as to match exact date and ATC
    kanta_medication_delivery_sample <- kanta_medication_delivery |>
      dplyr::slice_sample(n = 10)

    random_rows <- kanta_medication_delivery_sample |>
      dplyr::mutate(VACCINATION_AGE = as.character(round(as.numeric(EVENT_AGE),2)),
                    APPROX_VACCINATION_DATE = APPROX_EVENT_DAY,
                    DRUG = ATC_CODE,
                    PRODUCT_NUMBER = PRODUCT_CODE1,
                    DRUG_DEF = NA_character_,
                    METHOD = "IM",
                    INJECTION_SITE = "OO",
                    COVID = "0") |>
      dplyr::transmute(FINNGENID,
                       VACCINATION_AGE,
                       APPROX_VACCINATION_DATE,
                       DRUG,
                       PRODUCT_NUMBER,
                       DRUG_DEF,
                       METHOD,
                       INJECTION_SITE,
                       COVID)
    vaccination_data <- dplyr::bind_rows(vaccination_data, random_rows)

    # Add 5 random events into vaccination data from Kanta medication delivery so as to match date with gap < 14 days but same ATC
    kanta_medication_delivery_sample_gap <- kanta_medication_delivery |>
      dplyr::anti_join(kanta_medication_delivery_sample) |>
      dplyr::slice_sample(n = 5)

    random_rows_gap <- kanta_medication_delivery_sample_gap |>
      dplyr::mutate(APPROX_EVENT_DAY = as.Date(APPROX_EVENT_DAY),
                    offset = sample(1:14, n(), replace = TRUE),
                    APPROX_EVENT_DAY = APPROX_EVENT_DAY + offset,
                    EVENT_AGE = round(as.numeric(EVENT_AGE) + (offset/365.25),2),
                    APPROX_EVENT_DAY = as.character(APPROX_EVENT_DAY),
                    EVENT_AGE = as.character(EVENT_AGE)) |>
      dplyr::mutate(VACCINATION_AGE = EVENT_AGE,
                    APPROX_VACCINATION_DATE = APPROX_EVENT_DAY,
                    DRUG = ATC_CODE,
                    PRODUCT_NUMBER = PRODUCT_CODE1,
                    DRUG_DEF = NA_character_,
                    METHOD = "IM",
                    INJECTION_SITE = "OO",
                    COVID = "0") |>
      dplyr::transmute(FINNGENID,
                       VACCINATION_AGE,
                       APPROX_VACCINATION_DATE,
                       DRUG,
                       PRODUCT_NUMBER,
                       DRUG_DEF,
                       METHOD,
                       INJECTION_SITE,
                       COVID)
    vaccination_data <- dplyr::bind_rows(vaccination_data, random_rows_gap)

    # Add 1 random event into vaccination data from Kanta medication delivery and service sector data so as to match exact date and ATC
    kanta_service_sector_sample <- kanta_medication_delivery |>
      dplyr::inner_join(service_sector, by = c("FINNGENID", "EVENT_AGE", "APPROX_EVENT_DAY", "ATC_CODE" = "CODE1", "PRODUCT_CODE1" = "CODE3")) |>
      dplyr::anti_join(kanta_medication_delivery_sample) |>
      dplyr::anti_join(kanta_medication_delivery_sample_gap) |>
      dplyr::slice_sample(n = 1)

    random_row <- kanta_service_sector_sample |>
      dplyr::mutate(VACCINATION_AGE = as.character(round(as.numeric(EVENT_AGE),2)),
                    APPROX_VACCINATION_DATE = APPROX_EVENT_DAY,
                    DRUG = ATC_CODE,
                    PRODUCT_NUMBER = PRODUCT_CODE1,
                    DRUG_DEF = NA_character_,
                    METHOD = "IM",
                    INJECTION_SITE = "OO",
                    COVID = "0") |>
      dplyr::transmute(FINNGENID,
                       VACCINATION_AGE,
                       APPROX_VACCINATION_DATE,
                       DRUG,
                       PRODUCT_NUMBER,
                       DRUG_DEF,
                       METHOD,
                       INJECTION_SITE,
                       COVID)
    vaccination_data <- dplyr::bind_rows(vaccination_data, random_row)

  }

  return(vaccination_data)

}
