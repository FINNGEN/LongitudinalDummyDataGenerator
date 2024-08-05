
#' @title generate_dummy_kidney_data
#' @description FUNCTION_DESCRIPTION
#' @param kidney_level_data_version PARAM_DESCRIPTION, Default: 'R12v4'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: kidney dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param minimum_extended PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_kidney_register_data <- function(
    kidney_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13,
    minimum_extended = NULL
) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # Person level information to match for SEX variable in FUTURE. IT IS NEEDED NOW
  #summary_tables <- summary_data_versions_list[[person_level_data_version]]
  # covariates has almost all the info, hence we use it to produces the minimum extended
  #covariates_data <- scanReportToTibble(summary_tables$covariates$ScanReport_covaraites, n_patients_minimum)

  # process kidney data
  kidney_tables <- summary_data_versions_list[[kidney_level_data_version]]
  kidney_data <- scanReportToTibble(kidney_tables$kidney$ScanReport_kidney, n_patients_minimum)

  # EVENT_AGE has only 0 with frequency > 50 within original data which is why by default we will get only 0.0
  # To fix this we will assign random EVENT_AGE double numbers > 0.0 and < DEATH_FU_AGE from minimum_phenotype
  if(!is.null(minimum_extended)){

    # Get DEATH_FU_AGE from minimum_extended data
    kidney_data <- kidney_data |>
      dplyr::left_join(minimum_extended |> dplyr::select(FINNGENID, DEATH_FU_AGE, APPROX_BIRTH_DATE), by = "FINNGENID") |>
      dplyr::rowwise() |>
      dplyr::mutate(EVENT_AGE = sprintf("%.2f", runif(1, min = 0.01, max = as.double(DEATH_FU_AGE)))) |>
      dplyr::mutate(APPROX_EVENT_DAY = lubridate::ymd(APPROX_BIRTH_DATE) + round(as.double(EVENT_AGE) * 365.25)) |>
      dplyr::mutate(START_YEAR = lubridate::year(APPROX_EVENT_DAY)) |>
      dplyr::mutate(YEAR = START_YEAR) |>
      dplyr::select(-DEATH_FU_AGE, -APPROX_BIRTH_DATE)

  }

  ## Generate some random rows for tracking
  # Repeat for some 100 sample tracking age and some variable changes
  sampled_ids <- sample(unique(kidney_data$FINNGENID[kidney_data$START_YEAR >= 1999 & kidney_data$START_YEAR < 2021]), 50)
  kidney_data_sampled <- kidney_data |>
    dplyr::filter(FINNGENID %in% sampled_ids)

  # Generate tracking end age, date and year
  kidney_data_sampled <- kidney_data_sampled |>
    dplyr::mutate(
      end_year = START_YEAR + sample(1:(2021 - START_YEAR - 1), 1),
      APPROX_END_DATE_OF_TRACKING = as.character(as.Date(paste0(end_year, "-12-31"))),
      END_OF_TRACKING_AGE = as.character(as.double(EVENT_AGE) + (end_year - START_YEAR)),
      year_diff = end_year - START_YEAR
    ) |>
    dplyr::ungroup()

  # Generate additional rows for tracking years
  # These columns will be changed in the tracking years
  change_cols <- c("T1D", "T2D", "UNKNOWN_TYPE_OF_DIABETES", "IV_IRON", "ANTIHYPERTENSIVE_DRUG")
  treat_cols <- c("PREVIOUS_FORM_OF_TREATMENT","CURRENT_FORM_OF_TREATMENT")
  kidney_data_sampled <- kidney_data_sampled |>
    dplyr::group_by(FINNGENID) |>
    dplyr::do({
      original_row <- .
      additional_rows <- data.frame()

      for (i in 1:original_row$year_diff) {
        new_row <- original_row
        new_row$YEAR <- original_row$START_YEAR + i
        # Set other columns to NA
        for (col in colnames(new_row)) {
          if (!col %in% c("FINNGENID", "YEAR", change_cols, treat_cols)) {
            new_row[[col]] <- NA
          }
        }
        additional_rows <- bind_rows(additional_rows, new_row)
      }
      dplyr::bind_rows(original_row, additional_rows)
    }) |>
    dplyr::ungroup()

  # Change the cols for these additional rows
  kidney_data_sampled <- kidney_data_sampled |>
    dplyr::group_by(FINNGENID) |>
    dplyr::mutate_at(vars(one_of(change_cols)), function(col) {
      col[which(col == 0)[1]] <- 1
      col
    }) |>
    dplyr::ungroup()

  # Change the treatment cols
  kidney_data_sampled <- kidney_data_sampled |>
    dplyr::group_by(FINNGENID) |>
    dplyr::mutate(
      CURRENT_FORM_OF_TREATMENT = sample(unique(CURRENT_FORM_OF_TREATMENT), n(), replace = TRUE),
      PREVIOUS_FORM_OF_TREATMENT = lag(CURRENT_FORM_OF_TREATMENT, default = first(PREVIOUS_FORM_OF_TREATMENT))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-end_year, -year_diff)

  # Add the tracking generated rows
  kidney_data <- dplyr::bind_rows(
    kidney_data |> dplyr::filter(!FINNGENID %in% sampled_ids),
    kidney_data_sampled
  ) |>
    dplyr::arrange(FINNGENID)


  ## Generate random treatments
  # Select kidney data samples that have no tracking
  sampled_ids <- sample(unique(kidney_data$FINNGENID[kidney_data$START_YEAR >= 1999 &
                                                     kidney_data$START_YEAR < 2021 &
                                                     is.na(kidney_data$APPROX_END_DATE_OF_TRACKING)]), 20)
  kidney_data_sampled <- kidney_data |>
    dplyr::filter(FINNGENID %in% sampled_ids)

  # Change the treatment columns by adding additional rows
  kidney_data_sampled <- kidney_data_sampled |>
    dplyr::group_by(FINNGENID) |>
    dplyr::do({
      current_rows <- .
      num_new_rows <- sample(1:2, 1)
      last_current_cat <- current_rows$CURRENT_FORM_OF_TREATMENT[nrow(current_rows)]

      new_rows <- tibble::tibble()

      for (i in 1:num_new_rows) {
        # Generate a new event that is 2 or 3 years later than START_YEAR
        new_event_date <- lubridate::ymd(paste0(sample(current_rows$START_YEAR + 2:3, 1), "-01-01"))
        new_age <- as.character(as.double(current_rows$EVENT_AGE) + (year(new_event_date) - current_rows$START_YEAR))
        new_current_cat <- sample(unique(kidney_data$CURRENT_FORM_OF_TREATMENT), 1)

        # Generate new rows with changed treatment column values
        new_row <- current_rows[1, ] |>
          dplyr::mutate(
            FINNGENID = current_rows$FINNGENID[1],
            APPROX_EVENT_DAY = new_event_date,
            EVENT_AGE = new_age,
            CURRENT_FORM_OF_TREATMENT = last_current_cat,
            PREVIOUS_FORM_OF_TREATMENT = new_current_cat
          ) |>
          dplyr::mutate(dplyr::across(-c(FINNGENID, EVENT_AGE, APPROX_EVENT_DAY, PREVIOUS_FORM_OF_TREATMENT, CURRENT_FORM_OF_TREATMENT), ~NA))

        new_rows <- dplyr::bind_rows(new_rows, new_row)
        last_current_cat <- new_current_cat
      }

      current_rows <- dplyr::bind_rows(current_rows, new_rows)
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(FINNGENID, APPROX_EVENT_DAY)

  # Add the tracking generated rows
  kidney_data <- dplyr::bind_rows(
    kidney_data |> dplyr::filter(!FINNGENID %in% sampled_ids),
    kidney_data_sampled
  ) |>
    dplyr::arrange(FINNGENID)



  return(kidney_data)

}
