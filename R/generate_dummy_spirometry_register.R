
#' @title generate_dummy_spirometry_data
#' @description FUNCTION_DESCRIPTION
#' @param spirometry_level_data_version PARAM_DESCRIPTION, Default: 'R12v1'
#' @param n_patients_minimum PARAM_DESCRIPTION, Default: 100
#' @param description PARAM_DESCRIPTION, Default: spirometry dummy data generator
#' @param seed PARAM_DESCRIPTION, Default: 13
#' @param minimum_extended PARAM_DESCRIPTION, Default: NULL
#' @return list of created tables
#' @export
#' @importFrom dplyr mutate arrange desc distinct transmute left_join if_else select rename stringi
#' @importFrom lubridate as_date dyears make_date days year
generate_dummy_spirometry_data <- function(
    spirometry_level_data_version="R12v1",
    n_patients_minimum = 100,
    seed=13,
    minimum_extended = NULL

) {

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  # process spirometry data
  spirometry_tables <- summary_data_versions_list[[spirometry_level_data_version]]
  spirometry_data <- scanReportToTibble(spirometry_tables$spirometry$ScanReport_spirometry, n_patients_minimum)

  # scanReportTibble creates a single entry for each person. But these following columns needs to be fixed.
  # - BIRTH_DATE and SEX columns need to be filled out using minimum_extended
  # - AGE_AT_TEST and APPROX_DATE needs to be filled out randomly
  # - FEV1_BEST and FVC_BEST columns all have NULL values with FEV1_FVC_RATIO as 1.


  # Select 100 random patients from minimum_extended with the following criteria:
  # - Age range between 10 - 90 and more concentrated in the middle (using normal distribution with mean 45 and sd 20)
  # 60/40 female to male split
  if(!is.null(minimum_extended)){

    # Derive age from APPROX_BIRTH_DATE
    minimum_extended_age <- minimum_extended |>
      mutate(age = as.numeric(Sys.Date() - APPROX_BIRTH_DATE) / 365.25) |>
      filter(age >= 10, age <= 90)

    # Split by SEX and sample by age with normal distribution
    females <- minimum_extended_age |> filter(SEX == "female")
    males <- minimum_extended_age |> filter(SEX == "male")

    females_sample <- females |>
      mutate(weighted_age = dnorm(age, mean = 45, sd = 20)) |>
      slice_sample(n = 60, weight_by = weighted_age)
    males_sample <- males |>
      mutate(weighted_age = dnorm(age, mean = 45, sd = 20)) |>
      slice_sample(n = 40, weight_by = weighted_age)

    # Combine the Female and Male into 100 samples and shuffle the order
    minimum_extended_sample <- bind_rows(females_sample, males_sample) |>
      select(-age, -weighted_age) |>
      slice_sample(n = 100)

    # Copy the FINNGENID, APPROX_BIRTH_DATE, and SEX columns from sampled minimum_extendedinto the spirometry data
    spirometry_data <- spirometry_data |>
      mutate(FINNGENID = minimum_extended_sample$FINNGENID,
             BIRTH_DATE = minimum_extended_sample$APPROX_BIRTH_DATE,
             SEX = minimum_extended_sample$SEX)

    # Assign random AGE_AT_TEST between 10 and 90 and fill in the APPROX_DATE based on BIRTH_DATE and AGE_AT_TEST
    spirometry_data <- spirometry_data |>
      mutate(APPROX_DATE = BIRTH_DATE + sample(as.integer(365.25 * 10):as.integer(365.25 * 90), n(), replace = TRUE),
             AGE_AT_TEST = round(as.numeric(APPROX_DATE - BIRTH_DATE)/365.25, 3)
            )

    # Fixing FEV1_BEST and FVC_BEST
    # - Take FVC mean based on SEX. Given that males have higher lung volumes than females on average
    # - Correct the FVC mean based on AGE_AT_TEST. Given that there will be loss of lung capacity after age 25
    # - Sample FVC_BEST from normal distribution by flooring FVC_BEST at 1.0
    # - Calculate FEV1_FVC_RATIO based on FVC_BEST and OBSTRUCTION. Obstruction reduces the ratio
    # - Sample FEV1_BEST from FEV1_FVC_RATIO and FVC_BEST
    # - Calculate FEV1_POST and FVC_POST by adding random improvement based on OBSTRUCTION values
    # - Fix the FEV1_POST and FVC_POST as per MEASUREMENT_TYPE where FEV1_POST and FVC_POST should be NULL for single measurement type
    # - Calculate changes to FEV1 and FVC between pre and post along with percentage

    spirometry_data <- spirometry_data |>
      rowwise() |>
      mutate(
        # Base FVC mean & sd by SEX
        fvc_mean = case_when(
          SEX == "female" ~ 3.5,
          SEX == "male"   ~ 4.5
        ),
        fvc_sd = case_when(
          SEX == "female" ~ 0.5,
          SEX == "male"   ~ 0.6
        ),

        # Age correction
        # Lung function declines ~0.025L per year after age 25 due to loss of lung elasticity. No penalty applied before age 25.
        age_penalty = max(0, (AGE_AT_TEST - 25) * 0.025),
        fvc_mean    = fvc_mean - age_penalty,

        # -Sample FVC_BEST from normal distribution by flooring FVC_BEST at 1.0
        FVC_BEST = round(max(1.0, rnorm(1, mean = fvc_mean, sd = fvc_sd)), 3),

        # FEV1/FVC ratio bounds by OBSTRUCTION status for pre
        # Normal: 0.75-0.85; Obstructive: 0.35-0.69.
        ratio_min = case_when(
          OBSTRUCTION == "Normal"      ~ 0.75,
          OBSTRUCTION == "Obstructive" ~ 0.35
        ),
        ratio_max = case_when(
          OBSTRUCTION == "Normal"      ~ 0.85,
          OBSTRUCTION == "Obstructive" ~ 0.69
        ),

        # Calculate FEV1_FVC_RATIO based on FVC_BEST and OBSTRUCTION which make sure that FEV1_BEST <= FVC_BEST at all times.
        # Also, calculate FEV1_MAX and FVC_MAX based on 3 random estimates of FEV1_BEST and FVC_BEST
        FEV1_FVC_RATIO = round(runif(1, ratio_min, ratio_max), 2),
        FEV1_BEST  = round(FVC_BEST * FEV1_FVC_RATIO, 2),

        # FVC_max and FEV1_max
        FVC_MAX  = round(max(max(rnorm(3, mean = fvc_mean, sd = fvc_sd)), 1.0), 2),
        FEV1_MAX = round(FVC_MAX * FEV1_FVC_RATIO, 2),

        # post-bronchodilator improvement based on OBSTRUCTION status
        # Obstruction uses a wide range (5-20%) to capture mild-to-severe spread.
        # Normal lungs see only minimal change (2-5%).
        fev1_improv = case_when(
          OBSTRUCTION == "Normal"      ~ runif(1, 0.02, 0.05),
          OBSTRUCTION == "Obstructive" ~ runif(1, 0.05, 0.20)
        ),
        fvc_improv = case_when(
          OBSTRUCTION == "Normal"      ~ runif(1, 0.01, 0.04),
          OBSTRUCTION == "Obstructive" ~ runif(1, 0.03, 0.12)
        ),

        # Calculate post-bronchodilator FVC and FEV1
        # Single measurements get NA for all post columns.
        FVC_POST = if_else(
          MEASUREMENT_TYPE == "reversibility",
          round(FVC_BEST * (1 + fvc_improv), 2),
          NA_real_
        ),
        FEV1_POST = if_else(
          MEASUREMENT_TYPE == "reversibility",
          round(FEV1_BEST * (1 + fev1_improv), 2),
          NA_real_
        ),
        FEV1_FVC_POST = if_else(
          MEASUREMENT_TYPE == "reversibility",
          round(FEV1_POST / FVC_POST, 2),
          NA_real_
        ),

        # Calculate changes to FEV1 and FVC between pre and post along with percentage
        FEV1_CHANGE_L  = if_else(MEASUREMENT_TYPE == "reversibility", round(FEV1_POST - FEV1_BEST, 2), NA_real_),
        FEV1_CHANGE_PCT = if_else(MEASUREMENT_TYPE == "reversibility", round((FEV1_CHANGE_L / FEV1_BEST) * 100, 1), NA_real_),
        FVC_CHANGE_L = if_else(MEASUREMENT_TYPE == "reversibility", round(FVC_POST  - FVC_BEST,  2), NA_real_),
        FVC_CHANGE_PCT = if_else(MEASUREMENT_TYPE == "reversibility", round((FVC_CHANGE_L  / FVC_BEST)  * 100, 1), NA_real_)

      ) |>
      # Remove intermediate columns used for calculations
      select(-fvc_mean, -fvc_sd, -age_penalty, -ratio_min, -ratio_max,
             -fev1_improv, -fvc_improv) |>
      ungroup() |>
      transmute(
        FINNGENID, APPROX_DATE, SOURCE, SEX, BIRTH_DATE, AGE_AT_TEST, AGE_GROUP,
        IS_PEDIATRIC, HEIGHT_BEST, HEIGHT_CLEAN, HEIGHT_FLAG, WEIGHT_MEDIAN, WEIGHT_LATEST, WEIGHT_CLEAN, WEIGHT_FLAG,
        FEV1_BEST, FVC_BEST, FEV1_FVC_RATIO, FEV1_MAX, FVC_MAX,
        MEASUREMENT_TYPE,
        FEV1_POST, FVC_POST, FEV1_FVC_POST,
        FEV1_CHANGE_L, FEV1_CHANGE_PCT,
        FVC_CHANGE_L, FVC_CHANGE_PCT, SIGNIFICANT_RESPONSE, OBSTRUCTION
      )

  }

  return(spirometry_data)

}
