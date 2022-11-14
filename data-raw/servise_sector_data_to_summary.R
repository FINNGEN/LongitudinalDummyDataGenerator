# DESCRIPTION :
#
# 0. Calculate vocabulary and level: Append 2 column to service-sector-data: vocabulary and level, calculated from CATEGORY and ICDVER
# 1. Calculate observation_periods_summary table with: number of patients per first_visit_year (5 years bins), first_visit_age (5 years bins), years_to_last_visit (5 years bins), logmean and logsd of the number of visits per SOURCE. Except foe DEATH register where death n_person_death is calculated, if n_person_death>5.
# 2. Calculate visits_summary table with: number of visits per visit_year_bin, SOURCE,  CODE5, CODE6, and CODE7, with the logmean and logsd of the number of events per visit group
# 3. Calculate SOURCE_VOCAB_EVENT_YEAR_summary table with: n_events per SOURCE, vocabulary, and event_year,
# 4. Calculate CODE1_CODE2_CODE3_vocabulary_summary table with n_events per SOURCE, vocabulary and CODE1 CODE2 CODE3
# 5. Calculate CODE4_summary table with: n_events per SOURCE, vocabulary and CODE4 values in bins



# PARAMETERS --------------------------------------------------------------
n_patients_limit = 1
path_to_service_sector_data <- "../data_mnt/ss_sample.tsv"
output_folder = "../data_mnt/summary_data/"


# LOAD DATA ---------------------------------------------------------------
col_types <- readr::cols(
  FINNGENID = readr::col_character(),
  SOURCE = readr::col_character(),
  EVENT_AGE = readr::col_double(),
  APPROX_EVENT_DAY = readr::col_date(format = ""),
  CODE1 = readr::col_character(),
  CODE2 = readr::col_character(),
  CODE3 = readr::col_character(),
  CODE4 = readr::col_character(),
  CODE5 = readr::col_character(),
  CODE6 = readr::col_character(),
  CODE7 = readr::col_character(),
  ICDVER = readr::col_character(),
  CATEGORY = readr::col_character(),
  INDEX = readr::col_character()
)

service_sector_data <- readr::read_tsv(path_to_service_sector_data, col_types = col_types)


# SETUP -------------------------------------------------------------------
logger <- log4r::create.logger()

remove_n_patients_limit <- function(data, data_name, logger, n_patients_limit){
  n_codes_count <- data |>  nrow()

  data <- data |>
    dplyr::filter(n_patients>n_patients_limit)

  n_codes_count_allowed <- data |> nrow()

  log4r::info(logger, data_name, ": ",
              "n unique codes", n_codes_count,
              "n unique codes more than 5 patients", n5_codes_count,
              "percent unique codes lost", {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)})

  return(data)
}


# FUNCTION ----------------------------------------------------------------

## 0. Calculate vocabulary and level ---------------------------------------

# conver VOCAB into vocabulary and LEVEL into LEVEL
service_sector_data <- service_sector_data |>
  dplyr::mutate(
    vocabulary = dplyr::case_when(
      SOURCE %in% c("INPAT", "OUTPAT") ~ stringr::str_c(ICDVER, stringr::str_extract(CATEGORY, "^[:upper:]+") |> stringr::str_replace_na("")),
      SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT") ~ stringr::str_extract(CATEGORY, "^[:upper:]+"),
      SOURCE == "REIMB" ~ ICDVER,
      SOURCE == "PURCH" ~ "purch",
      SOURCE == "CANC" ~ "canc",
      SOURCE == "DEATH" ~ "death"
      ),
    level = dplyr::case_when(
      SOURCE %in% c("INPAT", "OUTPAT") ~ stringr::str_extract(CATEGORY, "[:digit:]+$") |> stringr::str_replace_na(""),
      SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT") ~ stringr::str_extract(CATEGORY, "[:digit:]+$"),
      SOURCE == "REIMB" ~ as.character(NA),
      SOURCE == "PURCH" ~ as.character(NA),
      SOURCE == "CANC" ~ as.character(NA),
      SOURCE == "DEATH" & CATEGORY=="c1" ~ "1",
      SOURCE == "DEATH" & CATEGORY=="c2" ~ "2",
      SOURCE == "DEATH" & CATEGORY=="c3" ~ "3",
      SOURCE == "DEATH" & CATEGORY=="c4" ~ "4",
      SOURCE == "DEATH" & CATEGORY=="I" ~ "5",
      SOURCE == "DEATH" & CATEGORY=="U" ~ "0"
    )
  )


## 1. Calculate observation_periods_summary ------------------------------------
log4r::info(logger, "Calculate observation_periods_summary")


### count number of visits per SOURCE per patient
n_visits_per_source_per_patient <- service_sector_data |>
  dplyr::distinct(FINNGENID, SOURCE, INDEX) |>
  dplyr::count(FINNGENID, SOURCE) |>
  tidyr::spread(SOURCE, n) |>
  dplyr::mutate_if(is.integer, ~dplyr::if_else(is.na(.), 0L, .))


### calculate observation periods per patient
observation_periods <- service_sector_data |>
  dplyr::select (FINNGENID, APPROX_EVENT_DAY, EVENT_AGE)
observation_periods_tmp_first <- observation_periods |>
  dplyr::arrange(APPROX_EVENT_DAY) |>
  dplyr::distinct (FINNGENID, .keep_all = TRUE) |>
  dplyr::rename(approx_day_first_event = APPROX_EVENT_DAY, age_first_event = EVENT_AGE)
observation_periods_tmp_last <- observation_periods |>
  dplyr::arrange(desc(APPROX_EVENT_DAY)) |>
  dplyr::distinct(FINNGENID, .keep_all = TRUE) |>
  dplyr::rename(approx_day_last_event = APPROX_EVENT_DAY, age_last_event = EVENT_AGE)
observation_periods_per_person <- dplyr::full_join(observation_periods_tmp_first, observation_periods_tmp_last, by="FINNGENID")

### Count periods and calculate lognorm distribution of visit counts for each SOURCE
observation_periods_summary <- observation_periods_per_person |>
  dplyr::transmute(
    FINNGENID=FINNGENID,
    first_visit_year=lubridate::year(approx_day_first_event),
    first_visit_age = round(age_first_event),
    years_to_last_visit =   (lubridate::year(approx_day_last_event) -lubridate::year(approx_day_first_event)),
    first_visit_year = first_visit_year- (first_visit_year %% 5),
    first_visit_age = first_visit_age - (first_visit_age %% 5),
    years_to_last_visit = years_to_last_visit - (years_to_last_visit %% 5)
  ) |>
  dplyr::inner_join(n_visits_per_source_per_patient, by="FINNGENID") |>
  #
  dplyr::group_by(first_visit_year, first_visit_age, years_to_last_visit) |>
  dplyr::summarise(
    n_patients = dplyr::n(),
    INPAT_logmean = mean(log(INPAT+0.5)), INPAT_logsd = sd(log(INPAT+0.5)),
    PURCH_logmean = mean(log(PURCH+0.5)), PURCH_logsd = sd(log(PURCH+0.5)),
    OUTPAT_logmean = mean(log(OUTPAT+0.5)), OUTPAT_logsd = sd(log(OUTPAT+0.5)),
    OPER_OUT_logmean = mean(log(OPER_OUT+0.5)), OPER_OUT_logsd = sd(log(OPER_OUT+0.5)),
    PRIM_OUT_logmean = mean(log(PRIM_OUT+0.5)), PRIM_OUT_logsd = sd(log(PRIM_OUT+0.5)),
    OPER_IN_logmean = mean(log(OPER_IN+0.5)), OPER_IN_logsd = sd(log(OPER_IN+0.5)),
    REIMB_logmean = mean(log(REIMB+0.5)), REIMB_logsd = sd(log(REIMB+0.5)),
    CANC_logmean = mean(log(CANC+0.5)), CANC_logsd = sd(log(CANC+0.5)),
    #for DEATH calculate prevalence if it done break the >n_patients_limit rule !!
    n_person_death = dplyr::if_else(sum(DEATH)>n_patients_limit, sum(DEATH), 0L),
    .groups="drop"
  )

### remove counts under limit
observation_periods_summary <- remove_n_patients_limit(observation_periods_summary, "observation_periods_summary", logger, n_patients_limit)


### save file
log4r::info(logger, "save observation_periods_summary table ")

observation_periods_summary |> readr::write_tsv(file.path(output_folder, "observation_periods_summary_summary.tsv"))



## 2. Calculate visits_summary ------------------------------------
log4r::info(logger, "Calculate visits_summary")

visit_year_cuts <-  c(0, 1986, 1995, 2002, 2010, 2018, 2100)

count_visits_per_patient <- service_sector_data |>
  # ignore CODE5 and CODE6 for PURCH : reimbursement in euros
  # ignore CODE7 for all except PRIM_OUT: hospital-type
  dplyr::mutate(
    CODE5 = dplyr::if_else(SOURCE=="PURCH", as.character(NA), CODE5),
    CODE6 = dplyr::if_else(SOURCE=="PURCH", as.character(NA), CODE6),
    CODE7 = dplyr::if_else(SOURCE=="PRIM_OUT", CODE7, as.character(NA))
  ) |>
  # count events per patient per visit
  dplyr::count(FINNGENID, SOURCE, INDEX, CODE5, CODE6, CODE7, APPROX_EVENT_DAY) |>
  # lower APPROX_EVENT_DAY precision
  dplyr::mutate(visit_year_bin = lubridate::year(APPROX_EVENT_DAY) |> cut(visit_year_cuts) ) |>
  dplyr::group_by(visit_year_bin, SOURCE, CODE5, CODE6, CODE7) |>
  dplyr::summarise(
    n_visits = dplyr::n(),
    n_events_per_visit_logmean = mean(log(n+0.5)), n_events_per_visit_logsd = sd(log(n+0.5)),
    n_patients = length(unique(FINNGENID)),
    .groups="drop")


### remove counts under limit
count_visits_per_patient <- remove_n_patients_limit(count_visits_per_patient, "count_visits_per_patient", logger, n_patients_limit)


### save file
log4r::info(logger, "save count_visits_per_patient table ")

count_visits_per_patient |> readr::write_tsv(file.path(output_folder, "count_visits_per_patient.tsv"))



## 3. Calculate SOURCE_VOCAB_EVENT_YEAR_summary ------------------------------------
log4r::info(logger, "Calculate SOURCE_VOCAB_EVENT_YEAR_summary")

### Count codes
SOURCE_VOCAB_EVENT_YEAR_summary <- service_sector_data |>
  dplyr::mutate(event_year = lubridate::year(APPROX_EVENT_DAY)) |>
  dplyr::group_by(SOURCE, vocabulary, event_year) |>
  dplyr::summarise(n_events=dplyr::n(), n_patients=length(unique(FINNGENID)), .groups="drop")

### remove counts under limit
SOURCE_VOCAB_EVENT_YEAR_summary <- remove_n_patients_limit(SOURCE_VOCAB_EVENT_YEAR_summary, "SOURCE_VOCAB_EVENT_YEAR_summary", logger, n_patients_limit)

### save file
log4r::info(logger, "save SOURCE_VOCAB_EVENT_YEAR_summary table ")

SOURCE_VOCAB_EVENT_YEAR_summary |> readr::write_tsv(file.path(output_folder, "SOURCE_VOCAB_EVENT_YEAR_summary.tsv"))



## 4. Calculate CODE1_CODE2_CODE3_vocabulary_summary ------------------------------------
log4r::info(logger, "Calculate CODE1_CODE2_CODE3_vocabulary_summary")


### Count codes
CODE1_CODE2_CODE3_vocabulary_summary <- service_sector_data |>
  dplyr::group_by(SOURCE, CODE1, CODE2, CODE3, vocabulary) |>
  dplyr::summarise(n_events = dplyr::n(), n_patients=length(unique(FINNGENID)), .groups="drop")

### remove counts under limit
CODE1_CODE2_CODE3_vocabulary_summary <- remove_n_patients_limit(CODE1_CODE2_CODE3_vocabulary_summary, "observation_periods_summary_summary", logger, n_patients_limit)


### save file
log4r::info(logger, "save CODE1_CODE2_CODE3_vocabulary_summary table ")

CODE1_CODE2_CODE3_vocabulary_summary |> readr::write_tsv(file.path(output_folder, "CODE1_CODE2_CODE3_vocabulary_summary.tsv"))



## 5. Calculate CODE4_summary ------------------------------------
log4r::info(logger, "Calculate CODE4_summary")

### Count codes
CODE4_summary <- service_sector_data |>
  dplyr::group_by(SOURCE, CODE4) |>
  dplyr::mutate(CODE4 = cut(as.integer(CODE4), breaks=c(-Inf,-1,0,1,2,3,4,5,10,50,100,Inf))) |>
  dplyr::summarise(n_events=dplyr::n(), n_patients=length(unique(FINNGENID)), .groups="drop")

### remove counts under limit
CODE4_summary <- remove_n_patients_limit(CODE4_summary, "CODE4_summary", logger, n_patients_limit)


### save file
log4r::info(logger, "save CODE4_summary table ")

CODE4_summary |> readr::write_tsv(file.path(output_folder, "CODE4_summary.tsv"))

















