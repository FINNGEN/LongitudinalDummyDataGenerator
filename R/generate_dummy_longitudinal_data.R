



#' generate_dummy_longitudinal_data
#'
#' Produces longituinal and baseline data in folder `output_folder` for `n_patients` number of patietns
#'
#' @param output_folder directory where to output the generated data
#' @param longitudinal_data_version at the moment only DF6v2 is avalilable
#' @param n_patients number of random patients to genenrate
#' @param seed seed used in the random processes
#' @param nTreaths number of cores to use when using paraller generation (if large you can use parallel::detectCores() -1)
#'
#' @return
#' @export
#'
#'
#' @importFrom ParallelLogger createLogger createFileAppender layoutTimestamp registerLogger logInfo makeCluster clusterRequire clusterApply stopCluster unregisterLogger
#' @importFrom tibble tibble
#' @importFrom dplyr count mutate select row_number group_by bind_rows distinct left_join rename
#' @importFrom tidyr nest
#' @importFrom readr write_tsv
#' @importFrom stringr str_c
#' @importFrom scales number
#'
generate_dummy_longitudinal_data<-function(
  output_folder,
  longitudinal_data_version="DF6v2",
  n_patients=300,
  n_cuts=3,
  seed=13,
  nTreaths=2
){


  # prepare parallel loger
  logger <- ParallelLogger::createLogger(
    name = "DUMMY DATA GENERATOR",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createFileAppender(
        layout = ParallelLogger::layoutTimestamp,
        fileName = file.path(output_folder, "generate_dummy.log.txt")
      )
    )
  )
  ParallelLogger::registerLogger(logger)


  ### SPLIT parameters for parallel
  ParallelLogger::logInfo("Break ", n_patients, " into ", n_cuts, " groups")
  # calculate ranges
  if(n_cuts >1){
    par_parameters <- tibble::tibble(bin = cut(1:n_patients, n_cuts)) %>% dplyr::count(bin, name = "n_patients") %>%
      dplyr::mutate(n_patients_offset=cumsum(n_patients)-.$n_patients[1]) %>% dplyr::select(-bin) %>%
      dplyr::mutate(seed = dplyr::row_number()+seed-1)

    par_parameters <- par_parameters %>%  dplyr::mutate(i=dplyr::row_number()) %>%  dplyr::group_by(i) %>% tidyr::nest() %>% .$data
  }else{
    par_parameters<-list(list(
      n_patients = n_patients,
      n_patients_offset = 0,
      seed = seed
    ))
  }


  ### RUN parallel
  ParallelLogger::logInfo("Run into ", nTreaths, " treaths")
  cluster <- ParallelLogger::makeCluster(numberOfThreads = nTreaths)
  #
  ParallelLogger::clusterRequire(cluster, "tidyverse")
  ParallelLogger::clusterRequire(cluster, "lubridate")
  ParallelLogger::clusterRequire(cluster, "scales")
  #
  res <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = par_parameters,
    longitudinal_data_version = longitudinal_data_version,
    fun = .generate_dummy_longitudinal_data
  )

  ### JOIN restuls from parallel
  ParallelLogger::logInfo("Join resuls")
  res <- res %>% dplyr::bind_rows()
  longitudinal_data <- res$longitudinal_data %>% dplyr::bind_rows()
  baseline_data <- res$baseline_data %>% dplyr::bind_rows()

  # doing parallel some indexes repeat for different patient: re calculate index
  unique_index <- longitudinal_data %>% dplyr::distinct(FINNGENID, INDEX) %>% dplyr::mutate(i=dplyr::row_number())
  longitudinal_data <- dplyr::left_join(
    longitudinal_data,
    unique_index,
    by = c("FINNGENID", "INDEX")
  ) %>% dplyr::select(-INDEX) %>% dplyr::rename(INDEX=i)

  ## SAVE
  ParallelLogger::logInfo("Save longitudinal_data")
  longitudinal_data %>%
    readr::write_tsv(file.path(output_folder, stringr::str_c("longitudinal_dummy_data_", scales::number(n_patients, scale = 0.001, suffix = "k"), "_", seed, ".tsv" )))
  ParallelLogger::logInfo("Saved longitudinal_data")


  ParallelLogger::logInfo("Save baseline_data")
  baseline_data %>%
    readr::write_tsv(file.path(output_folder, stringr::str_c("baseline_dummy_data_", scales::number(n_patients, scale = 0.001, suffix = "k"), "_", seed, ".tsv" )))
  ParallelLogger::logInfo("Save baseline_data")


  ParallelLogger::stopCluster(cluster)
  ParallelLogger::unregisterLogger(logger)

 ## return(longitudinal_data)

}




#' @importFrom ParallelLogger logInfo
#' @importFrom dplyr mutate select starts_with filter left_join transmute group_by ungroup distinct if_else row_number bind_rows count group_modify rename case_when arrange
#' @importFrom tidyr expand_grid nest unnest gather
#' @importFrom tibble tibble
#' @importFrom stringr str_c str_pad
#' @importFrom purrr pmap_dbl map2 pmap map_int map
#' @importFrom scales number
#' @importFrom lubridate interval
.generate_dummy_longitudinal_data<-function(
  par_parameters,
  longitudinal_data_version
){

  n_patients <- par_parameters$n_patients
  n_patients_offset <- par_parameters$n_patients_offset
  seed <- par_parameters$seed

  #set seed
  set.seed(seed)
  # load data
  data("summary_data_versions_list", package = "LongitudinalDummyDataGenerator")

  summary_tables <- summary_data_versions_list[[longitudinal_data_version]]

  ###
  ### Init info
  ###

  ParallelLogger::logInfo("Start generation of ", n_patients, " patients, using seed ", seed )

  ###
  ### Generate patients' observation periods and number of events ###
  ###
  # Generates n_patients` patients statistically similar to the data in `count_periods.tsv`.
  # Each column is a patient with observation period from `start_year` to `end_year`;
  # age at start `start_age`;
  # and number of events of each SOURCE type during the observation period `<SOURCE>`.


  # Calculate probability of observation types and Expand observation periods
  probabilities_periods <- summary_tables$count_periods %>%
    dplyr::mutate(per_patients = n_patients / sum(n_patients)) %>%
    dplyr::select(-n_patients) %>%
    # expand 5 year groups to 1 years
    tidyr::expand_grid(range1=0:4) %>% dplyr::mutate(start_year = start_year + range1, per_patients= per_patients/5) %>%
    tidyr::expand_grid(range2=0:4) %>% dplyr::mutate(start_age = start_age + range2, per_patients= per_patients/5) %>%
    tidyr::expand_grid(range3=0:4) %>% dplyr::mutate(observation_years = observation_years + range3, per_patients= per_patients/5) %>%
    dplyr::select(-dplyr::starts_with("range")) %>%
    # remove if end_year is over 2020
    dplyr::filter(start_year+observation_years<2020) %>%
    #
    dplyr::mutate(n_row = 1:nrow(.))


  ParallelLogger::logInfo("Generate cohort_data and number of events")
  #
  sampled_patients <- tibble::tibble(
    FINNGENID = stringr::str_c("FG", stringr::str_pad((1:n_patients)+n_patients_offset, width = 8, pad = "0")),
    n_row = sample(probabilities_periods$n_row, size = n_patients, replace = TRUE, prob = probabilities_periods$per_patients)
  ) %>%
    dplyr::left_join(probabilities_periods, by = "n_row") %>%
    # replace logmean logsd with random number of events
    dplyr::transmute(
      FINNGENID = FINNGENID,
      start_year = start_year,
      start_age = start_age,
      end_year = start_year+observation_years,
      INPAT = floor(purrr::pmap_dbl(.l=list(1, INPAT_logmean, INPAT_logsd), .f=rlnorm)),
      PURCH = floor(purrr::pmap_dbl(.l=list(1, PURCH_logmean, PURCH_logsd), .f=rlnorm)),
      OUTPAT = floor(purrr::pmap_dbl(.l=list(1, OUTPAT_logmean, OUTPAT_logsd), .f=rlnorm)),
      OPER_IN = floor(purrr::pmap_dbl(.l=list(1, OPER_IN_logmean, OPER_IN_logsd), .f=rlnorm)),
      OPER_OUT = floor(purrr::pmap_dbl(.l=list(1, OPER_OUT_logmean, OPER_OUT_logsd), .f=rlnorm)),
      PRIM_OUT = floor(purrr::pmap_dbl(.l=list(1, PRIM_OUT_logmean, PRIM_OUT_logsd), .f=rlnorm)),
      REIMB = floor(purrr::pmap_dbl(.l=list(1, REIMB_logmean, REIMB_logsd), .f=rlnorm)),
      CANC = floor(purrr::pmap_dbl(.l=list(1, CANC_logmean, CANC_logsd), .f=rlnorm)),
      DEATH = floor(purrr::pmap_dbl(.l=list(1, CANC_logmean, CANC_logsd), .f=rlnorm))
    ) %>%
    # calculate bday
    dplyr::mutate(birth_year = start_year-start_age) %>%
    dplyr::group_by(birth_year) %>% tidyr::nest() %>%
    dplyr::mutate(data = purrr::map2(.x=data, .y=birth_year, .f=~{
      .x %>% dplyr::mutate(birth_date = sample(
        seq(as.Date(stringr::str_c(.y,'/01/01')), as.Date(stringr::str_c(.y,'/12/31')), by="day"),
        nrow(.x),
        replace = TRUE)
      )
    })) %>% tidyr::unnest(data) %>%  dplyr::ungroup() %>%
    dplyr::select(-birth_year)

  ParallelLogger::logInfo("Created cohort_data with ", scales::number(sampled_patients %>% dplyr::distinct(FINNGENID) %>% nrow()), " patients")

  ###
  ### Generate events for each patient
  ###
  # Generate SOURCE, EVENT_YEAR and VOCAB
  #
  # Make a row for each event in each patient in `sampled_patients`.
  # Give to each event a weighted random EVENT_YEAR and VOCAB based on `event_year_count.tsv`
  #
  # (This is the most time consuming part, e.g. for 1000 patients 85sec)


  #  Calculate probability of EVENT_YEAR gruped by  SOURCE and VOCAB
  event_year_probabilities <- summary_tables$event_year_count %>%
    dplyr::group_by(SOURCE, VOCAB) %>%
    dplyr::mutate(per_events = n_events/sum(n_events)) %>%
    dplyr::select(-n_events, -n_patients) %>%
    dplyr::ungroup() %>%
    #
    dplyr::mutate(n_row = 1:nrow(.))

  ParallelLogger::logInfo("Create longitudinal_data from random cohort_data with columns SOURCE, EVENT_YEAR and VOCAB ")
  #
  sampled_events <- sampled_patients %>%
    tidyr::gather("SOURCE", "n_events", 5:13) %>%
    #
    dplyr::mutate( events = purrr::pmap(.l=list(SOURCE, start_year, end_year, n_events), .f=~{
      event_year_probabilities %>%
        dplyr::filter(SOURCE==..1 & EVENT_YEAR>=..2 & EVENT_YEAR<=..3) %>%
        .sample_probability_tibble("per_events", ..4) %>%
        dplyr::select(VOCAB, EVENT_YEAR )
    })) %>%
    #
    tidyr::unnest(events) %>%
    # correct EVENT_YEAR for DEATH events, all death event to last_year
    dplyr::mutate(
      EVENT_YEAR = dplyr::if_else(SOURCE=="DEATH", end_year, EVENT_YEAR),
      VOCAB = dplyr::if_else(SOURCE=="DEATH" & EVENT_YEAR > 1996, "10", VOCAB)
    ) %>%
    #
    dplyr::select(-n_events, -start_age, -end_year, -start_year, -birth_date)

  ParallelLogger::logInfo("Created longitudinal_data with ", scales::number(sampled_events %>% nrow()), " events")


  ###
  ### Append codes to each event
  ###
  # Give to each event a weighted random CODE1, CODE2, CODE3 combo based on SOURCE, VOCAB and EVENT_YEAR, using  `code_count.tsv`.

  # Calculate probability of CODEx gruped by  SOURCE and VOCAB
  codes_probabilities <- summary_tables$codes_count %>%
    dplyr::group_by(SOURCE, VOCAB) %>%
    dplyr::mutate(per_events = n_events/sum(n_events)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n_events, -n_patients)

  ParallelLogger::logInfo("Append to longitudinal_data colums CODE1, CODE2, CODE3 combo based on SOURCE, VOCAB and EVENT_YEAR ")
  #
  sampled_events <- sampled_events  %>%
    #
    dplyr::group_by(SOURCE, VOCAB) %>% tidyr::nest() %>%
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) %>%
    #
    dplyr::mutate( codes = purrr::pmap(.l=list(SOURCE, VOCAB, n_events), .f=~{
      codes_probabilities %>%
        dplyr::filter(SOURCE==..1 & VOCAB==..2) %>%
        .sample_probability_tibble("per_events", ..3) %>%
        dplyr::select(CODE1, CODE2, CODE3 )
    })) %>%
    dplyr::mutate(data = purrr::map2(data, codes, bind_cols)) %>%
    dplyr::select(-n_events, -codes) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup()
  #
  ParallelLogger::logInfo("Appened CODE1, CODE2, CODE3 to longitudinal_data ", scales::number(sampled_events %>% nrow()), " events")

  ###
  ### Append CODE4 to each event
  ###
  # Give to each event a weighted random CODE4 based on SOURCE, VOCAB and EVENT_YEAR, using  `code_count.tsv`.

  # Calculate probability of CODEx gruped by  SOURCE
  code4_probabilities <- summary_tables$code4_count %>% #filter(!is.na(CODE4)) %>%
    dplyr::mutate(
      n_events_nested = purrr::map2(.x=CODE4, .y=n_events, .f=.bins_to_tibble
      )
    ) %>% tidyr::unnest(n_events_nested) %>%
    dplyr::group_by(SOURCE) %>%
    dplyr::mutate(
      CODE4 = code,
      n_events = ne,
      per_events = n_events/sum(n_events)
    ) %>%
    dplyr::select(SOURCE, CODE4, per_events)  %>%
    dplyr::ungroup()

  ParallelLogger::logInfo("Append to longitudinal_data colums CODE4 based on SOURCE, VOCAB and EVENT_YEAR")
  #
  sampled_events <- sampled_events  %>%
    #
    dplyr::group_by(SOURCE) %>% tidyr::nest() %>%
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) %>%
    #
    dplyr::mutate( code4 = purrr::pmap(.l=list(SOURCE, n_events), .f=~{
      code4_probabilities %>%
        dplyr::filter(SOURCE==..1) %>%
        .sample_probability_tibble("per_events", ..2) %>%
        dplyr::select(CODE4 )
    })) %>%
    dplyr::mutate(data = purrr::map2(data, code4, bind_cols)) %>%
    dplyr::select(-n_events, -code4) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup()
  #
  ParallelLogger::logInfo("Appened CODE4 to longitudinal_data ", scales::number(sampled_events %>% nrow()), " events")

  ###
  ### Append LEVEL to each event
  ###
  # Give to each event a weighted random LEVEL based on SOURCE, using  `level_count.tsv`.

  level_probabilities <- summary_tables$level_count %>% #filter(!is.na(CODE4)) %>%
    dplyr::mutate(
      n_events_nested = purrr::map2(.x=LEVEL, .y=n_events, .f=.bins_to_tibble
      )
    ) %>% tidyr::unnest(n_events_nested) %>%
    dplyr::group_by(SOURCE) %>%
    dplyr::mutate(
      LEVEL = code,
      n_events = ne,
      per_events = n_events/sum(n_events)
    ) %>%
    dplyr::select(SOURCE, LEVEL, per_events) %>% dplyr::ungroup()

  ParallelLogger::logInfo("Append to longitudinal_data colums LEVEL based on SOURCE")
  #
  sampled_events <- sampled_events  %>%
    #
    dplyr::group_by(SOURCE) %>% tidyr::nest() %>%
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) %>%
    #
    dplyr::mutate( level = purrr::pmap(.l=list(SOURCE, n_events), .f=~{
      level_probabilities %>%
        dplyr::filter(SOURCE==..1) %>%
        .sample_probability_tibble("per_events", ..2) %>%
        dplyr::select(LEVEL)
    })) %>%
    dplyr::mutate(data = purrr::map2(data, level, bind_cols)) %>%
    dplyr::select(-n_events, -level) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup()
  #
  ParallelLogger::logInfo("Append to longitudinal_data colums LEVEL", scales::number(sampled_events %>% nrow()), " events")

  ###
  ### Calculate INDEX
  ###
  # If same SOURCE, FINNGENID, VOCAB, EVENT_YEAR and district LEVEL, assign a unique INDEX

  # this speeds up processing considerably
  level_to_index <- function(level){
    buffer <- rep(0,max(level))
    for(i in 1:length(level)){
      buffer[level[i]] = buffer[level[i]]+1
      level[i] = buffer[level[i]]
    }
    return(level)
    #return(level)
  }
  # a<-sample(1:5, 10, replace = T)
  # a
  # level_to_index(a)

  ParallelLogger::logInfo("If same SOURCE, FINNGENID, VOCAB, EVENT_YEAR and district LEVEL, assign a unique INDEX")
  #

  # following sources dont have a level, index is a number
  sampled_events_1index <- sampled_events %>%
    dplyr::filter(SOURCE %in% c("PURCH", "REIMB", "CANC")) %>%
    dplyr::group_by(SOURCE) %>%
    dplyr::mutate(INDEX=dplyr::row_number()) %>%
    dplyr::ungroup()
  # for others: calculate same index for distinct LEVELS with in same SOURCE, FINNGENID, VOCAB, EVENT_YEAR
  sampled_events_Nindex <- sampled_events %>%
    dplyr::filter(!(SOURCE %in% c("PURCH", "REIMB", "CANC"))) %>%
    dplyr::group_by(SOURCE, FINNGENID, VOCAB, EVENT_YEAR) %>% tidyr::nest() %>% dplyr::ungroup() %>% # sample_n(100) %>%
    dplyr::mutate(
      rn=1:nrow(.),
      data = purrr::map(.x=data, .f=~{
        .x %>% dplyr::mutate(INDEX=level_to_index(LEVEL+1))
      })) %>%
    tidyr::unnest(data) %>%
    # offset of 10000 fir the row number, lower values for the unique index with in group
    dplyr::mutate(INDEX = INDEX+rn*10000) %>% dplyr::select(-rn)

  sampled_events <- dplyr::bind_rows(
    sampled_events_1index,
    sampled_events_Nindex %>% dplyr::mutate(INDEX = INDEX+nrow(sampled_events_1index))
  )
  #
  ParallelLogger::logInfo("INDEX recalucalted for the longitudinal_data ", scales::number(sampled_events %>% nrow()), " events")


  # **TEST:** # If there is a visit/INDEX with many evetns, something may be wrong:
  #ParallelLogger::logInfo("TEST:  If there is a visit/INDEX with many evetns, something may be wrong", sampled_events %>%  dplyr::count(INDEX, sort = T) %>%  head(10) %>% print())

  ###
  ### Fix death events
  ###
  # As death events are calculated like the other type of events, it may be subjects with more than one death.
  # Join death events and recalculate LEVEL with sample with out replacement.

  ParallelLogger::logInfo("Fix death events")
  #
  sample_death_level <- function(level){
    # Maximun 5 values
    l_level = length(level)
    if(l_level>5){n_samples=5}else{n_samples=l_level}
    # sample without replacemtn
    prob <- level_probabilities %>% dplyr::filter(SOURCE=="DEATH")
    level <- sample(prob$LEVEL, size = n_samples, replace = FALSE, prob = prob$per_events)
    # if more than 5 samples add NA
    level <- c(level, rep(NA,l_level-n_samples))
    return(level)
  }

  # find FINNGENID with more than one DEATH
  overdeath_finngenids <- sampled_events %>% dplyr::filter(SOURCE=="DEATH") %>%
    dplyr::distinct(FINNGENID, INDEX) %>%  dplyr::count(FINNGENID, sort = T) %>%
    dplyr::filter(n>1) %>%  .$FINNGENID

  sampled_events <- dplyr::bind_rows(
    sampled_events %>% dplyr::filter(!(SOURCE=="DEATH" & (FINNGENID %in% overdeath_finngenids))),
    sampled_events %>% dplyr::filter( (SOURCE=="DEATH" & (FINNGENID %in% overdeath_finngenids))) %>%
      dplyr::group_by(FINNGENID) %>%
      dplyr::group_modify(.f=~{.x %>% dplyr::mutate(LEVEL = sample_death_level(LEVEL), INDEX=min(INDEX))})
  )
  #
  ParallelLogger::logInfo("Fixed ", scales::number(length(overdeath_finngenids))," death events")

  ###
  ### Recalcualte CODE4 for data hospitalisation
  ###
  # At this point, event with in the same visit (INDEX) may have different HOPITALDAYS.
  # Re assign HOPITALDAYS to index.

  ParallelLogger::logInfo("Recalcualte CODE4 for data hospitalisation")
  #
  inpat_outpat_hopitaldays <- sampled_events %>% dplyr::filter(SOURCE %in% c("INPAT", "OUTPAT")) %>% dplyr::distinct(SOURCE, INDEX) %>%
    #
    dplyr::group_by(SOURCE) %>% tidyr::nest() %>%
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) %>%
    #
    dplyr::mutate( code4 = purrr::pmap(.l=list(SOURCE, n_events), .f=~{
      code4_probabilities %>%
        dplyr::filter(SOURCE==..1) %>%
        .sample_probability_tibble("per_events", ..2) %>%
        dplyr::select(CODE4 )
    })) %>%
    dplyr::mutate(data = purrr::map2(data, code4, bind_cols)) %>%
    dplyr::select(-n_events, -code4) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup()

  sampled_events <- dplyr::left_join(
    sampled_events,
    inpat_outpat_hopitaldays %>% dplyr::rename(CODE4b = CODE4),
    by=c("SOURCE", "INDEX")
  ) %>% dplyr::mutate(CODE4 = dplyr::if_else(SOURCE %in% c("INPAT", "OUTPAT"), CODE4b, CODE4)) %>% dplyr::select(-CODE4b)
  #
  ParallelLogger::logInfo("Recalcualteed CODE4 for data hospitalisation")

  ###
  ### Reformat data as in source files
  ###
  ## longitudinal data

  ParallelLogger::logInfo("Reformat longitudinal data: rename death LEVELS; Re build CATEGORY and IDCVER; ")
  #
  sampled_events <-  sampled_events %>%
    # rename death LEVELS
    dplyr::mutate(LEVEL=dplyr::case_when(
      SOURCE=="DEATH" & LEVEL=="1"~"c1",
      SOURCE=="DEATH" & LEVEL=="2"~"c2",
      SOURCE=="DEATH" & LEVEL=="3"~"c3",
      SOURCE=="DEATH" & LEVEL=="4"~"c4",
      SOURCE=="DEATH" & LEVEL=="5"~"I",
      SOURCE=="DEATH" & LEVEL=="0"~"U",
      TRUE ~ as.character(LEVEL)
    )) %>%
    # Re build CATEGORY and IDCVER
    dplyr::mutate(ICDVER=dplyr::case_when(
      SOURCE %in% c("CANC", "DEATH", "INPAT", "OUTPAT", "REIMB") ~ VOCAB,
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::mutate(CATEGORY=dplyr::case_when(
      SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT") ~ stringr::str_c(VOCAB, LEVEL),
      TRUE ~ LEVEL
    ))


  # replace EVENT_YEAR  with APPREX_EVENT_DAYS. It has to be calculated for EVENT_YEAR within visit (same INDEX, SOURCE )
  ParallelLogger::logInfo("Reformat longitudinal data: replace EVENT_YEAR  with APPREX_EVENT_DAYS ")
  #
  distinct_event_years <-  sampled_events %>%
    dplyr::distinct(SOURCE, INDEX, EVENT_YEAR) %>%
    dplyr::group_by(EVENT_YEAR) %>% tidyr::nest() %>%
    dplyr::mutate(data = purrr::map2(.x=data, .y=EVENT_YEAR, .f=~{
      .x %>% dplyr::mutate(APPROX_EVENT_DAY = sample(
        seq(as.Date(stringr::str_c(.y,'/01/01')), as.Date(stringr::str_c(.y,'/12/31')), by="day"),
        nrow(.x),
        replace = TRUE)
      )
    })) %>% tidyr::unnest(data) %>%  dplyr::ungroup()

  sampled_events <- dplyr::left_join(
    sampled_events,
    distinct_event_years,
    by=c("EVENT_YEAR", "SOURCE","INDEX")
  )

  # calcualte EVENT_AGE
  ParallelLogger::logInfo("Reformat longitudinal data: calcualte EVENT_AGE ")
  #
  sampled_events <-  sampled_events %>%
    dplyr::left_join(sampled_patients %>% dplyr::select(FINNGENID, birth_date), by="FINNGENID") %>%
    dplyr::mutate(EVENT_AGE = as.numeric(lubridate::interval(birth_date, APPROX_EVENT_DAY),"years")) %>%
    dplyr::mutate(EVENT_AGE = round(EVENT_AGE, digits = 2)) %>%
    #
    dplyr::select(FINNGENID, SOURCE, EVENT_AGE, APPROX_EVENT_DAY, CODE1, CODE2, CODE3, CODE4, ICDVER, CATEGORY, INDEX)
  #
  ParallelLogger::logInfo("Reformated longitudinal data: ", scales::number(length(sampled_events))," events")

  ## baseline data
  ParallelLogger::logInfo("Reformat cohort_data: calcualte SEX ")
  #
  SEX_probabilities <- tibble::tibble(
    SEX = c("female", "male", as.character(NA)),
    per_sex = c(0.565, 0.432, 0.000332)
  )

  baseline_data <- sampled_patients %>%
    dplyr::transmute(
      FINNGENID = FINNGENID,
      SEX = sample(SEX_probabilities$SEX, nrow(.), replace = T, prob = SEX_probabilities$per_sex)
    )

  return(tibble::tibble(
    longitudinal_data = list(sampled_events %>% dplyr::arrange(FINNGENID, EVENT_AGE)),
    baseline_data = list(baseline_data %>% dplyr::arrange(FINNGENID))
  ))

}



#' @importFrom tibble tibble
.bins_to_tibble <- function(bin_lable, n_events){
  out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(NA))
  if(is.na(bin_lable)){return(out)}
  #
  if(bin_lable == "(-Inf,-1]"){ out <- out <- tibble::tibble(ne=as.integer(round(n_events/10)), code=as.integer(-10:-1))}
  #
  if(bin_lable == "(-1,0]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(0))}
  if(bin_lable ==  "(0,1]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(1))}
  if(bin_lable ==  "(1,2]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(2))}
  if(bin_lable ==  "(2,3]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(3))}
  if(bin_lable ==  "(3,4]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(4))}
  if(bin_lable ==  "(4,5]"){ out <- tibble::tibble(ne=as.integer(n_events), code=as.integer(5))}
  #
  if(bin_lable == "(5,10]"){ out <- tibble::tibble(ne=as.integer(round(n_events/5)), code=as.integer(6:10))}
  if(bin_lable == "(10,50]"){ out <- tibble::tibble(ne=as.integer(round(n_events/10)), code=as.integer(11:50))}
  if(bin_lable == "(50,100]"){ out <- tibble::tibble(ne=as.integer(round(n_events/50)), code=as.integer(51:100))}
  if(bin_lable == "(100, Inf]"){ out <- tibble::tibble(ne=as.integer(round(n_events/100)), code=as.integer(101:200))}
  # CATEGORY only
  if(bin_lable == "(10, Inf]"){ out <- tibble::tibble(ne=as.integer(round(19)), code=as.integer(11:29))}
  return(out)
}


#' @importFrom dplyr enquo mutate
.uncount_prob <- function(tb, probabilities_table, col_name ){
  col_name <- dplyr::enquo(col_name)
  a <- probabilities_table
  b <- sample(a$code, size = nrow(tb), replace = TRUE, prob = a$per_events)
  return(tb %>% dplyr::mutate(!!col_name:=b))
}

#' @importFrom dplyr slice
.sample_probability_tibble <- function(data, per_column, n_samples){
  ret <- data %>% dplyr::slice(0)
  if(nrow(data)!=0){
    ix <- sample(1:nrow(data), size = n_samples, replace = TRUE, prob = data[[per_column]])
    ret <- data %>% dplyr::slice(ix)
  }
  return(ret)
}



