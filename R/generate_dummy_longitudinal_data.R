



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
    longitudinal_data_version="R10v1",
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
    par_parameters <- tibble::tibble(bin = cut(1:n_patients, n_cuts)) |>
      dplyr::count(bin, name = "n_patients") |>
      dplyr::mutate(n_patients_offset = slider::slide_int(n_patients, sum, .before = Inf)) |>
      dplyr::select(-bin) |>
      dplyr::mutate(seed = dplyr::row_number()+seed-1)

    par_parameters <- par_parameters |>
      dplyr::mutate(i=dplyr::row_number()) |>
      dplyr::group_by(i) |>
      tidyr::nest() |> dplyr::pull(data)
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
  res <- res |> dplyr::bind_rows()
  longitudinal_data <- res$longitudinal_data |> dplyr::bind_rows()
  baseline_data <- res$baseline_data |> dplyr::bind_rows()

  # doing parallel some indexes repeat for different patient: re calculate index
  unique_index <- longitudinal_data |> dplyr::distinct(FINNGENID, INDEX) |> dplyr::mutate(i=dplyr::row_number())
  longitudinal_data <- dplyr::left_join(
    longitudinal_data,
    unique_index,
    by = c("FINNGENID", "INDEX")
  ) |> dplyr::select(-INDEX) |> dplyr::rename(INDEX=i)

  ## SAVE
  ParallelLogger::logInfo("Save longitudinal_data")
  longitudinal_data |>
    readr::write_tsv(file.path(output_folder, stringr::str_c("longitudinal_dummy_data_", scales::number(n_patients, scale = 0.001, suffix = "k"), "_", seed, ".tsv" )))
  ParallelLogger::logInfo("Saved longitudinal_data")


  ParallelLogger::logInfo("Save baseline_data")
  baseline_data |>
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
    par_parameters = list(
      n_patients = 30,
      n_patients_offset = 0,
      seed = 13
    ),
    longitudinal_data_version = "R10v2"
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
  ### Generate n_patients patients with observation periods and number of visits per SOURCE type ###
  ###

  # Calculate probability of observation types and Expand observation periods
  observation_periods_probabilities <- summary_tables$service_sector$observation_periods |>
    dplyr::mutate(
      per_patients = n_patients / sum(n_patients),
      per_death = n_person_death / n_patients
      ) |>
    dplyr::select(-n_patients, -n_person_death) |>
    # expand 5 year groups to 1 years
    tidyr::expand_grid(range1=0:4) |> dplyr::mutate(first_visit_year = first_visit_year + range1, per_patients= per_patients/5) |>
    tidyr::expand_grid(range2=0:4) |> dplyr::mutate(first_visit_age = first_visit_age + range2, per_patients= per_patients/5) |>
    tidyr::expand_grid(range3=0:4) |> dplyr::mutate(years_to_last_visit = years_to_last_visit + range3, per_patients= per_patients/5) |>
    dplyr::select(-dplyr::starts_with("range")) |>
    #
    dplyr::mutate(n_row = dplyr::row_number())


  ParallelLogger::logInfo("Generate observation periods and number of visits")

  # create n_patients with finngenid, random observation periods, and number of visits per SOURCE
  sampled_patients <- tibble::tibble(
    FINNGENID = stringr::str_c("FG", stringr::str_pad((1:n_patients)+n_patients_offset, width = 8, pad = "0")),
    n_row = sample(observation_periods_probabilities$n_row, size = n_patients, replace = TRUE, prob = observation_periods_probabilities$per_patients)
  ) |>
    dplyr::left_join(observation_periods_probabilities, by = "n_row") |>
    # replace logmean logsd with random number of events
    dplyr::transmute(
      FINNGENID = FINNGENID,
      first_visit_year = first_visit_year,
      first_visit_age = first_visit_age,
      last_visit_year = first_visit_year+years_to_last_visit,
      INPAT = floor(purrr::pmap_dbl(.l=list(1, INPAT_logmean, INPAT_logsd), .f=rlnorm)),
      PURCH = floor(purrr::pmap_dbl(.l=list(1, PURCH_logmean, PURCH_logsd), .f=rlnorm)),
      OUTPAT = floor(purrr::pmap_dbl(.l=list(1, OUTPAT_logmean, OUTPAT_logsd), .f=rlnorm)),
      OPER_IN = floor(purrr::pmap_dbl(.l=list(1, OPER_IN_logmean, OPER_IN_logsd), .f=rlnorm)),
      OPER_OUT = floor(purrr::pmap_dbl(.l=list(1, OPER_OUT_logmean, OPER_OUT_logsd), .f=rlnorm)),
      PRIM_OUT = floor(purrr::pmap_dbl(.l=list(1, PRIM_OUT_logmean, PRIM_OUT_logsd), .f=rlnorm)),
      REIMB = floor(purrr::pmap_dbl(.l=list(1, REIMB_logmean, REIMB_logsd), .f=rlnorm)),
      CANC = floor(purrr::pmap_dbl(.l=list(1, CANC_logmean, CANC_logsd), .f=rlnorm)),
      # mark if is death at the end ob observation based on death probability
      is_death = rbinom(dplyr::n(),1,per_death) == 1
    ) |>
    # calculate bday
    dplyr::mutate(birth_year = first_visit_year-first_visit_age) |>
    dplyr::group_by(birth_year) |> tidyr::nest() |>
    dplyr::mutate(data = purrr::map2(.x=data, .y=birth_year, .f=~{
      .x |> dplyr::mutate(birth_date = sample(
        seq(as.Date(stringr::str_c(.y,'/01/01')), as.Date(stringr::str_c(.y,'/12/31')), by="day"),
        nrow(.x),
        replace = TRUE)
      )
    })) |> tidyr::unnest(data) |>  dplyr::ungroup() |>
    dplyr::select(-birth_year)

  ParallelLogger::logInfo("Created observation periods with ", scales::number(sampled_patients |> dplyr::distinct(FINNGENID) |> nrow()), " patients")


  ###
  ### Generate visits for each patient
  ###
  ParallelLogger::logInfo("Generate visits for each patient")

  # provability of visit type + visit_year_bin in visit per SOURCE
  visit_type_probabilities <- summary_tables$service_sector$count_visits_per_patient |>
    dplyr:: mutate(
      n_events = floor(purrr::pmap_dbl(.l=list(1, n_events_per_visit_logmean , n_events_per_visit_logsd ), .f=rlnorm)),
      n_events= dplyr::if_else(n_events==0, 1, n_events),
      visit_year_bin_low = visit_year_bin |> stringr::str_extract("^\\([:digit:]+") |> stringr::str_remove("\\(") |>  as.integer() ,
      visit_year_bin_low = visit_year_bin_low +1,
      visit_year_bin_high = visit_year_bin |> stringr::str_extract("[:digit:]+\\]$") |> stringr::str_remove("\\]")  |>  as.integer()
    ) |>
    dplyr::group_by(SOURCE ) |>
    dplyr::mutate(per_visits = n_visits /sum(n_visits)) |>
    dplyr::ungroup() |>
    dplyr::select(-n_visits, -n_patients, -n_events_per_visit_logmean, -n_events_per_visit_logsd, -visit_year_bin) |>
    dplyr::mutate(n_row = dplyr::row_number())


  # for each patient and SOURCE generates n_visits with a n_events and visits year bin
  # adds an INDEX
  sampled_visits <- sampled_patients |>
    tidyr::gather("SOURCE", "n_visits", 5:12) |>
    #
    dplyr::mutate(
      events = purrr::pmap(.l=list(SOURCE, first_visit_year , last_visit_year , n_visits), .f=~{
        visit_type_probabilities |>
          dplyr::filter(SOURCE==..1 & visit_year_bin_low >=..2 & visit_year_bin_high <=..3) |>
          .sample_probability_tibble("per_visits", ..4) |>
          select(CODE5, CODE6, CODE7, n_events, visit_year_bin_low, visit_year_bin_high)
      })) |>
    #
    tidyr::unnest(events) |>
    dplyr::select(-n_visits, -first_visit_year, -last_visit_year, -birth_date, -first_visit_age ) |>
    # add INDEX
    dplyr::group_by(SOURCE) |>
    dplyr::mutate(INDEX = row_number()) |>
    dplyr::ungroup()


  ###
  ### Generate events with a vocabulary for each visit
  ###
  ParallelLogger::logInfo("Generate events for each visit")

  # provability of vocabulary per source type and year
  vocabulary_type_probabilities <- summary_tables$service_sector$SOURCE_VOCAB_EVENT_YEAR |>
    dplyr::group_by(SOURCE, vocabulary) |>
    dplyr::mutate(per_events = n_events/sum(n_events)) |>
    dplyr::select(-n_events, -n_patients) |>
    dplyr::ungroup() |>
    dplyr::mutate(n_row = dplyr::row_number())

  # for each visit generated n_events with valid vocabulary in that time period
  sampled_events <- sampled_visits |>
    dplyr::group_by(FINNGENID, SOURCE, visit_year_bin_low, visit_year_bin_high) |>
    dplyr::mutate(n_total_events = sum(n_events)) |>
    dplyr::group_by(FINNGENID, SOURCE, visit_year_bin_low, visit_year_bin_high, n_total_events) |>
    tidyr::nest() |>
    # For each visit calculate vocabulary for n_events based on SOURCE, and visit window
    dplyr::mutate(
      events = purrr::pmap(.l=list(SOURCE, visit_year_bin_low  , visit_year_bin_high , n_total_events ), .f=~{
        vocabulary_type_probabilities |>
          dplyr::filter(SOURCE==..1 & event_year  >=..2 & event_year  <=..3) |>
          .sample_probability_tibble("per_events", ..4) |>
          select(vocabulary, event_year)
      })) |>
    dplyr::ungroup() |>
    # join events and visit info
    dplyr::mutate(data = map2(.x=data, .y=events, ~{
      n = nrow(.x)
      dplyr::bind_cols(
        .x |> dplyr::sample_n(n)|> tidyr::uncount(n_events),
        .y |> dplyr::arrange(event_year)
      )
    })) |>
    tidyr::unnest(data) |>
    dplyr::select(-visit_year_bin_low, -visit_year_bin_high, -n_total_events, -events) |>
    # adjust the event_year to be the same for all the events in the same visit
    dplyr::group_by(SOURCE, INDEX) |>
    dplyr::mutate(event_year = mean(event_year) |>  round()) |>
    dplyr::ungroup() |>
    # calculate level
    dplyr::group_by(SOURCE, INDEX, vocabulary) |>
    dplyr::mutate(level = row_number()) |>
    dplyr::ungroup()


  ###
  ### Append codes to each event based on vocabulary
  ###
 ParallelLogger::logInfo("Append codes to each event based on vocabulary ")

  # Calculate probability of CODEx gruped by  SOURCE and VOCAB
  codes_probabilities <- summary_tables$service_sector$CODE1_CODE2_CODE3_vocabulary |>
    dplyr::group_by(SOURCE, vocabulary) |>
    dplyr::mutate(per_events = n_events/sum(n_events)) |>
    dplyr::ungroup() |>
    dplyr::select(-n_events, -n_patients)


  # Give to each event a weighted random CODE1, CODE2, CODE3 combo based on SOURCE, VOCAB and EVENT_YEAR, using  `code_count.tsv`.
  sampled_codes <- sampled_events  |>
    #
    dplyr::group_by(SOURCE, vocabulary) |>
    tidyr::nest() |>
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) |>
    #
    dplyr::mutate( codes = purrr::pmap(.l=list(SOURCE, vocabulary, n_events), .f=~{
      codes_probabilities |>
        dplyr::filter(SOURCE==..1 & vocabulary==..2) |>
        .sample_probability_tibble("per_events", ..3) |>
        dplyr::select(CODE1, CODE2, CODE3 )
    })) |>
    dplyr::mutate(data = purrr::map2(data, codes, dplyr::bind_cols)) |>
    dplyr::select(-n_events, -codes) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()
  #
  ParallelLogger::logInfo("Appened CODE1, CODE2, CODE3 to longitudinal_data ", scales::number(sampled_events |> nrow()), " events")

  ###
  ### Append CODE4 to each event
  ###
  ParallelLogger::logInfo("Append CODE4 to each event")

   # Calculate probability of CODE4 gruped by  SOURCE
  code4_probabilities <- summary_tables$service_sector$CODE4 |>
    dplyr::mutate(
      n_events_nested = purrr::map2(.x=CODE4, .y=n_events, .f=.bins_to_tibble
      )
    ) |> tidyr::unnest(n_events_nested) |>
    dplyr::group_by(SOURCE) |>
    dplyr::mutate(
      CODE4 = code,
      n_events = ne,
      per_events = n_events/sum(n_events)
    ) |>
    dplyr::select(SOURCE, CODE4, per_events)  |>
    dplyr::ungroup()

  # Give to each event a weighted random CODE4 based on SOURCE, VOCAB and EVENT_YEAR, using  `code_count.tsv`.
  sampled_codes <- sampled_codes  |>
    #
    dplyr::group_by(SOURCE) |> tidyr::nest() |>
    dplyr::mutate(n_events = purrr::map_int(data, nrow)) |>
    #
    dplyr::mutate( code4 = purrr::pmap(.l=list(SOURCE, n_events), .f=~{
      code4_probabilities |>
        dplyr::filter(SOURCE==..1) |>
        .sample_probability_tibble("per_events", ..2) |>
        dplyr::select(CODE4 )
    })) |>
    dplyr::mutate(data = purrr::map2(data, code4, dplyr::bind_cols)) |>
    dplyr::select(-n_events, -code4) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    # adjust CODE4 for INPAT
    dplyr::group_by(SOURCE, INDEX) |>
    dplyr::mutate(CODE4 = if_else(SOURCE=="INPAT", sample(CODE4,1), CODE4)) |>
    dplyr::ungroup()


    #
    ParallelLogger::logInfo("Appened CODE4 to longitudinal_data ", scales::number(sampled_events |> nrow()), " events")


    ###
    ### Reformat data as in source files
    ###
    ## longitudinal data

    ParallelLogger::logInfo("Reformat data as in source files ")
    #
    sampled_codes <-  sampled_codes|>
      # Re build CATEGORY and IDCVER
      dplyr::mutate(ICDVER=dplyr::case_when(
        SOURCE %in% c("INPAT", "OUTPAT", "REIMB") ~ stringr::str_extract(vocabulary, "[:digit:]+"),
        TRUE ~ as.character(NA)
      ))|>
      dplyr::mutate(CATEGORY=dplyr::case_when(
        SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT") ~ stringr::str_c(vocabulary, level),
        SOURCE %in% c("INPAT", "OUTPAT") ~  stringr::str_extract(vocabulary, "[:upper:]+") |>
          stringr::str_replace_na("") |> stringr::str_c(level),
        TRUE ~ as.character(NA)
      )) |>
      dplyr::select(-vocabulary, -level)


    # replace EVENT_YEAR  with APPREX_EVENT_DAYS. It has to be calculated for EVENT_YEAR within visit (same INDEX, SOURCE )
    ParallelLogger::logInfo("Reformat longitudinal data: replace EVENT_YEAR  with APPREX_EVENT_DAYS ")
    #
    distinct_event_years <-  sampled_codes|>
      dplyr::distinct(SOURCE, INDEX, event_year )|>
      dplyr::group_by(event_year )|> tidyr::nest()|>
      dplyr::mutate(data = purrr::map2(.x=data, .y=event_year, .f=~{
        .x|> dplyr::mutate(APPROX_EVENT_DAY = sample(
          seq(as.Date(stringr::str_c(.y,'/01/01')), as.Date(stringr::str_c(.y,'/12/31')), by="day"),
          nrow(.x),
          replace = TRUE)
        )
      }))|> tidyr::unnest(data)|>  dplyr::ungroup()

    sampled_codes <- dplyr::left_join(
      sampled_codes,
      distinct_event_years,
      by=c("event_year", "SOURCE","INDEX")
    ) |>
      dplyr::select(-event_year)




    ###
    ### calculate death
    ###
    ParallelLogger::logInfo("Calculate death events")


    death_patients <- sampled_codes |>
      dplyr::semi_join(sampled_patients |>
      dplyr::filter(is_death), by="FINNGENID") |>
      dplyr::arrange(desc(APPROX_EVENT_DAY)) |>
      dplyr::distinct(FINNGENID, .keep_all = T) |>
      dplyr::transmute(
        FINNGENID = FINNGENID,
        SOURCE = "DEATH",
        APPROX_EVENT_DAY = APPROX_EVENT_DAY+lubridate::days(sample(0:30,n())),
        CODE4 = as.integer(NA),
        CODE5 = as.character(NA),
        CODE6 = as.character(NA),
        CODE7 = as.double(NA),
        event_year = lubridate::year(APPROX_EVENT_DAY),
        ICDVER = case_when(
          event_year < 1988 ~ "8",
          event_year < 1996 ~ "9",
          TRUE ~ "10"
        ),
        vocabulary = "death", # TO FIX ICDVER
        INDEX = row_number()
      ) |>
      # add levels
      dplyr::mutate(level = sample(0:5, dplyr::n())) |>
      tidyr::uncount(level) |>
      dplyr::group_by(INDEX) |>
      dplyr::mutate(
        CATAEGORY = dplyr::row_number(),
        CATAEGORY=dplyr::case_when(
          CATAEGORY==1~"c1",
          CATAEGORY==2~"c2",
          CATAEGORY==3~"c3",
          CATAEGORY==4~"c4",
          CATAEGORY==5~"I",
          CATAEGORY==0~"U",
        TRUE ~ as.character(NA)
        )) |>
      # find code
      dplyr::group_by(SOURCE, vocabulary) |>
      tidyr::nest() |>
      dplyr::mutate(n_events = purrr::map_int(data, nrow)) |>
      #
      dplyr::mutate( codes = purrr::pmap(.l=list(SOURCE, vocabulary, n_events), .f=~{
        codes_probabilities |>
          dplyr::filter(SOURCE==..1 & vocabulary==..2) |>
          .sample_probability_tibble("per_events", ..3) |>
          dplyr::select(CODE1, CODE2, CODE3 )
      })) |>
      dplyr::mutate(data = purrr::map2(data, codes, dplyr::bind_cols))  |>
      tidyr::unnest(data) |>
      dplyr::ungroup()|>
      dplyr::select(-n_events, -codes, -vocabulary, -event_year)


    ###
    ### bind longitudinal and death
    ###
    ParallelLogger::logInfo("bind longitudinal and death")

    service_sector_data <- bind_rows(
      sampled_codes,
      death_patients |>  sample_n(round(nrow(death_patients)*0.9))
    )


  # calcualte EVENT_AGE
  ParallelLogger::logInfo("Reformat longitudinal data: calcualte EVENT_AGE ")
  #
  service_sector_data <-  service_sector_data|>
    dplyr::left_join(sampled_patients|> dplyr::select(FINNGENID, birth_date), by="FINNGENID")|>
    dplyr::mutate(EVENT_AGE = as.numeric(lubridate::interval(birth_date, APPROX_EVENT_DAY),"years"))|>
    dplyr::mutate(EVENT_AGE = round(EVENT_AGE, digits = 2))|>
    #
    dplyr::transmute(FINNGENID, SOURCE, EVENT_AGE, APPROX_EVENT_DAY, CODE1, CODE2, CODE3, CODE4, CODE5, CODE6, CODE7, ICDVER, CATEGORY, INDEX)

  #
  ParallelLogger::logInfo("Reformated longitudinal data: ", scales::number(length(sampled_events))," events")

    return(service_sector_data)

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
  return(tb|> dplyr::mutate(!!col_name:=b))
}

#' @importFrom dplyr slice
.sample_probability_tibble <- function(data, per_column, n_samples){
  ret <- data|> dplyr::slice(0)
  if(nrow(data)!=0){
    ix <- sample(1:nrow(data), size = n_samples, replace = TRUE, prob = data[[per_column]])
    ret <- data|> dplyr::slice(ix)
  }
  return(ret)
}



