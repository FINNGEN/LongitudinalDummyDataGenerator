# DESCRIPTION :
#
# 1 baseline data
# - Calculate observation periods for each patient: start_year start_age observation_year
# - Round to multiples of 5
# - Count the number of event by source for each patient
# - Count the number of unique observation periods and the lognorm distribution of n events


# PARAMETERS --------------------------------------------------------------
n_patients_limit = 5
path_to_service_sector_data <- "./ss_sample.tsv"
output_folder = "./summaty_data/"


# LOAD DATA ---------------------------------------------------------------
service_sector_data <- readr::read_tsv(path_to_service_sector_data)


# SETUP -------------------------------------------------------------------
logger <- log4r::create.logger()


# FUNCTION ----------------------------------------------------------------

## Calculate observation_period_summary


tic()
# count event types per patients
count_events_per_patient <- longitudinal_data_DF6 %>%
  count(FINNGENID, SOURCE) %>%
  spread(SOURCE, n) %>%
  mutate_if(is.integer, ~if_else(is.na(.), 0L, .))
toc()


## Count periods and calcualte lognorm distribution of counts for each SOURCE

count_periods <- cohort_data_DF6 %>% transmute(
  FINNGENID=FINNGENID,
  start_year=lubridate::year(approx_day_first_event),
  start_age = round(age_first_event),
  observation_years =   (lubridate::year(approx_day_last_event) -lubridate::year(approx_day_first_event)),
  start_year = start_year- (start_year %% 5),
  start_age = start_age - (start_age %% 5),
  observation_years = observation_years - (observation_years %% 5)
) %>%
  #
  inner_join(count_events_per_patient, by="FINNGENID") %>%
  #
  group_by(start_year, start_age, observation_years) %>%
  summarise(
    n_patients=n(),
    INPAT_logmean = mean(log(INPAT+0.5)), INPAT_logsd = sd(log(INPAT+0.5)),
    PURCH_logmean = mean(log(PURCH+0.5)), PURCH_logsd = sd(log(PURCH+0.5)),
    OUTPAT_logmean = mean(log(OUTPAT+0.5)), OUTPAT_logsd = sd(log(OUTPAT+0.5)),
    OPER_OUT_logmean = mean(log(OPER_OUT+0.5)), OPER_OUT_logsd = sd(log(OPER_OUT+0.5)),
    PRIM_OUT_logmean = mean(log(PRIM_OUT+0.5)), PRIM_OUT_logsd = sd(log(PRIM_OUT+0.5)),
    OPER_IN_logmean = mean(log(OPER_IN+0.5)), OPER_IN_logsd = sd(log(OPER_IN+0.5)),
    #
    REIMB_logmean = mean(log(REIMB+0.5)), REIMB_logsd = sd(log(REIMB+0.5)),
    CANC_logmean = mean(log(CANC+0.5)), CANC_logsd = sd(log(CANC+0.5)),
    .groups="drop"
  )


count_periods




n_codes_count <- count_periods %>%  nrow()
n5_codes_count <- count_periods %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")




count_periods %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv

count_periods %>%
  filter(n_patients>n_patients_limit)





# Longitudinal data
## preformat
Vocabulary information and level information are mixed into the fileds ICDVER and CATEGORY,
Separate these


tic()
# conver VOCAB into vocabulary and LEVEL into LEVEL
longitudinal_data_DF6 <- longitudinal_data_DF6 %>%
  mutate(ICDVER = if_else(
    SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT"),
    str_extract(CATEGORY, "^[:upper:]+"),
    ICDVER
  )) %>%
  mutate(CATEGORY = if_else(
    SOURCE %in% c("PRIM_OUT", "OPER_IN", "OPER_OUT"),
    str_extract(CATEGORY, "[:digit:]+"),
    CATEGORY
  ))  %>%
  rename(VOCAB=ICDVER, LEVEL=CATEGORY)%>%
  #replace NA in PURCH with "purch"
  mutate(VOCAB = if_else(SOURCE=="PURCH", "purch", VOCAB))
toc()



## 2.1 codes count
CODE1, CODE2, CODE3, VOCAB, n_event n_patients


tic()
codes_count <- longitudinal_data_DF6 %>%
  group_by(SOURCE, CODE1, CODE2, CODE3, VOCAB) %>%
  summarise(n_events=n(), n_patients=length(unique(FINNGENID)), .groups="drop")
toc()



n_codes_count <- codes_count %>%  nrow()
n5_codes_count <- codes_count %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")



a <- codes_count %>% count(SOURCE)
b <- codes_count %>% filter(n_patients>n_patients_limit) %>% count(SOURCE)
inner_join(a,b,by="SOURCE") %>% mutate(per_lost=(1-n.y/n.x)*100)



codes_count %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv(file.path(output_folder, "codes_count.tsv"))

codes_count %>%
  filter(n_patients>n_patients_limit)



## 2.2 n package and days visit count
Pre bin CODE4
SOURCE CODE4 n_events n_patients


tic()
code4_count <- longitudinal_data_DF6 %>%
  #filter(SOURCE %in% c("PURCH", "INPAT"))%>%
  group_by(SOURCE, CODE4) %>%
  #
  mutate(CODE4 = cut(as.integer(CODE4), breaks=c(-Inf,-1,0,1,2,3,4,5,10,50,100,Inf)))%>%
  #
  summarise(n_events=n(), n_patients=length(unique(FINNGENID)), .groups="drop")


toc()



n_codes_count <- code4_count %>%  nrow()
n5_codes_count <- code4_count %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")



code4_count %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv(file.path(output_folder, "code4_count.tsv"))

code4_count %>%
  filter(n_patients>n_patients_limit)



# 2.3 distribution of levels
recode DEATH levels with numbers 0:5
Pre bin LEVELS
SOURCE LEVEL n_events n_patients


tic()
level_count <- longitudinal_data_DF6 %>%
  #filter(SOURCE %in% c("PURCH", "INPAT"))%>%
  group_by(SOURCE, LEVEL) %>%
  #
  mutate(LEVEL=case_when(
    LEVEL=="c1"~"1",
    LEVEL=="c2"~"2",
    LEVEL=="c3"~"3",
    LEVEL=="c4"~"4",
    LEVEL=="I"~"5",
    LEVEL=="U"~"0",
    TRUE ~ LEVEL
  )) %>%
  mutate(LEVEL = cut(as.integer(LEVEL), breaks=c(-Inf,-1,0,1,2,3,4,5,10,Inf)))%>%
  #
  summarise(n_events=n(), n_patients=length(unique(FINNGENID)), .groups="drop")


toc()



n_codes_count <- level_count %>%  nrow()
n5_codes_count <- level_count %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")



level_count %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv(file.path(output_folder, "level_count.tsv"))

level_count %>%
  filter(n_patients>n_patients_limit)


# 2.4 Event year count
SOURCE VOCAB EVENT_YEAR n_events n_patients


tic()
year_count <- longitudinal_data_DF6 %>%
  group_by(SOURCE, VOCAB, EVENT_YEAR) %>%
  summarise(n_events=n(), n_patients=length(unique(FINNGENID)), .groups="drop")
toc()



n_codes_count <- year_count %>%  nrow()
n5_codes_count <- year_count %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")



year_count %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv(file.path(output_folder, "event_year_count.tsv"))

year_count %>%
  filter(n_patients>n_patients_limit)


# 2.5 count death


death <- longitudinal_data_DF6 %>% filter(SOURCE=="DEATH")



death_age_counts <- death %>% mutate(EVENT_AGE = round(EVENT_AGE)-round(EVENT_AGE)%%5) %>%
  group_by(EVENT_AGE) %>%
  summarise( n_patients=n(), .groups="drop")





n_codes_count <- death_age_counts %>%  nrow()
n5_codes_count <- death_age_counts %>% filter(n_patients>n_patients_limit) %>%  nrow()

str_glue("n unique codes {n_codes_count}
          n unique codes more than 5 patients {n5_codes_count}
          percent unique codes lost {percent(1-n5_codes_count/n_codes_count, accuracy=0.01)}")




death_age_counts %>%
  filter(n_patients>n_patients_limit) %>%
  write_tsv(file.path(output_folder, "death_age_counts.tsv"))

death_age_counts %>%
  filter(n_patients>n_patients_limit)





