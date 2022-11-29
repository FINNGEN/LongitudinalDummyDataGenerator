# Summary statistics finngen data tables for dummy data generation

The files in this folder summarize different characteristics of the FinnGen phenotype tables. 

All files include the number of patients who has that characteristic.
Characteristics with less than 5 patients have been removed. All FINNGENIDs are excluded. 

The aim of these files is aid to generate dummy longitudinal data for testing and development outside sandbox. 

## Service sector data 

### Source data  
Longitudinal data version: DF10v1
Source files: 
file:///finngen/library-red/finngen_R10/phenotype_1.0/data/finngen_R10_service_sector_detailed_longitudinal_1.0.txt


### Tables

Following subsections describe the columns in each of the files in this folder. 

All files have in common column `n_patients` indicating the number of patients in the source data with the row characteristics. 

ALL ROWS WITH `n_patients < 5` HAVE BEEN EXCLUDED

#### observation_periods_summary_summary.tsv
Count of patients with distinct observation periods and lognorm distribution of the number of visits per each SOURCE. 

first_visit_year = year of first observation round down to multiples of 5
first_visit_age  = age at first observation round down to multiples of 5
years_to_last_visit  = years between first and last observation round down to multiples of 5
<SOURCE>_logmean = log mean number of visists of type <SOURCE> during the observation period 
<SOURCE>_logsd = log standard deviation from the  number of visists of type <SOURCE> during the observation period
n_person_death = number of patients death if n_person_death >5 0 otherwise
n_patients = number of patients in the source data with the row characteristics. 

####  count_visits_per_patient.tsv
Number of visits with distinct SOURCE, CODE5, CODE6, and CODE7 during some time windows. 
As well as, lognorm distribution of the number of events per visit with the row characteristics. 


visit_year_bin = Time window, intervals 0, 1986, 1995, 2002, 2010, 2018, 2100. This is based on the observed code usage (e.g. CODE5, CODE6 are NA before 1986) 
SOURCE, CODE5, CODE6, CODE7 = unique code combination 
n_visits = Number of visit with the row characteristics. 
n_events_per_visit_logmean = log mean number of events per visit
n_events_per_visit_logsd = log standard deviation number of events per visit
n_patients = number of patients in the source data with the row characteristics. 


#### SOURCE_VOCAB_EVENT_YEAR_summary.tsv
Count EVENT_YEARS grouped by SOURCE and VOCAB. 

SOURCE, VOCAB, EVENT_YEAR = unique code combination 
n_event = number of times the code appears 
n_patients = number of distinct patients with that code


#### CODE1_CODE2_CODE3_vocabulary_summary.tsv
Count of unique codes combinations in the longitudinal data, grouped by SOURCE and vocabulary. 

SOURCE, CODE1, CODE2, CODE3, VOCAB, = unique code combination 
n_event = number of times the code appears 
n_patients = number of  distinct patients with that code


#### CODE4_summary.tsv
Count of values for CODE4 after binning, grouped by SOURCE. 
Bins are on the following ranges -Inf,-1,0,1,2,3,4,5,10,50,100,Inf. 
In the longitudinal data CODE 4 indicates the number of packages for PURCH and the number of hospitalization days for INPAT, NA for the rest. 

SOURCE, CODE4 = unique code combination 
n_event = number of times the code appears 
n_patients = number of distinct patients with that code

