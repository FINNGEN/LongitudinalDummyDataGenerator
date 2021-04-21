# Summary statistics of the longitudinal data for dummy data generation

The files in this folder summarize different characteristics of the FinnGen longitudinal data. 

All files include the number of patients who has that characteristic. Charactristics with less than 5 patients have been removed. All FINNGENIDs are excluded. 

The aim of these files is aid to generate dummy longitudinal data for testing and development outside sandbox. 

## Source data  
Longitudinal data version: DF6v2
Source files: 
file:///finngen/library-red/finngen_R6/phenotype_2.0/data/finngen_R6_v2_detailed_longitudinal.gz
file:///finngen/library-red/finngen_R6/phenotype_2.0/data/finngen_R6_v2_minimum_longitudinal.txt



## Output summary statistics 

Following subsections describe the columns in each of the files in this folder. 

All files have in common column `n_patients` indicating the number of patients in the source data with the row characteristics. 

ALL ROWS WITH `n_patients < 6` HAVE BEEN EXCLUDED

### codes_count.tsv
Count of unique codes combinations in the longitudinal data, grouped by SOURCE and VOCAB. 

SOURCE, CODE1, CODE2, CODE3, VOCAB, = unique code combination 
n_event = number of times the code appears 
n_patients = number of  distinct patients with that code


### code4_count.tsv
Count of values for CODE4 after binning, grouped by SOURCE. 
Bins are on the following ranges -Inf,-1,0,1,2,3,4,5,10,50,100,Inf. 
In the longitudinal data CODE 4 indicates the number of packages for PURCH and the number of hositalisation days for INPAT, NA for the rest. 

SOURCE, CODE4 = unique code combination 
n_event = number of times the code appears 
n_patients = number of distinct patients with that code


### level_count.tsv
Count of values for LEVEL after binning., grouped by SOURCE 
Bins are on the following ranges -Inf,-1,0,1,2,3,4,5,10,Inf. 
LEVEL is calculated from CATEGORY and indicated the level of the diagnose/operation. 

SOURCE, LEVEL = unique code combination 
n_event = number of times the code appears 
n_patients = number of distinct patients with that code


### event_year_count.tsv
Count EVENT_YEARS grouped by SOURCE and VOCAB. 

SOURCE, VOCAB, EVENT_YEAR = unique code combination 
n_event = number of times the code appears 
n_patients = number of distinct patients with that code


### death_age_counts.tsv
Age distribution at death  

EVENT_AGE = age at death
n_patients = number of distinct patients death at that age


### count_periods.tsv
Count of unique observation periods in the source data and lognorm distribution of events. 

start_year = year of first observation round down to multiples of 5
start_age  = age at first observation round down to multiples of 5
observation_year  = years between first and last observation round down to multiples of 5
<SOURCE>_logmean = mean number of events of type <SOURCE> during the observation period 
<SOURCE>_logsd = standard deviation from the  number of events of type <SOURCE> during the observation period

n_patients = number of patients in the source data with the row characteristics. 

