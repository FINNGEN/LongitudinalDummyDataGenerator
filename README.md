
# LongitudinalDummyDataGenerator
Scripts to summaryse features from FinnGen's register data and to produce statically similar dummy tables.


## Install 
``` r
install.packages("remotes")
remotes::install_github("FINNGEN/LongitudinalDummyDataGenerator")
```
## Run
``` r
LongitudinalDummyDataGenerator::generate_dummy_service_sector_data(
    output_folder = tempdir(), # directory where to ouput the data 
    longitudinal_data_version = "DF10v2", #at the moment only DF10v2 is avalilable
    n_patients = 100, # number of random patients to genenrate 
    seed = 13, # seed used in the random processes 
    nTreaths = 3 # number of cores to use when using paraller generation 
)
```


## How it works
1. Generate N patients with first observation year, last observation year, and number of visits of each SOURCE within the observation period. Proportion of these values are similar to real data.
1. Gives to each visit a number of events, service sector codes (CODE5, CODE6, CODE8, CODE9), and an INDEX. Proportion of the first two values are similar to real data with in some define time windows. 
2. Gives to each event a year and a vocabulary. Proportion of these values are similar to real data. Events in the same visit of same vocabulary get a distinct level.  
3. Gives to each event a code (CODE1, CODE2, CODE3), based on the year and vocabulary. Proportion of these values are similar to real data.  
4. Gives to each event a CODE4. Proportion of these values are similar to real data.  
7. Generates random months, days and bithdays. Calcualates consisten EVENT_AGES.
8. Creates DEATH visits at a random distance [0,30]day from the end of observation for a proportion of the patients with events and codes similar to the realdata. 


## Preserved and not preserved features

### What it preserves
 1. Patients with statically similar, observation periods and number of events.
 2. Exact same codes (CODE1, CODE2, CODE3), -except if combination is in less than 5 patients-
 3. Destiny of vocabularies statically similar.
 4. Number of packages, days of visit, and  levels statically similar.

### What it does not preserves
 1. Sex-related codes are not linked to sex (e.g. a male can be pregnant).
 2. Codes within same visit are not necessarily related (same INDEX may mix unrelated conditions).


