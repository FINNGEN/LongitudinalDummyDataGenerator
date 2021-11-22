
# LongitudinalDummyDataGenerator
Scripts to summaryse features from FinnGen's longitudinal_data and to produce statically similar dummy data.


## Install 
``` r
install.packages("remotes")
remotes::install_github("FINNGEN/LongitudinalDummyDataGenerator")
```
## Run
``` r
LongitudinalDummyDataGenerator::generate_dummy_longitudinal_data(
    output_folder = tempdir(), # directory where to ouput the data 
    longitudinal_data_version = "DF6v2", #at the moment only DF6v2 is avalilable
    n_patients = 100, # number of random patients to genenrate 
    seed = 13, # seed used in the random processes 
    nTreaths = 3 # number of cores to use when using paraller generation (if large you can use parallel::detectCores() -1)
)
```


## How it works
1. Generate N patients with first observation year, last observation year, and number of events of each SOURCE within the observation period. Proportion of these values are similar to real data.
2. Gives to each event a year and a vocabulary. Proportion of these values are similar to real data.  
3. Gives to each event a code (CODE1, CODE2, CODE3). Proportion of these values are similar to real data.  
4. Gives to each event a CODE4 and a LEVEL of diagose. Proportion of these values are similar to real data.  
5. Assings a unique INDEX to these events with the same patient, year, vocabulary, and distinct LEVEL.
6. Removes double deaths.
7. Generates random months, days and bithdays. Calcualates consisten EVENT_AGES.


## Preserved and not preserved features

### What it preserves
 1. Patients with statically similar, observation periods and number of events.
 2. Exact same codes (CODE1, CODE2, CODE3), -except if combination is in less than 5 patients-
 3. Desitiy of vocabularies statically similar.
 4. Number of packages, days of visit, and  levels statically similar.

### What it does not preserves
 1. Sex-related codes are not linked to sex (e.g. a male can be pregnant).
 2. Codes within same visit are not necesarely related (same INDEX may mix unrelated conditions).
 3. All condition in same INDEX are of the same vocabulary.


## TOIMPROVE ??

1. CATEGORY now has more levels tan should be for some vocabualries, for example in source SNOM has only 2 levels !!

2. 1 calcualte number of visits in patient observation. 2 add info to visit (including index), 3 add info to events. (may be it is more dificult)

<!-- badges: start -->
<!-- badges: end -->

The goal of LongitudinalDummyDataGenerator is to ...

