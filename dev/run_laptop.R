
library(LongitudinalDummyDataGenerator)
library(tidyverse)
library(tictoc)

output_dir <- "C:\\Users\\javier\\WorkSpace\\FINNGEN\\dummy_DF6_50k_13"


tic()
generate_dummy_longitudinal_data(output_dir, n_patients = 100)
toc()
