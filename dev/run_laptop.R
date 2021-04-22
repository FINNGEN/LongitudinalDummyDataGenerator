
library(LongitudinalDummyDataGenerator)
library(tidyverse)
library(tictoc)

output_dir <- "C:\\Users\\javier\\WorkSpace\\FINNGEN\\dummy_DF6_50k_13"


tic()
generate_dummy_longitudinal_data(output_folder, n_patients = 50000, n_cuts = 10, nTreaths = 3)
toc()
