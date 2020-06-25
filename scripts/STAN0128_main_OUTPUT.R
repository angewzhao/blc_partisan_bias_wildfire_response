# Author: Angela Zhao
# Version: 2020-06-22

# Libraries

.libPaths(
  new =
    c(
      "/Users/angel/Documents/R/win-library/3.6",
      "/Program Files/R/R-3.6.2/library"
    )
)
library(tidyverse)

library(haven)

# Parameters

file_raw <- here::here("data-raw/STAN0128_main_OUTPUT.sav")

file_out <- here::here("data/STAN0128_main_OUTPUT.rds")

#===============================================================================

Sys.time()


file_raw %>%
  read_sav() %>%
  mutate_if(is.labelled, as_factor) %>%
  write_dta(file_out)


Sys.time()
