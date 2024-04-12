# Test data from Reskubas and compare it to bencharmk data from OECD rdmx
library(readr)
library(noradstats)
library(dplyr)
library(purrr)

# 1. Imputed multilateral countries---------------------------------------

# OECD - benchmark data from the OECD SDMX API using the rsdmx package wrapped in the noradstats package
oecd_imputedcountries_benchmark <- noradstats::get_imputed_countries(startyear = 2011, endyear = 2021) |>
  rename(obs_value = usd_mill) |>
  select(-c(exchangerate, nok_mill))

# 2. DAC donors --------------------------------------------------------------

# OECD - benchmark data from the OECD SDMX API using the rsdmx package wrapped in the noradstats package
oecd_donors_benchmark <- noradstats::get_donors() |> 
  rename(obs_value = usd_mill)

# Save datasets in RData file --------------------------------------------
save(oecd_imputedcountries_benchmark,
     oecd_donors_benchmark,
     file = "data/processed/oecd_data.RData")
