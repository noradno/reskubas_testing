
# Testing the data table oecd_imputed_multilateral_countries table ------------------------------

# Libraries
library(tidyverse)
library(here)
#remotes::install_github("noradno/noradstats")
library(noradstats)

# Load test data and benchmark data  ---------------------------------------

# RKB - test data
oecd_imputedcountries_rkb <- readr::read_csv(
  here("data", "oecd_imputed_multilateral_countries.csv"),
  col_types = list(
    donor = col_character(),
    recipient = col_character(),
    obs_time = col_character()
    )
  )

# OECD - benchmark data from the OECD SDMX API using the rsdmx package wrapped in the noradstats package
oecd_imputedcountries_benchmark <- noradstats::get_imputed_countries(startyear = 2021, endyear = 2021)

oecd_imputedcountries_benchmark <- oecd_imputedcountries_benchmark |> 
  rename(obs_value = usd_mill) |> 
  select(-c(exchangerate, nok_mill))

# Testing function --------------------------------------------------------

# Check equality of spesific column in testing dataframe and benchmark dataframe
test_equal_values <- function(data_rkb, data_benchmark, var_rkb, var_benchmark) {
  rkb <- data_rkb |> 
    select({{ var_rkb }}) |> 
    arrange({{ var_rkb}} ) |> 
    distinct()
  
  benchmark <- data_benchmark |> 
    select({{ var_benchmark }}) |> 
    arrange({{ var_benchmark }}) |> 
    distinct()
  
  all.equal(rkb, benchmark)
}

# # Test: column names -----------------------------------------------------

# Result: OK
identical(names(oecd_imputedcountries_rkb), names(oecd_imputedcountries_benchmark))

# Test: aid_type column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$aidtype_label_en)),
          length(unique(oecd_imputedcountries_benchmark$aidtype_label_en)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = aidtype_label_en,
                  var_benchmark = aidtype_label_en)

# Test: donor column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$donor)),
          length(unique(oecd_imputedcountries_benchmark$donor)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = donor,
                  var_benchmark = donor)

# Test: donor_label_en column -----------------------------------------------------------

identical(length(unique(oecd_imputedcountries_rkb$donor_label_en)), length(unique(oecd_imputedcountries_benchmark$donor_label_en)))

test_equal_values(data_rkb = oecd_imputedcountries_rkb,
           data_benchmark = oecd_imputedcountries_benchmark,
           var_rkb = donor_label_en,
           var_benchmark = donor_label_en)

# recipient column ---------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$recipient)), length(unique(oecd_imputedcountries_benchmark$recipient)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
           data_benchmark = oecd_imputedcountries_benchmark,
           var_rkb = recipient,
           var_benchmark = recipient)

# recipient_label_en column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$recipient_label_en)), length(unique(oecd_imputedcountries_benchmark$recipient_label_en)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = recipient_label_en,
                  var_benchmark = recipient_label_en)

# obs_time column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$obs_time)), length(unique(oecd_imputedcountries_benchmark$obs_time)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = obs_time,
                  var_benchmark = obs_time)

# obs_value column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$obs_value)), length(unique(oecd_imputedcountries_benchmark$obs_value)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = obs_value,
                  var_benchmark = obs_value)

# powercode_label_en column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$powercode_label_en)), length(unique(oecd_imputedcountries_benchmark$powercode_label_en)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = powercode_label_en,
                  var_benchmark = powercode_label_en)

# datatype_label_en column ----------------------------------------------------------

# Length of unique values
identical(length(unique(oecd_imputedcountries_rkb$datatype_label_en)), length(unique(oecd_imputedcountries_benchmark$datatype_label_en)))

# Equal unique values
test_equal_values(data_rkb = oecd_imputedcountries_rkb,
                  data_benchmark = oecd_imputedcountries_benchmark,
                  var_rkb = datatype_label_en,
                  var_benchmark = datatype_label_en)


# Testing obs_value, grouped summaries ------------------------------------

test_equal_sums <- function(data_rkb, data_benchmark, group_var_rkb, group_var_benchmark) {
  rkb <- data_rkb |> 
    group_by({{ group_var_rkb }}) |> 
    summarise(sum(obs_value))
  
  benchmark <- data_benchmark |> 
    group_by({{ group_var_rkb }}) |> 
    summarise(sum(obs_value))
  
  identical(rkb, benchmark)
}

# Testing grouped amounts
test_equal_sums(data_rkb = oecd_imputedcountries_rkb,
                data_benchmark = oecd_imputedcountries_benchmark,
                group_var_rkb = recipient,
                group_var_benchmark = recipient)
