# Run tests of data from Resultat- og kunnskapsbasen

# 1. Load packages and data --------------------------------------------------

library(dplyr)
library(purrr)

# Data from Resultat- og kunnskapsbasen - test data
load("output/reskub.RData")

# Data from OECD - benchmark data
load("output/oecd_data.RData")

# 2. Function to compare columns in test data and benchmark data -------------

test_equal_values <- function(data_rkb = NULL,
                              data_benchmark = NULL,
                              var_rkb = NULL,
                              var_benchmark = NULL
                              ) {
  
  rkb <- data_rkb |> 
    select(all_of({{ var_rkb }})) |> 
    arrange({{ var_rkb}} )
  
  benchmark <- data_benchmark |> 
    select(all_of({{ var_benchmark }})) |> 
    arrange({{ var_benchmark }})
  
  equal_true <- all.equal(rkb, benchmark)
  
  if (length(equal_true) > 1) {
    return(paste("Fail: ", var_rkb, "is not correct"))
  } else {
    return(paste("Success: ", var_rkb, "is correct"))
  }
}

# 3. Apply test function to each column in a dataframe and return a list of test results-----------

# 3.1. Test OECD imputed countries dataset
map(.x = names(oecd_imputedcountries_rkb),
    .f = ~ test_equal_values(
      var_rkb = .x,
      var_benchmark = .x,
      data_rkb = oecd_imputedcountries_rkb,
      data_benchmark = oecd_imputedcountries_benchmark)
)

# 3.2. Test OECD DAC donors dataset
map(.x = names(oecd_donors_rkb),
    .f = ~ test_equal_values(
      var_rkb = .x,
      var_benchmark = .x,
      data_rkb = oecd_donors_rkb,
      data_benchmark = oecd_donors_benchmark)
)
