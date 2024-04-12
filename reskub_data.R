# Import data from Resultat- og kunnskapsbasen

library(odbc)
library(dplyr)
library(dbplyr)

# Credentials are defined in separate .Renviron file.
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "reskunnbasen-dev-sql.database.windows.net",
                 Database = "reskunnbasen-dev-db",
                 UID = Sys.getenv("userid"),
                 PWD = Sys.getenv("pwd"),
                 Port = 1433)

# List of tables
DBI::dbListTables(con)

# Variables in a spesific table
DBI::dbListFields(con, "OECD_imputed_multilateral_countries")

# OECD donors ------------------------------------------

db_oecd_donors_rkb <- tbl(con, in_schema("data", "OECD_dac_donors"))

oecd_donors_rkb <- db_oecd_donors_rkb |>
  collect()


# Imputed multilateral countries -------------------------------------

db_oecd_imputedcountries_rkb <- tbl(con, in_schema("data", "OECD_imputed_multilateral_countries"))

oecd_imputedcountries_rkb <- db_oecd_imputedcountries_rkb |>
  collect()

save(oecd_donors_rkb,
     oecd_imputedcountries_rkb,
     file = "data/processed/reskubas.RData")
