# Import data from Resultat- og kunnskapsbasen

library(DBI)
library(odbc)
library(dbplyr)
library(noradplot)
library(tidyverse)
ggnorad()


# Credentials are defined in separate .Renviron file.
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "reskunnbasen-dev-sql.database.windows.net",
                 Database = "reskunnbasen-dev-db",
                 UID = Sys.getenv("userid"),
                 PWD = Sys.getenv("pwd"),
                 Port = 1433)

# DBI::dbListTables(con)
# DBI::dbListFields(con, "OECD_imputed_multilateral_countries")

# OECD donors ------------------------------------------

df_oecd_donors <- tbl(con, in_schema("data", "OECD_dac_donors")) |> 
  group_by(donor_label_en) |> 
  summarise(usd_bill = sum(obs_value / 1000)) |> 
  ungroup() |> 
  slice_max(usd_bill, n = 10) |> 
  collect()

df_oecd_donors |> 
  mutate(donor_label_en = fct_reorder(donor_label_en, usd_bill)) |> 
  ggplot(aes(x = donor_label_en, y = usd_bill)) +
  geom_col(fill = norad_pal("green"), show.legend = F) +
  geom_text(aes(label = format(round(usd_bill, 1),
                               big.mark = " ",
                               decimal.mark = ",")),
            hjust = 1.2, color = "white") +
  scale_y_continuous(expand = c(0, NA)) +
  coord_flip() +
  labs(title = "Topp 10 giverland av bistand",
       subtitle = "Offisiell bistand (ODA) fra OECD-land i 2021. Utbetalinger i milliarder dollar (USD)",
       x = NULL, y = NULL,
       caption = "Kilde: Norad")
