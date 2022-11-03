library(democracyData)
library(tidyverse)
library(vdem)

fh.data <- download_fh(verbose = FALSE) %>%
  mutate(measure = "fh",
         outcome = case_when(
           status == "F" ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

eiu.data <- eiu %>%
  mutate(measure = "eiu",
         outcome = case_when(
           eiu > 5 ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

pacl.data <- pacl %>%
  mutate(extended_country_name = pacl_country,
         measure = "pacl",
         outcome = case_when(
           regime <= 2 & regime >= 0 ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

anckar.data <- anckar %>%
  mutate(measure = "anckar",
         outcome = case_when(
           regimenarrowcat == "Parliamentarism" | 
             regimenarrowcat == "Semi-presidentialism" |
             regimenarrowcat == "Presidentialism" ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

polity.data <- download_polity_annual(verbose = FALSE) %>%
  mutate(measure = "polity",
         outcome = case_when(
           polity2 >= 6 & polity2 <= 10 ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

vanhanen.data <- vanhanen %>%
  mutate(measure = "vanhanen",
         outcome = case_when(
           vanhanen_competition > 30 & 
             vanhanen_participation > 10 &
             vanhanen_democratization > 5.0 ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

vdem.data <- VDem_plus %>%
  mutate(measure = "vdem",
         outcome = case_when(
           v2x_polyarchy > 0.5 |
             v2x_libdem > 0.5 |
             v2x_partipdem > 0.5 |
             v2x_delibdem > 0.5 |
             v2x_egaldem > 0.5 ~ "democratic",
           TRUE ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

anrr.data <- anrr %>%
  mutate(measure = "anrr",
         outcome = case_when(
           dem == 1 ~ "democratic",
           dem != 1 ~ "non-democratic"
         )) %>%
  select(measure, year, extended_country_name, outcome)

total <- rbind(fh.data, eiu.data, pacl.data, anckar.data, polity.data, vanhanen.data, vdem.data, anrr.data)
save(total, file = "./My_Raw_Data.RData")
