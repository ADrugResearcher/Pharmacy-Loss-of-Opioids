# Populations cleaner
library(readr)
the_years <- lubridate::ymd(paste(seq(2018,2024, by = 1), "-01-01", sep = ""))
# Using the population at the start of the year for my estimate
pop <- read_csv("pop_estimates.csv") |> 
  janitor::clean_names() |> 
  mutate(ref_date = dmy(paste("-01", ref_date, sep = ""))) |> 
  filter(ref_date %in% the_years) |> 
  select(ref_date, geo, value) |>
  mutate(geo = str_replace_all(geo, c("British Columbia"= "BC",
                                      "Alberta" = "AB",
                                      "Ontario" ="ON",
                                      "Manitoba" = "MB",
                                      "Saskatchewan" = "SK",
                                      "Quebec" = "QC",
                                      "New Brunswick" = "NB",
                                      "Nova Scotia" = "NS",
                                      "Prince Edward Island" = "PE",
                                      "Newfoundland and Labrador" = "NL",
                                      "Yukon" = "YT",
                                      "Northwest Territories" = "NT",
                                      "Nunavut" = "NU")))
colnames(pop) <- c("the_year", "province", "population")

# Add the population for the territories

pop <- pop |> 
  mutate(province = str_replace_all(province, c("YT" = "Territories",
                                                "NT" = "Territories",
                                                "NU" = "Territories"))) |> 
  group_by(the_year, province) |> 
  summarise(population = sum(population)) |> 
  ungroup() |> 
  arrange(the_year, province)
