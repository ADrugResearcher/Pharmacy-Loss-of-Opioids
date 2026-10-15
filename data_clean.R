# How the data was cleaned 
# Included for those curious about changes made from the initial dataset

`%notin%` <- Negate(`%in%`)
library(readxl)
library(tidyverse)

#read file into R
#Data file
# Original CBC investigation Data
pharm <- read_excel("Drugs_Data.xlsx") |> 
  rowid_to_column("id")


pharm <- pharm |> 
  mutate(SUBMISSION_SUBSTANCE_DATA_QUANTITY = as.numeric(SUBMISSION_SUBSTANCE_DATA_QUANTITY))

# NOTE:
# ATIP Request to Health Canada made in 2025 for 2024 data
# At the time of the request HC redacted substantive information available 
# in the earlier data. In the 2024 data I determined that the 3 Territories 
# have been removed in toto from the data given their low n of incidents.
# More infuriating is that HC redacted the subtype in incidents of theft
# HC argued that with the previous dataset, this new one and 
# increased press attention towards the issue, it might be possible 
# for one to deanonymize pharmacies. I don't see how, but that changes
# what can be explored between the earlier and latter data.

pharm24 <- read_excel("pharma_2024.xlsx") |> 
  mutate(LOSS_ID = LOSS_ID2) |> 
  select(-LOSS_ID2) |> 
  mutate(LOSS_ID = LOSS_ID+ max(pharm$LOSS_ID)) |> 
  rowid_to_column("id") |> 
  mutate(id = id+ max(pharm$id))
# need similar colnames R being weird
new_colnames <- c("id", "loss_id", "province",
                  "date_loss", "report_date",
                  "brandname", "generic",
                  "quant", "unit", 
                  "substance_data", 
                  "subs_quant","facility",
                  "incident_type",
                  "incident_subtype",
                  "incident_other")
# Columns too bulky
colnames(pharm) <- new_colnames
colnames(pharm24) <- new_colnames

pharm24 <- pharm24 |> 
  mutate(province = str_to_title(province))
#merge CBC data with 2024 ATIP
pharm <- pharm |> 
  bind_rows(pharm24)

pharm <- pharm |> 
  mutate(incident_subtype = ifelse(
    str_detect(incident_subtype, "Redact"), "Redacted",
    incident_subtype)) |> 
  mutate(province = case_when(
    province == "British Columbia"~ "BC",
    province == "Alberta" ~ "AB",
    province == "Ontario" ~"ON",
    province == "Manitoba" ~ "MB",
    province == "Saskatchewan" ~ "SK",
    province == "Quebec" ~ "QC",
    province == "New Brunswick" ~ "NB",
    province == "Nova Scotia" ~ "NS",
    province == "Prince Edward Island" ~ "PE",
    province == "Newfoundland And Labrador" ~ "NL",
    province %in% c(
      "Yukon", "Northwest Territories",
      "NT", "Nunavut", "NU", "YT") ~ "Territories",
    str_detect(province, "Redact") ~ "Territories",
    .default = as.character(province)
  ))


####-------------------------Cleaning Time---------------------------------####

#Fixing the date to standard y-m-d
class(pharm$date_loss)
pharm <- pharm |> 
  mutate(across(contains("date"), ymd)) |> 
  mutate(across(all_of(c("facility", "brandname", "generic")), str_to_title),
         across(starts_with("incident"), str_to_title))

# I want to an a drug class column (e.g., opioids, stimulants, etc.) to
# the data - this makes it easier to filter, as well as a good colour variable
# This will allow us to match HC Drug Tables - picked via a bit of trial and error
pharm_new <- pharm |> 
  #filtering here to 2018 will speed things up
  filter(date_loss >= ymd("2018-01-01")) |> 
  #This will allow us to match the HC Data
  mutate(generic= case_when(
    # HC includes heroin, but not diacetylmorphine - so we'll fix that there
    str_detect(generic, "[Bb]uprenorphine") ~ "Buprenorphine",
    str_detect(generic, "[Aa]mph") &
      !str_detect(generic, "Lisdex|Dextro") ~ "Amphetamine",
    str_detect(generic, "Lisdexamphetamine") ~ "Lisdexamfetamine",
    is.character(generic) ~ generic
  )) |> 
  mutate(generic = str_remove_all(generic, "\"")) |> 
  mutate(generic_match = str_extract(generic, "^[0-9A-Za-z\\-]+"),
         month_loss = floor_date(date_loss, unit = "months"))

#For the precursor chemicals/drugs that are 2 words
two_word_generic <- c("Acetone", "Methyl", "Hypophosphorous", 
                      "White", "7-Keto", "Sodium", "potassium", "Acetic")
pharm_new <- pharm_new |> 
  mutate(generic_match = str_replace_all(generic_match, 
                                         c("Diamorphine" = "Diacetylmorphine",
                                           "Esketamine" = "Ketamine"
                                         ))) |> 
  mutate(generic_match = ifelse(generic_match %in% two_word_generic, 
                                generic, generic_match))
# HC Drug Categorization Table
source("drug_class.R")
# Joining the drug class to the data
pharm_new <- pharm_new |> 
  left_join(drug_class, by = c("generic_match" = "drug_name"))



####--------------------Data organizing/recoding--------------------------####
pharm_new <- pharm_new |> 
  select(id, province, month_loss, brandname, generic_match,
         quant, unit, substance_data, subs_quant, facility,
         incident_type, incident_subtype, incident_other,
         pharma_class, specific_class) |>
  filter(
           # I've limited the cases here to remove either the 0mg entries
           # or where half a pill/ML etc. goes missing
           quant >= 1) |> 
  mutate(grouped_cat = case_when(
    quant > 0 & quant <100 ~      "1-99",
    quant >= 100 & quant < 500 ~   "100-499",
    quant >= 500 & quant < 1000 ~ "500-999",
    quant >= 1000 & quant < 1500 ~ "1000-1499",
    quant >= 1500 & quant < 2000 ~ "1500-1999",
    quant >= 2000 ~ "2000+",
    .default = as.character(quant)
    # One limit here is that 2000+ actually downplays how big
    # Some of these losses/thefts are (they can be up to 15K pills)
    # But most of the data is actually small quantities (check the histogram)
  )) |> 
  mutate(the_year = floor_date(month_loss, unit = "years")) |> 
  # The data distinguishes between Hospital & non hospital LTC - but the n is small
  mutate(facility = ifelse(str_detect(facility, "Long Term"),
                           "All LTC", 
                           facility))

pharm_new <- pharm_new |> 
  mutate(incident_subtype = ifelse(str_detect(incident_subtype,
                                              "Loss Unexplained"),
                                   "Loss Unexplained",
                                   incident_subtype))

# Turning the grouped category above into an ordered factor g
pharm_new$grouped_cat <- factor(pharm_new$grouped_cat, 
                                levels = c("1-99",
                                           "100-499",
                                           "500-999",
                                           "1000-1499",
                                           "1500-1999",
                                           "2000+"),
                                ordered = T)
levels(pharm_new$grouped_cat)

# Some of the data hasn't been filled out properly, so this is a quick fix
pharm_new$substance_data[which(is.na(pharm_new$substance_data))] <- 
  str_extract(pharm_new$brandname[which(is.na(pharm_new$substance_data))], 
              "[0-9]+ Mg/Tab")
# To see which facilities are most represented
plyr::count(pharm_new$facility) |> 
  arrange(-freq)
# I've limited it to facilities with large quantities
tosearch <- c("Pharmacy", "Precursor Institution", 
              "Hospital", "Licensed Dealer - Ocs",
              "Dealer Producer", "All LTC")
####---------------- All Data Visualization (plot 3) -----------------------####

play <- pharm_new |> 
  select(id, province, the_year, generic_match, pharma_class,
         incident_type, incident_subtype,
         subs_quant, unit) |>
  mutate(incident_subtype = ifelse(str_detect(incident_subtype, 
                                              "Loss Unexplained"),
                                  "Loss Unexplained",
                                   incident_subtype)) |> 
  mutate(generic_match = 
           ifelse(str_detect(
             generic_match, "[Aa]mphetamine") &
               !str_detect(generic_match, "Methamphetamine|Lisdexamfetamine"),
             "Dextro/amphetamine", generic_match),
         incident_type = ifelse(incident_type == "Loss", "Lost",
                                incident_type),
         incident_subtype = ifelse(str_detect(incident_subtype, "Loss Unexplained"),
                                   "Loss Unexplained",incident_subtype)) |> 
  rename(incident_id = id) |> 
  rowid_to_column("id")

play2 <- play |> 
  select(-subs_quant, -incident_id, -id, -generic_match, 
         -incident_type, -incident_subtype) |> 
  pivot_longer(cols = -c(province, the_year)) |> 
  group_by_all() |> 
  summarise(the_count = n()) |> 
  ungroup() |> 
  mutate(name = str_replace_all(name, c(
    "pharma_class" = "Pharmaceutical Class",
    "unit" = "Unit of Measure"
  )))
p_lt <- play |> 
  select(the_year, incident_id, province, 
         incident_type, incident_subtype) |> 
  pivot_longer(cols = -c(province, incident_id,
                         the_year, incident_type)) |> 
  distinct(.keep_all = T) |> 
  select(-incident_id) |>
  group_by_all() |> 
  summarise(the_count = n()) |> 
  ungroup() |> 
  mutate(name = case_when(
    incident_type == "Lost" ~ "Lost Subtype",
    incident_type == "Theft" ~ "Theft Subtype",
  )) |> 
  select(-incident_type)


 
pharm_new <- pharm_new |> 
  filter(facility %in% tosearch
        ) |> 
  mutate(substance_data = str_to_title(substance_data),
         subs_quant = as.numeric(subs_quant)) |> 
  mutate(unit_type = as.numeric(str_extract(substance_data,
                                            "[0-9]+\\.*[0-9]*"))) |>
  arrange(unit_type) |> 
  mutate(substance_data = factor(substance_data),
         # standardized factor levels as integers
         unit_type = ifelse(unit_type == 2.5, 3, unit_type)) |> 
  filter(pharma_class %in% c("Sedatives/ Hypnotics", "Stimulants",
                             "Opioids","Dissociatives"))


pharm_new$substance_data <- factor(pharm_new$substance_data, 
                                levels= 
                                  unique(pharm_new$substance_data[order(
                                    pharm_new$unit_type)]), 
                                ordered=TRUE)

source("pop_cleaner.R")
save(pharm_new, pop, play2, file = "pharm_new.RData")
pharm_clean <- pharm_new |> 
  left_join(pharm[, c(1,2,6)], by = "id") |> 
  mutate(id = loss_id) |> 
  select(!c(brandname.y, loss_id)) |> 
  rename(brandname = brandname.x) |> 
  rename(loss_id = id)

pharm_clean <- pharm_clean[, -c(14:18)]
write.csv(pharm_clean, "cleaned_pharm_loss.csv")
rm(list=ls())
