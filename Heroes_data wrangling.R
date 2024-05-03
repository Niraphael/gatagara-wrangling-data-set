# Preparing a policy brief for Hero's school
# February 12, 2023
# Data wrangling

# Setting working directory

wd = "C:/R/Heroes"
data_wd = paste0(wd,"/data")
output_wd = paste0(wd,"/output")

# Load needed libraries
library(readxl)
library(tidyverse)
library(tidyr)
library(naniar)
# TO Clear environment
#rm(list = ls())

# Upload the data

setwd(data_wd)
data_raw1 = read_excel("children with disabilities who are studying in Heroes Inclusive Centre.xlsx")
data_raw2 = read_excel("ABANA BAFITE UBUMUGA BATIGA.xlsx")

#Renaming variable
names(data_raw)[names(data_raw1)=="Names of child"] <- "Names"
names(data_raw)[names(data_raw2)=="Names of child"] <- "Names"

# Remove duplicated observations
data_final = data_raw %>% 
  distinct(Names, .keep_all = TRUE)

# Correct answers from the database

data_final = data_final %>% 
  mutate(type_dis = case_when(type_handicap == "Autisme" | type_handicap == "Autistism" ~ "Autism",
                              type_handicap == "C.P" | type_handicap == "C.P/MR(Mental Retardation" ~ "CP",
                              type_handicap == "Down Sydrome" ~ "Down Sydrome",
                              TRUE ~"Other"),
         ubudehe =case_when(Ubudehe_cat == "2"|Ubudehe_cat == "Two" |Ubudehe_cat == "second"~ "Second",
                            Ubudehe_cat == "third"~ "Third",
                            TRUE ~ Ubudehe_cat),
         edu_fath = case_when(educ_father == "None" | educ_father == "primary"  ~ "Primary",
                              educ_father == "Secondary (OL)"|educ_father == "Sec" | educ_father == "sec"  ~ "Secondary",
                              educ_father == "Higher educat"|educ_father == "higher educa"| educ_father == "higher edu" |
                                educ_father == "higher educ" | educ_father == "higher Educationhigh"  ~ "Higher Education",
                              TRUE ~ educ_father),
         edu_moth = case_when(educ_mother == "None" | educ_mother == "primary"  ~ "Primary",
                              educ_mother == "Secondary (OL)"|educ_mother == "Sec" | educ_mother == "A2" |
                                educ_mother == "sec" | educ_mother == "Secondary(OL)"  ~ "Secondary",
                              educ_mother == "Higher educat"|educ_mother == "higher educa" | 
                                educ_mother == "higher educ"  ~ "Higher Education",
                              TRUE ~ educ_mother),
         occup_fath = case_when(occ_father == "None"|occ_father == "No Work" ~ "Unemployed",
                               TRUE ~ "Employed"),
         occup_moth = case_when(occ_mother == "None"|occ_mother == "No Work" ~ "Unemployed",
                               TRUE ~ "Employed"),
         mar_stat = case_when(Family_Status == "living together"|Family_Status == "married" |Family_Status == "Married"|
                                Family_Status == "Married (Legal)"| Family_Status == "married (Legal)higher"|
                                Family_Status == "marrried"~ "Partnered",
                              Family_Status == "Divorced"|Family_Status == "not married" |Family_Status == "Single"|
                                Family_Status == "single"|Family_Status == "Widow"~ "Living alone"),
         days = as.numeric(str_remove(str_remove(missed_days, "days"),"day")),
         absent= case_when(days>=0 & days <=5 ~ 0,
                           days >5 ~ 1))

data_final$occup_fath[is.na(data_final$occ_father)] = NA
data_final$occup_moth[is.na(data_final$occ_mother)] = NA
data_final$ubudehe[data_final$ubudehe=="None"] = NA

data_reduced = data_final %>% 
  select(type_dis, Sex, ubudehe, mar_stat, edu_fath, edu_moth, occup_fath, occup_moth, days, absent)

data_absent = data_reduced %>% 
  filter(absent == 1)

save(data_absent, file = "Absent.RData")

save(data_reduced, file = "All_type.RData")

autism = data_reduced %>% 
  filter(type_dis == "Autism")

save(autism, file = "Autism.RData")

DS = data_reduced %>% 
  filter(type_dis == "Down Sydrome")

save(DS, file = "DS.RData")

CP = data_reduced %>% 
  filter(type_dis == "CP")

save(CP, file = "CP.RData")




