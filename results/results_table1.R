
library(dplyr)
library(tidyr)

library(tibble)
library(data.table)
library(arsenal)


# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"

# need validation data also


# training data with non-feasible variables filtered out
compact_data <- fread(paste0(predict_datapath, 
                             "development_set_only_feasible_variables.csv")) %>%
    as_tibble %>%
    # add birth month variable to the predict data 
    mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) 


# # load composite data (Pre-AIC-data) if needed
# composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
#   as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble


validation_data <- fread("/data/projects/project_pvartiai/rsv/validation_data/validation_set_regression.csv") %>%
    as_tibble()


## names
# keylist_final
load("/data/projects/project_pvartiai/rsv/functions/keylist_final_variable_names.R")


    
all_data <- bind_rows(compact_data, validation_data)


table_data <- all_data %>%
  filter(!is.na(gest_days)&!is.na(weight_sd)) %>%
  mutate(gw_cat = case_when(
    gest_days/7 >= 37 ~ "Term (>37)",
    gest_days/7 < 37 & gest_days/7 >= 33 ~ "33-37",
    gest_days/7 < 33 & gest_days/7 >= 29 ~ "29-33",
    gest_days/7 < 29 ~ "<29"
  )) %>%
  mutate(weight_cat = case_when(
    weight_sd < -2 ~ "<-2 SD",
    weight_sd >= -2 & weight_sd <= 2 ~ "Within 2SD",
    weight_sd > 2 ~ ">+2 SD",
    is.na(weight_sd) ~ NA_character_
  )) %>%
  mutate(across(c(outcome, 
                  male_gender, 
                  weight_cat, 
                  twin, 
                  sib_0_4, 
                  sib_4_7, 
                  down, 
                  sib_resp_hosp,
                  smoking_neodg,
                  term_breathing,
                  q39_confirmed, 
                  any_family_asthma, 
                  any_severe_chd, 
                  asd_or_vsd_only), .fn = ~ as.factor(.)))



mycontrols  <- tableby.control(test=FALSE, 
                               total=TRUE,
                               numeric.stats=c("meansd"),
                               cat.stats=c("countpct"),
                               cat.simplify = TRUE)

tab_obj <- table_data %>%
  tableby(data = .,
          control = mycontrols,
          ~ outcome + gw_cat + weight_cat + mother_age +  male_gender + 
            twin + sib_0_4 + sib_4_7 + down + sib_resp_hosp + smoking_neodg + 
            term_breathing + q39_confirmed + any_family_asthma + any_severe_chd + asd_or_vsd_only)

keylist_final

tab_obj %>%
  summary(text = NULL,
          digits.pct = 2) %>% as.data.frame %>% 
  rename(feature = 1) %>%
  left_join(., keylist_final) %>% 
  select(longname, feature, "Overall (N=1257515)") %>%
  View()

