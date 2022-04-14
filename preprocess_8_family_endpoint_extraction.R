# Defining disease endpoints for parents, for the purpose of RSV risk study.
# Endpoints are prevalence filtered, only the more common endpoints (>10 000) are kept.

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)
library(caret)
library(rms)
library(pROC)
library(DescTools)
library(e1071)
library(quantreg)
# library(leaps)
library(knitr)


# the densified longitudinal endpoint file, containing the first occurrences of each EP

# loading required stuff #####

densified_wide_name <- "densified_first_events_DF8_no_omits_2022-03-17.txt"
ep_dir <- "/data/processed_data/endpointer/"
densified_path <- paste0(ep_dir, densified_wide_name)

# load vector 'omits', updated on 15.2.2022. It is based on FinnGen Endpoint definitions.
load("/home/pvartiai/RSV/data/wrangle/names/omitted_endpoints.R")
# load vector 'retained_enpoints', which contains the names of prevalence- and sanity-fintered endpoints. 
load("/home/pvartiai/RSV/data/wrangle/names/retained_endpoints.R")

# load all study ids
all_ids <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
  select = c("TNRO")) %>% as_tibble


# read densified endpoint file, immediately clear omitted eps
densified <- fread(densified_path) %>% as_tibble 

densified <- densified %>% 
  rename(TNRO = FINREGISTRYID) %>%
  filter(!(ENDPOINT %in% omits))
  

# read family members
sibling_path <- "/data/projects/project_pvartiai/rsv/family/siblings_from_mbr_and_relatives.csv"
mothers_path <- "/data/projects/project_pvartiai/rsv/family/mothers.csv"
fathers_path <- "/data/projects/project_pvartiai/rsv/family/fathers.csv" 

sibs <- fread(sibling_path) %>% as_tibble
mothers <- fread(mothers_path) %>% as_tibble
fathers <- fread(fathers_path) %>% as_tibble


# create endpoint files for family members
mother_eps <- densified %>%
  filter(TNRO %in% mothers$parent_TNRO) %>%
  rename(parent_TNRO = TNRO)

father_eps <- densified %>%
  filter(TNRO %in% fathers$parent_TNRO) %>%
  rename(parent_TNRO = TNRO)

sibs_eps <- densified %>%
  filter(TNRO %in% sibs$sibling_TNRO) %>%
  rename(sibling_TNRO = TNRO)


rm(densified)
gc()

# Keep endpoints occurring BEFORE child's birth

mother_eps_age <- full_join(mother_eps, mothers, by = "parent_TNRO") %>%
  filter(AGE <= mother_age) 

father_eps_age <- full_join(father_eps, fathers, by = "parent_TNRO") %>%
  filter(AGE <= father_age) 

sibs_eps_age <- full_join(sibs_eps, sibs, by = "sibling_TNRO") %>%
  filter(AGE <= sibling_agediff) 


# prevalence screen & rescue endpoints #####
#
# rescue endpoints are a custom list of interesting disease endpoints
# that we want to examine regardless of prevalence. E.g. bronchiolitis is such an example. 

rescue_endpoints <- c("J10_BRONCHIOLITIS",
                      "J10_ASTHMACOPDKELA",
                      "L12_URTICA_ALLERG",
                      "N14_BENIGNMAMDYSP",
                      "N14_HYPERTROPHYBREAST",
                      "N14_LUMPNASBREAST",
                      "N14_BREASTDISOTH",
                      "O15_BREAST_LACT_OTHER_DIS",
                      "N14_INFLAMMBREAST",

                      "E4_CONGEIOD",
                      "E4_HYTHYNAS",
                      # any thyroid? Thyroid medication?
                      "L12_URTICA_ALLERG",
                      "L12_DERMATITISECZEMA",
                      "L12_ERYTHEMANODOSUM")

# exclusion vector, to be used inside prevalence filter function
# maybe move at some point to the actual exclusion.
exclude_these_endpoints <- c(
  "KRA_PSY_ANYMENTAL_SUICID_PREG_NERV_EXMORE", # for some reason causing problems in stepwise filtering. 
  "KRA_PSY_EATING_EXMORE"
  )



#function extracts EP names from the data with > n occurrences
prevalence_screen <- function(data) {
  data %>% 
  group_by(ENDPOINT) %>%
  summarise(n_endpoint = n()) %>%

  # prevalence filter
  filter(n_endpoint > 2000 |
    ENDPOINT %in% rescue_endpoints) %>%

  filter(!(ENDPOINT %in% exclude_these_endpoints)) %>%
  #

  pull(ENDPOINT)
}

#get the endpoint names that occur >500 times in relative groups.
# We want to keep the same endpoint list for both parents, so that we can compare them.

mother_prev_ep_names <- prevalence_screen(mother_eps_age)
father_prev_ep_names <- prevalence_screen(father_eps_age)

parent_eps <- c(mother_prev_ep_names, father_prev_ep_names) %>% unique()

# prevalence-screen sibling endpoints separately. 
# siblings have different diseases in a different timeline. 
sibs_prev_ep_names <- prevalence_screen(sibs_eps_age)


# iterative step: we have manually created a 'retained_endpoints' character vector,
# where prevalence filtered endpoints are manually chekced (reducing the number from 660 to 330)
load("/home/pvartiai/RSV/data/wrangle/names/retained_endpoints.R")


# filter more rare endpoints away, and exclude extra variables from endpoint datasets.
# in the second iteration, the filtering is done with 'retained_endpoints' (instead of parent_eps)

mother_ep <- mother_eps_age %>%
  filter(ENDPOINT %in% c(retained_endpoints, rescue_endpoints)) %>%
  filter(!(ENDPOINT %in% exclude_these_endpoints)) %>%
  select(TNRO, parent_TNRO, ENDPOINT)
 
father_ep <- father_eps_age %>%
  filter(ENDPOINT %in% c(retained_endpoints, rescue_endpoints)) %>%
  select(TNRO, parent_TNRO, ENDPOINT)

sibling_ep <- sibs_eps_age %>%
  filter(ENDPOINT %in% sibs_prev_ep_names) %>%
  filter(!(ENDPOINT %in% exclude_these_endpoints)) %>%
  select(TNRO, sibling_TNRO, ENDPOINT)




rm(mother_eps_age, father_eps_age, sibs_eps_age)
gc()

# save in long and wide formats

# long

setwd("/data/projects/project_pvartiai/rsv/family/")

write.csv(mother_ep,
          "mother_endpoints_all_longitudinal_15022022.csv",
          row.names = FALSE)

write.csv(father_ep,
          "father_endpoints_longitudinal_15022022.csv",
          row.names = FALSE)

write.csv(sibling_ep, "sibling_endpoints_all_longitudinal_15022022.csv",
          row.names = FALSE)


# wide

# all_ids loaded

##########

# mother_ep <- fread("/home/pvartiai/RSV/data/predictors/family_endpoints/mother_endpoints_all_longitudinal_15022022.csv")
# father_ep <- fread("/home/pvartiai/RSV/data/predictors/family_endpoints/father_endpoints_all_longitudinal_15022022.csv")
# sibling_ep <- fread("/home/pvartiai/RSV/data/predictors/family_endpoints/sibling_endpoints_all_longitudinal_15022022.csv")

# all_ids <- fread("/home/pvartiai/RSV/data/id_and_basic_vars_all_study_patients.csv", 
#                 select = c("TNRO")) %>% as_tibble

setwd("/data/projects/project_pvartiai/rsv/predictors")

mother_ep_wide <- mother_ep %>%
  select(TNRO, ENDPOINT) %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = ENDPOINT,
              values_from = temp,
              values_fill = 0)

mother_ep_wide <- left_join(all_ids, mother_ep_wide, by = "TNRO") 
mother_ep_wide[is.na(mother_ep_wide)] <- 0

write.csv(mother_ep_wide,
          "mother_endpoints_wide_15022022.csv",
          row.names = FALSE)

rm(mother_ep_wide)
gc()



father_ep_wide <- father_ep %>%
  select(TNRO, ENDPOINT) %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = ENDPOINT,
              values_from = temp,
              values_fill = 0)

  father_ep_wide <- left_join(all_ids, father_ep_wide, by = "TNRO") 
  father_ep_wide[is.na(father_ep_wide)] <- 0


  write.csv(father_ep_wide,
          "father_endpoints_wide_15022022.csv",
          row.names = FALSE)

rm(father_ep_wide)
gc()

# pivot sibling endpoints to wide format
sib_ep_wide <- sibling_ep %>%
  select(TNRO, ENDPOINT) %>%
  unique() %>%
  mutate(temp = 1) %>%
  pivot_wider(names_from = ENDPOINT,
              values_from = temp,
              values_fill = 0)

  sib_ep_wide <- left_join(all_ids, sib_ep_wide, by = "TNRO")
  sib_ep_wide[is.na(sib_ep_wide)] <- 0 

# load sibling rsv outcome
sib_outcome <- fread(paste0(pred_path, "sibling_rsv_hospitalization.csv")) %>%
  as_tibble

# join custom endpoint (rsv hospitalization) to all other endpoints
sib_ep_wide <- left_join(sib_ep_wide, sib_outcome, by = "TNRO")


# mutate "J10_bronchiolitis" ep
sib_ep_wide <- sib_ep_wide %>%
  mutate(other_j21_endpoint = ifelse(J10_BRONCHIOLITIS == 1 & 
                                     sib_rsv_hospitalization == 0, 1, 0)) %>%
  select(-J10_BRONCHIOLITIS) 


  write.csv(sib_ep_wide, "sibling_endpoints_wide_15022022.csv",
          row.names = FALSE)



# sibling rsv hospitalizations, join this to previous endpoints
sib_outcome <- fread(paste0(pred_path, "sibling_rsv_hospitalization.csv")) %>%
  as_tibble















