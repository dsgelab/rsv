# preprocess: 
# defining basic vars and splitting into development and validation dataset. 

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

pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
fam_path <- "/data/projects/project_pvartiai/rsv/family/"



# S T A N D A R D     D E V I A T I O N S      O F      H E I G H T     A N D     W E I G H T 

mbr_c <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv") %>%
	as_tibble

all_bds <- mbr_c %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)
  

# read the data and replace commas with dots as decimal separator
sd_ref <- fread("/data/projects/project_pvartiai/rsv/refs/syntymakoko_ref.csv") %>%
	as_tibble() %>%
	mutate(across(everything(),
		gsub,
		pattern = ",",
		replacement = ".")) %>%
	mutate(across(everything(), as.numeric))


# 363000 head circumferences missing. Will exclude that. 
# 3000 lengths and 1000 weights missing.
# how is sex coded? 1 = male, 2 = female. In our data, only values are 1 and 2 (no unclears)
sd_data <- mbr_c %>%
	select(TNRO, gest_weeks, SUKUP, twin, SYNTYMAPAINO, SYNTYMAPITUUS, twin) %>%
	mutate(gest_weeks = round(gest_weeks, digits = 2)) %>%
	mutate(sex_is_boy = ifelse(SUKUP == 1, 1, 0),
		singleton = +(!twin)) %>%
	select(-SUKUP, -twin) %>%
	select(TNRO, gest_weeks, sex_is_boy, singleton, SYNTYMAPAINO, SYNTYMAPITUUS)



sd_data <- left_join(sd_data, sd_ref, by = c("gest_weeks", "singleton", "sex_is_boy"))

## testing
sd_data %>%
	mutate(weight_sd = (SYNTYMAPAINO - mu_paino)/sigma_paino,
		height_sd = (SYNTYMAPITUUS - mu_pituus)/sigma_pituu) %>%
	filter(weight_sd > 20)
## / testing / ##

sd_values <- sd_data %>%
	mutate(weight_sd = (SYNTYMAPAINO - mu_paino)/sigma_paino,
		height_sd = (SYNTYMAPITUUS - mu_pituus)/sigma_pituu) %>%
	select(TNRO, weight_sd)


# sanity check for sd values
# there are individual cases with weight 10+SD:s. Let's code them as missing
sd_values %>%
	filter(weight_sd > 8) 

sd_values <- sd_values %>%
	mutate(weight_sd = ifelse(weight_sd > 10, NA_real_, weight_sd)) 



setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(sd_values, "birth_size_sd_values.csv",
	row.names = FALSE)





# B A S E    P R E D I C T O R S 

# birth month, gest_weeks, sukupuoli, basic stuff
basic <- mbr_c %>%
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) %>%
  select(TNRO, outcome, outcome_3days, 
         period_duration, gest_days, birth_month, 
         SUKUP, twin, LAPSEN_SYNTYMAPVM) %>%
  # gender
  mutate(female_gender = as.numeric(SUKUP - 1)) %>%
  #invert gender for male gender
  mutate(male_gender = as.numeric(!female_gender)) %>% 
  # exclude old variables
  select(-SUKUP, -female_gender) %>%
  rename(outcome_duration = period_duration)


# D I S T A N C E    T O     N E X T   E P I D E M I C S 

# define the  epidemic peak during years 2007-2020 

# # infectious disease registry
# ttr_rsv <- fread("/data/processed_data/thl_infectious_diseases/infectious_diseases_2022-01-19.csv") %>% 
# 	as_tibble %>%
# 	select(c(
#   TNRO, birth_year, diagnosis_method, hospital_district, laboratory_type, microbe, reporting_group,
#   original_hospital_district, sampling_date, recording_week, zip_code
# )) %>%
#   filter(reporting_group == "['RSV']")


# define the epidemic peaks for each year
peaks <- mbr_c %>%
  select(outcome, TUPVA) %>%
  filter(outcome == 1) %>% 
  mutate(outcome_year = year(TUPVA),
         outcome_month = month(TUPVA)) %>%
  mutate(epidemic_year = ifelse(outcome_month <= 5, outcome_year, (outcome_year+1)))  %>%
  group_by(epidemic_year, outcome_month) %>%
  summarise(n = n()) %>%
  ungroup %>%
  group_by(epidemic_year) %>%
  slice_max(n, with_ties = FALSE) %>%
  filter(epidemic_year < 2021) %>%
  mutate(outcome_month = ifelse(outcome_month == 12, 0, outcome_month))


our_guess <- NULL
next_peak <- NULL
for (i in 2007:2020) {
  
  index <- which(grepl(i, peaks$epidemic_year))
  
  old_peak <- c(peaks$outcome_month[index-2],
                 peaks$outcome_month[index-4],
                 peaks$outcome_month[index-6])
  
  our_guess[index] <- (mean(old_peak) %>% round(digits = 0))
  
  next_guessed_peaks <-  c(peaks$outcome_month[index-1],
                          peaks$outcome_month[index-3],
                          peaks$outcome_month[index-5])
  
  next_peak[index] <- (mean(next_guessed_peaks) %>% round(digits = 0))
  
}

peaks$our_guess <- our_guess
peaks$next_peak <- next_peak
peaks


setwd("/data/projects/project_pvartiai/rsv/results/plots/")
write.csv(peaks, "epidemic_peaks_and_guesses.csv",
          row.names = FALSE)




### Distance from epidemic peak

## the peaks
# actual peaks 1998 - 2017
actual <- c(0,4,2,4,1,4,1,5,0,5,2,4,3,4,2,3,3,4,2,3)
# guess 2018, 2019, 2020
guess <- c(2, 4, 2)
# Actual peaks of 2018-2020
# c(1, 4, 2)
# actual plus guess
actual_plus_guess <- c(actual, guess)
next_actual_plus_guess <- c(4,2,4,1,4,1,5,0,5,2,4,3,4,2,3,3,4,2,3,2,4,2,4)



# bases on the average of the peaks 2 years and 4 years ago 
best_guess <- data.frame(
  epidemic_year = 1998:2020,
  peak_month = actual_plus_guess,
  next_peak = next_actual_plus_guess)


# select relevant variables, some are for checking purposes
epidem_years <- basic %>%
  mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
  mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1))) %>%
  # mutate(even_epidemic_year = ifelse(epidemic_year %% 2 == 0, 1, 0)) %>%
  select(TNRO, birth_month, birth_year, epidemic_year)


dist_to_next <- epidem_years %>%
  left_join(., best_guess, by = "epidemic_year") %>%
  
  # this works for those born at same year before epidemic
  mutate(dist = peak_month - birth_month) %>%
  
  # this works for those born previous year before the epidemic peak
  mutate(dist = ifelse((birth_year + 1 == epidemic_year & dist < 0), # condition 
                       12 - birth_month + peak_month, # if yes
                       dist)) %>% # if no
  
  # This works for those born during the same epidemic but after the peak
  mutate(dist = ifelse(birth_year == epidemic_year & dist < 0,
                       12 - birth_month + next_peak,
                       dist)) %>%
  select(TNRO, dist)







### chds for base predictors 
chd_path <- paste0(pred_path, "congenital_heart_defects.csv")
common_chd_path <- paste0(pred_path, "common_benign_congenital_heart_defects.csv")

# all variables are relevant in the new chd dataset
chd_basic <- fread(chd_path) %>% 
	as_tibble 



### BPD 
bpd_path <- paste0(pred_path, "bpds.csv")
bpd <- fread(bpd_path) %>% as_tibble

# sisarukset, määrä ja outcome [x]
sib_path <- paste0(pred_path, "number_of_siblings.R")
sibs <- fread(sib_path) %>% as_tibble %>% select(-have_sibs)
sibs <- left_join(select(all_bds, TNRO), sibs)
sibs <- sibs %>%
	mutate(across(everything(), ~ ifelse(is.na(.x), 0, .x)))




### down 
down_path <- paste0(pred_path, "down_syndrome_1mo.csv")
down <- fread(down_path) %>% as_tibble

# vanhempien iät [x]
mo_path <- paste0(fam_path, "mothers.csv")
fa_path <- paste0(fam_path, "fathers.csv")
mo_age <- fread(mo_path, select = c("TNRO", "mother_age")) %>% as_tibble
fa_age <- fread(fa_path, select = c("TNRO", "father_age")) %>% as_tibble

# sds
sd_values


# join everything for a development set
base_sets <- as.list("basic", "chd_basic", "bpd", "sibs", "down", "mo_age", "fa_age", "sd_values")
base_vars <- all_bds %>% select(TNRO)
base_vars <- left_join(base_vars, basic, by = "TNRO")
base_vars <- left_join(base_vars, dist_to_next, by = "TNRO")
base_vars <- left_join(base_vars, sd_values, by = "TNRO")
base_vars <- left_join(base_vars, chd_basic, by = "TNRO")
base_vars <- left_join(base_vars, bpd, by = "TNRO")
base_vars <- left_join(base_vars, sibs, by = "TNRO")
base_vars <- left_join(base_vars, down, by = "TNRO")
base_vars <- left_join(base_vars, mo_age, by = "TNRO")
base_vars <- left_join(base_vars, fa_age, by = "TNRO")







# B I R T H    R E G I S T R Y    S T R U C T U R A L      V A R I A B L E S

# section [x]
mbr_vars <- mbr_c %>% 
	select(TNRO, c_section_birth)

# # mother tongue will not be included to the prediction work
moto <- fread(paste0(pred_path, "mother_tongue.csv")) %>% as_tibble


# common chds for testing
common_chd_path <- paste0(pred_path, "common_benign_congenital_heart_defects.csv")
chd_common <- fread(common_chd_path) %>%
	as_tibble


# join datasets
mbr_structural <- all_bds %>% select(TNRO)
mbr_structural <- left_join(mbr_structural, mbr_vars, by = "TNRO")
mbr_structural <- left_join(mbr_structural, chd_common, by = "TNRO")
mbr_structural <- left_join(mbr_structural, moto, by = "TNRO") %>%
	  # create dummies of mother tongue
  mutate(mo_to_other_fisve = ifelse(mother_tongue %in% c("fi", "sv"), 0, 1)) %>%
  select(-mother_tongue)


# duplicated ids
dupl_ids_struct <- mbr_structural %>%
  filter(duplicated(TNRO)) %>%
  pull(TNRO)


mbr_structural <- mbr_structural %>%
  filter(!(TNRO %in% dupl_ids_struct) | mo_to_other_fisve == 1)

# save
setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(mbr_structural, "mbr_structural_vars.csv",
	row.names = FALSE)





# development and validation set dates

development_set <- base_vars %>%
    filter(LAPSEN_SYNTYMAPVM < as.Date("2017-06-1")) 

setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(development_set, "development_set.csv",
	row.names = FALSE)



validation_set <- base_vars %>%
    filter(LAPSEN_SYNTYMAPVM >= as.Date("2017-6-1")) 
    
setwd("/data/projects/project_pvartiai/rsv/validation_data/")
write.csv(validation_set, "validation_set.csv",
	row.names = FALSE)




