# CUSTOM ENDPOINTS

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



# load development set and birth dates

pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"
dev_path <- paste0(pred_path, "development_set.csv")

# load only development set, containing basic vars
development_set <- fread(dev_path) %>% as_tibble

# load id and birth date set
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble





# vectors of all the important variable names

# drugs
all_possible_drugs <- c(
  #  "J01CE", # penicillin
  "valproate",
  "N05AH",
  "kefexin",
  "opioids",
  "inhaled_steroids",
  "macrolide",
  "inhaled_opening_drugs",
  "N06AX",
  "montelucast",
  "po_cortison",
  "adrenaline",
  "M01AE",
  "N02BE",
  "N06AB",
  "common_uti_ab_amorion"
)

cool_fa_drugs <- all_possible_drugs %>%
  setdiff(., c("montelucast", "M01AE", "fluorocinolone", "N02BE"))

cool_mo_drugs <- all_possible_drugs %>%
  setdiff(., c("montelucast", "M01AE", "fluorocinolone", "N02BE"))

cool_preg_drugs <- all_possible_drugs

cool_sib_drugs <- all_possible_drugs %>%
  setdiff(., c("benz", "N05AH", "N06AX", "M01AE", "N02BE", "N06AB"))




# structural variables from birth registry

cool_birthreg_struct <- c(
  "c_section_birth",
  "only_vsd",
  "only_common_asd"
)


# sibling end points

cool_sib_ep <- c(
  'sib_resp_hosp',
  'sib_asthma_dg',
  'other_j21_endpoint',
  'J10_ASTHMACOPDKELA',
  'CHILDHOOD_ALLERGY',
  'K11_REFLUX',
  'ALLERG_RHINITIS',
  "L12_URTICARIA",
  'F5_BEHEMOCHILD',
  'F5_BEHAVE',
  "L12_ATOPIC",
  'AB1_INTESTINAL_INFECTIONS'
)





# neonatal diagnoses

cool_neodg <- c(
  "smoking",
  "Q39",
  "P961",
  "P58",
  "P24",
  "term_rds",
  "premie_rds",
  "P04_other",
  "O354",
  "P044",
  "O40",
  "P22_other_unsp",
  "O342",
  "O355",
  "P70",
  "P221",
  "P94",
  "Q67_68",
  "O990",
  "O993",
  "O995"
)

# all_possible_neodg <- c(
#   "Q39",
#   "P961",
#   "P58",
#   "P24",
#   "term_rds",
#   "premie_rds",
#   "P04_other",
#   "smoking",
#   "O354",
#   "O355",
#   "P044",
#   "O40",
#   "P22_other_unsp",
#   "O209",
#   "O342",
#   "P70",
#   "P221",
#   "P94",
#   "Q67_68",
#   "O2862",
#   "O990",
#   "O993",
#   "O995",
#   "O9980"
# )

# for the prediction model


# parents disease endpoints

# all_possible_parent_ep <- c(
#   'O15_BREAST_LACT_OTHER_DIS',
#   'F5_OTHERSUB',
#   'E4_CONGEIOD',
#   'F5_SEDAHYP',
#   'F5_SUBSNOALCO',
#   'F5_CANNABIS',
#   'AB1_VIRAL_HEPATITIS',
#   'L12_URTICA_ALLERG',
#   'K11_CARIES_DENTIN',
#   'F5_BIPO',
#   'F5_BULIMIA',
#   'F5_PSYCHOTH',
#   'N14_HYPERTROPHYBREAST',
#   'K11_REFLUX',
#   'F5_SOMATOFORM',
#   'F5_EMOPER',
#   'J10_LOWERINF',
#   'F5_KELAMENT',
#   'G6_MIGRAINE_WITH_AURA_TRIPTAN',
#   'L12_CELLULITIS',
#   'G6_HEADACHE',
#   'VWXY20_INTENTI_SELF_P_EXPOS_OTHER_UNSPE_CHEMIC_NOXIO_SUBST',
#   'K11_PULP_PERIAPICAL',
#   'K11_OESSTODUO',
#   'ALCOHOLACUTE10',
#   'F5_PHOBANX',
#   'G6_MIGRAINE_NO_AURA_TRIPTAN',
#   'F5_STRESSOTH',
#   'J10_CHRONRHINITIS',
#   'VWXY20_SUICI_OTHER_INTENTI_SELF_H',
#   'L12_PAPULOSQUAMOUS',
#   'E4_PCOS',
#   'J10_LOWCHRON',
#   # 'RX_CROHN_1STLINE', # glucocorticoids
#   'F5_DEPRESSIO',
#   'N14_INFLUTH',
#   # 'K11_IBS',
#   'N14_URETHRAOTH',
#   'G6_MIGRAINE',
#   'N14_VAGINITIS',
#   'F5_ANXIETY',
#   'J10_ASTHMACOPDKELA',
#   'F5_MOOD',
#   'N14_URINOTH',
#   'F5_PANIC',
#   'ALCOHOL_RELATED',
#   'F5_ALLANXIOUS',
#   'G6_SLEEPAPNO',
#   'E4_OVARDYS',
#   # 'ANTIDEPRESSANTS',
#   'L12_OTHERSKINSUBCUTIS',
#   'J10_INFLUPNEU',
#   'K11_INTESTOTH',
#   'O15_PUERP_SEPSIS',
#   'J10_PNEUMONIA',
#   'G6_NERPLEX',
#   'N14_FEMALEGENINF',
#   'N14_SALPHOOPH',
#   'N14_BREAST',
#   'G6_EPIPAROX',
#   'E4_ENDOGLAND',
#   'O15_PUERP_INFECT_OTHER',
#   'J10_SINUSITIS',
#   'CHILDHOOD_ALLERGY',
#   'H8_MED_SUPP',
#   'H8_EUSTSALP',
#   'N14_MESNRUIRREG',
#   'H8_MIDDLEMASTOID',
#   'L12_DERMATITISNAS',
#   'O15_EXCESS_VOMIT_PREG',
#   'ALLERG_RHINITIS',
#   'MIGRAINE_TRIPTAN',
#   'O15_HAEMORRH_EARLY_PREG',
#   'L12_URTICARIA',
#   'H8_NONSUPPNAS',
#   'N14_ENDOMETRIOSIS',
#   'E4_PCOS_CONCORTIUM',
#   'AB1_INTESTINAL_INFECTIONS',
#   'J10_TONSILLECTOMY',
#   'L12_DERMATITISECZEMA',
#   'L12_ATOPIC',
#   'N14_FEMALEGENNONINF',
#   'E4_HYTHYNAS',
#   'L12_INFECT_SKIN',
#   'J10_UPPERDIS',
#   'M13_KNEEDERANGEMENTS',
#   'E4_THYROID',
#   'J10_CHRONTONSADEN',
#   'K11_ORAL',
#   'K11_GINGIVITIS_PERIODONTAL',
#   'H7_ALLERGICCONJUNCTIVITIS',
#   'RX_ANTIHYP',
#   'E4_HYTHY_AI_STRICT',
#   'O15_ABORT_MEDICAL',
#   'K11_APPENDACUT',
#   'N14_PYELONEPHR',
#   'FG_CVD',
#   'O15_PREG_ABORT',
#   'O15_ABORT_SPONTAN',
#   'N14_RENALTUB'
#   # 'GEST_DIABETES'
# )

# possibly update for 
cool_fa_ep <- c(
  "F5_KELAMENT",
  "K11_REFLUX",
  "L12_URTICARIA",
  "L12_CELLULITIS",
  "J10_ASTHMACOPDKELA",
  'AB1_VIRAL_HEPATITIS'
)

cool_mo_ep <- c(
  "F5_KELAMENT",
  "K11_REFLUX",
  "L12_URTICARIA",
  "L12_CELLULITIS",
  "O15_BREAST_LACT_OTHER_DIS",
  "J10_ASTHMACOPDKELA",
  "N14_HYPERTROPHYBREAST",
  "N14_FEMALEGENINF",
  'AB1_VIRAL_HEPATITIS',
  "M13_FIBROBLASTIC"
)


# combine the vectors to a single list of interesting variables
cool_vars <- c(
  cool_fa_drugs,
  cool_mo_drugs,
  cool_sib_drugs,
  cool_birthreg_struct,
  cool_sib_ep,
  cool_preg_drugs,
  cool_neodg,
  cool_fa_ep,
  cool_mo_ep
)

# # single list of all possible interesting variables. Might be useful for plotting
# all_possible_vars <- c(
#   cool_fa_drugs,
#   cool_mo_drugs,
#   cool_sib_drugs,
#   cool_birthreg_struct,
#   all_interesting_sib_ep,
#   cool_preg_drugs,
#   all_possible_neodg,
#   all_possible_parent_ep,
#   all_possible_parent_ep
# )


# variable source registries for prediction model
cool_var_sources <- c(
  rep("father_drugs", length(cool_fa_drugs)),
  rep("mother_drugs", length(cool_mo_drugs)),
  rep("sib_drugs", length(cool_sib_drugs)),
  rep("mbr_structural", length(cool_birthreg_struct)),
  rep("sib_ep", length(cool_sib_ep)),
  rep("mother_drug_pregnancy", length(cool_preg_drugs)),
  rep("neodg", length(cool_neodg)),
  rep("father_ep", length(cool_fa_ep)),
  rep("mother_ep", length(cool_mo_ep)))

# variable source names for all possible variables
# all_possible_var_sources <- c(
#   rep("father_drugs", length(cool_fa_drugs)),
#   rep("mother_drugs", length(cool_mo_drugs)),
#   rep("mbr_structural", length(cool_birthreg_struct)),
#   rep("sib_ep", length(all_interesting_sib_ep)),
#   rep("mother_drug_pregnancy", length(cool_preg_drugs)),
#   rep("neodg", length(all_possible_neodg)),
#   rep("father_ep", length(all_possible_parent_ep)),
#   rep("mother_ep", length(all_possible_parent_ep))
# )


# combine variable name and source to a data frame. Again twice for prediction model variables and for all possible variables
cool_var_df <- data.frame(var = cool_vars, source = cool_var_sources) %>%
  as_tibble

# all_possible_var_df <- data.frame(var = all_possible_vars, source = all_possible_var_sources) %>%
#   as_tibble


### soucre names

allnames <- c(
  "father_drugs",
  "mother_drugs",
  "sib_drugs",
  "mbr_structural",
  "sib_ep",
  "mother_drug_pregnancy",
  "neodg",
  "father_ep",
  "mother_ep"
)

filenames <- c(
  "meds_father_beforepreg_wide.csv",
  "meds_mother_beforepreg_wide.csv",
  "meds_sib_wide.csv",
  "mbr_structural_vars.csv",
  "sibling_endpoints_wide_15022022.csv",
  "meds_pregnancy_wide.csv",
  "neonatal_diagnoses_feb_22.csv",
  "father_endpoints_wide_15022022.csv", 
  "mother_endpoints_wide_15022022.csv"
)


# data frame of filenames
filename_df <- data.frame(source = allnames, filename = filenames) %>% as_tibble


## loop to read all the predictor datasets and extracts variables of interest

# set up the result dataframe
composite_data <- development_set


# loop begins
# loop gives warnings for the drug names that are not included to the sibling drugs, 
for(i in 1:length(allnames)) {
  
  tempname <- allnames[i]
  
  # replace cool_var_df with all_possible_var_df if 
  # want to examine all possible vars
  tempvar_names <- cool_var_df %>% # replacement here
    filter(source == tempname) %>%
    pull(var)
  
  tempdata <- fread(paste0(pred_path, filenames[i]), 
                    select = c("TNRO", tempvar_names)) %>% as_tibble %>%
    rename_with(~ paste0(., "_", tempname), -TNRO)
  
  
  composite_data <- left_join(composite_data, tempdata, by = "TNRO")
  
  print(i)
  
}
# loop ends


### loop for number of features
n_of_features <- data.frame(
  source = allnames,
  n = rep(0, length(allnames)))


for(i in 1:length(allnames)) {
  
  tempname <- allnames[i]
  
  # # replace cool_var_df with all_possible_var_df if 
  # # want to examine all possible vars
  # tempvar_names <- cool_var_df %>% # replacement here
  #   filter(source == tempname) %>%
  #   pull(var)
  
  tempdata_names <- fread(paste0(pred_path, filenames[i]), 
                          nrow = 0) 
  
  
  # length of colnames vector, -1 because of ID name
  n_of_features$n[i] <- length(tempdata_names - 1)
  
  print(i)
  
}

n_of_features

### loop ends



composite_data %>% glimpse








# create features that combine information from end points
composite_data2 <- composite_data %>%
  # create term breathing difficulties
  mutate(
    term_breathing = case_when(
      (P221_neodg & gest_days > 258) == 1 ~ 1,
      term_rds_neodg == 1 ~ 1,
      (P22_other_unsp_neodg & gest_days > 258) == 1 ~ 1,
      (P24_neodg == 1 & gest_days > 258 ) ~ 1,
      TRUE ~ 0)) %>%
  # term hypoglycemia
  mutate(
    term_hypoglycemia = case_when(
      (P70_neodg == 1 & gest_days > 258) ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    # Valproate?
    neonate_substance = case_when(
      P961_neodg == 1 ~ 1, 
      P044_neodg == 1 ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    mother_asthma = case_when(
      (inhaled_steroids_mother_drugs == 1 |
         J10_ASTHMACOPDKELA_mother_ep == 1) ~ 1,
      TRUE ~ 0),
    father_asthma = case_when(
      (inhaled_steroids_father_drugs == 1 |
         J10_ASTHMACOPDKELA_father_ep == 1) ~ 1,
      TRUE ~ 0),
    sib_asthma = case_when(
      (inhaled_opening_drugs_sib_drugs == 1 & inhaled_steroids_sib_drugs == 1&sib_asthma_dg_sib_ep == 1)|
        J10_ASTHMACOPDKELA_sib_ep == 1 ~ 1,
      TRUE ~ 0)) %>%
  # mutate(pregnancy_other_antibiotic = case_when(
  #     kefexin_mother_drug_pregnancy == 1 ~ 1,
  #     fluorocinolone_mother_drug_pregnancy == 1 ~ 1,
  #     macrolide_mother_drug_pregnancy == 1 ~ 1,
  #     TRUE ~ 0)) %>%
  
  # remove variables that are used to create the composite variables
  select(-c(
    # neonatal breathing diagnoses
    "term_rds_neodg",
    "premie_rds_neodg",
    "P24_neodg",
    "P22_other_unsp_neodg",
    "P221_neodg",
    
    # hypoglycemia
    "P70_neodg",
    
    # neonatal withdrawal things
    "P04_other_neodg",
    "P961_neodg",
    "P044_neodg",
    "O355_neodg",
    
    # relative asthma
    "J10_ASTHMACOPDKELA_father_ep",
    "J10_ASTHMACOPDKELA_mother_ep",
    "J10_ASTHMACOPDKELA_sib_ep",
    "sib_asthma_dg_sib_ep",
    
    
    "inhaled_steroids_mother_drugs",
    "inhaled_steroids_father_drugs",
    "inhaled_steroids_sib_drugs",
    "inhaled_opening_drugs_sib_drugs",
    
    "chd_oper_later"
    
    # pregnancy antibiotics
    # "kefexin_mother_drug_pregnancy",
    # "fluorocinolone_mother_drug_pregnancy",
    # "macrolide_mother_drug_pregnancy"
  )) %>%
  
  mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
  mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1)))

# save temporary data to official composite_data variable
composite_data <- composite_data2

# code for defining a categorical variable for birth year and birth month
# composite_data <- composite_data %>%
#   mutate(epidemic_year = as.factor(epidemic_year)) %>%
#   mutate(birth_yearmonth = paste(birth_month, birth_year, sep = "/")) %>%
#   mutate(birth_yearmonth = as.factor(birth_yearmonth)) %>%
#   mutate(birth_month_factor = as.factor(birth_month))



# save composite data for further use
# we can further define the variables later, but we'll currently do the 
# stepwise AIC filtering with the original variables. 

setwd("/data/projects/project_pvartiai/rsv/predictors")
write.csv(composite_data, "composite_data_all_candidate_predictors.csv",
          row.names = FALSE)



setwd("/data/projects/project_pvartiai/rsv/results_refined/")
write.csv(n_of_features, "n_of_features.csv",
          row.names = FALSE)







# # # # # #     V A L I D A T I O N   D A T A
# load development set and birth dates

val_dir <- "/data/projects/project_pvartiai/rsv/validation_data/"
val_path <- paste0(val_dir, "validation_set.csv")

# load only development set, containing basic vars
validation_set <- fread(val_path) %>% as_tibble

validation_set %>%
  pull(LAPSEN_SYNTYMAPVM) %>%
  summary()


# set up the result dataframe
composite_val <- validation_set


# loop begins
# loop gives warnings for the drug names that are not included to the sibling drugs, 
for(i in 1:length(allnames)) {
  
  tempname <- allnames[i]
  
  # replace cool_var_df with all_possible_var_df if 
  # want to examine all possible vars
  tempvar_names <- cool_var_df %>% # replacement here
    filter(source == tempname) %>%
    pull(var)
  
  tempdata <- fread(paste0(pred_path, filenames[i]), 
                    select = c("TNRO", tempvar_names)) %>% as_tibble %>%
    rename_with(~ paste0(., "_", tempname), -TNRO)
  
  composite_val <- left_join(composite_val, tempdata, by = "TNRO")
  
  print(i)
  
}
# loop ends



composite_val %>% glimpse

composite_val <- composite_val %>%
  # create term breathing difficulties
  mutate(
    term_breathing = case_when(
      (P221_neodg & gest_days > 258) == 1 ~ 1,
      term_rds_neodg == 1 ~ 1,
      (P22_other_unsp_neodg & gest_days > 258) == 1 ~ 1,
      (P24_neodg == 1 & gest_days > 258 ) ~ 1,
      TRUE ~ 0)) %>%
  # term hypoglycemia
  mutate(
    term_hypoglycemia = case_when(
      (P70_neodg == 1 & gest_days > 258) ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    # Valproate?
    neonate_substance = case_when(
      P961_neodg == 1 ~ 1, 
      P044_neodg == 1 ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    mother_asthma = case_when(
      (inhaled_steroids_mother_drugs == 1 |
         J10_ASTHMACOPDKELA_mother_ep == 1) ~ 1,
      TRUE ~ 0),
    father_asthma = case_when(
      (inhaled_steroids_father_drugs == 1 |
         J10_ASTHMACOPDKELA_father_ep == 1) ~ 1,
      TRUE ~ 0),
    sib_asthma = case_when(
      (inhaled_opening_drugs_sib_drugs == 1 & inhaled_steroids_sib_drugs == 1&sib_asthma_dg_sib_ep == 1)|
        J10_ASTHMACOPDKELA_sib_ep == 1 ~ 1,
      TRUE ~ 0)) %>%
  # mutate(pregnancy_other_antibiotic = case_when(
  #     kefexin_mother_drug_pregnancy == 1 ~ 1,
  #     fluorocinolone_mother_drug_pregnancy == 1 ~ 1,
  #     macrolide_mother_drug_pregnancy == 1 ~ 1,
  #     TRUE ~ 0)) %>%
  
  # remove variables that are used to create the composite variables
  select(-c(
    # neonatal breathing diagnoses
    "term_rds_neodg",
    "premie_rds_neodg",
    "P24_neodg",
    "P22_other_unsp_neodg",
    "P221_neodg",
    
    # hypoglycemia
    "P70_neodg",
    
    # neonatal withdrawal things
    "P04_other_neodg",
    "P961_neodg",
    "P044_neodg",
    "O355_neodg",
    
    # relative asthma
    "J10_ASTHMACOPDKELA_father_ep",
    "J10_ASTHMACOPDKELA_mother_ep",
    "inhaled_steroids_mother_drugs",
    "inhaled_steroids_father_drugs",
    "inhaled_steroids_sib_drugs",
    "inhaled_opening_drugs_sib_drugs",
    "sib_asthma_dg_sib_ep",
    
    "chd_oper_later"
    
    # pregnancy antibiotics
    # "kefexin_mother_drug_pregnancy",
    # "fluorocinolone_mother_drug_pregnancy",
    # "macrolide_mother_drug_pregnancy"
  )) %>%
  
  mutate(birth_year = as.numeric(year(LAPSEN_SYNTYMAPVM))) %>%
  mutate(epidemic_year = ifelse(birth_month <= 5, birth_year, (birth_year+1)))

composite_val %>% glimpse



setwd("/data/projects/project_pvartiai/rsv/validation_data/")
write.csv(composite_val, "validation_set_all_candidates.csv",
          row.names = FALSE)
