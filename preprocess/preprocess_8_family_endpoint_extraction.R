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

# old ep version
# densified_wide_name <- "densified_first_events_DF8_no_omits_2022-03-17.txt"
densified_wide_name <- "densified_first_events_DF10_no_omits_2022-09-20.txt"
ep_dir <- "/data/processed_data/endpointer/"
densified_path <- paste0(ep_dir, densified_wide_name)

# path where predictor datasets are saved
pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"

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
  rename(TNRO = FINREGISTRYID) %>% # change to FINREGISTRYID if neede
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
  filter(AGE <= (mother_age)) 

father_eps_age <- full_join(father_eps, fathers, by = "parent_TNRO") %>%
  filter(AGE <= (father_age)) 

sibs_eps_age <- full_join(sibs_eps, sibs, by = "sibling_TNRO") %>%
  filter(AGE <= (sibling_agediff)) 


# prevalence screen & rescue endpoints #####
#
# rescue endpoints are a custom list of interesting disease endpoints
# that we want to examine regardless of prevalence. E.g. bronchiolitis is such an example. 

rescue_endpoints <- c("D3_CVID",
                      "D3_IMMUDEFANTIBODY",
                      "D3_COMBIMMUDEF",
                      "D3_IMMUDEF",
                      "I9_MYOCARDITIS"
)

# exclusion vector, to be used inside prevalence filter function
# maybe move at some point to the actual exclusion.
exclude_these_endpoints <- c(
  "KRA_PSY_ANYMENTAL_SUICID_PREG_NERV_EXMORE", # for some reason causing problems in stepwise filtering. 
  "KRA_PSY_EATING_EXMORE",
  'AB1_BACT_BIR_OTHER_INF_AGENTS',
  'AB1_OTHER_INFECTIONS',
  'AB1_OTHER_VIRAL',
  'AB1_SEQULAE_INF_NOS',
  'AB1_SEQULEAE_OF_INFECTIONS_AND_PARASITES',
  'AB1_SEXUAL_TRANSMISSION_NOS',
  'AB1_VIRAL_NOS',
  'ALCOHOLACUTE10',
  'ANTIDEPRESSANTS',
  'ASTHMA_ACUTE_RESPIRATORY_INFECTIONS',
  'ASTHMA_INFECTIONS',
  'ASTHMA_OBESITY',
  'ASTHMA_PNEUMONIA',
  'AUD_SWEDISH',
  'AUTOIMMUNE',
  'AUTOIMMUNE_NONTHYROID',
  'BLEEDING',
  'C_STROKE',
  'C3_CANCER',
  'CARDIAC_ARRHYTM',
  'CD2_BENIGN_BONE_CARTILAGE',
  'CD2_BENIGN_BONE_LONG_LOWER',
  'CD2_BENIGN_BREAST',
  'CD2_BENIGN_CONNECTIVE_SOFT',
  'CD2_BENIGN_FEMALE_GEN_OTH',
  'CD2_BENIGN_HEAMANGIOMA_LYMPHANGIOMA',
  'CD2_BENIGN_HEMANGIOMA',
  'CD2_BENIGN_LEIOMYOMA_UTERI',
  'CD2_BENIGN_LIPO_SKIN_TRUNK',
  'CD2_BENIGN_LIPOMATOUS',
  'CD2_BENIGN_MELANOCYTIC',
  'CD2_BENIGN_MELANOCYTIC_FACE',
  'CD2_BENIGN_MELANOCYTIC_NOS',
  'CD2_BENIGN_MELANOCYTIC_TRUNK',
  'CD2_BENIGN_OVARY',
  'CD2_BENIGN_SKIN',
  'CD2_INSITU_CERVIX_UTERI',
  'CD2_LEIOMYOMA_ENDOMETRIOSIS',
  'D3_ANAEMIA_IRONDEF_BLOODLOSS',
  'D3_ANAEMIA_IRONDEF_NAS',
  'D3_PURPURA_AND3_OTHER_HAEMORRHAGIC',
  'DENTAL_TMD',
  'DENTAL_TMD_FIBRO',
  'DM_KETOACIDOSIS',
  'DM_SEVERAL_COMPLICATIONS',
  'E4_DM1NASCOMP',
  'E4_DMNAS',
  'E4_PCOS_BROAD',
  'F5_ALCOHOL_DEPENDENCE',
  'F5_DEPRESSION_DYSTHYMIA',
  'F5_DEPRESSION_PSYCHOTIC',
  'F5_DEPRESSION_RECURRENT',
  'F5_EATOTH',
  'F5_MENTALUNSPE',
  'F5_MOODOTH',
  'F5_PSYCHOTH',
  'FALLS',
  'FE',
  'FE_MODE',
  'FG_OTHHEART',
  'G6_MIGRAINE_NO_AURA_TRIPTAN',
  'G6_MIGRAINE_WITH_AURA',
  'G6_SLEEPAPNO_INCLAVO',
  'GE',
  'H7_CONJUNCTIVITISACUNONATOPIC',
  'H7_CONJUNCTIVITISATOPIC',
  'H7_EYEOTH',
  'H7_STRABOTH',
  'H7_VISUDISTURBSUB',
  'H8_EXT_INFNAS',
  'H8_EXT_NAS',
  'H8_HL_CON_NAS',
  'H8_HL_SEN_NAS',
  'H8_OTIMEDNAS',
  'H8_SUP_OTINAS',
  'I9_DISVEINLYMPH',
  'I9_OTHARR',
  'I9_VEINSOTH',
  'J10_COLD',
  'J10_INFLUPNEU',
  'J10_LOWERINF',
  'J10_VIRALPNEUMO',
  'JOINTPAIN',
  'K11_ANOMALI_DENTAL_ARCH_RELATIONS1',
  'K11_APHTA_RECUR_INCLAVO',
  'K11_CARIES_DENTIN',
  'K11_CROWDI_TEETH',
  'K11_DISORD_TEETH',
  'K11_EMBED_TEETH',
  'K11_EMBIMPACT_TEETH',
  'K11_EROSION_INCLAVO',
  'K11_IMPACTED_TEETH',
  'K11_LIVER',
  'K11_MAJOR_ANOMALI_JAW_SIZE',
  'K11_MAXIL_MOLAR_FAILED_ERUPT_OBSTRUCTION_TOOTH',
  'K11_MN_RETRGN',
  'K11_MN_RETRGN_AND_SURG',
  'K11_OPEN_BITE',
  'K11_OTHDIG',
  'K11_OTHDISDIG',
  'K11_OTHENTERCOL',
  'K11_OTHFUNC',
  'K11_OTHGASTR',
  'K11_PARODON_OPER',
  'K11_PARODON_OPER_DENTAL_OPER',
  'K11_PERITON',
  'K11_PULP_PERIAPICAL',
  'K11_PULPITIS_1_ONLYAVO',
  'K11_PULPITIS_3',
  'KRA_PSY_ANYMENTAL',
  'L12_INFECT_SKIN_OTHER',
  'L12_OTHERCONTACT',
  'L12_OTHERSKINSUBCUTIS',
  'M13_ARTHROSIS_INCLAVO',
  'M13_DEFORMDORSONAS',
  'M13_DISSYNOTENDNAS',
  'M13_DORSALGIA',
  'M13_DORSALGIANAS',
  'M13_DORSOPATHYOTH',
  'M13_EARLY_LUMBAR_PROLAPSE',
  'M13_ENTESOPATHYOTH',
  'M13_KNEEDERANGEMENTS',
  'M13_LIMBPAIN',
  'M13_LOWBACKPAINORANDSCIATICA',
  'M13_MENISCUSDERANGEMENTS',
  'M13_MUSCLE',
  'M13_MUSCULOSKELEOTH',
  'M13_MYALGIA',
  'M13_OSTEOCHONDRO',
  'M13_OTHERBONE',
  'M13_OTHERJOINT',
  'M13_RHEUMATISM',
  'M13_SOFTTISSUE',
  'M13_SOFTTISSUEOTH',
  'MIGRAINE_TRIPTAN',
  'MUCOPROCT',
  'N14_BARTHOCYS',
  'N14_CERVICAL_HSIL',
  'N14_CERVICAL_LSIL',
  'N14_ENDOMETRIOSIS_ASRM_STAGE1_2',
  'N14_ENDOMETRIOSIS_ASRM_STAGE3_4',
  'N14_ENDOMETRIOSIS_DEEP',
  'N14_ENDOMETRIOSIS_NOS',
  'N14_ENDOMETRIOSIS_OVARY',
  'N14_ENDOMETRIOSIS_PELVICPERITONEUM',
  'N14_ENDOMETRIOSIS_RECTPVAGSEPT_VAGINA',
  'N14_ENDOMETRIOSIS_UTERUS',
  'N14_FEMALE_GENITAL_HSIL',
  'N14_FEMALE_GENITAL_LSIL',
  'N14_FEMALEGENNONINF',
  'N14_FEMALEINFERT',
  'N14_FEMGENPAIN',
  'N14_FIOTHNAS',
  'N14_HYPERTROPHYBREAST',
  'N14_LUMPNASBREAST',
  'N14_OTHFEMPELINF',
  'N14_OTHINFVAGVULV',
  'N14_OTHNONINFOVA',
  'N14_OTHNONINFUTER',
  'N14_OTHNONINFVAG',
  'N14_OTHNONINFVULV',
  'N14_TUBULOINTNEPHRNAS',
  'N14_VULVAL_LSIL',
  'NEURODEGOTH',
  'O15_ABORT_FAILED',
  'O15_ABORT_SPONTAN',
  'O15_AMNIOT_OTHER',
  'O15_ANTENAT_ABNORM',
  'O15_ANTEPART_HAEM_NAS',
  'O15_COMPLIC_ABORT_ECTOP_MOLAR',
  'O15_COMPLIC_LAB_DELIV',
  'O15_CONCEPT_ABNORM',
  'O15_DELIV_CAESAR',
  'O15_DELIV_FORCEP_VAC',
  'O15_DELIV_MULTIP',
  'O15_DELIV_OTHER_ASSIST',
  'O15_DELIV_PERIN_LACER',
  'O15_DELIV_SPONT',
  'O15_EXCESS_VOMIT_PREG',
  'O15_EXIST_HYPERT_COMPLIC',
  'O15_FAILED_INDUCT',
  'O15_FALSE_LAB',
  'O15_GESTAT_HYPERT',
  'O15_GESTAT_OEDEM_PREINUR',
  'O15_HAEMORRH_EARLY_PREG',
  'O15_HYPTENSPREG',
  'O15_ICP_WIDE',
  'O15_LABOUR_ABNORM_FORCES',
  'O15_LABOUR_FETAL_STRESS',
  'O15_LABOUR_INTRAPART_HAEMORRH',
  'O15_LABOUR_LONG',
  'O15_LABOUR_MALPOS',
  'O15_LABOUR_OBSTR_OTHER',
  'O15_LABOUR_OTHER',
  'O15_LABOUR_PELVIC_ABNORM',
  'O15_LABOUR_UMBILICAL',
  'O15_MAT_CARE_MALPRESENT',
  'O15_MATERN_CARE',
  'O15_MATERN_CARE_CERVICAL_INCOMPETENCE',
  'O15_MATERN_CARE_DISPROP',
  'O15_MATERN_CARE_FETAL_ABNORM',
  'O15_MATERN_CARE_OTHER',
  'O15_MATERN_CARE_PELVIC_ABNORM',
  'O15_MATERN_HYPERT_UNS',
  'O15_MATERN_INFECT_ELSEWHERE',
  'O15_MEMBR_PREMAT_RUPT',
  'O15_MULTIP_GEST',
  'O15_OBSTET_TRAUMA_OTHER',
  'O15_OTHER_MATERN_DIS_ELSEWHERE',
  'O15_PLAC_DISORD',
  'O15_PLAC_PRAEVIA',
  'O15_PLAC_PREMAT_SEPAR',
  'O15_POLYHYDR',
  'O15_POOR_FETGRO',
  'O15_POSTPART_HAEMORRH_ANATOMY',
  'O15_POSTPART_HAEMORRH_PLACENTA',
  'O15_POSTPART_HEAMORRH',
  'O15_PRE_OR_ECLAMPSIA',
  'O15_PREEC_OR_FETGRO',
  'O15_PREECLAMPS',
  'O15_PREECLAMPS_CHRONIC_HYPERT',
  'O15_PREG_ABORT',
  'O15_PREG_ECTOP',
  'O15_PREG_GU_INFECT',
  'O15_PREG_HYDAT',
  'O15_PREG_MATERN_CARE',
  'O15_PREG_OTHER_MAT_DISORD',
  'O15_PREG_PROLONGED',
  'O15_PREG_VENOUS_HAEMORRH',
  'O15_PRETERM',
  'O15_PUERP_COMPL_NAS',
  'O15_PUERP_INFECT_OTHER',
  'O15_PUERP_SEPSIS',
  'O15_RETAINED_PLAC_MEMB_NO_HEAMORRH',
  'O15_TWIN_GEST',
  'OTHER_SYSTCON_FG',
  'PAIN',
  'PULM_INFECTIONS',
  'RHEU_ARTHRITIS_OTH',
  'RHEUMA_ARHTROPAT_REACTIVE',
  'RHEUMA_NOS',
  'RHEUMA_OTHER_WIDE',
  'RHEUMA_SERONEG',
  'RHEUMA_SEROPOS_WIDE',
  'RX_ANTIHYP',
  'RX_CODEINE_TRAMADOL',
  'RX_CROHN_1STLINE',
  'RX_CROHN_2NDLINE',
  'RX_INFERTILITY',
  'RX_N05C',
  'RX_PARACETAMOL_NSAID',
  'RX_STATIN',
  'SLEEP',
  'SMOKING_DEPEND',
  'ST19_FRACT_FEMUR',
  'ST19_FRACT_FOOT_ANKLE',
  'ST19_FRACT_FOREA',
  'ST19_FRACT_LOWER_LEG_INCLU_ANKLE',
  'ST19_FRACT_LUMBAR_SPINE_PELVIS',
  'ST19_FRACT_RIBS_STERNUM_THORACIC_SPINE',
  'ST19_FRACT_SHOUL_UPPER_ARM',
  'ST19_FRACT_SKULL_FACIAL_BONES',
  'ST19_FRACT_WRIST_HAND_LEVEL',
  'ST19_TOXIC_EFFECT_ETHAN',
  'T1D_WIDE',
  'T2D_WIDE',
  'TEMPOROMANDIB_INCLAVO',
  'ULCERNAS',
  'VIRALPNEUMONAS',
  'Z21_PROCREATIVE_MANAG',
  'Z21_TOBAC_USE',
  'AB1_DERMATOPHYTOSIS',
  'CHRONLARGE',
  'E4_DM1COMA',
  'H7_CATARACTOTHER',
  'H8_CHOLEASTOMA',
  'H8_NOISEINNER',
  'HERING_BILATERAL',
  'J10_PNEUMONONBACT',
  'K11_DISLIVOTH',
  'K11_ORALCYST',
  'K11_PERIODON_CHRON',
  'L12_NAIL_INGROW',
  'M13_ARTHROSIS_OTH',
  'M13_BONEDISNAS',
  'M13_FOREIGNBODY',
  'M13_PREPATELLARBURSITIS',
  'M13_RADICULOPATHY',
  'M13_SHOULDERNAS',
  'N14_MALEINFERT',
  'ST19_FRACT_NECK',
  'AB1_BACT_INTEST_OTH',
  'AB1_OTHER_BACTERIAL',
  'E4_METABOLIA',
  'H7_RETINOPATHYDIAB',
  'H7_VISUDISTURB',
  'H7_DIPLOPIA'
)



#function extracts EP names from the data with > n occurrences
prevalence_screen <- function(data) {
  data %>% 
    group_by(ENDPOINT) %>%
    summarise(n_endpoint = n()) %>%
    
    # prevalence filter
    filter(n_endpoint > 1500 |
             ENDPOINT %in% rescue_endpoints) %>%
    
    filter(!(ENDPOINT %in% exclude_these_endpoints)) %>%
    #
    
    pull(ENDPOINT)
}

#get the endpoint names that occur >1500 times in relative groups.
# We want to keep the same endpoint list for both parents, so that we can compare them.
mother_prev_ep_names <- prevalence_screen(mother_eps_age)
father_prev_ep_names <- prevalence_screen(father_eps_age)
parent_eps <- c(mother_prev_ep_names, father_prev_ep_names) %>% unique()

parent_eps %>% as_tibble %>% print(n = 1000)


# prevalence-screen sibling endpoints separately. 
# siblings have different diseases in a different timeline. 
sibs_prev_ep_names <- prevalence_screen(sibs_eps_age)




# iterative step: we have manually created a 'retained_endpoints' character vector,
# where prevalence filtered endpoints are manually chekced (reducing the number from 660 to 330)
load("/home/pvartiai/RSV/data/wrangle/names/retained_endpoints.R")
retained_entpoints <- c(retained_endpoints, "J10_ASTHMA_MAIN_EXMORE", "J10_ASTHMA_EXMORE")


# filter more rare endpoints away, and exclude extra variables from endpoint datasets.
# in the second iteration, the filtering is done with 'retained_endpoints' (instead of parent_eps)

mother_ep <- mother_eps_age %>%
  filter(ENDPOINT %in% c(mother_prev_ep_names, rescue_endpoints)) %>%
  filter(!(ENDPOINT %in% exclude_these_endpoints)) %>%
  select(TNRO, parent_TNRO, ENDPOINT)

father_ep <- father_eps_age %>%
  filter(ENDPOINT %in% c(father_prev_ep_names, rescue_endpoints)) %>%
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

# mother_ep <- fread("/data/projects/project_pvartiai/rsv/family/mother_endpoints_all_longitudinal_15022022.csv")
# father_ep <- fread("/data/projects/project_pvartiai/rsv/family/father_endpoints_all_longitudinal_15022022.csv")
# sibling_ep <- fread("/data/projects/project_pvartiai/rsv/family/sibling_endpoints_all_longitudinal_15022022.csv")
# pred_path <- "/data/projects/project_pvartiai/rsv/predictors/"


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
sib_outcome <- fread(paste0(pred_path, "sibling_resp_hospitalization.csv")) %>%
  as_tibble


sib_asthma_dg <- fread(paste0(pred_path, "sibling_asthma_dg.csv")) %>%
  as_tibble

# join custom endpoints (rsv hospitalization, sibling asthma diagnosis) to all other endpoints
sib_ep_wide <- left_join(sib_ep_wide, sib_outcome, by = "TNRO") %>%
  left_join(., sib_asthma, by = "TNRO")


# mutate "J10_bronchiolitis" ep
sib_ep_wide <- sib_ep_wide %>%
  mutate(other_j21_endpoint = ifelse(J10_BRONCHIOLITIS == 1 & 
                                       sib_resp_hosp == 0, 1, 0)) %>%
  select(-J10_BRONCHIOLITIS) 


write.csv(sib_ep_wide, "sibling_endpoints_wide_15022022.csv",
          row.names = FALSE)








