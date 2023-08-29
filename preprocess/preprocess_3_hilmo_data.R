# HILMO outcomes

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)
library(ggplot2)

# hilmo directories
hilmo_dir <- "/data/processed_data/thl_hilmo/"

# hilmo exists in processed and original. From now on, we can use only processed hilmo.
hilmo_name <- "thl2019_1776_hilmo.csv.finreg_IDsp"
new_hilmo_name <- "THL2021_2196_HILMO_2019_2021.csv.finreg_IDsp"
hilmo_dg_name <- "thl2019_1776_hilmo_diagnoosit_kaikki.csv.finreg_IDsp"
new_hilmo_dg_name <- "THL2021_2196_HILMO_DIAG.csv.finreg_IDsp"

# load basic population data, mbr_c
mbr_c <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_1_mbr.csv") %>% 
  as_tibble

# create id and birthday vectors for hilmo selection
all_ids <- mbr_c$TNRO

kids_bd <- mbr_c %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)

all_bds <- mbr_c %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)

# mothers
mothers <- fread("/data/projects/project_pvartiai/rsv/family/mothers.csv") %>%
  as_tibble

# siblings
sibs <- fread("/data/projects/project_pvartiai/rsv/family/siblings_from_mbr_and_relatives.csv") %>%
  as_tibble

# just sibling birthdays
sib_bd <- sibs %>%
  select(sibling_TNRO, sibling_dob)

# old path
# mothers <- fread("/home/pvartiai/RSV/data/family/mothers.csv") %>% as_tibble

# updated preprocess 1 data
viim_kk <- mbr_c %>% select(TNRO, gest_days) %>%
  as_tibble

mother_dg <- left_join(mothers, viim_kk, by = "TNRO") %>%
  mutate(kid_dob = as.Date(kid_dob)) %>%
  mutate(viim_kk = kid_dob - gest_days) %>%
  select(-gest_days, -parent_gender, -mother_age, -parent_dob)







# # #    R A W  H I L M O     L O A D I N G    A N D    F I L T E R I N G    


# LOAD THE FILTERED DATASETS
# updated paths
hilmo_original_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_original_kids.csv") %>%
  as_tibble()
hilmo_orig_dg_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_orig_dg_kids.csv") %>%
  as_tibble()
pregnancy_hilmo_compl_entries <- fread("/data/projects/project_pvartiai/rsv/wrangle/mothers_raw_o_diagnoses_for_recoding_feb_2022.csv") %>%
  as_tibble()
hilmo_sibs <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_sibling_entries.csv") %>%
  as_tibble()
hilmo_dg_sibs <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_sibling_diagnoses.csv") %>%
  as_tibble()






# #### NOT RUN: LOADING AND FILTERING THE RAW HILMO FOR PATIENTS OF INTEREST

# loading the whole hilmo
# Note: it's ok to use "just" the main hilmo dataset, containing hospitalizations from 1994 onwards
# hilmo_raw <- fread(paste0(hilmo_dir, hilmo_name),
#                    select = c("HILMO_ID", "TNRO", "PALTU", "PALA", "EA", "TUPVA", "LPVM")) %>%
#         as_tibble()

# new_hilmo <- fread(paste0(hilmo_dir, new_hilmo_name),
#                    select = c("HILMO_ID", "TNRO", 
#                               "KIIREELLISYYS", "YHTEYSTAPA", # the new PALA (in/outpatient variables)
#                               "PALTU", "PALA", "EA", "TUPVA", "LPVM")) %>% 
# as_tibble %>%
# # mutate for joining with the old data
# mutate(PALTU = as.numeric(PALTU))



# # The whole hilmo (main file) diagnoses
# hilmo_diagnoses <- fread(paste0(hilmo_dir, hilmo_dg_name)) %>% as_tibble()
# new_hilmo_diagnoses <- fread(paste0(hilmo_dir, new_hilmo_dg_name)) %>% as_tibble



# # filter hilmo only for our study children
# hilmo_original_kids_old <- hilmo_raw %>%
#     filter(TNRO %in% all_ids) %>%
#     mutate(KIIREELLISYYS = NA_character_,
#           YHTEYSTAPA = NA_character_) %>%
#     select(c("HILMO_ID", "TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM"))

# # filter new hilmo for only the kids
# new_hilmo_kids <- new_hilmo %>%
#   filter(TNRO %in% all_ids) %>%
#     select(c("HILMO_ID", "TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM")) %>%
#     mutate(PALTU = as.numeric(PALTU))

# # how to join these variables?
# hilmo_original_kids <- bind_rows(hilmo_original_kids_old, new_hilmo_kids)


# # same filtering for hilmo_diagnoses
# hilmo_orig_dg_kids_old <- hilmo_diagnoses %>%
#     filter(TNRO %in% all_ids)

# # same filtering for new hilmo diagnoses
# new_hilmo_dg_kids <- new_hilmo_diagnoses %>%
#   filter(TNRO %in% all_ids)

# # bind rows of the old and new hilmo data
# hilmo_orig_dg_kids <- bind_rows(hilmo_orig_dg_kids_old, new_hilmo_dg_kids)



# # filter mothers' hilmo entries
# hilmo_mothers_old <- hilmo_raw %>%
#   filter(TNRO %in% mother_dg$parent_TNRO) %>%
#   rename(parent_TNRO = TNRO) %>%
#   mutate(KIIREELLISYYS = NA_character_,
#           YHTEYSTAPA = NA_character_) %>%
#   select(c("HILMO_ID", "parent_TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM"))

# # new 2019-2021 data
# hilmo_mothers_new <- new_hilmo %>%
#   filter(TNRO %in% mother_dg$parent_TNRO) %>%
#   rename(parent_TNRO = TNRO) %>%
#   select(c("HILMO_ID", "parent_TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM")) %>%
#   mutate(PALTU = as.numeric(PALTU))

# # bind old and new mother's data
# hilmo_mothers_raw <- bind_rows(hilmo_mothers_old, hilmo_mothers_new)


# all_mother_hilmo_entries <- inner_join(hilmo_mothers_raw, mother_dg, by = "parent_TNRO") 


# pregnancy_hilmo_entries <- all_mother_hilmo_entries %>%
#   filter(TUPVA <= kid_dob) %>%
#   filter(LPVM > viim_kk)

# pregnancy_hilmo_dgs <- hilmo_diagnoses %>%
#   bind_rows(., new_hilmo_diagnoses) %>%
#   filter(HILMO_ID %in% pregnancy_hilmo_entries$HILMO_ID) %>%
#   select(-TNRO)

# pregnancy_hilmo_compl_entries <- inner_join(pregnancy_hilmo_entries, 
#                                             pregnancy_hilmo_dgs, 
#                                             by = "HILMO_ID") %>% 
# select(TNRO, HILMO_ID, parent_TNRO, KOODI) %>%
# rename(value = KOODI) %>%
# filter(substr(value, 1, 1) == "O" | substr(value, 1, 3) %in% c("E03", "E66", "Z35")) %>%
# # create a 3-digit q_level variable
# mutate(o_level = substr(value, start = 1, stop = 3))


# # filter hilmo for study patients' siblings
# # sibling birth dates
# sib_bd <- sibs %>%
#     select(sibling_TNRO, sibling_dob)


# # sibs' rsv bronchiolitis
# hilmo_sibs_old <- hilmo_raw %>%
#     filter(TNRO %in% sibs$sibling_TNRO) %>%
#     mutate(KIIREELLISYYS = NA_character_,
#           YHTEYSTAPA = NA_character_) %>%
#     select(c("HILMO_ID", "TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM"))

# hilmo_sibs_new <- new_hilmo %>%
#   filter(TNRO %in% sibs$sibling_TNRO) %>%
#     select(c("HILMO_ID", "TNRO", "PALTU",
#             "KIIREELLISYYS", "YHTEYSTAPA",
#             "PALA", "EA", "TUPVA", "LPVM")) %>%
#     mutate(PALTU = as.numeric(PALTU))


# hilmo_sibs <- bind_rows(hilmo_sibs_old, hilmo_sibs_new)


# hilmo_dg_sibs <- hilmo_diagnoses %>%
#   bind_rows(., new_hilmo_diagnoses) %>%
#   filter(TNRO %in% sibs$sibling_TNRO)




# # save original, filtered hilmos 
# setwd("/data/projects/project_pvartiai/rsv/wrangle")
# write.csv(pregnancy_hilmo_compl_entries, "mothers_raw_o_diagnoses_for_recoding_feb_2022.csv", 
#   row.names = FALSE)
# write.csv(hilmo_original_kids, "hilmo_original_kids.csv",
#     row.names = FALSE)
# write.csv(hilmo_orig_dg_kids, "hilmo_orig_dg_kids.csv", 
#     row.names = FALSE)
# write.csv(hilmo_sibs, "hilmo_sibling_entries.csv", 
#     row.names = FALSE)
# write.csv(hilmo_dg_sibs, "hilmo_sibling_diagnoses.csv", 
#     row.names = FALSE)


# # clean complete hilmo diagnoses from memory
# rm(hilmo_raw)
# rm(new_hilmo)
# rm(hilmo_diagnoses)
# rm(new_hilmo_diagnoses)
# gc()

#### RAW HILMO FILTERING ENDS







# KIDS FIRST 7 DAYS' OF NEONATAL DIAGNOSES # # # # # # # # # # # # # # # # # # # # #

index_bds <- mbr_c %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)


hilmo_kids_filtered <- left_join(hilmo_original_kids, index_bds, by = "TNRO") %>%
  filter(!is.na(LAPSEN_SYNTYMAPVM)) %>%
  mutate(age_at_outcome = as.numeric(difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))) %>%
  # events occurring at first week of life:
  filter(age_at_outcome < 7) %>%
  as_tibble
  # do not filter out second or consecutive hospitalizations, they contain mostly p and q diagnoses
  # we will consider information during the 1st week, and from these diagnoses, we only get the neonatal (p) dgs
  # The script for filtering only the 1st hosps:
  # group_by(TNRO) %>% 
  # arrange(age_at_outcome, .by_group = TRUE) %>%
  # slice_head() %>%
  # ungroup()


babies_hilmo_dgs <- hilmo_orig_dg_kids %>%
  filter(HILMO_ID %in% hilmo_kids_filtered$HILMO_ID) %>%
  rename(value = KOODI) %>%
  select(HILMO_ID, TNRO, value)


# rsv inpatient outcome during 1st seven days of life
rsv_dg_codes_for_exclusion <- c("J210", "B974", "J205", "J121")

too_young_outcomes_dg <- babies_hilmo_dgs %>%
  filter(value %in% rsv_dg_codes_for_exclusion)

too_young_outcomes <- hilmo_kids_filtered %>%
  filter(HILMO_ID %in% too_young_outcomes_dg$HILMO_ID) %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3")

too_young_outcome_ids <-  too_young_outcomes %>%
  pull(TNRO) ### n = 74+1, 74 in old hilmo, 1 in new hilmo (2019-2021)


# remove "too young outcomes" from the first HILMO neonatal diagnoses dataset 
babies_hilmo_dgs <- babies_hilmo_dgs %>%
  filter(!(TNRO %in% too_young_outcome_ids))


# save first neonatal diagnoses from HILMO
setwd("/data/projects/project_pvartiai/rsv/wrangle")
write.csv(babies_hilmo_dgs, "index_babies_first_hilmo_diagnoses.csv",
          row.names = FALSE)







#        E X C L U D E   T H O S E    W I T H    O U T C O M E   A T   F I R S T    W E E K

setwd("/data/projects/project_pvartiai/rsv/wrangle/")

mbr_c <- mbr_c %>%
  filter(!(TNRO %in% too_young_outcome_ids))

# overwrite the first study population data, with too young outcomes excluded completely
write.csv(mbr_c, "preprocess_1_mbr.csv",
  row.names = FALSE)


all_ids <- setdiff(all_ids, too_young_outcome_ids)

all_bds <- all_bds %>%
  filter(!(TNRO %in% too_young_outcome_ids))

kids_bd <- all_bds












# # #  I D E N T I F Y    F I R S T   R S V   E N T R I E S     


# goals
# 1 identify all J210 hospitalisations
# 2 identify other than j210 rsv diagnoses
# 3 check outcome rates with TTR positive samples
# 3b do this especially with more weird combinations - RSVs as side diagnoses

# all rsv-related diagnoses
# Upddated on 7/2022, to include also other diagnoses than J21.0

rsv_dg_codes <- c("J210")
# not included ("J205","J121")


# select relevant diagnoses from hilmo
rsv_dgs <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% rsv_dg_codes)

# other than j210 dgs (used for comparison only)
rsv_dgs_other <- hilmo_orig_dg_kids %>%
  filter(KOODI %in% c("B974",
                      "J205",
                      "J121"))

# side dgs
rsv_dgs_side <- rsv_dgs %>%
  filter(substr(KENTTA, 1, 3) %in% c("PIT", "SDG"))

# only main dgs
rsv_dgs_main <- rsv_dgs %>%
  filter(!substr(KENTTA, 1, 3) %in% c("PIT", "SDG"))

# compare diagnosis types
rsv_dgs %>%
  select(KENTTA) %>% table


# hilmo ID's corresponding all possible RSV diagnosis
rsv_ids <- rsv_dgs$HILMO_ID %>% unique
rsv_ids_other <- rsv_dgs_other$HILMO_ID %>% unique


# personal ids of all possible RSV diagnosis
rsv_personal_ids <- rsv_dgs$TNRO %>% unique
rsv_personal_ids_other <- rsv_dgs_other$TNRO %>% unique



# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_rsv_entries <- hilmo_original_kids %>%
  filter(HILMO_ID %in% rsv_ids)

# check the rates of all RSV diagnoses yearly
all_rsv_entries %>%
  mutate(tupva_year = year(TUPVA)) %>%
  group_by(tupva_year) %>%
  summarise(n = n()) %>%
  print(n = 100)

all_rsv_entries_other <- hilmo_original_kids %>%
  filter(HILMO_ID %in% rsv_ids_other)

# all inpatient entries (with rsv diagnosis). Inpatient: PALA == 1
# Period_duration indicates the duration of the hospitalization
rsv_inpatients <- all_rsv_entries %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3") %>%
  mutate(period_duration = difftime(LPVM, TUPVA, units = "days"))

rsv_inpatients_other <- all_rsv_entries_other %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3") %>%
  mutate(period_duration = difftime(LPVM, TUPVA, units = "days"))



# Combine birth dates to the inpatient entries
rsv_inpatients <- left_join(rsv_inpatients, all_bds, by = "TNRO")
rsv_inpatients_other <- left_join(rsv_inpatients_other, all_bds, by = "TNRO")



# calculate kids' age at hilmo entry
rsv_inpatients <- rsv_inpatients %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))

rsv_inpatients_other <- rsv_inpatients_other %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))


# filter only cases where age at admission is from 7 to 365 days
rsv_inpatients_agefilt <- rsv_inpatients %>%
  filter(age_at_outcome < 366 & age_at_outcome > 6)

rsv_inpatients_agefilt_other <- rsv_inpatients_other %>%
  filter(age_at_outcome < 366 & age_at_outcome > 6)



# still 1086 duplficates!
# identify all duplicate FINREGISTRY IDs in rsv inpatient list

all_inp_ids <- rsv_inpatients_agefilt$TNRO
all_inp_ids_other <- rsv_inpatients_agefilt_other$TNRO

# duplicated ids (one row per single id)
duplicate_inp_ids <- all_inp_ids[duplicated(all_inp_ids)] 
duplicate_inp_ids_other <- all_inp_ids_other[duplicated(all_inp_ids_other)] 



# only unique IDs
unique_inp_ids <- setdiff(all_inp_ids, duplicate_inp_ids)
unique_inp_ids_other <- setdiff(all_inp_ids_other, duplicate_inp_ids_other)



#unique RSV hilmo entries
only_unique_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
  filter(TNRO %in% unique_inp_ids)

only_unique_rsv_inpatient_entries_other <- rsv_inpatients_agefilt_other %>%
  filter(TNRO %in% unique_inp_ids_other)


# All RSV entries from the duplicated, arranged by age
duplicated_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
  filter(TNRO %in% duplicate_inp_ids) %>% 
  group_by(TNRO) %>%
  arrange(age_at_outcome, .by_group = TRUE)

duplicated_rsv_inpatient_entries_other <- rsv_inpatients_agefilt_other %>%
  filter(TNRO %in% duplicate_inp_ids_other) %>% 
  group_by(TNRO) %>%
  arrange(age_at_outcome, .by_group = TRUE)


# select the first inpatient rsv entry - the one with the youngest age
first_from_duplicates_rsv_entries <- duplicated_rsv_inpatient_entries %>%
  slice_head()
first_from_duplicates_rsv_entries_other <- duplicated_rsv_inpatient_entries_other %>%
  slice_head()



# outcomes is the final dataset of relevant hilmo entries
outcomes <- bind_rows(only_unique_rsv_inpatient_entries, 
                      first_from_duplicates_rsv_entries) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM)


# other than j210 rsv diagnoses, hospitalized at age 7-365 days
outcomes_other <- bind_rows(only_unique_rsv_inpatient_entries_other, 
                            first_from_duplicates_rsv_entries_other) %>%
  mutate(outcome = 1) %>%
  mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
  select(-LAPSEN_SYNTYMAPVM) %>%
  filter(!(TNRO %in% outcomes$TNRO))


# create vector of IDs if needed
rsv_inpatient_hilmo_ids <- outcomes$HILMO_ID

# final join and saving ######
mbr_hilmo <- left_join(mbr_c, outcomes, by = "TNRO") %>%
  mutate(outcome = ifelse(is.na(outcome), 0, outcome)) %>%
  mutate(outcome_3days = ifelse(is.na(outcome_3days), 0, outcome_3days)) %>%
  # just in case, let's remove a variable called V1, a column number
  select(-any_of(c("V1")))


mbr_hilmo %>%
  filter(outcome == 1)


setwd("/data/projects/project_pvartiai/rsv/wrangle/")
write.csv(mbr_hilmo, "preprocess_3_mbr_hilmo_outcomes.csv",
          row.names = FALSE)








#  S I B L I N G      R E S P     H O S P I T A L I Z A T I O N    # # # # # # # # # #




### sibling young age respiratory infection hospitalizations

hilmo_sibs
hilmo_dg_sibs
all_bds

# diagnoses with unspecified bronchitis/bronchiolitis
resp_dg_sib <- hilmo_dg_sibs %>%
  filter(substr(KOODI, 1, 3) %in% c("J21") |
   KOODI %in% rsv_dg_codes)

# hilmo ID's corresponding all possible j21.9 diagnosis
resp_id_sib <- resp_dg_sib$HILMO_ID %>% unique

# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_resp_sib_entries <- hilmo_sibs %>%
  filter(HILMO_ID %in% resp_id_sib)

# all inpatient entries. Inpatient: PALA == 1
resp_sib_inpatients <- all_resp_sib_entries %>%
  filter(PALA == 1|YHTEYSTAPA == "R80"|KIIREELLISYYS == "3") %>%
  rename(sibling_TNRO = TNRO)

# Combine birth dates to the inpatient entries
resp_sib_inpatients <- left_join(resp_sib_inpatients, sibs, by = "sibling_TNRO")

# calculate kids' age at hilmo entry
resp_sib_inpatients <- resp_sib_inpatients %>%
  mutate(age_at_outcome = difftime(TUPVA, sibling_dob, units = "days"))


# filter only cases where age at admission is BETWEEN 1 AND 3 YEARS
resp_sib_inpatients_agefilt <- resp_sib_inpatients %>%
  filter(age_at_outcome > 6 & age_at_outcome < 1460)

# filter if hospitalization occurs before birth of the child (important!)
resp_sib_inpatients_agefilt <- resp_sib_inpatients_agefilt  %>%
  filter(TUPVA < LAPSEN_SYNTYMAPVM)

# pull the ID vector
sib_ids_with_resp_hosp <- resp_sib_inpatients_agefilt %>% pull(sibling_TNRO) %>% unique

# find unique sibling ids who have been hospitalized for OTHER BRONCHITIS
cases_with_sib_resp_hosp <- sibs %>%
  select(TNRO, sibling_TNRO) %>%
  mutate(sib_resp_hosp = case_when(
    sibling_TNRO %in% sib_ids_with_resp_hosp ~ 1,
    TRUE ~ 0)) %>%
  filter(sib_resp_hosp == 1) %>%
  select(TNRO, sib_resp_hosp) %>% 
  distinct



# join sib_rsv_hospitalizations to the column containing all ids
sib_resp_hosps <- all_bds %>%
  left_join(cases_with_sib_resp_hosp, by = "TNRO") %>%
  select(-LAPSEN_SYNTYMAPVM) %>% 
  mutate(sib_resp_hosp = ifelse(is.na(sib_resp_hosp), 
                                                   0, 
                                                   sib_resp_hosp)) 



setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(sib_resp_hosps, "sibling_resp_hospitalization.csv",
          row.names = FALSE)





## sibling asthma diagnosis

sibs
hilmo_sibs
hilmo_dg_sibs
all_bds

# diagnoses with unspecified bronchitis/bronchiolitis
a_dg_sib <- hilmo_dg_sibs %>%
  filter(substr(KOODI, 1, 3) %in% c("J45"))

# hilmo ID's corresponding all possible j21.9 diagnosis
a_id_sib <- a_dg_sib$HILMO_ID %>% unique

# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_a_sib_entries <- hilmo_sibs %>%
  filter(HILMO_ID %in% a_id_sib) %>%
  rename(sibling_TNRO = TNRO)


# Combine birth dates to the inpatient entries
a_sib_entries <- left_join(all_a_sib_entries, sibs, by = "sibling_TNRO")

# calculate kids' age at hilmo entry
a_sib_entries <- a_sib_entries %>%
  mutate(age_at_outcome = difftime(TUPVA, sibling_dob, units = "days"))

a_sib_entries %>%
  select(sibling_TNRO, sibling_agediff, sibling_dob, LAPSEN_SYNTYMAPVM, age_at_outcome)

# filter only cases where diagnosis date is before the index child's birth
a_sib_entries_agefilt <- a_sib_entries %>%
  filter(TUPVA < LAPSEN_SYNTYMAPVM)

# pull the ID vector
sib_ids_with_asthma_dg <- a_sib_entries_agefilt %>% pull(sibling_TNRO) %>% unique

# find unique sibling ids who have been hospitalized for OTHER BRONCHITIS
cases_with_sib_asthma <- sibs %>%
  select(TNRO, sibling_TNRO) %>%
  mutate(sib_asthma_dg = case_when(
    sibling_TNRO %in% sib_ids_with_asthma_dg ~ 1,
    TRUE ~ 0)) %>%
  filter(sib_asthma_dg == 1) %>%
  select(TNRO, sib_asthma_dg) %>% 
  distinct



# join sib_rsv_hospitalizations to the column containing all ids
sib_asthma <- all_bds %>%
  left_join(cases_with_sib_asthma, by = "TNRO") %>%
  select(-LAPSEN_SYNTYMAPVM) %>% 
  mutate(sib_asthma_dg = ifelse(is.na(sib_asthma_dg), 
                                0, 
                                sib_asthma_dg))




setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(sib_asthma, "sibling_asthma_dg.csv",
          row.names = FALSE)





















