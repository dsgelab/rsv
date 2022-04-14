# HILMO outcomes

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)

# hilmo directories
hilmo_dir <- "/data/processed_data/thl_hilmo/"

# hilmo exists in processed and original. From now on, we can use only processed hilmo.
hilmo_name <- "thl2019_1776_hilmo.csv.finreg_IDsp"
hilmo_dg_name <- "thl2019_1776_hilmo_diagnoosit_kaikki.csv.finreg_IDsp"

# load basic population data, mbr_c
mbr_c <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_1_mbr.csv") %>% 
	as_tibble

# create id and birthday vectors for hilmo selection
all_ids <- mbr_c$TNRO

kids_bd <- mbr_c %>%
    select(TNRO, LAPSEN_SYNTYMAPVM)

all_bds <- mbr_c %>%
    select(TNRO, LAPSEN_SYNTYMAPVM)

# updated mothers path
mothers <- fread("/data/projects/project_pvartiai/rsv/family/mothers.csv") %>%
	as_tibble

sibs <- fread("/data/projects/project_pvartiai/rsv/family/siblings_from_mbr_and_relatives.csv") %>%
    as_tibble

# old path
# mothers <- fread("/home/pvartiai/RSV/data/family/mothers.csv") %>% as_tibble

# updated preprocess 1 data
viim_kk <- mbr_c %>% select(TNRO, gest_days) %>%
    as_tibble

mother_dg <- left_join(mothers, viim_kk, by = "TNRO") %>%
    mutate(kid_dob = as.Date(kid_dob)) %>%
    mutate(viim_kk = kid_dob - gest_days) %>%
    select(-gest_days, -parent_gender, -mother_age, -parent_dob)




# Hilmo loading and filtering # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## If need to load the filtered files
## paths for hilmo_original_kids and hilmo_orig_dg_kids need to be updated at some point, when running the whole script
# hilmo_original_kids <- read_feather("/home/pvartiai/RSV/data/wrangle/hilmo_original_kids.feather")
# hilmo_orig_dg_kids <- read_feather("/home/pvartiai/RSV/data/wrangle/hilmo_orig_dg_kids.feather")
# pregnancy_hilmo_compl_entries <- fread("/data/projects/project_pvartiai/rsv/wrangle/mothers_raw_o_diagnoses_for_recoding_feb_2022.csv")


## updated paths
hilmo_original_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_original_kids.csv")
hilmo_orig_dg_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_orig_dg_kids.csv")


# loading the whole hilmo
hilmo_raw <- fread(paste0(hilmo_dir, hilmo_name),
                   select = c("HILMO_ID", "TNRO", "PALTU", "PALTUTAR",
                                         "PALA", "EA", "TUPVA", "LPVM")) %>%
        as_tibble()

hilmo_diagnoses <- fread(paste0(hilmo_dir, hilmo_dg_name)) %>% as_tibble()


# filter all other than children away
hilmo_original_kids <- hilmo_raw %>%
    filter(TNRO %in% all_ids) %>%
    select(c("HILMO_ID", "TNRO", "PALTU", "PALTUTAR",
            "PALA", "EA", "TUPVA", "LPVM"))

# same filtering for hilmo_diagnoses
hilmo_orig_dg_kids <- hilmo_diagnoses %>%
    filter(TNRO %in% all_ids)


# filter mothers' hilmo entries
hilmo_mothers_raw <- hilmo_raw %>%
	filter(TNRO %in% mother_dg$parent_TNRO) %>%
	rename(parent_TNRO = TNRO)

all_mother_hilmo_entries <- inner_join(hilmo_mothers_raw, mother_dg, by = "parent_TNRO") 

pregnancy_hilmo_entries <- all_mother_hilmo_entries %>%
	filter(TUPVA <= kid_dob) %>%
	filter(LPVM > viim_kk)

pregnancy_hilmo_dgs <- hilmo_diagnoses %>%
	filter(HILMO_ID %in% pregnancy_hilmo_entries$HILMO_ID) %>%
	select(-TNRO)

pregnancy_hilmo_compl_entries <- inner_join(pregnancy_hilmo_entries, 
                                            pregnancy_hilmo_dgs, 
                                            by = "HILMO_ID") %>% 
select(TNRO, HILMO_ID, parent_TNRO, KOODI) %>%
rename(value = KOODI) %>%
filter(substr(value, 1, 1) == "O" | substr(value, 1, 3) %in% c("E03", "E66", "Z35")) %>%
# create a 3-digit q_level variable
mutate(o_level = substr(value, start = 1, stop = 3))


setwd("/data/projects/project_pvartiai/rsv/wrangle")
write.csv(pregnancy_hilmo_compl_entries, "mothers_raw_o_diagnoses_for_recoding_feb_2022.csv", 
	row.names = FALSE)






# KIDS FIRST 7 DAYS' OF NEONATAL DIAGNOSES # # # # # # # # # # # # # # # # # # # # #

index_bds <- mbr_c %>%
    select(TNRO, LAPSEN_SYNTYMAPVM)

hilmo_kids_filtered <- left_join(hilmo_original_kids, index_bds, by = "TNRO") %>%
	filter(!is.na(LAPSEN_SYNTYMAPVM)) %>%
    mutate(age_at_outcome = as.numeric(difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))) %>%
    # events occurring at first week of life:
    filter(age_at_outcome < 8) %>%
    group_by(TNRO) %>% 
    arrange(age_at_outcome, .by_group = TRUE) %>%
    slice_head() %>%
    ungroup()

babies_hilmo_dgs <- hilmo_orig_dg_kids %>%
    filter(HILMO_ID %in% hilmo_kids_filtered$HILMO_ID) %>%
    rename(value = KOODI) %>%
    select(HILMO_ID, TNRO, value)

# save first neonatal diagnoses from HILMO
setwd("/data/projects/project_pvartiai/rsv/wrangle")
write.csv(babies_hilmo_dgs, "index_babies_first_hilmo_diagnoses.csv",
	row.names = FALSE)



# clean complete hilmo diagnoses from memory
rm(hilmo_raw)
rm(hilmo_diagnoses)
gc()



# save original, filtered hilmos with feather
setwd("/data/projects/project_pvartiai/rsv/wrangle")
write.csv(hilmo_original_kids, "hilmo_original_kids.csv",
	row.names = FALSE)
write.csv(hilmo_orig_dg_kids, "hilmo_orig_dg_kids.csv", 
	row.names = FALSE)

# write_feather(hilmo_original_kids, "/home/pvartiai/RSV/data/wrangle/hilmo_original_kids.feather")
# write_feather(hilmo_orig_dg_kids, "/home/pvartiai/RSV/data/wrangle/hilmo_orig_dg_kids.feather")

# if need to read
# hilmo_original_kids <- read_feather("hilmo_original_kids.feather")
# hilmo_orig_dg_kids <- read_feather("hilmo_orig_dg_kids.feather")


# identify first RSV entries from HILMO #########

# all possible RSV entries
rsv_dgs <- hilmo_orig_dg_kids %>%
    filter(KOODI == "J210")

rsv_dgs %>%
    select(KENTTA) %>% table

# hilmo ID's corresponding all possible RSV diagnosis
rsv_ids <- rsv_dgs$HILMO_ID %>% unique

# personal ids of all possible RSV diagnosis
rsv_personal_ids <- rsv_dgs$TNRO %>% unique

# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_rsv_entries <- hilmo_original_kids %>%
    filter(HILMO_ID %in% rsv_ids)

# all inpatient entries (with rsv diagnosis). Inpatient: PALA == 1
# Period_duration indicates the duration of the hospitalization
rsv_inpatients <- all_rsv_entries %>%
    filter(PALA == 1) %>%
    mutate(period_duration = difftime(LPVM, TUPVA, units = "days"))

# Combine birth dates to the inpatient entries
rsv_inpatients <- left_join(rsv_inpatients, all_bds, by = "TNRO")

# calculate kids' age at hilmo entry
rsv_inpatients <- rsv_inpatients %>%
    mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"))

# filter only cases where age at admission is from 7 to 365 days
rsv_inpatients_agefilt <- rsv_inpatients %>%
    filter(age_at_outcome < 366 & age_at_outcome > 6)

# still 1086 duplficates!
# identify all duplicate FINREGISTRY IDs in rsv inpatient list

all_inp_ids <- rsv_inpatients_agefilt$TNRO

# duplicated ids (one row per single id)
duplicate_inp_ids <- all_inp_ids[duplicated(all_inp_ids)] 

# only unique IDs
unique_inp_ids <- setdiff(all_inp_ids, duplicate_inp_ids)

#unique RSV hilmo entries
only_unique_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
    filter(TNRO %in% unique_inp_ids)

# All RSV entries from the duplicated, arranged by age
duplicated_rsv_inpatient_entries <- rsv_inpatients_agefilt %>%
    filter(TNRO %in% duplicate_inp_ids) %>% 
    group_by(TNRO) %>%
    arrange(age_at_outcome, .by_group = TRUE)

# select the first inpatient rsv entry - the one with the youngest age
first_from_duplicates_rsv_entries <- duplicated_rsv_inpatient_entries %>%
    slice_head()

# outcomes is the final dataset of hilmo entries
outcomes <- bind_rows(only_unique_rsv_inpatient_entries, 
                      first_from_duplicates_rsv_entries) %>%
    mutate(outcome = 1) %>%
    mutate(outcome_3days = ifelse(period_duration > 2, 1, 0)) %>%
    select(-LAPSEN_SYNTYMAPVM)

# create vector of IDs if needed
rsv_inpatient_hilmo_ids <- outcomes$HILMO_ID

# final join and saving ######
mbr_hilmo <- left_join(mbr_c, outcomes, by = "TNRO") %>%
    mutate(outcome = ifelse(is.na(outcome), 0, outcome)) %>%
    mutate(outcome_3days = ifelse(is.na(outcome_3days), 0, outcome_3days))

# save
setwd("/data/projects/project_pvartiai/rsv/wrangle/")
write.csv(mbr_hilmo, "preprocess_3_mbr_hilmo_outcomes.csv",
	row.names = FALSE)
# write_feather(mbr_hilmo, "/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.feather")
# 



#  S I B L I N G      R S V    B R O N C H I O L I T I S    # # # # # # # # # #

# sibling birth dates
sib_bd <- sibs %>%
    select(sibling_TNRO, sibling_dob)


# sibs' rsv bronchiolitis
hilmo_sibs <- hilmo_raw %>%
    filter(TNRO %in% sibs$sibling_TNRO) %>%
    select(c("HILMO_ID", "TNRO", "PALTU", "PALTUTAR",
            "PALA", "EA", "TUPVA", "LPVM"))

hilmo_dg_sibs <- hilmo_diagnoses %>%
    filter(TNRO %in% sibs$sibling_TNRO)

rsv_dg_sib <- hilmo_dg_sibs %>%
    filter(KOODI == "J210") 

# hilmo ID's corresponding all possible RSV diagnosis
rsv_id_sib <- rsv_dg_sib$HILMO_ID %>% unique

# personal ids of all possible RSV diagnosis
rsv_sib_personal_ids <- rsv_dg_sib$sibling_TNRO %>% unique

# all possible HILMO entries containing RSV diagnosis, based on HILMO_ID
all_rsv_sib_entries <- hilmo_sibs %>%
    filter(HILMO_ID %in% rsv_id_sib)

# all inpatient entries (with rsv diagnosis). Inpatient: PALA == 1
# Period_duration indicates the duration of the hospitalization
rsv_sib_inpatients <- all_rsv_sib_entries %>%
    filter(PALA == 1) %>%
    rename(sibling_TNRO = TNRO)

# Combine birth dates to the inpatient entries
rsv_sib_inpatients <- left_join(rsv_sib_inpatients, sib_bd, by = "sibling_TNRO")

# calculate kids' age at hilmo entry
rsv_sib_inpatients <- rsv_sib_inpatients %>%
    mutate(age_at_outcome = difftime(TUPVA, sibling_dob, units = "days"))


# filter only cases where age at admission is from 7 to 365 days
rsv_sib_inpatients_agefilt <- rsv_sib_inpatients %>%
    filter(age_at_outcome < 366 & age_at_outcome > 6)

sib_ids_with_outcome <- rsv_sib_inpatients_agefilt %>% pull(sibling_TNRO) %>% unique

# find unique sibling ids who have been hospitalized
cases_with_sib_outcome <- sibs %>%
    select(TNRO, sibling_TNRO) %>%
    mutate(sib_rsv_hospitalization = case_when(
        sibling_TNRO %in% sib_ids_with_outcome ~ 1,
        TRUE ~ 0)) %>%
    filter(sib_rsv_hospitalization == 1) %>%
    select(TNRO, sib_rsv_hospitalization) %>% 
    distinct

# join sib_rsv_hospitalizations to the column containing all ids
sib_outcomes <- all_bds %>%
    left_join(cases_with_sib_outcome, by = "TNRO") %>%
    select(-LAPSEN_SYNTYMAPVM) %>% 
        mutate(sib_rsv_hospitalization = ifelse(is.na(sib_rsv_hospitalization), 
        0, 
        sib_rsv_hospitalization)) 


setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(sib_outcomes, "sibling_rsv_hospitalization.csv",
        row.names = FALSE)












