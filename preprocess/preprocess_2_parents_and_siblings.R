# This script extracts parents' and siblings' ids and ages from DVV relatives
# continue from the preprocessed hilmo-mbr -data including all patients (so not development/validation set)

# in the end, also define mother_tongue variable that's saved in a different file in 'predictors'


#libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)


# dvv files
dvv_dir <- "/data/processed_data/dvv/"
dvv_relative_name <- "Tulokset_1900-2010_tutkhenk_ja_sukulaiset.txt.finreg_IDsp"
dvv_relative <- fread(paste0(dvv_dir, dvv_relative_name)) %>% as_tibble

# preprocessed RSV data
rsvdata <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_1_mbr.csv") %>% as_tibble

# birthdays from original MBR 
original_bds <- fread("/data/processed_data/thl_birth/birth_2022-03-08.csv",
                      select = c("LAPSI_TNRO", "AITI_TNRO", "AIDIN_SYNTYMAPVM", "LAPSEN_SYNTYMAPVM", "KUOLLEISUUS")) %>%
  as_tibble() %>%
  filter(LAPSI_TNRO != "") %>%
  filter(KUOLLEISUUS == 3) %>%
  select(-KUOLLEISUUS) %>%
  rename(TNRO = LAPSI_TNRO) %>% as_tibble

all_ids <- rsvdata %>% select(TNRO)

all_bds <- left_join(all_ids, original_bds, by = "TNRO")

kids_bds <- all_bds %>% 
  select(TNRO, LAPSEN_SYNTYMAPVM)

kids_relatives <- dvv_relative %>%
  filter(Relative_ID %in% rsvdata$TNRO)

# parents #####
parents_vars <- kids_relatives %>%
  filter(Relationship == 2) %>%
  rename(TNRO = Relative_ID,
         parent_TNRO = FINREGISTRYID) %>%
  select(TNRO, parent_TNRO) 

# in DVV relatives, there are no missing genders
parent_data <- dvv_relative %>%
  filter(FINREGISTRYID %in% parents_vars$parent_TNRO) %>%
  filter(Relationship == 0) %>%
  select(FINREGISTRYID, Sex, Relative_DOB, Home_town) %>%
  rename(parent_TNRO = FINREGISTRYID,
         parent_dob = Relative_DOB,
         parent_home_town = Home_town)


parents_data_joined <- left_join(parents_vars, parent_data, by = "parent_TNRO")




all_ids <- rsvdata %>% select(TNRO)

all_bds <- left_join(all_ids, original_bds, by = "TNRO")

kids_bds <- all_bds %>% 
  select(TNRO, LAPSEN_SYNTYMAPVM)

kids_relatives <- dvv_relative %>%
  filter(Relative_ID %in% rsvdata$TNRO)

# parents #####
parents_vars <- kids_relatives %>%
  filter(Relationship == 2) %>%
  rename(TNRO = Relative_ID,
         parent_TNRO = FINREGISTRYID) %>%
  select(TNRO, parent_TNRO) 

# in DVV relatives, there are no missing genders
parent_data <- dvv_relative %>%
  filter(FINREGISTRYID %in% parents_vars$parent_TNRO) %>%
  filter(Relationship == 0) %>%
  select(FINREGISTRYID, Sex, Relative_DOB, Home_town) %>%
  rename(parent_TNRO = FINREGISTRYID,
         parent_dob = Relative_DOB,
         parent_home_town = Home_town)


parents_data_joined <- left_join(parents_vars, parent_data, by = "parent_TNRO")






# missing mother ids in birth registry. Hence complicated recoding for the parent type and id variables
parents_pre <- left_join(all_bds, parents_data_joined, by = "TNRO") %>%
  mutate(parent_type = case_when(
    AITI_TNRO == parent_TNRO ~ "mother",
    is.na(AITI_TNRO) & Sex == 2 ~ "mother",
    !is.na(AITI_TNRO) & AITI_TNRO != parent_TNRO ~ "father_or_else",
    is.na(AITI_TNRO) & Sex == 1 ~ "father_or_else",
    TRUE ~ NA_character_
  )) %>%
  # If the parent has no data in the 'parents_data_joined' file, 
  # they are only in the medical birth registry and
  # thus must be mothers - if mother id not missing
  mutate(parent_type = ifelse(is.na(parent_type)&!is.na(AITI_TNRO), "mother", parent_type),
         parent_TNRO = ifelse(is.na(parent_TNRO), AITI_TNRO, parent_TNRO),
         Sex = ifelse(is.na(Sex)&!is.na(AITI_TNRO), 2, Sex),
         parent_dob = ifelse(is.na(parent_dob)&!is.na(AIDIN_SYNTYMAPVM), AIDIN_SYNTYMAPVM, parent_dob)) 


# Missing mothers 
# mother ids missing in DVV relatives are extracted from birth regsitry #####
parents_pre_wide <- parents_pre %>%
  select(TNRO, parent_TNRO, parent_type) %>%
  pivot_wider(names_from = parent_type, 
              values_from = parent_TNRO) %>%
  select(-any_of("NA"))

# still 16859 mother id's missing. They will be obtained from birth registry!
missing_mothers <- parents_pre_wide %>% 
  filter(is.na(mother))

missing_mothers <- left_join(missing_mothers, all_bds, by = "TNRO") %>%
  mutate(mother = AITI_TNRO) %>%
  pivot_longer(cols = c(father_or_else, mother)) %>%
  rename(parent_TNRO = value, parent_type = name) %>%
  filter(parent_type == "mother") %>%
  mutate(Sex = 2,
         parent_dob = as.double(AIDIN_SYNTYMAPVM)) 

# missing fathers #####
# we can find approx 6500 father id's that were not identified by looking at the child relative variable,
# but instead by identifying relatives with 'father' status

missing_father_ids <- parents_pre_wide %>%
  filter(is.na(father_or_else))

# careful here, we need to get id's right. FINREGISTRYID is now the child's id, relativeid is fathers' id
missing_fathers <- dvv_relative %>%
  select(FINREGISTRYID, Relationship, Relative_ID, Sex, Relative_DOB) %>%
  filter(Relationship == "3i") %>%
  filter(FINREGISTRYID %in% missing_father_ids$TNRO) %>%
  select(-Relationship) %>%
  rename(TNRO = FINREGISTRYID,
         parent_dob = Relative_DOB,
         parent_TNRO = Relative_ID,
         parent_gender = Sex) %>%
  mutate(parent_type = "father_or_else") %>%
  select(TNRO, parent_TNRO, parent_type, parent_dob, parent_gender)


# add missing mothers to the data

parents_pre_mothers <- bind_rows(parents_pre, missing_mothers) %>%
  select(-c(AITI_TNRO, AIDIN_SYNTYMAPVM, parent_home_town)) %>%
  rename(parent_gender = Sex) %>%
  select(TNRO, parent_TNRO, parent_type, parent_dob, parent_gender) %>%
  # Cchecked the origin from DVV relatives, confirmed that mothers dob 
  # in medical birth registry match
  mutate(parent_dob = as.Date(parent_dob, origin = "1970-01-01"))


parents <- bind_rows(parents_pre_mothers, missing_fathers)

parents1 <- left_join(parents, all_bds, by = "TNRO") %>%
  rename(kid_dob = LAPSEN_SYNTYMAPVM) %>%
  select(TNRO, kid_dob, parent_TNRO, parent_dob, parent_type, parent_gender) %>%
  mutate(parent_age = as.numeric(difftime(kid_dob, parent_dob, units = "days"))/365.25)


mothers <- parents1 %>%
  filter(parent_type == "mother") %>%
  select(-parent_type) %>%
  rename(mother_age = parent_age) %>%
  left_join(all_ids, ., by = "TNRO")

# any missing mothers?
mothers %>%
  filter(is.na(parent_TNRO)) # 0

mothers %>%
  pull(kid_dob) %>% summary() # ok


fathers <- parents1 %>%
  filter(parent_type == "father_or_else") %>%
  select(-parent_type) %>%
  rename(father_age = parent_age) %>%
  # filter away all female-registered fathers
  filter(parent_gender == 1) %>%
  left_join(all_ids, ., by = "TNRO")


# missing check
missing_fathers <- fathers %>%
  filter(is.na(parent_TNRO)) %>%
  select(TNRO)
# 39586


all_bds %>%
  filter(TNRO %in% missing_fathers$TNRO) %>%
  mutate(year = year(LAPSEN_SYNTYMAPVM)) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  print(n=100)


# Checks
# - Two mothers have male gender. We decide to trust the medical birth registry.
# - 98 father-defined have female gender registered. They are excluded
# - No study patients have more than two parents.
# - No-one has missing mother id in medical birth registry.


# missingness
# no missing mother ids
# father ID: 39,596




setwd("/data/projects/project_pvartiai/rsv/family/")

write.csv(fathers, file = "fathers.csv",
          row.names = FALSE)
write.csv(mothers, file = "mothers.csv",
          row.names = FALSE)
write.csv(parents, file = "parents.csv",
          row.names = FALSE)







# siblings #####

#  we will do a sibling pair data for full medical birth registry, not only for 
# RSV data patients, to identify all previous siblings

# Connect siblings from mothers in original medical birth registry
sibs_pre <- original_bds %>%
  select(TNRO, AITI_TNRO, LAPSEN_SYNTYMAPVM) %>%
  filter(TNRO != "") %>%
  group_by(AITI_TNRO) %>%
  arrange(LAPSEN_SYNTYMAPVM, .by_group = TRUE) %>%
  ungroup()

# calculate number of siblings (from mothers side) for each ibirth registry id
mothers_sibs <- sibs_pre %>% group_by(AITI_TNRO) %>%
  summarise(n_of_sibs = n()) %>% ungroup

sib_ids <- sibs_pre %>% select(-LAPSEN_SYNTYMAPVM) %>%
  rename(sibling_TNRO = TNRO)

sibs <- left_join(sibs_pre, mothers_sibs, by = "AITI_TNRO")

sibs_from_mbr_long <- inner_join(sibs, sib_ids, by = "AITI_TNRO")


sibs_from_mbr_long <- sibs_from_mbr_long %>%
  filter(!is.na(TNRO)) %>%
  filter(!is.na(sibling_TNRO)) %>%
  filter(TNRO != sibling_TNRO) %>%
  select(TNRO, LAPSEN_SYNTYMAPVM, AITI_TNRO, sibling_TNRO, n_of_sibs)

sib_bds <- original_bds %>%
  select(TNRO, LAPSEN_SYNTYMAPVM) %>%
  filter(TNRO != "") %>%
  rename(sibling_TNRO = TNRO, 
         sibling_dob = LAPSEN_SYNTYMAPVM) 

siblings_mbr <- left_join(sibs_from_mbr_long, sib_bds, by = "sibling_TNRO")

siblings_mbr <- siblings_mbr %>% select(TNRO, LAPSEN_SYNTYMAPVM, sibling_TNRO, sibling_dob) %>%
  mutate(Relationship = "4a")


# sibs from DVV relatives ######
# this we will only do for rsv study patients


rel_sibs_all <- dvv_relative %>%
  filter(Relative_ID %in% rsvdata$TNRO) %>%
  filter(substr(Relationship, 1, 1) == "4") %>%
  select(FINREGISTRYID, Relative_ID, Relative_DOB, Relationship) %>%
  rename(TNRO = Relative_ID,
         sibling_TNRO = FINREGISTRYID) %>%
  distinct()

# load birth dates from minimal pheno
mini_bds <- fread("/data/processed_data/minimal_phenotype/minimal_phenotype_2022-03-28.csv",
                  select = c("FINREGISTRYID", "date_of_birth")) %>%
  rename(sibling_TNRO = FINREGISTRYID,
         sibling_dob = date_of_birth)



rel_sibs <- left_join(rel_sibs_all, mini_bds, by = "sibling_TNRO")


index_bds <- kids_bds 

siblings_rel <- left_join(rel_sibs, index_bds, by = "TNRO") %>%
  select(TNRO, LAPSEN_SYNTYMAPVM, sibling_TNRO, sibling_dob, Relationship)

#
#
#
#
#


# AN ATTEMPT TO IDENTIFY SIBLINGS FROM FINREGISTRY PEDIGREE FILES
### Siblings from combining parent's ids
# Aoxing's pedigree
pedi_path <- "/data/processed_data/dvv/"
pedi_name <- "dvv_pedigree_withfamid.20220501.tsv"
relpair_name <- "Index_RelativePair_basic.20220501.tsv"

# read Aoxing's pedigree

pedi <- fread(paste0(pedi_path, pedi_name)) %>%
  as_tibble() %>%
  rename(TNRO = ID)

#from birth registry, select all mother ids
bd_mothers <- original_bds %>%
  select(TNRO, AITI_TNRO) %>%
  rename(bd_mother_id = AITI_TNRO)

# connect mother ids to the pedigree.
# adds around 19 000 mother IDs
enhanced_pedi <- left_join(pedi, bd_mothers) %>%
  mutate(MOTHER_ID = ifelse(is.na(MOTHER_ID)&!is.na(bd_mother_id), bd_mother_id, MOTHER_ID))

# check
enhanced_pedi %>%
  filter(is.na(MOTHER_ID)) 

rsv_relevant_pedi <- enhanced_pedi %>%
  filter(!(is.na(MOTHER_ID)&is.na(FATHER_ID))) %>%
 # filter(MOTHER_ID %in% parents$parent_TNRO | FATHER_ID %in% parents$parent_TNRO) %>%
  select(TNRO, Birth_Date, FATHER_ID, MOTHER_ID, sex) 


sib_candidates <- rsv_relevant_pedi %>%
  select(-sex) %>%
  rename(sibling_TNRO = TNRO,
         sib_dob = Birth_Date)

joining_attempt <- full_join(rsv_relevant_pedi, sib_candidates, by = c("FATHER_ID", "MOTHER_ID"))

# join full sibs
pedigree_full_sibs <- joining_attempt %>%
  filter(TNRO != sibling_TNRO) %>%
  mutate(sibling_agediff = difftime(Birth_Date, sib_dob, units = "days")) %>%
  mutate(sibling_agediff = as.numeric(sibling_agediff) / 365.25)

# connect half siblings (father) via family pedigree
pedigree_half_sibs_father <- full_join((rsv_relevant_pedi %>%
                                          filter(!is.na(FATHER_ID))), 
                                       (sib_candidates %>% 
                                          filter(!is.na(FATHER_ID))), 
                                       by = "FATHER_ID") %>%
  filter(MOTHER_ID.x != MOTHER_ID.y | is.na(MOTHER_ID.x) | is.na(MOTHER_ID.y)) %>%
  filter(TNRO != sibling_TNRO)

# connect half siblings (mother) via family pedigree
pedigree_half_sibs_mother <- full_join((rsv_relevant_pedi %>%
                                          filter(!is.na(MOTHER_ID))), 
                                       (sib_candidates %>% 
                                          filter(!is.na(MOTHER_ID))), 
                                       by = "MOTHER_ID") %>%
  filter(FATHER_ID.x != FATHER_ID.y | is.na(FATHER_ID.x) | is.na(FATHER_ID.y)) %>%
  filter(TNRO != sibling_TNRO)

full_sibs <- left_join(all_bds, pedigree_full_sibs) %>%
  select(-any_of(c("AITI_TNRO", "AIDIN_SYNTYMAPVM", "Birth_Date", "MOTHER_ID", "FATHER_ID", "sex"))) %>%
  filter(!is.na(sibling_TNRO))


half_sibs_mother <- left_join(all_bds, pedigree_half_sibs_mother) %>%
  select(-any_of(c("AITI_TNRO", "AIDIN_SYNTYMAPVM", "Birth_Date", "MOTHER_ID", "sex"))) %>%
  mutate(sibling_agediff = difftime(LAPSEN_SYNTYMAPVM, sib_dob, units = "days")) %>%
  mutate(sibling_agediff = as.numeric(sibling_agediff) / 365.25) %>%
  filter(!is.na(sibling_TNRO)) %>%
  rename(FATHER_ID = FATHER_ID.x,
         sibling_father_id = FATHER_ID.y)


half_sibs_father <- left_join(all_bds, pedigree_half_sibs_father) %>%
  select(-any_of(c("AITI_TNRO", "AIDIN_SYNTYMAPVM", "Birth_Date", "FATHER_ID", "sex"))) %>%
  mutate(sibling_agediff = difftime(LAPSEN_SYNTYMAPVM, sib_dob, units = "days")) %>%
  mutate(sibling_agediff = as.numeric(sibling_agediff) / 365.25) %>%
  filter(!is.na(sibling_TNRO)) %>%
  rename(MOTHER_ID = MOTHER_ID.x,
         sibling_mother_id = MOTHER_ID.y)

older_full_sibs <- full_sibs %>%
  filter(sibling_agediff > 0) %>%
  filter(sibling_agediff < 18)


older_halfsibs_f <- half_sibs_father %>%
  filter(sibling_agediff > 0) %>%
  filter(sibling_agediff < 18)

older_halfsibs_m <- half_sibs_mother %>%
  filter(sibling_agediff > 0) %>%
  filter(sibling_agediff < 18)

n.of.sibs <- function(sib_data) {
  sib_data %>%
  mutate(sib_age_0_4 = ifelse(sibling_agediff < 4, 1, 0),
         sib_age_4_7 = ifelse(between(sibling_agediff, 4, 7), 1, 0),
         sib_age_over7 = ifelse(sibling_agediff > 7, 1, 0)
  ) %>%
    select(-LAPSEN_SYNTYMAPVM) %>%
    distinct() %>%
    group_by(TNRO) %>%
    summarise(sib_0_4 = sum(sib_age_0_4),
              sib_4_7 = sum(sib_age_4_7),
              sib_over7 = sum(sib_age_over7)
    ) %>%
    ungroup()
}

n_of_halfsibs_f <- n.of.sibs(sib_data = older_halfsibs_f)
n_of_halfsibs_m <- n.of.sibs(sib_data = older_halfsibs_m)
n_of_fullsibs <- n.of.sibs(sib_data = older_full_sibs)

# ids who have siblings
n_id_with_fullsibs <- n_of_fullsibs %>% select(TNRO) %>% distinct() %>% nrow()

n_id_halfsibs <- c(
 (n_of_halfsibs_f %>% pull(TNRO)),
 (n_of_halfsibs_m %>% pull(TNRO))) %>% unique %>% length
  


# relative pairs ARE ONLY FOR INDEX PERSONS. NOT WORTH IT
# 
# relpair <- fread(paste0(pedi_path, relpair_name)) %>%
#   as_tibble() %>%
#   rename(TNRO = ID)
# 
# relpair_rsv <- relpair %>%
#   # select only ids that are in the rsv study population
#   filter(TNRO %in% all_bds$TNRO) %>%
#   filter(relationship_pair %in% c(4, 5, 6)) %>%
#   # rename sib id variable
#   rename(sibling_TNRO = Relative_ID) %>%
#   # join birth date variables
#   left_join(., mini_bds, by = "sibling_TNRO") %>%
#   left_join(all_bds, by = "TNRO") %>%
#   # calculate sibling agediff
#   mutate(sibling_agediff = difftime(LAPSEN_SYNTYMAPVM, sibling_dob, units = "days")) %>%
#   mutate(sibling_agediff = as.numeric(sibling_agediff) / 365.25)
# 





### Combining sibling data
# bind homogenous sibling data from dvv relatives and birth registry
# keep only older siblings - twins will be excluded
siblings <- bind_rows(siblings_mbr, siblings_rel) %>% 
  select(-Relationship) %>%
  distinct() %>%
  mutate(sibling_agediff = interval(sibling_dob, LAPSEN_SYNTYMAPVM) / years(1)) %>%
  filter(sibling_TNRO != "") %>%
  filter(TNRO != "") %>%
  filter(sibling_agediff > 0.1) %>%
  filter(sibling_agediff <= 18) 

# test
siblings %>%
  filter(TNRO == "FR0000028")

n_of_sibs <- siblings %>%
  mutate(sib_age_0_4 = ifelse(sibling_agediff < 4, 1, 0),
         sib_age_4_7 = ifelse(between(sibling_agediff, 4, 7), 1, 0),
         sib_age_over7 = ifelse(sibling_agediff > 7, 1, 0)
  ) %>%
  select(-LAPSEN_SYNTYMAPVM) %>%
  distinct() %>%
  group_by(TNRO) %>%
  summarise(sib_0_4 = sum(sib_age_0_4),
            sib_4_7 = sum(sib_age_4_7),
            sib_over7 = sum(sib_age_over7)
  ) %>%
  ungroup()


n_of_sibs <- n_of_sibs %>%
  mutate(sib_0_4 = ifelse(sib_0_4 > 0, 1, 0),
         sib_4_7 = ifelse(sib_4_7 > 0, 1, 0),
         sib_over7 = ifelse(sib_over7 > 0, 1, 0),
         have_sibs = 1) %>%
  left_join(all_ids, ., by = "TNRO")





setwd("/data/projects/project_pvartiai/rsv/predictors")
write.csv(n_of_sibs, "number_of_siblings.R",
    row.names = FALSE)


# file is saved bc of long for loop 
setwd("/data/projects/project_pvartiai/rsv/family/")
write.csv(siblings, "siblings_from_mbr_and_relatives.csv",
    row.names = FALSE)



# #
# #
# # mother tongue from dvv relatives
# #
# #



# mother_tongue_pre <- dvv_relative %>%
#     filter(Relative_ID %in% pull(all_ids, TNRO)) %>%
#     select(Relative_ID, Mother_tongue) %>%
#     distinct() %>%
#     rename(TNRO = Relative_ID) %>%
#     mutate(mother_tongue = case_when(
#     Mother_tongue %in% c("fi", "sv", "ru") ~ Mother_tongue,
#     Mother_tongue == "" ~ NA_character_,
#     TRUE ~ "other")) %>%
#     select(-Mother_tongue)

# mother_tongue <- left_join(all_ids, mother_tongue_pre, by = "TNRO")
# mother_tongue[is.na(mother_tongue)] <- "other"

# setwd("/data/projects/project_pvartiai/rsv/predictors/")
# write.csv(mother_tongue, "mother_tongue.csv",
#     row.names = FALSE)


# rm(mo_to, mother_tongue_rel)









