# lcreate bpd,  pediatric cardiac operations, down and esophagus malformation variables
library(tidyr)
library(data.table)
library(dplyr)
# names and dirs #####

hilmo_dir <- "/data/processed_data/thl_hilmo/"
tmp_name <- "thl2019_1776_hilmo_toimenpide.csv.finreg_IDsp"
tmp_new_name <- "THL2021_2196_HILMO_TOIMP.csv.finreg_IDsp"

hilmo_name <- "thl2019_1776_hilmo.csv.finreg_IDsp"
hilmo_dg_name <- "thl2019_1776_hilmo_diagnoosit_kaikki.csv.finreg_IDsp"

hilmo_orig_dir <- "/data/original_data/new/"
hilmo_orig_name <- "thl2019_1776_hilmo.csv.finreg_IDs"




# read the files #####
# hilmo entries for study patient kids
hilmo_orig_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_original_kids.csv") 
hilmo_orig_dg_kids <- fread("/data/projects/project_pvartiai/rsv/wrangle/hilmo_orig_dg_kids.csv") 




# ids and bds
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv",
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>%
  as_tibble



## neonatal diagnoses - we want them raw - i.e. uncoded, so that we can group cardiac defects
mbr_hilmo <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv")

# create a long data with dgs
mbr_dgs <- mbr_hilmo %>%
  as_tibble() %>%
  select(TNRO, RDIAG1:RDIAG10, SDIAG1:SDIAG10, ICD10_1:ICD10_10) %>%
  pivot_longer(cols = -TNRO) %>%
  select(-name) %>%
  filter(value != "") %>%
  rename(dg = value)


### malformations registry
malf <- fread("/data/processed_data/thl_malformations/malformations_anomaly_2022-01-26.csv") %>%
    as_tibble

# select only ICD10 diagnoses from malformations registry, only for index patients
# No missing ICD10 diagnoses

malf <- malf %>%
  filter(TNRO %in% all_bds$TNRO) %>%
  select(TNRO, ICD10) %>%
  rename(dg = ICD10) %>%
  mutate(dg = gsub("\\.", "", dg))

# Q diagnoses (malformations) in HILMO
hilmo_q <- hilmo_orig_dg_kids %>%
  filter(substr(KOODI, 1, 1) == "Q") %>%
  rename(dg = KOODI)

# Load Hilmo Operations registry
hilmo_kids_tmp_old <- fread(paste0(hilmo_dir, tmp_name)) %>% as_tibble %>%
  filter(TNRO %in% all_bds$TNRO)

hilmo_kids_tmp_new <- fread(paste0(hilmo_dir, tmp_new_name)) %>% as_tibble %>%
  filter(TNRO %in% all_bds$TNRO)

hilmo_kids_tmp <- bind_rows(hilmo_kids_tmp_old, hilmo_kids_tmp_new)


# check how many missing dates
# hilmo_kids_tmp %>%
#   select(TOIMPALKUPVM) %>% is.na %>% sum


# # # # # #      B P D      # # # # # # # # # # # # # # #

# BPD - bronchopulmonary dysplasia #####
bpd <- hilmo_orig_dg_kids %>%
  filter(KOODI == "P271") %>%
  select(TNRO) %>% unique %>%
  mutate(bpd = 1)

bpd <- left_join(select(all_bds, TNRO), bpd) %>%
  mutate(bpd = ifelse(is.na(bpd), 0, bpd))


setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(bpd, "bpds.csv", row.names = FALSE)



# # # # # # # # # # #  C O N G E N I T A L    H E A R T    D E F E C T S    # # # # # # #

# Long list of heart operation codes #
# to be used in identifying heart operations from hilmo operation registry
heart_oper_codes <- c(
  "FDA00",
  "FDA10",
  "FDA96",
  "FDB11",
  "FDB12",
  "FDB13",
  "FDB20",
  "FDB31",
  "FDB32",
  "FDB33",
  "FDB34",
  "FDB35",
  "FDB96",
  "FDC00",
  "FDC10",
  "FDC20",
  "FDC96",
  "FDD00",
  "FDD10",
  "FDD13",
  "FDD20",
  "FDD96",
  "FDE10",
  "FDE20",
  "FDE31",
  "FDE32",
  "FDE96",
  "FDF00",
  "FDF05",
  "FDG00",
  "FDG10",
  "FDG96",
  "FDH00",
  "FDH96",
  "FDJ00",
  "FDJ02",
  "FDJ10",
  "FDJ20",
  "FDJ30",
  "FDJ31",
  "FDJ42",
  "FDJ43",
  "FDJ96",
  "FDM00",
  "FDM10",
  "FDM96",
  "FDN02",
  "FDW96",
  "FFC00",
  "FFC02",
  "FFC03",
  "FFC22",
  "FFC40",
  "FFC42",
  "FFC50",
  "FFC96",
  "FFF00",
  "FFF10",
  "FFF20",
  "FFF96",
  "FFE02",
  "FFE10",
  "FFE96",
  "FFG00",
  "FFG10",
  "FFG20",
  "FFG30",
  "FFG96",
  "FFH00",
  "FFH10",
  "FFH22",
  "FFH25",
  "FFH96",
  "FFJ00",
  "FFJ10",
  "FFJ96",
  "FFK10",
  "FFK20",
  "FFK30",
  "FFK96",
  "FGA00",
  "FGA10",
  "FGA32",
  "FGA96",
  "FGB00",
  "FGB10",
  "FGB96",
  "FGC00",
  "FGC10",
  "FGC96",
  "FGD00",
  "FGD03",
  "FGD10",
  "FGD30",
  "FGD40",
  "FGD96",
  "FGE00",
  "FGE10",
  "FGE20",
  "FGE50",
  "FGE96",
  "FGW96",
  "FHA00",
  "FHA10",
  "FHA20",
  "FHA96",
  "FHB00",
  "FHB40",
  "FHB42",
  "FHB50",
  "FHB96",
  "FHC00",
  "FHC96",
  "FHD00",
  "FHD02",
  "FHD05",
  "FHD10",
  "FHD20",
  "FHD96",
  "FHE00",
  "FHE20",
  "FHE30",
  "FHE40",
  "FHE50",
  "FHE55",
  "FHE96",
  "FHF00",
  "FHF10",
  "FHF20",
  "FHF30",
  "FHF96",
  "FHG00",
  "FHG10",
  "FHG12",
  "FHG20",
  "FHG96",
  "FHH00",
  "FHH20",
  "FHH96",
  "FHJ00",
  "FHJ10",
  "FHJ96",
  "FHW96",
  "FJD00",
  "FJD10",
  "FJD20",
  "FJD96",
  "FJE00",
  "FJE10",
  "FJE20",
  "FJE30",
  "FJE42",
  "FJE96",
  "FJF00",
  "FJF10",
  "FJF12",
  "FJF20",
  "FJF96",
  "FJW96",
  "FKA00",
  "FKA10",
  "FKA20",
  "FKA96",
  "FKB00",
  "FKB10",
  "FKB20",
  "FKB30",
  "FKB96",
  "FKC00",
  "FKC10",
  "FKC20",
  "FKC30",
  "FKC40",
  "FKC50",
  "FKC60",
  "FKC61",
  "FKC80",
  "FKC96",
  "FKD00",
  "FKD10",
  "FKD20",
  "FKD96",
  "FKE05",
  "FKE10",
  "FKE50",
  "FKE60",
  "FKE90",
  "FKW96",
  "FLE00",
  "FLE10",
  "FLE20",
  "FLE32",
  "FLE96",
  "FMA00",
  "FMA10",
  "FMA20",
  "FMA96",
  "FMB00",
  "FMB10",
  "FMB20",
  "FMB96",
  "FMC00",
  "FMC10",
  "FMC20",
  "FMC96",
  "FMD00",
  "FMD10",
  "FMD15",
  "FMD20",
  "FMD30",
  "FMD40",
  "FMD96",
  "FME05",
  "FME10",
  "FME20",
  "FME30",
  "FME40",
  "FME60",
  "FME70",
  "FME90",
  "FMW96",
  "FB1AC",
  "FB1AT",
  "FB1BC",
  "FB1BD",
  "FB1BG",
  "FB1BT",
  "FB1CC",
  "FB1CG",
  "FB1DG",
  "FB1ST",
  "FBA00",
  "FBA10",
  "FBA96",
  "FBB00",
  "FBC00",
  "FBC10",
  "FBC96",
  "FBD00",
  "FBD10",
  "FBD12",
  "FBD20",
  "FBD96",
  "FBE00",
  "FBE10",
  "FBE20",
  "FBE32",
  "FBE35",
  "FBE42",
  "FBE96",
  "FBF00",
  "FBF10",
  "FBF20",
  "FBF96",
  "FBG00",
  "FBG10",
  "FBG96",
  "FBH00",
  "FBH10",
  "FBH96",
  "FBJ10",
  "FBJ96",
  "FBK00",
  "FBK01",
  "FBK03",
  "FBK10",
  "FBK13",
  "FBK96",
  "FBL00",
  "FBL30",
  "FBL40",
  "FBL96",
  "FBM00",
  "FBM96",
  "FBN00",
  "FBN10",
  "FBN96",
  "FBW96",
  "FNK10",
  "FQA00",
  "FQA10",
  "FQA20",
  "FQA30",
  "FQA40",
  "FQA96",
  "FQB00",
  "FQB10",
  "FQB20",
  "FQB30",
  "FQB96",
  "FQW96",
  "TFC00",
  "TFC10",
  "TFC20",
  "TFC25",
  "TFC99"
)

heart_ops <- hilmo_kids_tmp %>% filter(TOIMP %in% heart_oper_codes)

heart_ops %>%
  select(TOIMPALKUPVM) %>% is.na %>% sum

heartop_hilmo_ids <- heart_ops$HILMO_ID %>% unique()
heartop_hilmo_ids %>% length

heartop_hilmo_entries <- hilmo_orig_kids %>% 
  filter(HILMO_ID %in% heartop_hilmo_ids)

heartop_hilmo_entries <- left_join(heartop_hilmo_entries, all_bds, by = "TNRO")

heartop_hilmo_entries <- heartop_hilmo_entries %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days"),
         event_dur = difftime(LPVM, TUPVA, units = "days"))



write.csv(heartop_hilmo_entries, "/home/pvartiai/RSV/data/wrangle/heart_operations.feather")

heartops <- heartop_hilmo_entries %>% select(TNRO, age_at_outcome) %>%
  mutate(oper_1mo = ifelse(age_at_outcome < 32, 1, 0),
         oper_1year = ifelse(age_at_outcome < 366, 1, 0),
         oper_later = ifelse(age_at_outcome > 365, 1, 0))












# severe CHD icd10 diagnoses ####
critical_chd <- c(
  "Q2000",
  "Q2001",
  "Q201",
  "Q202",
  "Q203",
  "Q2040",
  "Q2041",
  "Q2048",
  "Q2130",
  "Q2131",
  "Q2132",
  "Q2133",
  "Q220",
  "Q2241",
  "Q226",
  "Q2321",
  "Q234",
  "Q2452",
  "Q2520",
  "Q2521",
  "Q2540",
  "Q255",
  "Q2573",
  "Q2578",
  "Q262"
)

conditionally_critical_chd_oper_1mo <- c(
  "Q205",
  "Q208",
  "Q209",
  "Q221",
  "Q222",
  "Q223",
  "Q2240",
  "Q228",
  "Q229",
  "Q230",
  "Q2310",
  "Q2311",
  "Q2320",
  "Q233",
  "Q238",
  "Q239",
  "Q243",
  "Q244",
  "Q248",
  "Q249",
  "Q251",
  "Q253",
  "Q256",
  "Q258",
  "Q259",
  "Q263"
)

conditionally_severe_chd_oper_12mo <- c(
  "Q2100",
  "Q2108",
  "Q2109",
  "Q2121",
  "Q2128",
  "Q2129",
  "Q214",
  "Q2180",
  "Q2188",
  "Q219",
  "Q225",
  "Q250"
)


# NOT RUN
# FOR THE SEVERE ONES, WE ARE USING CHD CLASSIFICATION BASED ON ONLY OPERATIONS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # congenital heart defects ######

# heartops

# all_bds

# mbr_dgs

# hilmo_kids_dgs

# # surely critical 

# hilmo_critical_chds <- hilmo_q %>%
#   filter(dg %in% critical_chd) %>%
#   select(TNRO) %>%
#   distinct() %>%
#   mutate(critical_chd = 1)

# mbr_critical_chds <- mbr_dgs %>%
#   filter(dg %in% critical_chd) %>%
#   select(TNRO) %>%
#   mutate(critical_chd = 1) %>%
#   distinct()

# malf_critical_chds <- malf %>%
#   filter(dg %in% critical_chd) %>%
#   select(TNRO) %>%
#   mutate(critical_chd = 1) %>%
#   distinct()

# critical_chds <- bind_rows(hilmo_critical_chds, 
#                             mbr_critical_chds, 
#                             malf_critical_chds) %>% 
#   distinct

# rm(hilmo_critical_chds, mbr_critical_chds)

# # conditionally critical, oper 1mo

# hilmo_condit_crit_chd <- hilmo_orig_dg_kids %>%
#   filter(KOODI %in% conditionally_critical_chd_oper_1mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_crit_oper_1mo = 1)

# mbr_condit_crit_chd <- mbr_dgs %>%
#   filter(dg %in% conditionally_critical_chd_oper_1mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_crit_oper_1mo = 1)

# malf_condit_crit_chd <- malf %>%
#   filter(dg %in% conditionally_critical_chd_oper_1mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_crit_oper_1mo = 1)

# condit_crit_chd_1mo <- bind_rows(hilmo_condit_crit_chd, mbr_condit_crit_chd, malf_condit_crit_chd) %>% distinct



# # conditionally critical, oper 12mo

# hilmo_condit_sev_chd <- hilmo_orig_dg_kids %>%
#   filter(KOODI %in% conditionally_severe_chd_oper_12mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_sev_oper_12mo = 1)

# mbr_condit_sev_chd <- mbr_dgs %>%
#   filter(dg %in% conditionally_severe_chd_oper_12mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_sev_oper_12mo = 1) 


# malf_condit_sev_chd <- malf %>%
#   filter(dg %in% conditionally_severe_chd_oper_12mo) %>%
#   select(TNRO) %>% distinct() %>%
#   mutate(condit_sev_oper_12mo = 1)


# condit_sev_chd_12mo <- bind_rows(hilmo_condit_sev_chd, mbr_condit_sev_chd, malf_condit_sev_chd) %>% distinct




# # create data containing all malformation diagnoses
# # from hilmo, mbr, and malformations registry

# q2 <- bind_rows(
#   (mbr_dgs %>%
#      filter(substr(dg, 1, 2) == "Q2") %>%
#      select(TNRO, dg)),
  
#   (hilmo_orig_dg_kids %>%
#      rename(dg = KOODI) %>%
#      select(TNRO, dg) %>%
#      filter(substr(dg, 1, 2) == "Q2")),

#   (malf %>%
#     select(TNRO, dg) %>%
#     filter(substr(dg, 1, 2) == "Q2"))
#   )

# # create variables for the common chds
# common_chds <- q2 %>%
#   mutate(common_asd = ifelse(substr(dg, 1, 5) %in% c("Q2110",
#                                                      "Q2111"), 1, 0),
#          vsd = ifelse(substr(dg, 1, 4) == "Q210", 1, 0)) %>%
#          # pda = ifelse(substr(dg, 1, 4) == "Q250", 1, 0)) %>%
#   # mutate(other_chd = ifelse((common_asd == 0 & vsd == 0 & pda == 0), 1, 0)) %>%
#   mutate(other_chd = ifelse(!(substr(dg, 1, 4) %in% c("Q211", "Q210")), 1, 0))


# common_chds <- common_chds %>%
#   select(-dg) %>%
#   pivot_longer(cols = c(common_asd, vsd, other_chd)) %>%
#   filter(value == 1) %>%
#   distinct() %>%
#   pivot_wider(names_from = name, 
#               values_from = value,
#               values_fill = 0)


# common_chd_and_op <- full_join(common_chds, heartops) %>%
#   select(-age_at_outcome)

# common_chd_and_op[is.na(common_chd_and_op)] <- 0

# conditionally critical and severe heart defects, depending on operation

# # use these data
# heartops
# critical_chds
# condit_crit_chd_1mo 
# condit_sev_chd_12mo
# common_chds 

# # join conditional diagnoses to operations
# condit_chds <- full_join(condit_crit_chd_1mo, condit_sev_chd_12mo)

# condit_chd_ops <- full_join(condit_chds, heartops) %>%
#   select(-age_at_outcome)

# condit_chd_ops[is.na(condit_chd_ops)] <- 0

# condit_chd_ops <- condit_chd_ops %>%
#   mutate(condit_crit = ifelse((condit_crit_oper_1mo == 1 & oper_1mo == 1), 1, 0),
#          condit_sev = ifelse((condit_sev_oper_12mo == 1 & oper_1year == 1), 1, 0),
#          other_op_1year = ifelse((oper_1year == 1 & condit_sev == 0 & condit_crit == 0), 1, 0)
#          ) %>%
#   select(TNRO, oper_later, condit_crit, condit_sev, other_op_1year) %>%
#   distinct()



# # do a sensitivity analysis table for Emmi
# # 
# #
# focus_chds <- full_join(condit_crit_chd_1mo, condit_sev_chd_12mo)
# focus_chds <- full_join(focus_chds, critical_chds)
# focus_chd_ops <- full_join(focus_chds, heartops) %>%
#   select(-age_at_outcome)

# focus_chd_ops[is.na(focus_chd_ops)] <- 0

# focus_chd_ops <- focus_chd_ops %>% as_tibble

# focus_chd_ops %>%
#   select(oper_later, condit_sev_oper_12mo, oper_1year) %>% table



# # make long format of the conditional heart defects
# condit_chds <- condit_chd_ops %>%
#   pivot_longer(cols = -TNRO) %>% 
#   distinct() %>%
#   filter(value == 1) %>%
#   pivot_wider(id_cols = TNRO,
#               names_from = name, 
#               values_from = value,
#               values_fill = 0) 



# severe_chds <- full_join(critical_chds, condit_chds) 
# severe_chds[is.na(severe_chds)] <- 0

# severe_chds <- severe_chds %>%
#   mutate(critical_chd = ifelse(condit_crit == 1, 1, critical_chd)) %>%
#   select(-condit_crit) %>%
#   mutate(oper_later = ifelse(critical_chd == 1, 0, oper_later),
#          condit_sev = ifelse(critical_chd == 1, 0, condit_sev),
#          other_op_1year = ifelse(critical_chd == 1, 0, other_op_1year))





# all_chds <- full_join(severe_chds, common_chds)

# all_chds[is.na(all_chds)] <- 0

# all_chds <- all_chds %>%
#   mutate(vsd = ifelse(critical_chd == 1, 0, vsd),
#          common_asd = ifelse(critical_chd == 1, 0, common_asd))
#          # other_chd = ifelse(critical_chd == 1, 0, other_chd)
#          ) %>%

#   pivot_longer(cols = -TNRO) %>%

#   filter(value == 1) %>%
#   pivot_wider(names_from = name, 
#               values_from = value,
#               values_fill = 0) %>%
#   mutate(
#     only_vsd = ifelse((vsd == 1 & 
#                         critical_chd == 0 & 
#                         other_op_1year == 0 & 
#                         condit_sev == 0 & 
#                         other_chd == 0 & 
#                         common_asd == 0),
#                         1, 0),
#     only_common_asd = ifelse((common_asd == 1 & 
#                           critical_chd == 0 & 
#                           other_op_1year == 0 & 
#                           condit_sev == 0 & 
#                           other_chd == 0 & 
#                           vsd == 0), 
#                         1, 0)) %>%
#   select(-vsd, -common_asd, -other_chd) 

# all_chds 
  


# all_chds <- all_chds %>%
#   rename(chd_operation_after_1y = oper_later,
#          chd_severe = condit_sev,
#          chd_only_vsd = only_vsd,
#          chd_common_asd = only_common_asd,
#          chd_other_operation_by_1year = other_op_1year)


# # join with all id variables and replace NA's with 0s

# final_chds <- left_join(select(all_bds, TNRO), all_chds, by = "TNRO")

# final_chds[is.na(final_chds)] <- 0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OLD CHD SCRIPT ENDS HERE, 












new_heartops <- heartop_hilmo_entries %>% select(TNRO, age_at_outcome) %>%
  group_by(TNRO) %>%
  arrange(age_at_outcome, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(chd_oper_1mo = ifelse(age_at_outcome <= 31, 1, 0),
         chd_oper_1year = ifelse(age_at_outcome < 366 &
                            age_at_outcome > 31, 1, 0),
         chd_oper_later = ifelse(age_at_outcome > 365, 1, 0)) %>%
  select(-age_at_outcome)

## check ##
heartop_hilmo_entries %>% select(TNRO, age_at_outcome) %>%
  group_by(TNRO) %>%
  arrange(age_at_outcome, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(chd_oper_1mo = ifelse(age_at_outcome <= 31, 1, 0),
         chd_oper_1year = ifelse(age_at_outcome < 366 &
                            age_at_outcome > 31, 1, 0),
         chd_oper_later = ifelse(age_at_outcome > 365, 1, 0)) %>%
  group_by(chd_oper_later) %>%
  summarise(mean = mean(age_at_outcome))
## / check / ##

new_heartops %>%
  pull(TNRO) %>% duplicated %>% sum

final_chds <- left_join(select(all_bds, TNRO), new_heartops, by = "TNRO")
final_chds[is.na(final_chds)] <- 0

setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(final_chds, file = "congenital_heart_defects.csv",
  row.names = FALSE)







#           C O M M O N   C H D S    


# create data containing all malformation diagnoses
# from hilmo, mbr, and malformations registry



# filter diagnoses about cardiovascular malformations (starting with Q2)
q2 <- bind_rows(
  (mbr_dgs %>%
     filter(substr(dg, 1, 2) == "Q2") %>%
     select(TNRO, dg)),
  
  (hilmo_orig_dg_kids %>%
     rename(dg = KOODI) %>%
     select(TNRO, dg) %>%
     filter(substr(dg, 1, 2) == "Q2")),
  
  (malf %>%
     select(TNRO, dg) %>%
     filter(substr(dg, 1, 2) == "Q2"))
)


# create variables for the common chds
new_common_chds <- q2 %>%
  mutate(asd = ifelse(substr(dg, 1, 5) %in% c("Q2110", "Q2118", "Q2119"), 1, 0),
         vsd = ifelse(substr(dg, 1, 4) == "Q210", 1, 0)) %>%
  # pda = ifelse(substr(dg, 1, 4) == "Q250", 1, 0)) %>%
  # mutate(other_chd = ifelse((common_asd == 0 & vsd == 0 & pda == 0), 1, 0)) %>%
  mutate(other_chd = ifelse(!(substr(dg, 1, 4) %in% c("Q211", "Q210", "Q250")), 1, 0))



new_common_chds <- new_common_chds %>%
  select(-dg) %>%
  pivot_longer(cols = c(asd, vsd, other_chd)) %>%
  filter(value == 1) %>%
  distinct() %>%
  pivot_wider(names_from = name, 
              values_from = value,
              values_fill = 0)

# join and replace NAs with 0s
common_chd_and_op <- full_join(new_common_chds, new_heartops)
common_chd_and_op[is.na(common_chd_and_op)] <- 0

only_common_chds <- common_chd_and_op %>%
  
  mutate(only_vsd = ifelse(vsd == 1 &
                             asd == 0 &
                             other_chd == 0 &
                             chd_oper_1mo == 0 & 
                             chd_oper_1year == 0, 1, 0)) %>%
  
  mutate(only_common_asd = ifelse(asd == 1 & 
                                    other_chd == 0 &
                                    vsd == 0 & 
                                    chd_oper_1mo == 0 & 
                                    chd_oper_1year == 0, 1, 0)) %>%
  select(TNRO, only_vsd, only_common_asd)


# join with id variables to create a dataframe of correct length
# and replace NAs with 0
final_common_chds <- left_join(select(all_bds, TNRO), only_common_chds, by = "TNRO")
final_common_chds[is.na(final_common_chds)] <- 0

final_common_chds %>%
  summarise(n_vsd = sum(only_vsd),
            n_asd = sum(only_common_asd))

setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(final_common_chds, file = "common_benign_congenital_heart_defects.csv",
          row.names = FALSE)








# DOWN SYNDROMES

chromo_hilmo <- hilmo_q %>%
  filter(substr(dg, 1, 3) == "Q90")

hilmo_tupva <- hilmo_orig_kids %>%
  select(HILMO_ID, TNRO, TUPVA)

hilmo_tupva <- left_join(hilmo_tupva, all_bds, by = "TNRO")


hilmo_q_with_dates <- left_join(hilmo_orig_dg_kids, 
                                hilmo_tupva, 
                                by = c("HILMO_ID", "TNRO"))

# all downs diagnosed during 1st year of life
# same logic than in congenital heart defects
early_hilmo_chromo <- hilmo_q_with_dates %>%
  filter(substr(KOODI, 1, 3) == "Q90") %>%
  mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days")) %>%
  filter(age_at_outcome < 365) %>%
  select(TNRO) %>% distinct

# hilmo_q %>% 
#   filter(substr(dg, 1, 3) == "Q90") %>%
#   pull(TNRO) %>% unique %>% length


chromo_mbr <- mbr_dgs %>%
  filter(substr(dg, 1, 3) == "Q90") %>%
  select(TNRO) %>% distinct()

chromo_malf <- malf %>%
  filter(substr(dg, 1, 3) == "Q90") %>%
  select(TNRO) %>%
  distinct()


downs <- bind_rows(early_hilmo_chromo, chromo_mbr, chromo_malf) %>%
  distinct() %>%
  mutate(down = 1)

downs <- left_join(select(all_bds, TNRO), downs, by = "TNRO")
downs[is.na(downs)] <- 0


setwd("/data/projects/project_pvartiai/rsv/predictors")
write.csv(downs, "down_syndrome_1mo.csv",
    row.names = FALSE)



  
# ESOPHAGEAL MALFORMATIONS

hilmo_eso <- hilmo_q %>%
  filter(substr(dg, 1, 3) == "Q39")


early_hilmo_eso <- hilmo_q_with_dates %>%
    filter(substr(KOODI, 1, 3) == "Q39") %>%
    mutate(age_at_outcome = difftime(TUPVA, LAPSEN_SYNTYMAPVM, units = "days")) %>%
    filter(age_at_outcome < 365) %>%
    select(TNRO) %>% distinct


eso_mbr <- mbr_dgs %>%
  filter(substr(dg, 1, 3) == "Q39") %>%
  select(TNRO) %>% distinct()

eso_malf <- malf %>%
  filter(substr(dg, 1, 3) == "Q39") %>%
  select(TNRO) %>%
  distinct()


esophagus <- bind_rows(early_hilmo_eso, eso_mbr, eso_malf) %>%
  distinct() %>%
  mutate(q39_confirmed = 1)

esophagus <- left_join(select(all_bds, TNRO), esophagus, by = "TNRO")
esophagus[is.na(esophagus)] <- 0


setwd("/data/projects/project_pvartiai/rsv/predictors")
write.csv(esophagus, "esophagus_malformations.csv",
    row.names = FALSE)









# NOT RUN
# Additional script which explores the common CHD diagnosis definitions



# base <- mbr_hilmo %>%
#   select(TNRO, gest_days, outcome)

# heartops

# # filter diagnoses about cardiovascular malformations (starting with Q2)
# q2 <- bind_rows(
#   (mbr_dgs %>%
#      filter(substr(dg, 1, 2) == "Q2") %>%
#      select(TNRO, dg)),
  
#   (hilmo_orig_dg_kids %>%
#      rename(dg = KOODI) %>%
#      select(TNRO, dg) %>%
#      filter(substr(dg, 1, 2) == "Q2")),
  
#   (malf %>%
#      select(TNRO, dg) %>%
#      filter(substr(dg, 1, 2) == "Q2"))
# )

# mbr_dgs %>% select(TNRO) %>% distinct 

# ### for Emmi and Santtu


# q2


# chd_explore_long <- q2 %>%
#   mutate(chd_type = case_when(
#     dg == "Q2110" ~ "Q21.10_asd_secundum",
#     dg == "Q2111" ~ "Q21.11_pfo",
#     dg == "Q2112" ~ "Q21.12_sinus_venosus_asd",
#     dg == "Q2113" ~ "Q21.13_sinus_coronarius_defect",
#     dg == "Q2114" ~ "Q21.14_lutenbach",
#     dg == "Q2115" ~ "Q21.15_common_atrium",
#     dg == "Q2118" ~ "Q21.18_other_asd",
#     dg == "Q2119" ~ "Q21.19_unsp_asd",
#     substr(dg, 1, 4) == "Q210" ~ "any_vsd",
#     dg == "Q250" ~ "Q25.0_pda",
#     dg %in% critical_chd ~ "critical",
#     TRUE ~ "other"
#   )) 

# chd_explore <- chd_explore_long %>%
#   select(TNRO, chd_type) %>%
#   distinct() %>%
#   mutate(chd_value = 1) %>%
#   pivot_wider(names_from = chd_type,
#               values_from = chd_value, 
#               values_fill = 0) %>%
#   left_join(., heartops, by = "TNRO") %>%
#   select(-age_at_outcome) %>%
#   mutate(across(c(oper_1mo, 
#                   oper_1year, 
#                   oper_later), 
#                 ~ ifelse(is.na(.x), 0, .x)))

# # total n, overlaps not considered
# chd_explore %>%
#   select(-TNRO) %>%
#   colSums() %>%
#   as.data.frame() %>%
#   print(quote = TRUE)


# #Q21.12
# chd_explore %>%
#   filter(Q21.12_sinus_venosus_asd == 1 &
#            other == 0 &
#            critical == 0 & 
#            any_vsd == 0 &
#            oper_1mo == 0 & oper_1year == 0)

# # only isolated asds (overlaps between not considered)
# m <- chd_explore %>%
#   filter(other == 0 &
#            critical == 0 & 
#            any_vsd == 0 &
#            oper_1mo == 0 & 
#            oper_1year == 0 &
#            Q21.13_sinus_coronarius_defect == 0 & 
#            Q21.14_lutenbach == 0 &
#            Q21.15_common_atrium == 0) %>%
#   select(TNRO, 
#          Q21.10_asd_secundum,
#          Q21.12_sinus_venosus_asd, 
#          Q21.18_other_asd, 
#          Q21.19_unsp_asd) %>%
#   filter(if_any(everything(), ~ .x == 1))



# m %>%
#   group_by(Q21.10_asd_secundum,
#           Q21.12_sinus_venosus_asd, 
#           Q21.18_other_asd, 
#           Q21.19_unsp_asd) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   pivot_longer(cols = c(Q21.10_asd_secundum,
#                         Q21.12_sinus_venosus_asd, 
#                         Q21.18_other_asd, 
#                         Q21.19_unsp_asd)) 


















# ####

# explore_common_chds <- q2 %>%
#   mutate(asd_secundum = ifelse(substr(dg, 1, 5) %in% c("Q2110"), 1, 0),
#          vsd_other_unsp = ifelse(substr(dg, 1, 5) %in% c("Q2108", "Q2109"), 1, 0),
#          vsd_multiple = ifelse(substr(dg, 1, 5) %in% c("Q2100"), 1, 0),
#          pfo = ifelse(substr(dg, 1, 5) %in% c("Q2111"), 1, 0)) %>%
#   # pda = ifelse(substr(dg, 1, 4) == "Q250", 1, 0)) %>%
#   # mutate(other_chd = ifelse((common_asd == 0 & vsd == 0 & pda == 0), 1, 0)) %>%
#   mutate(other_chd = ifelse(!(substr(dg, 1, 4) %in% c("Q211", "Q210")), 1, 0))

# explore_common_chds_2 <- explore_common_chds %>%
#   select(-dg) %>%
#   pivot_longer(cols = c(asd_secundum, vsd_other_unsp, vsd_multiple, pfo)) %>%
#   filter(value == 1) %>%
#   distinct() %>%
#   pivot_wider(names_from = name, 
#               values_from = value,
#               values_fill = 0)

# explore_common_chds_2



# explore_chd_and_op <- full_join(explore_common_chds_2, new_heartops)
# explore_chd_and_op[is.na(explore_chd_and_op)] <- 0

# explore_common_chds_3 <- explore_chd_and_op %>%
  
#   mutate(only_asd_secundum = ifelse(asd_secundum == 1 &
#                             vsd_other_unsp == 0 &
#                               pfo == 0 &
#                               vsd_multiple == 0 &
#                              other_chd == 0 &
#                              chd_oper_1mo == 0 & 
#                              chd_oper_1year == 0, 1, 0)) %>%
  
#   mutate(only_pfo = ifelse(asd_secundum == 0 &
#                                       vsd_other_unsp == 0 &
#                                       pfo == 1 &
#                                       vsd_multiple == 0 &
#                                       other_chd == 0 &
#                                       chd_oper_1mo == 0 & 
#                                       chd_oper_1year == 0, 1, 0)) %>%
  
#   mutate(only_vsd_other = ifelse(asd_secundum == 0 &
#                                    vsd_other_unsp == 1 &
#                                    pfo == 0 &
#                                    vsd_multiple == 0 &
#                                    other_chd == 0 &
#                                    chd_oper_1mo == 0 & 
#                                    chd_oper_1year == 0, 1, 0)) %>%
  
#   mutate(only_vsd_multiple = ifelse(asd_secundum == 0 &
#                                       vsd_other_unsp == 0 &
#                                       pfo == 0 &
#                                       vsd_multiple == 1 &
#                                       other_chd == 0 &
#                                       chd_oper_1mo == 0 & 
#                                       chd_oper_1year == 0, 1, 0)) %>%
  
#   select(-c(other_chd, chd_oper_later, chd_oper_1year, chd_oper_1mo,
#             vsd_multiple, vsd_other_unsp, pfo, asd_secundum))


# explore_common_chds_3

# # join with id variables to create a dataframe of correct length
# # and replace NAs with 0
# test_common_chds <- left_join(base, explore_common_chds_3, by = "TNRO") %>%
#     as_tibble
# test_common_chds[is.na(test_common_chds)] <- 0


# test_common_chds


# chd_base_model <- glm(data = test_common_chds,
#                       family = binomial(),
#                       formula = outcome ~ only_asd_secundum + 
#                         only_pfo + 
#                         only_vsd_other + 
#                         only_vsd_multiple)

# chd_ga_model <- update(chd_base_model, ~ . + rcs(gest_days, 4))

# summary(chd_base_model)
# summary(chd_ga_model)

