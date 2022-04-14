# NEW preprocess 1 script

# extract the study population and key variables from the birth registry (mbr)

# From mbr, we 
# a) extract the most important variables,
# b) exclude dead
# c) filter patients according to the study period.


# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)

# file names and paths ######
mbr_dir <- "/data/processed_data/thl_birth/"
mbr_file <- "birth_2022-03-08.csv"

rsv_dir <- "/home/pvartiai/RSV/data/"
setwd(rsv_dir)

hilmo_dir <- "/data/processed_data/thl_hilmo/"

# hilmo exists in processed and original. From now on, we can use only processed hilmo.
hilmo_name <- "thl2019_1776_hilmo.csv.finreg_IDsp"
hilmo_dg_name <- "thl2019_1776_hilmo_diagnoosit_kaikki.csv.finreg_IDsp"

hilmo_orig_dir <- "/data/original_data/new/"
hilmo_orig_name <- "thl2019_1776_hilmo.csv.finreg_IDs"

# medical birth registry, loading, selecting and editing ######




#load the mbr. We keep mbr_oribinal for later use.
mbr_original <- fread(paste0(mbr_dir, mbr_file)) %>% 
    as_tibble


mbr_raw <- mbr_original

# rename id as "TNRO". 
# exclude irrelevant variables. They are either not relevant or inconsistently recorded.
mbr_raw <- mbr_raw %>%
    rename(TNRO = LAPSI_TNRO) %>%
    select(-c(AITI_HETU_OK, LAPSI_HETU_OK, POLILLA, FOOLIHAPPOLISA_KK, IVF, ICSI,
              PAS, BLASTOKYSTIVILJELY, ALKION_VITRIFIKAATIO, ALKIODIAGNOSTIIKKA,
              LUOVUTETTU_SUKUSOLU, INSEMINAATIO, KYPSYTYSHOITO, HEDHOITO_EI_TIETOA_TYYPISTA,
              KOEPUTKI, ALKIONSIIRTO, MUUKEINO, NISKATURVOTUS_PAKSUUS_MM, 
              KROMOSOMIEN_SEULONTA_NIPT, TRISOMIA_21_RISKILUKU, KORIONVI,
              LVESITUT, ALKURASKAUDEN_INFEKTIOSEULA, YMPARILEIKATTU, TROMBOOSIPROF,
              TUKIOMM, SKESTO_AVAUT, SKESTO_AVAUT_H, SKESTO_PONN, SKESTO_AVAUT_MIN, 
              SKESTO_PONN_H, SKESTO_PONN_MIN, EI_LIEVITYSTA, KAYNNISTYS, EDISTAMINEN,
              PUHKAISU, OKSITOSIINI, PROSTAGLANDIINI, ISTUKANIRROITUS, KAAVINTA,
              OMPELU, YMPARILEIKKAUKSEN_AVAUS, KOHDUNPOISTO, EMBOLISAATIO, AITISIIRRETTY,
              VUODON_MAARA, LAPSEN_LAHTOKLO, LAPSEN_KUOLINKLO, AITI_TULOPVM, AITI_LAHTOPVM,
              VERENVUO, MUUSYY, TARKENNETTU_ULTRA, STREPTOKOKKI_B_SEULA, 
              SYNNYTYS_PALTU, EI_LIEVITYS_TIETOA, VERENSIIRTO, ETINEN,
              ISTIRTO, RKOURIS, HARTIADYSTOKIA,ASFYKSIA, PERATILA,
              MUUTARPO, SYNTYMAKLO, KAKSOSUUDEN_TYYPPI, APGAR_5MIN,
              LAPSEN_LAHTOPVM, LAPSEN_RAVINTO_7VRK,
              LISAMAITO, NAPAVALTIMOPH, NAPALASKIMOPH,
              KESKENMENOJA, KESKEYTYKSIA, ULKOPUOLISIA, KUOLLEENASYNT, TARKASTUKSET,
              FOOLIHAPPOLISA, ANTIBIOOTTI_ENINT_2VRK, ELVYTYS, ELVYTYS_ALKU_JALKEEN,
              VERENVAIHTO, VALOHOITO, BCG_ROKOTUS, HEPATIITTI_B_ROKOTUS, MUUSAIR,
              KORTIKOSTEROIDI, INSULIINI_ALOITETTU, MUU_RASKAUS_DIABETES_HOITO,
              SOKERI_TEHTY, AMMATTILUOKITUS, SOSEKO, GBS_PROFYLAKSIA,
              HENGITYS_AVUSTUS_ALKU, SYNTYMAPAIKKA, NESTETAYTTO_ALKU,
              PAINELUELVYTYS_ALKU, HYPOGLYKEMIAN_IV_GLUK_HOITO,
              KANSALAISUUS, VIILENNYSHOITO, HAPPI_KYLLASTEISYYS_SEULA,
              NEUVOLAPVM,
              HYPOTYREOOSI, AINEENVAIHDUNTA, KVITAMIINI,
              TILASTOVUOSI, AITI_IKA, AVOLIITTO, 
              AIEMMATRASKAUDET,
              VERENPAI, ENNENAIK, ASEKTIO,
              LISAHAPPI_ALKU, ADRENALIINI_ALKU,
              AIDIN_ANTIBIOOTTIHOITO, DIABETES, SYNTYMATILATUNNUS,
              APAINO, SIVIILISAATY
    ))



# select those born after 1.1.1998 according to dates
mbr_raw <- mbr_raw %>%
    filter(LAPSEN_SYNTYMAPVM >= as.Date("1998-01-01")) %>%
    filter(LAPSEN_SYNTYMAPVM <= as.Date("2019-12-31"))



# Identify & filter out the deceased from medical birth registry #####

#  n of dead = 1839, all deceased are in the medical birth registry 
# (comparison with COD done, see end of this script)
# exclusion thus only based on medical birth registry.
# setdiff(exclude_dead_cod$TNRO, mbr_dead$TNRO)

# checked if death dates match, they do (except 1, inaccuray of 1 day)
# left_join(mbr_dead, exclude_dead_cod, by = "TNRO") %>%
#   filter(KPV.x != KPV.y)

mbr_dead <- mbr_raw %>%
    filter(!is.na(LAPSEN_KUOLINPVM)) %>%
    mutate(lifespan = as.numeric(difftime(LAPSEN_KUOLINPVM, LAPSEN_SYNTYMAPVM, units = "days"))) %>%
    filter(lifespan < 8) %>%
    select(TNRO, lifespan, LAPSEN_KUOLINPVM) %>%
    rename(age_at_death = lifespan,
           KPV = LAPSEN_KUOLINPVM)


# exclude those with death date recorded at 0-7 days of age, 
# or those that have KUOLLEISUUS == 1 indicating stillbirth

dead_0_7_days <- mbr_dead$TNRO

mbr_raw_notdead <- mbr_raw %>%
    filter(!(TNRO %in% dead_0_7_days)) %>%
    # filter out stillbirths
    filter(KUOLLEISUUS != 1)


# Create important variables (gestational age, smoking) to birth registry ######

# mbr_c <- mbr_raw_notdead %>%

#     # preliminary smoking
#     mutate(smoking = as.numeric(TUPAKOINTITUNNUS)) %>%
#     mutate(smoking = case_when(
#         # any evidence of smoking will be coded as 1, others as 0 (even if missing)
#         smoking == 1 ~ 0,
#         smoking %in% c(2:4) ~ 1,
#         smoking == 9 ~ 0
#     )) %>%
#     mutate(c_section_birth = case_when(
#         SYNNYTYSTAPATUNNUS %in% c(5:7) ~ 1,
#         TRUE ~ 0
#         )) %>%
#     select(-c(LAPSEN_KUOLINPVM, SYNNYTYSTAPATUNNUS))


# NEW SCRIPT WHEN THE DATA IS UPDATED
mbr_c <- mbr_raw_notdead %>%
    mutate(gest_weeks_full = as.numeric(str_sub(KESTOVKPV, 1, 2))) %>%
    mutate(gest_weekdays = str_sub(KESTOVKPV, -1, -1) %>% as.numeric) %>%
    mutate(gest_weeks_as_days = gest_weeks_full*7) %>%
    mutate(gest_days = (gest_weeks_as_days + gest_weekdays)) %>%
    mutate(gest_weeks = gest_days / 7) %>%
    select(-KESTOVKPV, 
        -gest_weeks_full, 
        -gest_weekdays, 
        -gest_weeks_full, 
        -gest_weeks_as_days) %>%
    # birth month
    mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) %>%

    # preliminary smoking
    mutate(smoking = as.numeric(TUPAKOINTITUNNUS)) %>%
    mutate(smoking = case_when(
        smoking == 1 ~ 0,
        smoking %in% c(2:4) ~ 1,
        smoking == 9 ~ NA_real_
    )) %>%
    mutate(c_section_birth = case_when(
        SYNNYTYSTAPATUNNUS %in% c(5:7) ~ 1,
        TRUE ~ 0
        )) %>%
    select(-c(LAPSEN_KUOLINPVM, SYNNYTYSTAPATUNNUS)) 


# TWINS
pre_twin_data <- mbr_c %>% select(AITI_TNRO, TNRO, MONISIKI_SYNNYTYSTUNNUS) %>% 
    mutate(twin_alive = ifelse(!is.na(MONISIKI_SYNNYTYSTUNNUS), 1, 0)) %>%
    filter(twin_alive == 1) %>%
    mutate(mother_dupl = duplicated(AITI_TNRO) | duplicated(AITI_TNRO, fromLast = TRUE)) 
    
sus_twin_mothers <- pre_twin_data %>% arrange(AITI_TNRO) %>%
    filter(!mother_dupl) %>%
    pull(AITI_TNRO)

twins <- pre_twin_data %>% 
    mutate(twin = ifelse(!mother_dupl, 0, twin_alive)) %>%
    select(TNRO, twin)







# check: check the mortality of suspicious twin mothers. In our data, there are
# 336 mothers who supposedly have twins but have only 1 baby in our data. 
# in the whole dataset, there are 330 stillborn infants in those mothers. 
# Enough to exclude these mothers from twin definition.
# we also get roughly same number of twins by copypasting mother id and childs' DOB.

# mbr_original %>%
# filter(AITI_TNRO %in% sus_twin_mothers) %>%
# select(KUOLLEISUUS) %>% table


#join twin variable to the main dataset
mbr_c <- left_join(mbr_c, twins, by = "TNRO") %>% 
    mutate(twin = ifelse(is.na(twin), 0, twin))


## G E S T    D A Y S (delete soon)

# # new original birth registry
# thl_birth_path <- "/data/original_data/thl_birth/thl2021_2196_synret.csv.finreg_IDs"

# # load only gestational days variable
# dur <- fread(thl_birth_path, select = c("LAPSI_TNRO", "KESTOVKPV")) %>% as_tibble

# # missing gest days? 9000 empty gest_days variable in the original data.
# # dur %>% filter(KESTOVKPV == "")

# # wranlge character variable no numerical duration
# #  9000 missing
# kesto <- dur %>% 
#     rename(TNRO = LAPSI_TNRO) %>%
#     mutate(gest_weeks_full = as.numeric(str_sub(KESTOVKPV, 1, 2))) %>%
#     mutate(gest_weekdays = str_sub(KESTOVKPV, -1, -1) %>% as.numeric) %>%
#     mutate(gest_weeks_as_days = gest_weeks_full*7) %>%
#     mutate(gest_days = (gest_weeks_as_days + gest_weekdays)) %>%
#     mutate(gest_weeks = gest_days / 7) %>%
#     select(-KESTOVKPV,
#         -gest_weeks_full, 
#         -gest_weekdays, 
#         -gest_weeks_full, 
#         -gest_weeks_as_days)


# around 3000 gestational ages are missing from our data. 50% of those can be 
# computed from birth date and date of mother's last period. 
missgest_ids <- mbr_c %>% filter(is.na(gest_days)) %>% select(TNRO)

# get the dates (bd an last period)
bd_and_viimkk <- mbr_c %>%
    select(TNRO, LAPSEN_SYNTYMAPVM, VIIMEINEN_KUUKAUTISPVM) %>%
    filter(TNRO %in% missgest_ids$TNRO) %>%
    filter(!is.na(VIIMEINEN_KUUKAUTISPVM))

# compute age
kesto_missing <- bd_and_viimkk %>%
    mutate(gest_days = as.numeric(difftime(LAPSEN_SYNTYMAPVM, 
                                    VIIMEINEN_KUUKAUTISPVM, 
                                    units = "days"))) %>%

    # sanity mutations
    mutate(gest_days = ifelse(gest_days > 310, NA_real_, gest_days)) %>%
    mutate(gest_days = ifelse(gest_days < 250, NA_real_, gest_days)) %>%
    filter(!is.na(gest_days)) %>%
    mutate(gest_weeks = gest_days / 7) %>%
    select(-c(LAPSEN_SYNTYMAPVM, VIIMEINEN_KUUKAUTISPVM))

kesto_missing
    
# join to the original data

mbr_c$gest_days[mbr_c$TNRO %in% kesto_missing$TNRO] <- kesto_missing$gest_days
mbr_c$gest_weeks[mbr_c$TNRO %in% kesto_missing$TNRO] <- kesto_missing$gest_weeks

# check the n of missing gest days after computing approx. 1500 gest ages.
# around 2000 missing. This is fine.
mbr_c %>% filter(is.na(gest_days))



setwd("/data/projects/project_pvartiai/rsv/wrangle/")
write.csv(mbr_c, "preprocess_1_mbr.csv",
	row.names = FALSE)


