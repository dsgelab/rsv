# This script recodes 
#   - neonatal diagnoses from medical birth registry, 
#   - the P diagnoses from HILMO.
#   - mothers' O diagnoses from HILMO

# The recoding is done by Pekka Vartiainen's manual classification.
# The classification is based on both the rarity and the pathogenic process 
# of the disease.



# in consideration:
# 
# - Update diagnosis count path to the synced directory?

# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)

# load and edit the data #####


# read the birth registry data joined with HILMO outcomes
mbr_hilmo <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv") %>% as_tibble
mbr_c <- mbr_hilmo

## read hilmo data: neonatal dgs and mothers' dgs
# hilmo_original_kids <- read_feather("/home/pvartiai/RSV/data/wrangle/hilmo_original_kids.feather")
# hilmo_orig_dg_kids <- read_feather("/home/pvartiai/RSV/data/wrangle/hilmo_orig_dg_kids.feather")
first_hilmo_dgs <- fread("/data/projects/project_pvartiai/rsv/wrangle/index_babies_first_hilmo_diagnoses.csv") %>% as_tibble
mothers_hilmo_dgs <- fread("/data/projects/project_pvartiai/rsv/wrangle/mothers_raw_o_diagnoses_for_recoding_feb_2022.csv") %>% as_tibble


# create a long data with dgs
mbr_c_long <- mbr_c %>%
    select(TNRO, RDIAG1:RDIAG10, SDIAG1:SDIAG10, ICD10_1:ICD10_10) %>%
    pivot_longer(cols = -TNRO) %>%
    filter(value != "")





###             ###
###             ###
#   P diagnoses   #
#   from birth    # 
#   registry      #
###             ###
###             ###




# select relevant neonatal diagnoses
mbr_c_long_p <- mbr_c_long %>%
    filter(substr(value, 1, 1) == "P" | substr(value, 1, 4) %in% c("Z383", "Z384", "Z385"))

# super long script to recode all p diagnoses
mbr_c_long_p <- mbr_c_long_p %>%
    mutate(p_diag = case_when(
        # commented are excluded classes that appeared to be too rare.

        #P00 mother's diseases
        value == "P000" ~ "P000",
        value == "P002" ~ "P002",
        
        #P01 premature rupture of membranes. 
        # value == "P010" ~ "P010",
        value == "P0110" ~ "P0110",
        value == "P0111" ~ "P0111",
        value == "P015" ~ "P015",
        # too rare 
        # substr(value, 1, 4) == "P012" ~ "P012",
        
        #P02 placenta, umbilical cord
        # value == "P021" ~ "P021",
        # value %in% c("P022", "P023") ~ "P022-P023",
        # value %in% c("P024", "P025", "P026") ~ "P024-P026",
        value == "P027" ~ "P027",
        
        #P03 method of delivery, effect on baby. EXCLUDE, sections are not reliably recorded
        
        #P04 harmful substances
        value == "P042" ~ "P042",
        # value == "P043" ~ "P043",
        value == "P044" ~ "P044",
        value %in% c("P040", "P041", "P043", "P045", "P046", "P048", "P049") ~ "P04_other",
        
        # P05 poor growth (SGA, small related to gest.age)
        # excluded because we get gest.size as SDs

        # value == "P050" ~ "P050",
        # value == "P051" ~ "P051",
        # value %in% c("P052", "P059") ~ "P052-9",
        
        
        # P07 prematurity
        # EXCLUDE WEIGHT DGs, use number from birth regstr. instead
        value == "P072" ~ "P072",
        value == "P073" ~ "P073",
        
        # P08 LGA = baby is too big, group .1 and .2
        # (exclude P08.3 overdue but normal weight)
        # exclude, we ge SDs
        # value == "P080" ~ "P080",
        # value == "P081" ~ "P081",
        
        
        #P10 CNS birth injuries
        # too rare
        # substr(value, start = 1, stop = 3) == "P10" ~ "P10-P11",
        # substr(value, start = 1, stop = 3) == "P11" ~ "P10-P11",
        
        # other birth injuries
        ((substr(value, start = 1, stop = 3) %in% c("P12", "P13", "P14", "P15"))&
            !(value %in% c("P120", "P134", "P140"))) ~ "P12-P15_other",
        value == "P120" ~ "P120",
        value == "P134" ~ "P134",
        value == "P140" ~ "P140",
        
        # P20 fetal hypoxemia, now grouped
        substr(value, start = 1, stop = 3) == "P20" ~ "P20",
        
        # P21 asphyxias
        value == "P210" ~ "P210",
        value == "P211" ~ "P211",
        value == "P219" ~ "P219",
        
        # P22 breathing problems
        value == "P220" ~ "P220",
        value == "P221" ~ "P221",
        value %in% c("P228", "P229") ~ "P22_other_unsp",
        
        #P23 pneumonias
        # Possibly regroup this
        # viral
        substr(value, start = 1, stop = 3) == "P23" ~ "P23",
        
        
        # P24 aspirations
        # value == "P240" ~ "P240",
        # value %in% c("P241", "P242", "P243", "P248", "P249") ~ "P241-9",
        substr(value, 1, 3) == "P24" ~ "P24",
        
        #P25 pneumothorax
        substr(value, start = 1, stop = 3) == "P25" ~ "P25",
        
        #P26 lung bleeding, exclude bc small n
        
        #P27 long-term lung problems
        #P271 = BPD
        value == "P271" ~ "P271",
        
        
        # Apneas and other breathing problems
        value %in% c("P282", "P283", "P284", "P285") ~ "P282-5",
        value %in% c("P280", "P281", "P288", "P289") ~ "P28_other",
        
        # P29 heart problems			
        value == "P291" ~ "P291",
        value == "P2930" ~ "P2930",
        value == "P2931" ~ "P2931",
        value %in% c("P290", "P292", "P294", "P298", "P299") ~ "P29_other",
        
        # P35 viral infections
        # substr(value, start = 1, stop = 3) == "P35" ~ "P35",
        
        #P36 bacterial infections
        value == "P360" ~ "P360",
        value %in% c("P361", "P362", "P363", "P364", "P365", "P368") ~ "P36_specified",
        substr(value, start = 1, stop = 4) == "P369" ~ "P369",
        
        #P37 Other microbiological infections. Excl, small n
        
        #P38-P39 infections according to site (eye, UTI, skin...)
        substr(value, start = 1, stop = 3) == "P38" ~ "P38-P39",
        substr(value, start = 1, stop = 3) == "P39" ~ "P38-P39",
        
        #P50-P51 bleedings, also P54
        substr(value, start = 1, stop = 3) %in% c("P50", "P51", "P54") ~ "P50-51_and_P54",
        
        # P52 non-traumatic ICHs
        value %in% c("P520", "P521") ~ "P520-1",
        value %in% c("P522", "P523", "P524", "P525", "P526", "P528", "P529") ~ "P52_other",
        
        
        
        #P55 hemolytic conditions 
        substr(value, start = 1, stop = 3) %in% "P55" ~ "P55",
        substr(value, start = 1, stop = 3) %in% "P58" ~ "P58",
        
        
        #P58 Jaundice bc of extravascular conditions (like bruising)
        substr(value, start = 1, stop = 3) == "P58" ~ "P58",
        
        #P59 Jaundice
        value == "P590" ~ "P590", # prematurity jaundice
        value %in% c("P591", "P592") ~ "P591-2", # jaundice bc of liver or bile duct problems
        value %in% c("P593", "P598", "P599") ~ "P593-9",
        
        #P70 glucose metabolism
        # value %in% c("P700", "P701") ~ "P700-701", # diabetes of mother
        # value == "P704" ~ "P704", # other hypoglycemia
        # value %in% c("P702", "P703", "P708", "P709") ~ "P70_other",
        substr(value, start = 1, stop = 3) == "P70" ~ "P70",
        
        # hypocalcemia
        value == "P711" ~ "P711",
        
        # Exclude P77 NEC and other gi problems, small n
        
        # temerature regulation problems
        # value == "P77" ~ "P77",
        # substr(value, start = 1, stop = 3) %in% c("P80", "P81") ~ "P80-81",
        
        
        #P83 skin problems
        value == "P831" ~ "P831",
        value == "P835" ~ "P835",
        
        #P90 seizures
        value == "P90" ~ "P90",
        
        #P91 cerebral problems
        substr(value, start = 1, stop = 3) == "P91" ~ "P91",
        
        #P92 feeding problems
        value %in% c("P920", "P921") ~ "P920-1",
        value %in% c("P922", "P923") ~ "P922-3",
        
        #P94 muscle tone problems
        substr(value, start = 1, stop = 3) == "P94" ~ "P94",
        
        #P96 other conditions
        value == "P961" ~ "P961", #mothers addictive substances
        
        # risk 
        value %in% c("Z350", "Z351", "Z352", "Z353", 
            "Z357", "Z358", "Z359") ~ "Z35_risk",
        
        
        ##Hurray! end of value P codes!
        # 74 groups
        # Keep other values the same
        TRUE ~ NA_character_
        
    )
    )

mbr_c_long_p$p_diag %>% table
mbr_c_long_p$value %>% table



###             ###
###             ###
#   Q diagnoses   #
###             ###
###             ###



mbr_c_long_q <- mbr_c_long %>%
    filter(substr(value, 1, 1) == "Q") %>%
    # shorten q levels to 4 digits
    mutate(value = (substr(value, start = 1, stop = 4))) %>%
    # create a 3-digit q_level variable
    mutate(q_level = substr(value, start = 1, stop = 3))


## count q levels and create name vectors

q_counts <- mbr_c_long_q %>%
    group_by(value) %>%
    summarise(count = n()) %>%
    arrange(-count)

# get common Q dgs
q_over_500 <- q_counts %>% 
    filter(count > 499) %>%
    # this filters out the common CHDs
    filter(!(substr(value, 1, 3) %in% c("Q21", "Q25")))

# take their names
common_q_names <- (q_over_500$value %>% as.character)


# take common levels.
common_q_levels <- common_q_names %>% substr(., start = 1, stop = 3) %>% unique
#add common chds to vector
common_q_levels <- c(common_q_levels, "Q21", "Q25")

# vector of CHD names
common_chd_names <- c("Q250",
                      "Q251", 
                      "Q210", 
                      "Q211")

# Q levels that we want to skip
excluded_q_levels <- c(
    "Q08",
    "Q27",
    "Q28",
    "Q57",
    "Q59"
)

### Q recoding, step 1

mbr_c_long_q$q_diag <- NULL

mbr_c_long_q <- mbr_c_long_q %>%
    mutate(q_diag = case_when(
        
        # keep the whole q-dg in the most common ones
        value %in% common_q_names ~ value,
        
        # keep the interesting and common CHDs
        value %in% common_chd_names ~ value,
        
        # if a level is one of the common levels but diagnosis not common, use '_other' as level category
        q_level %in% common_q_levels ~ paste0(q_level, "_other"),
        
        q_level %in% excluded_q_levels ~ NA_character_,
        
        TRUE ~ q_level
        
    ))

#check
mbr_c_long_q$q_diag %>% table


## Q recoding, step 2

mbr_c_long_q$temp_q_diag <- NULL

mbr_c_long_q <- mbr_c_long_q %>%
    mutate(temp_q_diag = case_when(
        
        # Q00-07 malformations of the nervous system
        q_diag %in% c("Q00", "Q01", "Q02", "Q03", "Q04", "Q05", "Q06", "Q07") ~ "Q00-07",
        
        # Q10-Q18 face
        q_diag %in% c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18") ~ "Q10-18",
        
        # Q20-28 
        q_diag %in% c("Q26", "Q27") ~ "Q26_Q27_other",
        
        # Q30-34
        q_diag %in% c("Q30", "Q31", "Q32", "Q33", "Q34") ~ "Q30-34",
        
        #Q38_other
        q_diag == "Q38_other" ~ NA_character_,
        
        #Q40 exclude
        q_diag == "Q40" ~ NA_character_,
        
        #Q41-Q44 bowels, lower GI tract, biliary tract, others
        q_diag %in% c("Q41", "Q42", "Q43", "Q44", "Q45") ~ "Q41-44",
        
        # Q50-56 females' genital anomalies
        q_diag %in% c("Q50", "Q51", "Q52", "Q53_other", "Q54_other", "Q55_other", "Q56") ~ "Q50-56_other",
        
        # Q63-64 other kidney-urinary tract
        q_diag %in% c("Q60", "Q61", "Q62_other", "Q63", "Q64") ~ "Q60-64_other",
        
        # Q65
        q_diag %in% c("Q650", "Q651", "Q653", "Q654", "Q656", "Q65_other") ~ "Q65",
        
        #Q66
        q_diag %in% c("Q660", "Q662", "Q66_other") ~ "Q66",
        
        #Q67-68
        q_diag %in% c("Q67", "Q68") ~ "Q67-68",
        
        #Q69-70 digitorum
        q_diag %in% c("Q69_other", "Q690", "Q70") ~ "Q69-70",
        
        # Q71-74 limb anomalies and defects
        q_diag %in% c("Q71", "Q72", "Q73", "Q74") ~ "Q71-74",
        
        # Q75-79
        q_diag %in% c("Q75", "Q76", "Q77", "Q78", "Q79") ~ "Q75-79",
        
        # Q80, Q81, Q83, Q84 skin
        q_diag %in% c("Q80", "Q81", "Q82_other", "Q83", "Q84") ~ "Q80-84_other",
        
        #Q85-89
        q_diag %in% c("Q85", "Q86", "Q87", "Q88", "Q89") ~ "Q85-89",
        
        # Q92-99 other and not specified chromosomal abnormalities
        q_diag %in% c("Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99") ~ "Q91-99",
        
        
        TRUE ~ q_diag
        
    ))

#check
mbr_c_long_q$temp_q_diag %>% table

mbr_c_long_q$q_diag <- mbr_c_long_q$temp_q_diag




###             ###
###             ###
#   O diagnoses   #
###             ###
###             ###




mbr_c_long_o <- mbr_c_long %>%
    filter(substr(value, 1, 1) == "O" | substr(value, 1, 3) %in% c("E03", "E66", "Z35")) %>%
    # create a 3-digit q_level variable
    mutate(o_level = substr(value, start = 1, stop = 3))

o_counts <- mbr_c_long_o %>%
    group_by(value) %>%
    summarise(count = n()) %>%
    arrange(-count)

# get common O dgs
o_over_2000 <- o_counts %>% 
    filter(count > 2000) 


# take their names
common_o_names <- (o_over_2000$value %>% as.character)


# take common levels.
common_o_levels <- common_o_names %>% substr(., start = 1, stop = 3) %>% unique

# create vectors to be excluded

exclude_o_2_digits <- c("O0")

excluded_o_3_digits <- c(
    # empty and faulty levels
    "O00", "OOO",
    "O15", "O16", "O17", "O18", "O19",
    "O27", "O29",
    "O37", "O38", "O39",
    "O49", "O50", "O51", "O52", "O53", "O54","O56", "O57", "O58", "O59",
    "O76", "O77", "O78", "O79", "O85", "O87", "O88", "O89",
    "O90", "O91", "O92", "O93", "O94", "O95", "O96", "O97", "O98",
    "OO8", "OZ3", "OZ6",
    
    # actual dgs with many n's, but not relevant or low n
    "O25", #malnutrition during pregnany, low n
    "O28", #abnormal findings in checupks, very heterogeneus
    "O30", # multiple pregnancies, extracted elsewhere
    "O32", # dgs for mothers treatments bc of "poikkeava tarjonta"
    "O31", #multipara-specific complications, n = 500, not relevant 
    "O33", #mothers treatment due to fetopelv disproportio, not relevant
    "O46", # bleeding before labour, not specific enough
    "O61", # Failed induction of labour 
    "O62", # too weak contractions, not relevant for child
    "O64", # related to presentation
    "O66", # mother's pelvis problems complicating labour, not relevant
    "O74", # anesthesiacomplications
    "O80", # code for labour, indicates normality 
    "O83", # Other assisted labour, heterogeneous, not relevant
    "O84" # multiple births, we get twin variables elsewhere
)



interesting_o_dgs <- c(
    # O20-29
    #O23 to O23 
    "O235",
    "O240", "O241", "O244", "O249",
    "O260",
    #O30-49
    "O343",
    "O354", "O355", "O358", 
    "O360", "O361", "O365", "O366", # isoimmunization and abnormal growth. 
    "O410", "O411",
    #O60-79
    #O80-99
    "O820", "O8210", "O8211",
    "O990", "O993", "O995",
    "Z357" # social problems in pregnancy
)

# interesting, or 'rescue' levels
interesting_o_levels <- substr(interesting_o_dgs, 1, 3) %>% unique()

keepall_o_names <- unique(c(common_o_names, interesting_o_dgs))
# 1. create a vector of dgs that we want to exclude
remove_o_names <- keepall_o_names[substr(keepall_o_names, 1, 3) %in% excluded_o_3_digits]
# 2. exclude the diagnoses
keepall_o_names <- setdiff(keepall_o_names, remove_o_names)

keepall_o_levels <- unique(c(common_o_levels, interesting_o_levels))
# remove excluded levels from the "keepall" levels
keepall_o_levels <- setdiff(keepall_o_levels, excluded_o_3_digits)



## O DIAGNOSIS RECODING 

# STEP 1

mbr_c_long_o$o_diag <- NULL

mbr_c_long_o <- mbr_c_long_o %>%
    mutate(o_diag = case_when(
        

        # keep the whole o-dg in the most common ones
        value %in% keepall_o_names ~ value,
        
        # if a level is one of the common levels but diagnosis not common, 
        # recode as missing. Frequency check revealed that all were relatively rare (n < 4000)
        # and information value is low. Interesting in epidemiological analyses
        substr(value, 1, 3) %in% keepall_o_levels ~ NA_character_,
        
        # excluded variables
        substr(value, 1, 3) %in% excluded_o_3_digits ~ NA_character_,
        
        substr(value, 1, 2) == "O0"~ NA_character_,
        
        TRUE ~ o_level
    ))


mbr_c_long_o$o_diag %>% table



# STEP 2

mbr_c_long_o$temp_o_diag <- NULL

mbr_c_long_o <- mbr_c_long_o %>%
    mutate(temp_o_diag = case_when(
        # these group common diagnoses back to levels
        # hypertension
        substr(o_diag, 1, 3) %in% c("O10", "O11", "O12", "O13") ~ "O10-13",

        #undefined conditions
        o_diag %in% c("O2688", "O269") ~ NA_character_,

        # exclude mother's treatment because of other or unspecified reasons.
        # This is not relevant in the kids' model, we get more information from 
        # the kids' diagnoses. 

        substr(o_diag, 1, 4) %in% c("O360", "O361") ~ "O36_immunization",
        
        substr(o_diag, 1, 3) == "O42" ~ "O42",
        
        substr(o_diag, 1, 3) == "O44" ~ "O44",
        
        substr(o_diag, 1, 3) == "O81" ~ "O81",
        
        o_diag == "O821" ~ "O8210",
        
        substr(o_diag, 1, 3) == "O84" ~ "O84",

        o_diag %in% c("O991", "O992", "O994", "O996", "O997", "O9989", "O999") ~ NA_character_,
        
        substr(o_diag, 1, 1) == "Z" & substr(o_diag, 1, 4) != "Z357" ~ NA_character_,
        
        # these combine wrongly coded diagnoses to correct level (there exists only one level)
        o_diag == "O40_other" ~ "O40",
        o_diag == "O48_other" ~ "O48",
        o_diag == "O60_other" ~ "O60",
        
        # the rest
        TRUE ~ o_diag   
    ))

mbr_c_long_o$temp_o_diag %>% table
mbr_c_long_o$o_diag <- mbr_c_long_o$temp_o_diag


###             ###
###             ###
#   P diagnoses   #
#   from HILMO    #
###             ###
###             ###

# longitudinal dataset created in the previous script
first_hilmo_dgs

#
# super long script to recode all p diagnoses
first_hilmo_dgs <- first_hilmo_dgs %>%
    mutate(p_diag = case_when(

        # commented are excluded classes that appeared to be too rare.

        #P00 mother's diseases
        value == "P000" ~ "P000",
        value == "P002" ~ "P002",
        
        #P01 premature rupture of membranes. 
        # value == "P010" ~ "P010",
        value == "P0110" ~ "P0110",
        value == "P0111" ~ "P0111",
        value == "P015" ~ "P015",
        # substr(value, 1, 4) == "P012" ~ "P012",
        
        # P02 placenta, umbilical cord
        # value == "P021" ~ "P021",
        # value %in% c("P022", "P023") ~ "P022-P023",
        # value %in% c("P024", "P025", "P026") ~ "P024-P026",
        value == "P027" ~ "P027",
        
        # P03 method of delivery, effect on baby. 
        # EXCLUDE, sections are not reliably recorded
        
        
        
        #P04 harmful substances
        value == "P042" ~ "P042",
        # value == "P043" ~ "P043",
        value == "P044" ~ "P044",
        value %in% c("P040", "P041", "P043", "P045", "P046", "P048", "P049") ~ "P04_other",
        
        # P05 poor growth (SGA, small related to gest.age)
        # excluded because we get gest.size as SDs

        # value == "P050" ~ "P050",
        # value == "P051" ~ "P051",
        # value %in% c("P052", "P059") ~ "P052-9",
        
        
        # P07 prematurity
        # EXCLUDE WEIGHT DGs, use number from birth regstr. instead
        value == "P072" ~ "P072",
        value == "P073" ~ "P073",
        
        # P08 LGA = baby is too big, group .1 and .2
        # (exclude P08.3 overdue but normal weight)
        # exclude, we ge SDs
        # value == "P080" ~ "P080",
        # value == "P081" ~ "P081",
        
        
        #P10 CNS birth injuries
        # too rare
        # substr(value, start = 1, stop = 3) == "P10" ~ "P10-P11",
        # substr(value, start = 1, stop = 3) == "P11" ~ "P10-P11",
        
        # other birth injuries
        ((substr(value, start = 1, stop = 3) %in% c("P12", "P13", "P14", "P15"))&
            !(value %in% c("P120", "P134", "P140"))) ~ "P12-P15_other",
        value == "P120" ~ "P120",
        value == "P134" ~ "P134",
        value == "P140" ~ "P140",
        
        # P20 fetal hypoxemia, now grouped
        substr(value, start = 1, stop = 3) == "P20" ~ "P20",
        
        # P21 asphyxias
        value == "P210" ~ "P210",
        value == "P211" ~ "P211",
        value == "P219" ~ "P219",
        
        # P22 breathing problems
        value == "P220" ~ "P220",
        value == "P221" ~ "P221",
        value %in% c("P228", "P229") ~ "P22_other_unsp",
        
        #P23 pneumonias
        # Possibly regroup this
        # viral
        substr(value, start = 1, stop = 3) == "P23" ~ "P23",
        
        
        # P24 aspirations
        # value == "P240" ~ "P240",
        # value %in% c("P241", "P242", "P243", "P248", "P249") ~ "P241-9",
        substr(value, 1, 3) == "P24" ~ "P24",
        
        #P25 pneumothorax
        substr(value, start = 1, stop = 3) == "P25" ~ "P25",
        
        #P26 lung bleeding, exclude bc small n
        
        #P27 long-term lung problems
        #P271 = BPD
        value == "P271" ~ "P271",
        
        
        # Apneas and other breathing problems
        value %in% c("P282", "P283", "P284", "P285") ~ "P282-5",
        value %in% c("P280", "P281", "P288", "P289") ~ "P28_other",
        
        # P29 heart problems            
        value == "P291" ~ "P291",
        value == "P2930" ~ "P2930",
        value == "P2931" ~ "P2931",
        value %in% c("P290", "P292", "P294", "P298", "P299") ~ "P29_other",
        
        # P35 viral infections
        # substr(value, start = 1, stop = 3) == "P35" ~ "P35",
        
        #P36 bacterial infections
        value == "P360" ~ "P360",
        value %in% c("P361", "P362", "P363", "P364", "P365", "P368") ~ "P36_specified",
        substr(value, start = 1, stop = 4) == "P369" ~ "P369",
        
        #P37 Other microbiological infections. Excl, small n
        
        #P38-P39 infections according to site (eye, UTI, skin...)
        substr(value, start = 1, stop = 3) == "P38" ~ "P38-P39",
        substr(value, start = 1, stop = 3) == "P39" ~ "P38-P39",
        
        #P50-P51 bleedings, also P54
        substr(value, start = 1, stop = 3) %in% c("P50", "P51", "P54") ~ "P50-51_and_P54",
        
        # P52 non-traumatic ICHs
        value %in% c("P520", "P521") ~ "P520-1",
        value %in% c("P522", "P523", "P524", "P525", "P526", "P528", "P529") ~ "P52_other",
        
        
        
        #P55 hemolytic conditions 
        substr(value, start = 1, stop = 3) %in% "P55" ~ "P55",
        substr(value, start = 1, stop = 3) %in% "P58" ~ "P58",
        
        
        #P58 Jaundice bc of extravascular conditions (like bruising)
        substr(value, start = 1, stop = 3) == "P58" ~ "P58",
        
        #P59 Jaundice
        value == "P590" ~ "P590", # prematurity jaundice
        value %in% c("P591", "P592") ~ "P591-2", # jaundice bc of liver or bile duct problems
        value %in% c("P593", "P598", "P599") ~ "P593-9",
        
        #P70 glucose metabolism
        # value %in% c("P700", "P701") ~ "P700-701", # diabetes of mother
        # value == "P704" ~ "P704", # other hypoglycemia
        # value %in% c("P702", "P703", "P708", "P709") ~ "P70_other",
        substr(value, start = 1, stop = 3) == "P70" ~ "P70",
        
        # hypocalcemia
        value == "P711" ~ "P711",
        
        # Exclude P77 NEC and other gi problems, small n
        
        # temerature regulation problems
        # value == "P77" ~ "P77",
        # substr(value, start = 1, stop = 3) %in% c("P80", "P81") ~ "P80-81",
        
        
        #P83 skin problems
        value == "P831" ~ "P831",
        value == "P835" ~ "P835",
        
        #P90 seizures
        value == "P90" ~ "P90",
        
        #P91 cerebral problems
        substr(value, start = 1, stop = 3) == "P91" ~ "P91",
        
        #P92 feeding problems
        value %in% c("P920", "P921") ~ "P920-1",
        value %in% c("P922", "P923") ~ "P922-3",
        
        #P94 muscle tone problems
        substr(value, start = 1, stop = 3) == "P94" ~ "P94",
        
        #P96 other conditions
        value == "P961" ~ "P961", #mothers addictive substances
        
        # risk pregnancy
        value %in% c("Z350", "Z351", "Z352", "Z353", 
            "Z357", "Z358", "Z359") ~ "Z35_risk",
        
        # the rest
        TRUE ~ NA_character_
    ))


###             ###
###             ###
#   Q diagnoses   #
#   from HILMO    #
#   (babies)      #
###             ###
###             ###

first_hilmo_dgs_q <- first_hilmo_dgs %>%
    filter(substr(value, 1, 1) == "Q") %>%
    # shorten q levels to 4 digits
    mutate(value = (substr(value, start = 1, stop = 4))) %>%
    # create a 3-digit q_level variable
    mutate(q_level = substr(value, start = 1, stop = 3))


## use predefined vectors of common/interesting q diagnosis and levels
# defined in MBR recoding script
common_q_names
common_q_levels
common_chd_names
excluded_q_levels

### Q recoding, step 1

first_hilmo_dgs_q$q_diag <- NULL

first_hilmo_dgs_q <- first_hilmo_dgs_q %>%
    mutate(q_diag = case_when(
        
        # keep the whole q-dg in the most common ones
        value %in% common_q_names ~ value,
        
        # keep the interesting and common CHDs
        value %in% common_chd_names ~ value,
        
        # if a level is one of the common levels but diagnosis not common, use '_other' as level category
        q_level %in% common_q_levels ~ paste0(q_level, "_other"),
        
        q_level %in% excluded_q_levels ~ NA_character_,
        
        TRUE ~ q_level
        
    ))

#check
first_hilmo_dgs_q$q_diag %>% table


## Q recoding, step 2

first_hilmo_dgs_q$temp_q_diag <- NULL

first_hilmo_dgs_q <- first_hilmo_dgs_q %>%
    mutate(temp_q_diag = case_when(
        
        # Q00-07 malformations of the nervous system
        q_diag %in% c("Q00", "Q01", "Q02", "Q03", "Q04", "Q05", "Q06", "Q07") ~ "Q00-07",
        
        # Q10-Q18 face
        q_diag %in% c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18") ~ "Q10-18",
        
        # Q20-28 
        q_diag %in% c("Q26", "Q27") ~ "Q26_Q27_other",
        
        # Q30-34
        q_diag %in% c("Q30", "Q31", "Q32", "Q33", "Q34") ~ "Q30-34",
        
        #Q38_other
        q_diag == "Q38_other" ~ NA_character_,
        
        #Q40 exclude
        q_diag == "Q40" ~ NA_character_,
        
        #Q41-Q44 bowels, lower GI tract, biliary tract, others
        q_diag %in% c("Q41", "Q42", "Q43", "Q44", "Q45") ~ "Q41-44",
        
        # Q50-56 females' genital anomalies
        q_diag %in% c("Q50", "Q51", "Q52", "Q53_other", "Q54_other", "Q55_other", "Q56") ~ "Q50-56_other",
        
        # Q63-64 other kidney-urinary tract
        q_diag %in% c("Q60", "Q61", "Q62_other", "Q63", "Q64") ~ "Q60-64_other",
        
        # Q65
        q_diag %in% c("Q650", "Q651", "Q653", "Q654", "Q656", "Q65_other") ~ "Q65",
        
        #Q66
        q_diag %in% c("Q660", "Q662", "Q66_other") ~ "Q66",
        
        #Q67-68
        q_diag %in% c("Q67", "Q68") ~ "Q67-68",
        
        #Q69-70 digitorum
        q_diag %in% c("Q69_other", "Q690", "Q70") ~ "Q69-70",
        
        # Q71-74 limb anomalies and defects
        q_diag %in% c("Q71", "Q72", "Q73", "Q74") ~ "Q71-74",
        
        # Q75-79
        q_diag %in% c("Q75", "Q76", "Q77", "Q78", "Q79") ~ "Q75-79",
        
        # Q80, Q81, Q83, Q84 skin
        q_diag %in% c("Q80", "Q81", "Q82_other", "Q83", "Q84") ~ "Q80-84_other",
        
        #Q85-89
        q_diag %in% c("Q85", "Q86", "Q87", "Q88", "Q89") ~ "Q85-89",
        
        # Q92-99 other and not specified chromosomal abnormalities
        q_diag %in% c("Q91", "Q92", "Q93", "Q94", "Q95", "Q96", "Q97", "Q98", "Q99") ~ "Q91-99",
        
        
        TRUE ~ q_diag
        
    ))

#check
first_hilmo_dgs_q$temp_q_diag %>% table

first_hilmo_dgs_q$q_diag <- first_hilmo_dgs_q$temp_q_diag


first_hilmo_dgs_q <- first_hilmo_dgs_q %>%
    select(HILMO_ID, TNRO, value, p_diag, q_diag)

# bind p and q diagnoses
pq_dgs_from_hilmo <- left_join(first_hilmo_dgs, 
                               first_hilmo_dgs_q, 
                               by = c("HILMO_ID", "TNRO", "value", "p_diag")) %>%
    mutate(diag = ifelse(!is.na(q_diag), q_diag, p_diag)) %>%
    select(TNRO, diag) %>%
    filter(!is.na(diag))

# this contains the combined P- and Q-diagnoses from HILMO. 
pq_dgs_from_hilmo





###             ###
###             ###
#   O diagnoses   #
#   from HILMO    #
#   (mothers)     #
###             ###
###             ###





# Mothers' hilmo entries from pregnancy time created in the hilmo preprocess script, 
# loaded in the beginning.
mothers_hilmo_dgs

# create a target variable
mothers_hilmo_dgs$o_diag <- NULL

# the levels and values are defined earlier.
mothers_hilmo_dgs <- mothers_hilmo_dgs %>%
    mutate(o_diag = case_when(

        # keep the whole o-dg in the most common ones
        value %in% keepall_o_names ~ value,
        
        # if a level is one of the common levels but diagnosis not common, 
        # recode as missing. Frequency check revealed that all were relatively rare (n < 4000)
        # and information value is low. Interesting in epidemiological analyses
        substr(value, 1, 3) %in% keepall_o_levels ~ NA_character_,
        
        # excluded variables
        substr(value, 1, 3) %in% excluded_o_3_digits ~ NA_character_,
        
        # "O0 -codes" do not contain anything interesting
        substr(value, 1, 2) == "O0"~ NA_character_,
        
        TRUE ~ o_level        
    ))

mothers_hilmo_dgs$o_diag %>% table


# STEP 2

mothers_hilmo_dgs$temp_o_diag <- NULL

mothers_hilmo_dgs <- mothers_hilmo_dgs %>%
    mutate(temp_o_diag = case_when(
        # these group common diagnoses back to levels
        # hypertension
        substr(o_diag, 1, 3) %in% c("O10", "O11", "O12", "O13") ~ "O10-13",

        #undefined conditions
        o_diag %in% c("O2688", "O269") ~ NA_character_,

        # exclude mother's treatment because of other or unspecified reasons.
        # This is not relevant in the kids' model, we get more information from 
        # the kids' diagnoses. 

        substr(o_diag, 1, 4) %in% c("O360", "O361") ~ "O36_immunization",
        
        substr(o_diag, 1, 3) == "O42" ~ "O42",
        
        substr(o_diag, 1, 3) == "O44" ~ "O44",
        
        substr(o_diag, 1, 3) == "O81" ~ "O81",
        
        o_diag == "O821" ~ "O8210",
        
        substr(o_diag, 1, 3) == "O84" ~ "O84",

        o_diag %in% c("O991", "O992", "O994", "O996", "O997", "O9989", "O999") ~ NA_character_,
        
        substr(o_diag, 1, 1) == "Z" & substr(o_diag, 1, 4) != "Z357" ~ NA_character_,
        
        # these combine wrongly coded diagnoses to correct level (there exists only one level)
        o_diag == "O40_other" ~ "O40",
        o_diag == "O48_other" ~ "O48",
        o_diag == "O60_other" ~ "O60",
        
        # the rest
        TRUE ~ o_diag   
    ))


o_dgs_from_hilmo <- mothers_hilmo_dgs %>%
    select(TNRO, temp_o_diag) %>%
    rename(value = temp_o_diag) %>%
    filter(!is.na(value))


### join MBR and HILMO neonatal diagnoses


# join MBR and HILMO neonatal diagnoses #####

## Harmonize and join long data sets

harmonized_o <- mbr_c_long_o %>%
    select(-c(temp_o_diag, value, name, o_level)) %>%
    rename(value = o_diag) %>%
    filter(!is.na(value))

harmonized_p <- mbr_c_long_p %>%
    select(-c(name, value)) %>%
    rename(value = p_diag) %>%
    filter(!is.na(value))

harmonized_q <- mbr_c_long_q %>%
    select(-c(temp_q_diag, name, value, q_level)) %>%
    rename(value = q_diag) %>%
    filter(!is.na(value))

harmonized_hilmo_dgs <- pq_dgs_from_hilmo %>%
    rename(value = diag)

harmonized_hilmo_mother_dgs <- o_dgs_from_hilmo




# join and remove duplicate diagnoses
all_dg_long <- bind_rows(harmonized_o,
                         harmonized_p,
                         harmonized_q,
                         harmonized_hilmo_dgs,
                         harmonized_hilmo_mother_dgs) %>% distinct() %>%
                rename(dg = value) %>%
                mutate(value = 1) %>%
                mutate(dg = gsub("-", "_", dg)) %>%
                # filter out not wanted diagnoses
                filter(!(dg %in% c("P700_701", "P072", "P073",
                                  "O470")))





# count the diagnosis rates and save them to dir, for future use
dg_counts <- all_dg_long %>%
    filter(!is.na(dg)) %>%
    select(dg) %>% group_by(dg) %>% summarise(n = n()) %>%
    ungroup()

# Update path to the synced directory?
# write.csv(dg_counts, "/home/pvartiai/RSV/results/neonatal_dg_count.csv")




# pivot wider to the semilong format
all_dg_wide <- all_dg_long %>%
    pivot_wider(
        id_cols = TNRO,
        names_from = dg, 
        values_from = value, 
        values_fill = 0)


# select IDs and birth registry variables we want to use for composite definition
all_ids_and_composites <- mbr_c %>% select(TNRO, smoking, gest_weeks)

# left_join with index patients, so that we have a row for each index patient. 
# it will generate NAs which are replaced by zero. (
# NAs if there are no positive values in diagnoses)

neonatal_diagnoses <- left_join(all_ids_and_composites, all_dg_wide, by = "TNRO")
neonatal_diagnoses[is.na(neonatal_diagnoses)] <- 0

neonatal_diagnoses <- neonatal_diagnoses %>%
    # smoking
    rename(smoking_pre = smoking) %>%
    mutate(smoking = ifelse((smoking_pre == 1 | P042 == 1), 1, 0)) %>%
    select(-smoking_pre) %>%
    
    # premie and RDS
    mutate(premie_rds = case_when(
        (P220 == 1 & gest_weeks < 37) ~ 1,
        TRUE ~ 0
        )) %>%
    mutate(term_rds = case_when(
        (P220 == 1 & gest_weeks >= 37) ~ 1,
        TRUE ~ 0
        )) %>%
    select(-P220, -gest_weeks)


setwd("/data/projects/project_pvartiai/rsv/predictors/")
write.csv(neonatal_diagnoses, "neonatal_diagnoses_feb_22.csv",
    row.names = FALSE)














