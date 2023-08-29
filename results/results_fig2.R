

# results_n_screening

# load the libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(rms)
library(biglm)
library(ggrepel)
library(gridExtra)
library(scales)

# just in case
select <- dplyr::select

# paths

result_path <- "/data/projects/project_pvartiai/rsv/results_refined/"
train_dir <- "/data/projects/project_pvartiai/rsv/predictors/"

# load datasets
base_predictors <- fread(paste0(train_dir, "development_set.csv")) %>% as_tibble 

fa_ep <- fread(paste0(result_path, "father_ep_featureselect_results.csv")) %>%
  as_tibble() %>%
  mutate(parent = "father") %>% 
  filter(!is.na(or_uni))

mo_ep <- fread(paste0(result_path, "mother_ep_featureselect_results.csv")) %>%
  as_tibble() %>%
  mutate(parent = "mother") %>% 
  filter(!is.na(or_uni))

sib_ep <- fread(paste0(result_path, "sib_ep_featureselect_results.csv")) %>%
  as_tibble() %>%
  select(-any_of("V1")) %>%
  mutate(parent = "sibling") %>% 
  filter(!is.na(or_uni))


# drugs

mother_drug <- fread(paste0(result_path, "meds_mother_beforepreg_featureselect_results.csv")) %>%
  as_tibble() %>%
  select(-any_of("V1")) %>%
  mutate(parent = "mother") %>% 
  filter(!is.na(or_uni))


father_drug <- fread(paste0(result_path, "meds_father_beforepreg_featureselect_results.csv")) %>%
  as_tibble() %>%
  select(-any_of("V1")) %>%
  mutate(parent = "father") %>% 
  filter(!is.na(or_uni))


sibling_drug <- fread(paste0(result_path, "meds_sib_featureselect_results.csv")) %>%
  as_tibble() %>%
  select(-any_of("V1")) %>%
  mutate(parent = "sibling") %>% 
  filter(!is.na(or_uni))


pregnancy_drug <- fread(paste0(result_path, "meds_mother_preg_featureselect_results.csv")) %>%
  as_tibble() %>%
  select(-any_of("V1")) %>%
  mutate(parent = "mother") %>% 
  filter(!is.na(or_uni))




# endpoint names
ep_filename <- "finngen_endpoints_2021-09-02.csv"
ep_path <- "/data/processed_data/endpoint_metadata/"
ep_keys <- fread(paste0(ep_path, ep_filename),
                 select = c("LEVEL", "OMIT", "NAME", "LONGNAME")) %>%
  as_tibble

# create a reference sheet for endpoint groups. 

ep_groupkeys <- ep_keys %>%
  # level 1 endpoints are groups
  filter(LEVEL == "1") %>%
  # rename for nicer names
  rename(feature = NAME, 
         longname = LONGNAME) %>%
  # mutate the endpoint group name to be only the 1st 3 chars, to correspond with the featureselect data
  mutate(group = substr(feature, 1, 3)) %>%
  select(group, longname) %>%
  # rename the most long names 
  mutate(longname = case_when(
    group == "H8_" ~ "Diseases of the ear",
    group == "H7_" ~ "Diseases of the eye",
    group == "L12" ~ "Diseases of the skin",
    group == "M13" ~ "Diseases of the musculoskeletal system",
    group == "O15" ~ "Pregnancy-related conditions in previous pregnancies",
    group == "D3_" ~ "Diseases of the blood",
    group == "Q17" ~ "Congenital malformations",
    group == "AB1" ~ "Infectious and parasitic diseases",
    TRUE ~ longname)) %>%
  mutate(longname = str_wrap(longname, width = 20))




#### parent disease groups
pre_drugs <- bind_rows(mother_drug, father_drug, sibling_drug) %>%
  mutate(group = "Drug purchases",
         longname = "All prescription\ndrug purchases") %>%
  select(-any_of("V1")) %>%
  # categorise p values
  mutate(p_cat = ifelse(p_uni < 0.05/1200, "p<6e-5", "n.s.") %>%
           as.factor()) %>%
  mutate(parent = factor(parent, levels = c("father", "mother", "sibling")))


# combine mother and father and sibling data
par_ep_pre <- bind_rows(mo_ep, fa_ep, sib_ep) %>%
  mutate(group = substr(feature, 1, 3)) %>%
  select(-any_of("V1")) %>%
  # combine groups "KRA" and "F5" into mental health
  mutate(group = case_when(
    group == "KRA" ~ "F5_",
    group == "AST" ~ "J10",
    group == "sib" ~ "AB1",
    TRUE ~ group
  )) %>%
  # categorise p values
  mutate(p_cat = ifelse(p_uni < 0.05/1200, "p<6e-5", "n.s.") %>%
           as.factor()) %>%
  
  # join the keys
  left_join(., ep_groupkeys, by = "group") %>%
  mutate(parent = factor(parent, levels = c("father", "mother", "sibling")))


par_ep <- bind_rows(par_ep_pre, pre_drugs) %>%
  mutate(longname = longname %>%
           as.factor %>%
           fct_infreq() %>%
           fct_rev())


par_ep %>%
  group_by(group) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 100)

# get the names of those groups with n > 5, to keep only major disease groups
# exclude also some random groups
group_names <- par_ep %>%
  group_by(group) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  # filter out unwanted groups that are too unspecific
  filter(!(group %in% c("rcs", "RX_", "RHE", "chd", "FG_", "CD2", "P16", 
                        "O15", "D3_", "Q17", "H7_", "G6_"))) %>%
  pull(group)


ep_groupkeys %>%
  filter(group %in% group_names)


# par_ep %>%
#   arrange(desc(or_uni)) %>% View()

# position
pos <- position_jitterdodge(dodge.width = 0.35,
                            jitter.width = 0.09,
                            seed = 2)



# plot
a_data <- par_ep %>%
  # filter based on group n
  filter(group %in% group_names) %>%
  # add labels
  mutate(label = case_when(
    feature == "montelucast" & parent == "sibling" ~ "Montelucast", 
    feature == "valproate" & parent == "mother" ~ "Valproate",
    feature == "E4_CONGEIOD" & parent == "mother" ~ "Congenital iodine deficiency or hypothyreosis",
    feature == "O15_BREAST_LACT_OTHER_DIS" & parent == "mother" ~ "Disorders of breast and lactation associated with childbirth",
    feature == "H8_SUP_ACUTE" & parent == "sibling" ~ "Acute otitis media",
    feature == "sib_resp_hosp" ~ "Hospitalisation for viral bronchitis",
    feature == "F5_OTHERSUB" & parent == "mother" ~ "Substance abuse diagnosis",
    feature == "F5_OPIOIDS" & parent == "father" ~ "Opioid use disorder",
    grepl("J10_ASTHMACOPDKELA", feature)&parent == "sibling" ~ "Asthma diagnosis",
    TRUE ~ ""
  )) %>%
  arrange(p_cat)



a <- a_data %>%
  ggplot(.,
         aes(x = longname,
             y = or_uni,
             col = parent,
             group = parent,
             # alpha = p_cat,
             fill = p_cat:parent,
             label = str_wrap(label, 15))) + 
  # point
  geom_point(position = pos, 
             size = 2.5,
             shape = 21,
             stroke = 0.3) + 

  # labels
  geom_text_repel(color = "black",
                  box.padding = 0.6, 
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  seed = 41,
                  min.segment.length = 0,
                  position = pos,
                  size = 3.5) +
  coord_flip() + 
  # scales
  #  scale_alpha_manual(values = c(0.1, 1),
  #                   guide = "none") + 
  scale_y_continuous(breaks = pretty_breaks(),
                     limits = c(0.6, 3.1)) + 
  
  # color
  scale_color_manual(values = c("lightblue3", "lightpink2", "olivedrab3"),
                     drop = FALSE) + 
  
  # fill scale
  scale_fill_manual(values = c("white", "white", "white", 
                               "royalblue3", "lightcoral", "olivedrab")) + 
  
  # theme
  labs(col = "A) Family members' variables",
       y = "Odds ratio for RSV-hospitalisation\n(adjusted for known risks)") + 
  theme_bw(base_size = 13) + 
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.justification="left",
    legend.margin=margin(-1,-1,-1,-1),
    # legend.spacing.y = unit(0.1, 'cm'),
    legend.box.margin=margin(-5,-10,-10,-5)) + 
  # reference lines
  geom_hline(yintercept = 1, linetype = 2) +
  guides(colour = guide_legend(title.position="top",
                               override.aes = list(fill = c("royalblue3",
                                                            "lightcoral",
                                                            "olivedrab"))),
         title.hjust = 0.5,
         fill = FALSE)

a




# neonatal & Pregnancy disease groups

neodg_pre <- fread(paste0(result_path, "neodg_featureselect_results.csv")) %>%
  as_tibble() %>%
  # remove row name variable
  select(-any_of("V1")) %>% 
  # filter missing values
  filter(!is.na(or_uni)) %>%
  # select only relevant variables
  select(-any_of(c("or_multi", "p_multi", "coef_multi", "se_multi",
                   "p_aic", "coef_aic", "se_aic"))) %>%
  # categorise p values according to very conservative bonferroni correction
  mutate(p_cat = ifelse(p_uni < 1e-3, "sig", "n.s.") %>%
           as.factor()) %>%
  # filter out rcs variables 
  filter(!(substr(feature, 1, 2) %in% c("E0", "E6", "sm", "Z3", "rc"))) %>%
  mutate(group = substr(feature, 1, 1)) %>%
  mutate(group = case_when(
    group == "c" ~ "Q",
    group %in% c("t", "p") ~ "P",
    substr(feature, 1, 4) == "only" ~ "Q",
    TRUE ~ group
  )) %>%
  mutate(group = factor(group,
                        levels = c("O", "P", "Q"),
                        labels = c("Mother's pregnancy\ndiagnoses", "Neonate's\ndiagnoses", "Neonate's\nmalformations"))) %>%
  filter(!is.na(group))


preg_drug <- pregnancy_drug %>%
  mutate(group = "Mother's prescription\ndrug purchase\nduring pregnancy",
         longname = "Mother's pescription\ndrug purchases") %>%
  # filter missing values
  filter(!is.na(or_uni)) %>%
  # de-select row number variable
  select(-any_of("V1")) %>%
  # categorise p values
  mutate(p_cat = ifelse(p_uni < 1e-6, "sig", "n.s.") %>%
           as.factor())

neodg <- bind_rows(neodg_pre, preg_drug) %>%
  filter(!(substr(feature, 1, 3) %in% c("(In", "rcs"))) %>%
  mutate(label = case_when(
    feature == "Q39" ~ "Esophagus malformations",
    feature == "P044" ~ "Mother's substance abuse",
    feature == "valproate" ~ "Valproate",
    feature == "O355" ~ "Fetal injury from drugs",
    feature == "P961" ~ "Withdrawal symptoms",
    TRUE ~ ""
  ))




# highest ors
neodg %>%
  arrange(desc(or_uni))

neodg %>%
  pull(group) 


# no missing p values!
neodg %>%
  filter(is.na(p_cat))
neodg %>% filter(is.na(group))

# position
pos_b <- position_jitter(width = 0.05)

b <- neodg %>%
  ggplot(.,
         aes(x = group,
             y = or_uni,
           #  alpha = p_cat,
             fill = p_cat,
             label = str_wrap(label, 15))) + 
  # non-significant point
  geom_point(data = filter(neodg, p_cat == "n.s."),
             position = pos_b,
             size = 2.5, 
             shape = 1,
             stroke = 0.2,
             col = "grey28") + 
  # significant point
  geom_point(data = filter(neodg, p_cat == "sig"),
             position = pos_b, 
             size = 2.5,
             shape = 19,
             stroke = 0.2) + 
  
  # label
  geom_text_repel(color = "black",
                  box.padding = 0.6, 
                  max.overlaps = Inf,
                  segment.size = 0.2,
                  seed = 41,
                  min.segment.length = 0,
                  position = pos_b,
                  size = 3.5) +
  #flip coordinates
  coord_flip() + 
  #scales
  scale_alpha_manual(values = c(0.1, 1),
                     guide = "none") + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(y = "Odds ratio for RSV hospitalisation\n(adjusted for known risks)",
       subtitle = "B) Pregnancy and neonatal\n     variables") + 
  # reference line
  geom_hline(yintercept = 1,
             linetype = 2,
             col = "black") + 
  theme_bw(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12)
  ) + 
  guides(fill = FALSE)


b










# # # # # # # # # #   father    v s    mother 
# update with Sakari's or gender test

sakaris.gender.test <- function(data) {
  data1 <- data %>%
    mutate(t0_diff = log(or_uni_mother) - log(or_uni_father),
           sd_t_diff = sqrt(se_uni_mother^2 + se_uni_father^2),
           t0_diff_z = t0_diff/sd_t_diff,
           pval_sex = pnorm(abs(t0_diff_z), lower.tail=FALSE)*2)
  data1
}



most_significant_eps <- par_ep %>%
  filter(group %in% group_names) %>%
  select(feature, group, or_uni, p_uni, se_uni, n, parent, p_cat) %>%
  pivot_wider(names_from = parent,
              values_from = c(or_uni, p_uni, se_uni, n, p_cat)) %>%
  sakaris.gender.test() %>%
  # filter for bonferroni -corrected p value in sex difference
  filter(pval_sex < 0.01) %>%
  select(feature, pval_sex)






compare_these_eps <- par_ep %>%
  filter(feature %in% most_significant_eps$feature) %>%
  group_by(feature) %>%
  summarise(n = n()) %>%
  filter(n>1) %>%
  pull(feature)





par_ep_wide <- par_ep %>%
  filter(feature %in% compare_these_eps) %>%
  select(feature, or_uni, p_uni, se_uni, n, parent, group, p_cat) %>%
  pivot_wider(names_from = parent,
              values_from = c(or_uni, p_uni, se_uni, n, p_cat)) %>%
  mutate(group = case_when(
    group %in% c("AB1", "E4_", "H7_", "H8_", "L12", "N14", "ANT", "APP", "RX_") ~ "other",
    group %in% c("PUL") ~ "J10",
    TRUE ~ group
  )) %>%
  left_join(., ep_groupkeys, by = "group") %>%
  left_join(., most_significant_eps) %>%
  mutate(longname = ifelse(is.na(longname), "other", longname)) %>%
  # create labels 
  mutate(label = case_when(
    feature == "F5_OTHERSUB" ~ "Substance use disorder, other substances",
    feature == "J10_ASTHMACOPDKELA" ~ "Asthma diagnosis",
    # feature == "J10_LOWCHRON" ~ "Chronic lower respiratory tract diseases",
    feature == "KRA_PSY_ANXIETY_EXMORE" ~ "Anxiety disorders",
    feature == "M13_FIBROBLASTIC" ~ "Fibroblastic disorders",
    feature == "valproate" ~ "Valproate purchase",
    feature == "F5_PSYTRANS" ~ "Acute psychotic disorders",
    # feature == "M13_SOFTOVERUSE" ~ "Overuse-related soft tissue disorders",
    TRUE ~ ""
  )) %>%
  mutate(pcat_sexdif = as.factor(ifelse(pval_sex<(0.05/nrow(par_ep)/2), "sig", "n.s.")))


# par_ep_wide %>%
#   arrange(desc(or_uni_mother)) %>% View()

compare_these_eps

par_ep_wide %>% 
  filter(feature == "M13_FIBROBLASTIC") %>%
  select(label)

c <- par_ep_wide %>%
  ggplot(., aes( x = or_uni_mother,
                 y = or_uni_father,
                 label = str_wrap(label, 15))) + 
  geom_point(size = 2.5) + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_y_continuous(limits = c(0.5, 2.7)) + 
  scale_x_continuous(limits = c(0.5, 2.7)) + 
  theme_bw(base_size = 13) + 
  theme(
    legend.spacing.y = unit(0.5, 'cm'),
    legend.position = "bottom") + 
  # legend
  guides(color = guide_legend(byrow = TRUE)) + 
  labs(x = "Odds ratio (adjusted)\nfor mother's health endpoint",
       y = "Odds ratio (adjusted)\nfor father's health endpoint") + 
  geom_text_repel(color = "black",
                  box.padding = 0.8, 
                  max.overlaps = Inf,
                  min.segment.length = 0,
                  nudge_x = 0.2,
                  segment.size = 0.2,
                  size = 3.5) + 
  labs(subtitle = "C) Mother's and father's variables compared")

c





a
b 
c 


my_layout <- rbind(c(1, 2), c(1, 3))
my_layout

my_plotlist <- list(a, b, c)

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=985&height=1195

ggsave(g, 
       file = "figure_2.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 12,
       width = 9,
       # units = "cm",
       dpi = 1200
)


########################################################################
########################################################################

# EXTRA PART of saving plots for journal style

########################################################################
########################################################################


a_non_annotated <- a_data %>%
  ggplot(.,
         aes(x = longname,
             y = or_uni,
             col = parent,
             group = parent,
             # alpha = p_cat,
             fill = p_cat:parent,
             label = str_wrap(label, 15))) + 
  # point
  geom_point(position = pos, 
             size = 2.5,
             shape = 21,
             stroke = 0.3) + 


  coord_flip() + 
  # scales
  #  scale_alpha_manual(values = c(0.1, 1),
  #                   guide = "none") + 
  scale_y_continuous(breaks = pretty_breaks(),
                     limits = c(0.6, 3.1)) + 
  
  # color
  scale_color_manual(values = c("lightblue3", "lightpink2", "olivedrab3"),
                     drop = FALSE) + 
  
  # fill scale
  scale_fill_manual(values = c("white", "white", "white", 
                               "royalblue3", "lightcoral", "olivedrab")) + 
  
  # theme
  labs(col = "A) Family members' variables",
       y = "Odds ratio for RSV-hospitalisation\n(adjusted for known risks)") + 
  theme_bw(base_size = 13) + 
  theme(
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.justification="left",
    legend.margin=margin(-1,-1,-1,-1),
    # legend.spacing.y = unit(0.1, 'cm'),
    legend.box.margin=margin(-5,-10,-10,-5)) + 
  # reference lines
  geom_hline(yintercept = 1, linetype = 2) +
  guides(colour = guide_legend(title.position="top",
                               override.aes = list(fill = c("royalblue3",
                                                            "lightcoral",
                                                            "olivedrab"))),
         title.hjust = 0.5,
         fill = FALSE)


ggsave(a_non_annotated, 
       file = "figure_2_panel_a.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 12,
       width = 8,
       # units = "cm",
       dpi = 1200
)


b_non_annotated <- neodg %>%
  ggplot(.,
         aes(x = group,
             y = or_uni,
             # alpha = p_cat,
             fill = p_cat,
             label = str_wrap(label, 15))) + 

  # non-significant point
  geom_point(data = filter(neodg, p_cat == "n.s."),
             position = pos_b,
             size = 2.5, 
             shape = 1,
             stroke = 0.2,
             col = "grey26") + 

  # significant point
  geom_point(data = filter(neodg, p_cat == "sig"),
             position = pos_b, 
             size = 2.5,
             shape = 19,
             stroke = 0.2) +

  #flip coordinates
  coord_flip() + 
  #scales
  scale_alpha_manual(values = c(0.1, 1),
                     guide = "none") + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  labs(y = "Odds ratio for RSV-hospitalisation\n(adjusted for known risks)",
       subtitle = "B) Pregnancy and neonatal\n     variables") + 
  # reference line
  geom_hline(yintercept = 1,
             linetype = 2,
             col = "black") + 
  theme_bw(base_size = 13) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12)
  ) + 
  guides(fill = FALSE)


## save
ggsave(b_non_annotated, 
       file = "figure_2_panel_b.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 8,
       width = 8,
       # units = "cm",
       dpi = 1200
)



c_non_annotated <- par_ep_wide %>%
  ggplot(., aes( x = or_uni_mother,
                 y = or_uni_father,
                 label = str_wrap(label, 15))) + 
  geom_point(size = 2.5) + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_y_continuous(limits = c(0.5, 2.7)) + 
  scale_x_continuous(limits = c(0.5, 2.7)) + 
  theme_bw(base_size = 13) + 
  theme(
    legend.spacing.y = unit(0.5, 'cm'),
    legend.position = "bottom") + 
  # legend
  guides(color = guide_legend(byrow = TRUE)) +
    # legend
  guides(color = guide_legend(byrow = TRUE)) + 
  labs(x = "Odds ratio (adjusted)\nfor mother's health endpoint",
       y = "Odds ratio (adjusted)\nfor father's health endpoint") + 
  labs(subtitle = "C) Mother's and father's variables compared")

ggsave(c_non_annotated, 
       file = "figure_2_panel_c.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 8,
       width = 8,
       # units = "cm",
       dpi = 1200
)








