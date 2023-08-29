
# FIGURE 2

library(tidyr)
library(dplyr)
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
# splines2 package and function 'ns' will (hopefully) 
# work better in predicting fitted values when using spline functions
# There was a problem in predicting using 'rcs' in rms package. See https://docs.google.com/document/d/1QTnRRcKFMXZ7mM8L60B5kSme4M87xZ4tLIn2iV316XM/edit#heading=h.jnbga7daq4jd
library(splines2)
library(tidymodels)
library(gridExtra)





# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"


# training data with non-feasible variables filtered out
compact_data <- fread(paste0(predict_datapath, 
                             "development_set_only_feasible_variables.csv")) %>%
  as_tibble %>%
  # add birth month variable to the predict data 
  mutate(birth_month = as.numeric(month(LAPSEN_SYNTYMAPVM))) 


recent_data <- compact_data %>%
  filter(epidemic_year > 2006)

# # load composite data (Pre-AIC-data) if needed
# composite_data <- fread(paste0(comp_datapath, "composite_data_all_candidate_predictors.csv")) %>%
#   as_tibble

# ids and birth dates
all_bds <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv", 
                 select = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>% as_tibble




# Load model objects and knot locations

setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
visualise_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

keylist_final %>% print(n = 100)

keylist_final$longname[keylist_final$feature == "gest_days"] <- "Gestational age\nat birth, weeks"
keylist_final$longname[keylist_final$feature == "dist"] <- "Months from birth to\nnext epidemic peak"













#       P R E D I C T I O N S    W I T H   N E W   D A T A  - visualise predictors

# D I S T
#### create a result dataframe

# result_frame

# different values of predictor
allvalues <- 1:1300/100
# length (to be used as the size of data frame)
le <- length(allvalues)


# result_frame
result_frame_dist <- data.frame(
  # variable in question
  gest_days = rep(mean(recent_data$gest_days, na.rm = T), le),
  # other cont
  dist = allvalues, 
  weight_sd = rep(mean(recent_data$weight_sd, na.rm = TRUE), le),
  mother_age = rep(mean(recent_data$mother_age, na.rm = TRUE), le),
  
  twin = rep(mean(recent_data$twin), le),
  male_gender = rep(mean(recent_data$male_gender), le),
  # categorical
  sib_0_4 = rep(mean(recent_data$sib_0_4), le),
  sib_4_7 = rep(mean(recent_data$sib_4_7), le),
  # rest is binary
  down = rep(mean(recent_data$down), le),
  sib_resp_hosp = rep(mean(recent_data$sib_resp_hosp), le),
  smoking_neodg = rep(mean(recent_data$smoking_neodg), le),
  # neonate_substance = rep(mean(recent_data$neonate_substance), le),
  any_family_asthma = rep(mean(recent_data$any_family_asthma), le),
  any_severe_chd = rep(mean(recent_data$any_severe_chd), le),
  term_breathing = rep(mean(recent_data$term_breathing), le),
  asd_or_vsd_only = rep(mean(recent_data$asd_or_vsd_only), le),
  q39_confirmed = rep(mean(recent_data$q39_confirmed), le))




# predict, with intervals?
pred <- predict(visualise_fit, newdata = result_frame_dist, se.fit = TRUE) 

probs_dist <- exp(pred$fit)/(1+exp(pred$fit))
ci_high <-  with(pred, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ci_low <-  with(pred, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))

plot_pred_dist <- tibble(
  dist = allvalues,
  predict = probs_dist,
  ci_low = ci_low,
  ci_high = ci_high)

# distplot <- plot_pred_dist %>%
#   ggplot(., aes(
#     x = dist, 
#     y = predict,
#     ymin = ci_low,
#     ymax = ci_high)) + 
#   geom_line(size = 1) + 
#   geom_ribbon(alpha = 0.4, fill = "grey") + 
#   scale_x_continuous(breaks = pretty_breaks(n = 7)) +
#   theme_bw() + 
#   labs(y = "Predicted probability\n(other variables averaged)")

# distplot



# GEST AGE


# different values of predictor
allvalues <- 183:310
# length (to be used as the size of data frame)
le <- length(allvalues)


# result_frame
result_frame_gestage <- data.frame(
  # variable in question
  gest_days = allvalues,
  # other cont
  dist = rep(mean(recent_data$dist, na.rm = TRUE), le), 
  weight_sd = rep(mean(recent_data$weight_sd, na.rm = TRUE), le),
  mother_age = rep(mean(recent_data$mother_age, na.rm = TRUE), le),
  
  twin = rep(mean(recent_data$twin), le),
  male_gender = rep(mean(recent_data$male_gender), le),
  # categorical
  sib_0_4 = rep(mean(recent_data$sib_0_4), le),
  sib_4_7 = rep(mean(recent_data$sib_4_7), le),
  # rest is binary
  down = rep(mean(recent_data$down), le),
  sib_resp_hosp = rep(mean(recent_data$sib_resp_hosp), le),
  smoking_neodg = rep(mean(recent_data$smoking_neodg), le),
  # neonate_substance = rep(mean(recent_data$neonate_substance), le),
  any_family_asthma = rep(mean(recent_data$any_family_asthma), le),
  any_severe_chd = rep(mean(recent_data$any_severe_chd), le),
  term_breathing = rep(mean(recent_data$term_breathing), le),
  asd_or_vsd_only = rep(mean(recent_data$asd_or_vsd_only), le),
  q39_confirmed = rep(mean(recent_data$q39_confirmed), le))




# predict, with intervals?
pred <- predict(visualise_fit, newdata = result_frame_gestage, se.fit = TRUE) 

probs <- exp(pred$fit)/(1+exp(pred$fit))
ci_high <-  with(pred, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ci_low <-  with(pred, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))

plot_pred_gestage <- tibble(
  gest_days = allvalues,
  predict = probs,
  ci_low = ci_low,
  ci_high = ci_high)

# gestage_plot <- plot_pred_gestage %>%
#   ggplot(., aes(
#     x = gest_days, 
#     y = predict,
#     ymin = ci_low,
#     ymax = ci_high)) + 
#   geom_line(size = 1) + 
#   geom_ribbon(alpha = 0.4, fill = "grey") + 
#   scale_x_continuous(breaks = pretty_breaks(n = 7)) +
#   theme_bw() + 
#   labs(y = "Predicted probability\n(other variables averaged)")

# gestage_plot




# MOTHER_AGE

# different values of predictor
recent_data$mother_age %>% unique %>% range
allvalues <- c(140:560)/10
# length (to be used as the size of data frame)
le <- length(allvalues)


# result_frame
result_frame_motherage <- data.frame(
  # variable in question
  gest_days = rep(mean(recent_data$gest_days, na.rm = TRUE), le), 
  # other cont
  dist = rep(mean(recent_data$dist, na.rm = TRUE), le), 
  weight_sd = rep(mean(recent_data$weight_sd, na.rm = TRUE), le),
  mother_age = allvalues,
  
  twin = rep(mean(recent_data$twin), le),
  male_gender = rep(mean(recent_data$male_gender), le),
  # categorical
  sib_0_4 = rep(mean(recent_data$sib_0_4), le),
  sib_4_7 = rep(mean(recent_data$sib_4_7), le),
  # rest is binary
  down = rep(mean(recent_data$down), le),
  sib_resp_hosp = rep(mean(recent_data$sib_resp_hosp), le),
  smoking_neodg = rep(mean(recent_data$smoking_neodg), le),
  # neonate_substance = rep(mean(recent_data$neonate_substance), le),
  any_family_asthma = rep(mean(recent_data$any_family_asthma), le),
  any_severe_chd = rep(mean(recent_data$any_severe_chd), le),
  term_breathing = rep(mean(recent_data$term_breathing), le),
  asd_or_vsd_only = rep(mean(recent_data$asd_or_vsd_only), le),
  q39_confirmed = rep(mean(recent_data$q39_confirmed), le))


# predict, with intervals?
pred <- predict(visualise_fit, newdata = result_frame_motherage, se.fit = TRUE) 

probs <- exp(pred$fit)/(1+exp(pred$fit))
ci_high <-  with(pred, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ci_low <-  with(pred, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))

plot_pred_motherage <- tibble(
  mother_age = allvalues,
  predict = probs,
  ci_low = ci_low,
  ci_high = ci_high)

# motherage_plot <- plot_pred_motherage %>%
#   ggplot(., aes(
#     x = mother_age, 
#     y = predict,
#     ymin = ci_low,
#     ymax = ci_high)) + 
#   geom_line(size = 1) + 
#   geom_ribbon(alpha = 0.4, fill = "grey") + 
#   scale_x_continuous(breaks = pretty_breaks(n = 7)) +
#   theme_bw() + 
#   labs(y = "Predicted probability\n(other variables averaged)")

# motherage_plot





# WEIGHT_SD


# different values of predictor
recent_data$weight_sd %>% unique %>% range(na.rm = TRUE)
allvalues <- c(-400:400)/100
# length (to be used as the size of data frame)
le <- length(allvalues)


# result_frame
result_frame_weight <- data.frame(
  # variable in question
  gest_days = rep(mean(recent_data$gest_days, na.rm = TRUE), le), 
  # other cont
  dist = rep(mean(recent_data$dist, na.rm = TRUE), le), 
  weight_sd = allvalues,
  mother_age = rep(mean(recent_data$mother_age, na.rm = TRUE), le),
  
  twin = rep(mean(recent_data$twin), le),
  male_gender = rep(mean(recent_data$male_gender), le),
  # categorical
  sib_0_4 = rep(mean(recent_data$sib_0_4), le),
  sib_4_7 = rep(mean(recent_data$sib_4_7), le),
  # rest is binary
  down = rep(mean(recent_data$down), le),
  sib_resp_hosp = rep(mean(recent_data$sib_resp_hosp), le),
  smoking_neodg = rep(mean(recent_data$smoking_neodg), le),
  # neonate_substance = rep(mean(recent_data$neonate_substance), le),
  any_family_asthma = rep(mean(recent_data$any_family_asthma), le),
  any_severe_chd = rep(mean(recent_data$any_severe_chd), le),
  term_breathing = rep(mean(recent_data$term_breathing), le),
  asd_or_vsd_only = rep(mean(recent_data$asd_or_vsd_only), le),
  q39_confirmed = rep(mean(recent_data$q39_confirmed), le))

# predict, with intervals?
pred <- predict(visualise_fit, newdata = result_frame_weight, se.fit = TRUE) 

probs <- exp(pred$fit)/(1+exp(pred$fit))
ci_high <-  with(pred, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
ci_low <-  with(pred, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))

plot_pred_weight <- tibble(
  weight_sd = allvalues,
  predict = probs,
  ci_low = ci_low,
  ci_high = ci_high)

# weight_plot <- plot_pred_weight %>%
#   ggplot(., aes(
#     x = weight_sd, 
#     y = predict,
#     ymin = ci_low,
#     ymax = ci_high)) + 
#   geom_line(size = 1) + 
#   geom_ribbon(alpha = 0.4, fill = "grey") + 
#   scale_x_continuous(breaks = pretty_breaks(n = 7)) +
#   theme_bw() + 
#   labs(y = "Predicted probability\n(other variables averaged)")

# weight_plot





# all results combinded

# recode every result frame to a same format
weight_res <- plot_pred_weight %>%
  pivot_longer(cols = weight_sd)

mother_age_res <- plot_pred_motherage %>%
  pivot_longer(cols = mother_age)

gest_days_res <- plot_pred_gestage %>%
  pivot_longer(cols = gest_days) %>%
  mutate(value = value/7)

dist_res <- plot_pred_dist %>%
  pivot_longer(cols = dist)

# all probabilities done with new mock data for continuous variables, for faceting
all_cont_res <- bind_rows(weight_res, mother_age_res, gest_days_res, dist_res) %>%
  rename(feature = name) %>%
  left_join(keylist_final, by = "feature") %>%
  mutate(longname = str_wrap(longname, 20))




# plot with faceting
nice_facet <- all_cont_res %>%
  ggplot(., aes(
    x = value, 
    y = predict,
    ymin = ci_low,
    ymax = ci_high,
    group = longname)) + 
  facet_wrap( ~ longname, 
              scales = "free_x",
              strip.position = "bottom") +   
  # confidence interval
  geom_ribbon(fill = "gray89") + 
  # line
  geom_line(size = 1) +
  scale_x_continuous(breaks = pretty_breaks(n = 8),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  # set viewing window with coord_cartesian so that geom_ribbon doesn't break
  coord_cartesian(ylim = c(0, 0.053)) + 
  labs(y = "Predicted probability of\nRSV hospitalisation\n(other variables averaged)",
       x = NULL) + 
  theme_bw(base_size = 14) + 
  theme(
    # aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 12))

nice_facet





### binary variable coefs
coef(summary(visualise_fit))

var_order <- c("sib_0_4", "sib_4_7", "twin", "sib_resp_hosp", "any_family_asthma",
               "down", "q39_confirmed", "any_severe_chd", "asd_or_vsd_only",
               "smoking_neodg", "term_breathing", "male_gender") %>% rev

longname_order <- keylist_final %>%
  filter(feature %in% var_order)

longname_order <- longname_order[match(var_order, longname_order$feature),] %>%
  pull(longname)

bin_coef <- coef(summary(visualise_fit)) %>%
  as.data.frame %>%
  rownames_to_column(., var = "feature") %>% 
  as_tibble(.name_repair = "unique") %>%
  filter(!(substr(feature, 1, 3) %in% c("rcs", "(In"))) %>%
  select(-any_of("`z value`")) %>%
  rename(
    coef = Estimate,
    se = `Std. Error`,
    p = `Pr(>|z|)`,
    z = `z value`
  ) %>%
  select(-z) %>%
  mutate(or = exp(coef),
         ci_high = exp(coef + 1.96*se),
         ci_low = exp(coef - 1.96*se)) %>%
  select(-any_of(c("coef", "se"))) %>%
  left_join(., keylist_final, by = "feature") %>%
  mutate(longname = str_wrap(longname, 26)) %>%
  mutate(longname = fct_relevel(longname, longname_order)) %>%
  mutate(group = ifelse(
    feature %in% c("twin", "sib_0_4", "sib_4_7", "sib_resp_hosp",
                   "smoking_neodg", "any_family_asthma"),
    "Family members",
    "Child"))





bin_or_plot <- bin_coef %>%
  ggplot(aes(
    x = or,
    xmax = ci_high,
    xmin = ci_low, 
    y = longname
  )) + 
  geom_point() + 
  geom_errorbarh(height = 0.2) + 
  scale_x_log10(breaks = pretty_breaks(n = 4)) + 
  geom_vline(xintercept = 1,
             linetype = 2) + 
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12)) +
  labs(y = NULL,
       x = "Odds ratio\n(adjusted for other variables)") + 
  
  facet_wrap( ~ group,
              ncol = 1,
              scales = "free_y",
              strip.position = "left") + 
  theme_bw(base_size = 14) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 14)) 



bin_or_plot


# arrange to facets

nice_facet
bin_or_plot

my_layout <- rbind(c(1, 2), c(1, 2))
my_layout

my_plotlist <- list(nice_facet, bin_or_plot)

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

# size: 
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1227&height=878

ggsave(g, 
       file = "figure_3.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 9,
       width = 11,
       dpi = 1200
)


ggsave(nice_facet, 
       file = "figure_3_panel_left.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 9,
       width = 11,
       dpi = 1200
)

ggsave(bin_or_plot, 
       file = "figure_3_panel_right.eps",
       path = "/data/projects/project_pvartiai/rsv/results/plots/",
       device = cairo_ps,
       height = 9,
       width = 11,
       dpi = 1200
)
