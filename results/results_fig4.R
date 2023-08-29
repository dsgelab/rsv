
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

#### LOAD FINNISH VALIDATION DATA
# load datasets: predict_data and all_bds

setwd("/data/projects/project_pvartiai/rsv/modeling")
predict_datapath <- "/data/projects/project_pvartiai/rsv/modeling/"
comp_datapath <- "/data/projects/project_pvartiai/rsv/predictors/"
val_path <- "/data/projects/project_pvartiai/rsv/validation_data/"

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



test_set <- fread(paste0(val_path, "validation_set_regression.csv")) %>%
  as_tibble()


# Load model objects and knot locations

setwd("/data/projects/project_pvartiai/rsv/functions/model/")

final_form_simple <- get(load("/data/projects/project_pvartiai/rsv/functions/model/final_formula_object_nointeract.R"))
load("rcsknots_motherage.R")
load("rcsknots_dist.R") 
load("rcsknots_gestdays.R")
load("rcsknots_weightsd.R")
model_fit <- get(load("final_model_fit.R"))

setwd("/data/projects/project_pvartiai/rsv/functions/")
load("keylist_final_variable_names.R")

keylist_final %>% print(n = 100)



####### AUC 

##### LOAD SWEDISH VALIDATION RESULTS
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"
auc_swe_yearly <- get(load(paste0(swed_path, "yearly_auc.R")))

overall_auc_3last <- get(load(paste0(swed_path, "auc_3_last.R")))

overall_auc_swe <- tibble(
  auc = overall_auc_3last[2],
  ci_high = overall_auc_3last[3],
  ci_low = overall_auc_3last[1],
  epidemic_year = "Pooled data\nfrom 2018-2020",
  Country = "Sweden")

indiv_auc_swe <- auc_swe_yearly %>%
  filter(epidemic_year %in% 2018:2020) %>%
  mutate(Country = "Sweden",
         epidemic_year = as.character(epidemic_year)) %>%
  rename(ci_low = ci_low_auc,
         ci_high = ci_auc_high) %>%
  mutate(se_auc = (ci_high - auc)/1.96) 

meta_swe <- rma(auc,
                sei = se_auc,
                data = indiv_auc_swe)

rma_swe <- tibble(
  auc = meta_swe$b %>% as.numeric,
  ci_low = meta_swe$ci.lb,
  ci_high = meta_swe$ci.ub,
  epidemic_year = "Random effect\nmeta-analysis",
  Country = "Sweden")


auc_swe_yearly <- bind_rows(indiv_auc_swe, rma_swe, overall_auc_swe)


##### VALIDATION IN FINLAND

# predict
test_set$pred <- predict(model_fit, newdata = test_set, type = "response")

# overall auc
overall_auc_obj <- as.numeric(ci.auc(test_set$outcome, test_set$pred))

overall_auc <- tibble(
  auc = overall_auc_obj[2],
  ci_high = overall_auc_obj[3],
  ci_low = overall_auc_obj[1],
  epidemic_year = "Pooled data\nfrom 2018-2020",
  model = "Clinical prediction model"
)

yearly_auc <- test_set %>%
  group_by(epidemic_year) %>%
  summarise(auc = as.numeric(auc(outcome, pred)),
            ci_low = as.numeric(ci.auc(outcome, pred)[1]),
            ci_high = as.numeric(ci.auc(outcome, pred)[3])) %>%
  mutate(se_auc = (ci_high - auc)/1.96) %>%
  mutate(epidemic_year = as.character(epidemic_year)) %>%
  mutate(Country = "Finland")


meta_analysis <- rma(auc,
                     sei = se_auc,
                     data = yearly_auc)

rma <- tibble(
  auc = meta_analysis$b %>% as.numeric,
  ci_low = meta_analysis$ci.lb,
  ci_high = meta_analysis$ci.ub,
  epidemic_year = "Random effect\nmeta-analysis")

indiv_auc <- bind_rows(yearly_auc, rma, overall_auc) %>%
  mutate(model = "Clinical\nprediction\nmodel") %>%
  mutate(Country = "Finland")




# AUC results

# Finland
yearly_auc
indiv_auc

# Sweden
auc_swe_yearly


## Plot yearly auc
all_indiv_auc <- bind_rows(auc_swe_yearly, indiv_auc) %>%
  select(-any_of(c("n_overall", "n_hosp", "model", "se_auc"))) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year,
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020")) %>%
  mutate(facet = case_when(
    epidemic_year %in% c("2018", "2019", "2020") ~ "Individual epidemic years",
    epidemic_year %in% c("Random effect\nmeta-analysis", "Pooled data\nfrom 2018-2020") ~ "Summary of 2018-2020"
  ))

pos <- position_dodge(width = 0.1)

indiv_auc_plot <- all_indiv_auc %>%
  ggplot(aes(
    y = epidemic_year,
    x = auc,
    xmin = ci_low,
    xmax = ci_high,
    col = Country)) +
  geom_point(position = pos) + 
  geom_errorbarh(height = 0.1,
                 position = pos) +
  theme_bw(base_size = 14) + 
  # facet_wrap(~facet, scales = "free_x",
  #            strip.position = "top") + 
  theme(legend.position = "top",
        legend.margin = margin(t = -25),
        legend.spacing.x = unit(4, "mm"),
        legend.spacing.y = unit(0, "mm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank()) +
  geom_hline(yintercept = 3.5,
             col = "black",
             linetype = 1) +
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  coord_flip() + 
  labs(x = "C-statistic",
       y = NULL) +
  labs(subtitle = "A")


indiv_auc_plot



####### AUC ENDS




##### CALIBRATION

# predict
test_set$pred <- predict(model_fit, newdata = test_set, type = "response")
test_set$logit <- predict(model_fit, newdata = test_set)

# calib plot object (finland)
calib <- test_set %>%
  select(epidemic_year, outcome, pred) %>%
  mutate(prob_bin = ntile(pred, 10)) %>%
  group_by(epidemic_year, prob_bin) %>%
  summarise(mean_pred = mean(pred, na.rm = T),
            mean_obs = mean(outcome, na.rm = T)) %>%
  mutate(even_year = as.factor(ifelse(epidemic_year %% 2 == 0, "even", "odd"))) %>%
  na.omit() %>%
  mutate(Country = "Finland")

#### OVERALL - cil and slope

# calibration slope
slope_fit <- glm(data = test_set,
                 formula = outcome ~ logit,
                 family = binomial())

slope <- coef(summary(slope_fit))[2,1]
se_slope <- coef(summary(slope_fit))[2,2]

overall_slope <- tibble(
  slope = slope,
  ci_high = slope + 1.96*se_slope,
  ci_low = slope - 1.96*se_slope,
  epidemic_year = "Pooled data\nfrom 2018-2020",
  Country = "Finland")



# offset model for calibration in the large
offset_fit <- glm(data = test_set,
                  formula = outcome ~ offset(1*logit),
                  family = binomial())

cil <- coef(summary(offset_fit))[1]
se_cil <- coef(summary(offset_fit))[2]

overall_cil <- tibble(
  cil = cil,
  ci_high = cil + 1.96*se_cil,
  ci_low = cil - 1.96*se_cil,
  epidemic_year = "Pooled data\nfrom 2018-2020",
  Country = "Finland")


### YEARLY
yearly_intercept <- NULL
yearly_slope <- NULL

for(i in 2018:2020) {
  
  data_for_year <- test_set %>%
    filter(epidemic_year == i)
  
  
  # calibration slope
  slope_fit <- glm(data = data_for_year,
                   formula = outcome ~ logit,
                   family = binomial())
  
  slope <- coef(summary(slope_fit))[2,1]
  se_slope <- coef(summary(slope_fit))[2,2]
  
  slope_df <- tibble(
    slope = slope,
    ci_high = slope + 1.96*se_slope,
    ci_low = slope - 1.96*se_slope,
    epidemic_year = i,
    Country = "Finland")
  
  # bind results to the existing results data  
  yearly_slope <- bind_rows(yearly_slope, slope_df)
  
  # offset model for calibration in the large
  offset_fit <- glm(data = data_for_year,
                    formula = outcome ~ offset(1*logit),
                    family = binomial())
  
  cil <- coef(summary(offset_fit))[1]
  se_cil <- coef(summary(offset_fit))[2]
  
  cil_df <- tibble(
    cil = cil,
    ci_high = cil + 1.96*se_cil,
    ci_low = cil - 1.96*se_cil,
    epidemic_year = i,
    Country = "Finland")
  
  # bind the results to the existing results data
  yearly_intercept <- bind_rows(yearly_intercept, cil_df)
  
}

yearly_intercept 
yearly_slope


meta.analyse <- function(df, metric) {
  
  df <- df %>%
    mutate(epidemic_year = as.character(epidemic_year))
  
  df$se <- (df[["ci_high"]] - df[[metric]])/1.96
  value <- df[[metric]]
  se <- df[["se"]]
  
  meta <- rma(value, sei = se)
  
  meta_df <- tibble(
    ci_low = meta$ci.lb,
    ci_high = meta$ci.ub,
    epidemic_year = "Random effect\nmeta-analysis")
  
  meta_df[[metric]] <- meta$b %>% as.numeric
  
  df_2 <- bind_rows(df, meta_df)
  
  return(df_2)
}

yearly_intercept <- meta.analyse(df = yearly_intercept, metric = "cil")
yearly_slope <- meta.analyse(df = yearly_slope, metric = "slope")


finnish_intercept <- bind_rows(yearly_intercept, 
                               overall_cil) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year, 
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020")) %>%
  mutate(Country = "Finland")


finnish_slope <- bind_rows(yearly_slope, 
                           overall_slope) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year, 
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020")) %>%
  mutate(Country = "Finland")


finnish_slope
finnish_intercept
calib

##### LOAD SWEDISH VALIDATION RESULTS
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"

swe_calib_names <- c("overall_cil_3last.R",
                     "overall_slope_3last.R",
                     "yearly_calib.R",
                     "yearly_intercept.R",
                     "yearly_slope.R")

overall_cil_3last_swe <- get(load(paste0(swed_path, swe_calib_names[1]))) %>%
  mutate(Country = "Sweden",
         epidemic_year = "Pooled data\nfrom 2018-2020")

overall_slope_3last_swe <- get(load(paste0(swed_path, swe_calib_names[2]))) %>%
  mutate(Country = "Sweden",
         epidemic_year = "Pooled data\nfrom 2018-2020")

calib_swe <- get(load(paste0(swed_path, swe_calib_names[3]))) %>%
  mutate(even_year = as.factor(ifelse(epidemic_year %% 2 == 0, "even", "odd"))) 

yearly_intercept_swe <- get(load(paste0(swed_path, swe_calib_names[4])))
yearly_slope_swe <- get(load(paste0(swed_path, swe_calib_names[5])))



### Calibration plot data in sweden

calib_compare <- calib_swe %>%
  filter(epidemic_year %in% 2018:2020) %>%
  mutate(Country = "Sweden") %>%
  bind_rows(., calib)

compare_intercept <- yearly_intercept_swe %>%
  filter(epidemic_year %in% 2018:2020) %>%
  mutate(se = (ci_high-cil)/1.96) %>%
  meta.analyse(., metric = "cil") %>%
  bind_rows(., overall_cil_3last_swe) %>%
  mutate(Country = "Sweden") %>%
  bind_rows(finnish_intercept) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year,
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020")) %>%
  mutate(facet = case_when(
    epidemic_year %in% c("2018", "2019", "2020") ~ "Individual epidemic years",
    epidemic_year %in% c("Random effect\nmeta-analysis", "Pooled data\nfrom 2018-2020") ~ "Summary of 2018-2020"
  ))


compare_slope <- yearly_slope_swe %>%
  filter(epidemic_year %in% 2018:2020) %>%
  mutate(se = (ci_high-cil)/1.96)%>%
  meta.analyse(., metric = "slope") %>%
  bind_rows(., overall_slope_3last_swe) %>%
  mutate(Country = "Sweden")%>%
  bind_rows(finnish_slope) %>%
  mutate(epidemic_year = fct_relevel(epidemic_year,
                                     "2018", "2019", "2020", 
                                     "Random effect\nmeta-analysis",
                                     "Pooled data\nfrom 2018-2020")) %>%
  mutate(facet = case_when(
    epidemic_year %in% c("2018", "2019", "2020") ~ "Individual epidemic years",
    epidemic_year %in% c("Random effect\nmeta-analysis", "Pooled data\nfrom 2018-2020") ~ "Summary of 2018-2020"
  ))




### PLOTS

indiv_auc_plot

pos <- position_dodge(width = 0.1)

intercept_plot <- compare_intercept %>%
  ggplot(aes(x = epidemic_year,
             y = cil, 
             ymin = ci_low,
             ymax = ci_high,
             col = Country)) + 
  geom_point(position = pos) + 
  geom_errorbar(width = 0.1,
                position = pos) + 
  labs(y = "Calibration-in-the-large",
       x = NULL) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 3.5, col = "black") + 
  # facet
  # facet_wrap(~facet, scales = "free_x",
  #            strip.position = "bottom") + 
  theme_bw(base_size = 14) + 
  # theme
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  guides(color = FALSE)

intercept_plot

slope_plot <- compare_slope %>%
  ggplot(aes(x = epidemic_year,
             y = slope, 
             ymin = ci_low,
             ymax = ci_high,
             col = Country)) + 
  geom_point(position = pos) + 
  geom_errorbar(width = 0.1,
                position = pos) + 
  labs(x = NULL,
       y = "Calibration slope") + 
  geom_hline(yintercept = 1, linetype = 2) + 
  geom_vline(xintercept = 3.5, col = "black") + 
  theme_bw(base_size = 14) + 
  # facet_wrap(~facet, scales = "free_x",
  #            strip.position = "bottom") + 
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) + 
  guides(color = FALSE)

#  annotate("rect", xmin = 3.5, ymin = -Inf, ymax = Inf, xmax = Inf,
#             alpha = .2, col = "lightgrey")

slope_plot

labs <- calib_compare %>%
  filter(prob_bin == 10) %>%
  select(epidemic_year, mean_pred, mean_obs, Country) %>%
  rename(label = epidemic_year,
         x = mean_pred,
         y = mean_obs) %>%
  mutate(epidemic_year = label) 
labs$y[2] <- 0.064
labs$x[2] <- 0.046
labs$y[1] <- 0.045


calib_plot_x <- calib_compare %>%
  ggplot(., aes(
    x = mean_pred,
    y = mean_obs,
    group = factor(epidemic_year),
    col = Country)) + 
  geom_point() +
  stat_smooth(span = 1,
              se = FALSE,
              size = 0.8, 
              inherit.aes = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  labs(x = "Predicted probability",
       y = "Observed outcome rate") + 
  
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) +
  scale_linetype_manual(values = c(1, 2)) + 
  guides(linetype=FALSE) + 
  facet_wrap(~Country) +
  
  geom_text(
    data = labs,
    mapping = aes(x = x, y = y, label = label),
    hjust = -0.3,
    vjust = 0.2,
    color = "black") + 
  
  theme(legend.position = "top") + 
  theme_bw(base_size = 14) + 
  guides(color = FALSE) + 
  labs(subtitle = "B")


calib_plot_x


### combine

my_plotlist <- list(indiv_auc_plot,
                    intercept_plot, 
                    slope_plot, 
                    calib_plot_x)

my_layout <- cbind(c(1,1,1,2,2,3,3,4,4,4))

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g4 <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

g4

#size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=791&height=1079

ggsave(plot = g4,
       filename = "figure_4.eps",
       device = cairo_ps,
       width = 7.91,
       height = 10.79,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")




ggsave(plot = indiv_auc_plot,
       filename = "figure_4_panel_a.eps",
       device = cairo_ps,
       width = 7.91,
       height = 4,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")


ggsave(plot = intercept_plot,
       filename = "figure_4_panel_b.eps",
       device = cairo_ps,
       width = 7.91,
       height = 4,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")


ggsave(plot = slope_plot,
       filename = "figure_4_panel_c.eps",
       device = cairo_ps,
       width = 7.91,
       height = 4,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")

calib_plot_x_no_labels <- calib_compare %>%
  ggplot(., aes(
    x = mean_pred,
    y = mean_obs,
    group = factor(epidemic_year),
    col = Country)) + 
  geom_point() +
  stat_smooth(span = 1,
              se = FALSE,
              size = 0.8, 
              inherit.aes = TRUE) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  labs(x = "Predicted probability",
       y = "Observed outcome rate") + 
  
  scale_color_manual(values =c("#9fbfd4", "#D6C533")) +
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.001, 0.11)) +
  scale_linetype_manual(values = c(1, 2)) + 
  guides(linetype=FALSE) + 
  facet_wrap(~Country) +
  
  theme(legend.position = "top") + 
  theme_bw(base_size = 14) + 
  guides(color = FALSE) + 
  labs(subtitle = "B")


  
  ggsave(plot = calib_plot_x_no_labels,
       filename = "figure_4_panel_d.eps",
       device = cairo_ps,
       width = 10,
       height = 4.4,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")






### scatter of cil and case numbers

yearly_intercept_swe
yearly_slope_swe

yearly_intercept_scatter <- yearly_intercept %>%
  filter(epidemic_year %in% c("2018", "2019", "2020")) %>%
  mutate(epidemic_year = as.numeric(epidemic_year))

#load auc and case numbers
swed_path <- "/data/projects/project_pvartiai/rsv/results/sweden/"
auc_swe_yearly <- get(load(paste0(swed_path, "yearly_auc.R")))

n_cases_swe <- auc_swe_yearly %>%
  select(epidemic_year, n_hosp, n_overall)

swe_cil_scatter_data <- left_join(yearly_intercept_swe, n_cases_swe, by = "epidemic_year") %>%
  mutate(metric = "Calibration-in-the-large") %>%
  rename(measure = cil)

auc_swe_scatter <- auc_swe_yearly %>%
  mutate(metric = "C-statistic") %>%
  rename(measure = auc,
         ci_low = ci_low_auc,
         ci_high = ci_auc_high)




scatter_data <- bind_rows(swe_cil_scatter_data, auc_swe_scatter) %>%
  mutate(hosp_prevalence = n_hosp / n_overall) %>%
  mutate(ref = ifelse(metric == "Calibration-in-the-large", 0, NA))



scatter <- scatter_data %>%
  ggplot(aes(
    x = hosp_prevalence*100,
    y = measure,
    ymin = ci_low,
    ymax = ci_high
  )) + 
  geom_point() + 
  geom_errorbar(width = 0.02) + 
  facet_wrap(~metric,
             scales = "free_y") + 
  theme_bw(base_size = 14) + 
  labs(x = "Prevalence of RSV hospitalisation during an epidemic year, %",
       y= NULL) + 
  geom_hline(aes(yintercept = ref), linetype = 2) + 
  theme(strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 14))

scatter

# size
# https://localhost:8443/rstudio/graphics/plot_zoom_png?width=1136&height=670

ggsave(plot = scatter,
       filename = "suppl_scatter.eps",
       device = cairo_ps,
       width = 11.36,
       height = 6.70,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")



