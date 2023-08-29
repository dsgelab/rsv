
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(forcats)
library(data.table)
library(feather)
library(ggplot2)
library(gridExtra)



# load outcome dates
outcome_dates <- fread("/data/projects/project_pvartiai/rsv/wrangle/preprocess_3_mbr_hilmo_outcomes.csv",
                       select = c("TNRO", "LAPSEN_SYNTYMAPVM", "outcome", "TUPVA", "age_at_outcome")) %>%
  as_tibble 


all_ids <- outcome_dates %>%
  pull(TNRO)

all_bds <- outcome_dates %>%
  select(TNRO, LAPSEN_SYNTYMAPVM)

outcomes_by_year <- outcome_dates %>%
  filter(outcome == 1) %>%
  mutate(outcome_year = year(TUPVA)) %>%
  group_by(outcome_year) %>%
  summarise(n = n())

outcomes_by_year %>% print(n = 100)

# load infectious disease registry, and keep only rsv samples
ttr_rsv <- fread("/data/processed_data/thl_infectious_diseases/infectious_diseases_2022-05-24.csv") %>% 
  as_tibble %>%
  select(c(
    TNRO, diagnosis_method, laboratory_type,
    reporting_group, sampling_date
  )) %>%
  filter(reporting_group == "['RSV']") %>%
  filter(TNRO %in% all_ids) %>%
  left_join(., all_bds, by = "TNRO") %>%
  mutate(age_at_outcome = as.numeric(difftime(sampling_date, 
                                              LAPSEN_SYNTYMAPVM,
                                              units = "days"))) %>%
  filter(age_at_outcome < 365.25)


rsv_tests <- ttr_rsv %>%
  select(TNRO, LAPSEN_SYNTYMAPVM, sampling_date) %>%
  mutate(positive_rsv_test = 1)


rsv_tests

compare_test_with_outcome <- outcome_dates %>%
  filter(outcome == 1) %>%
  left_join(., rsv_tests, by = c("TNRO", "LAPSEN_SYNTYMAPVM")) %>%
  mutate(diff_test_outcome = as.numeric(difftime(sampling_date, TUPVA, units = "days"))) %>%
  mutate(positive_rsv_test = ifelse(abs(diff_test_outcome) >= 10, 0, positive_rsv_test)) %>%
  mutate(positive_rsv_test = ifelse(is.na(positive_rsv_test), 
                                    0, 
                                    positive_rsv_test)) %>%
  mutate(outcome_year = year(TUPVA)) %>%
  group_by(outcome_year) %>%
  summarise(rate_of_test = mean(positive_rsv_test),
            n_outcome = n()) %>%
  filter(outcome_year > 2003) 


compare_test_with_outcome


outcome_rsv_test_plot <- compare_test_with_outcome %>%
  ggplot(., aes(x = outcome_year,
                y = rate_of_test*100)) + 
  geom_col(fill = "grey",
           col = "black") + 
  theme_bw() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 105)) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
  labs(x = "Calendar year",
       y = "Percentage of RSV bronchiolitis\n(J21.0) hospitalisations\nwith positive RSV test",
       title = "c) Fraction of test-positive hospitalised children")

a <- outcome_rsv_test_plot





# age at outcome

outcome_age_hist <- outcome_dates %>%
  filter(outcome == 1) %>%
  mutate(age_at_outcome = age_at_outcome/30.4) %>%
  ggplot(., aes(x = age_at_outcome)) +
  geom_histogram(bins = 24,
                 col = "black",
                 fill = "#6495ED") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 2600)) + 
  theme_bw()


outcome_age_dens <- outcome_dates %>%
  filter(outcome == 1) %>%
  mutate(age_at_outcome = age_at_outcome/30.4) %>%
  ggplot(., aes(x = age_at_outcome)) +
  geom_density(col = "#333399",
               size = 1,
               fill = "lightgrey",
               alpha = 0.5) + 
  scale_x_continuous(breaks = pretty_breaks(),
                     expand = c(0,0),
                     limits = c(-0.2, 12.2)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.24)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
  theme_bw() + 
  labs(x = "Age at outcome,\nmonths",
       title = "b) Density plot of age at hospitalisation")






# # # # # # #   outcome epidemic pattern


# positive rsv tests in children <1 years
test <- rsv_tests %>%
  select(TNRO, sampling_date, positive_rsv_test) %>%
  rename(outcome = positive_rsv_test) %>%
  mutate(label = "Positive RSV tests in children <1 year old") %>%
  filter(sampling_date > as.Date("2003-1-1"))

hosp <- outcome_dates %>%
  filter(outcome == 1) %>%
  select(TNRO, TUPVA, outcome) %>%
  rename(sampling_date = TUPVA) %>%
  mutate(label = "RSV bronchiolitis hospitalisations in children <1 year old")

pattern_comp <- bind_rows(test, hosp) %>%
  mutate(month = floor_date(sampling_date, 'month')) %>%
  group_by(label, month) %>%
  summarise(n = n()) %>%
  mutate(facet = ifelse(month < as.Date("2007-1-1"), 0, 1))


pattern_plot <- pattern_comp %>%
  ggplot(aes(x = month,
             group = label,
             y = n,
             col = label,
             linetype = label)) + 
  geom_line() + 
  scale_linetype_manual(values = c(2, 1),
                        name = NULL) + 
  scale_color_manual(values = c("darkgrey", "black")) + 
  theme_bw() + 
  scale_x_date(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 800)) +
  # facet_wrap(~ facet, 
  #            nrow = 2,
  #            scales = "free_x") + 
  theme(axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        strip.text.x = element_blank()) + 
  guides(linetype=guide_legend(title=NULL),
         col = guide_legend(title = NULL)) + 
  labs(y = "Number of cases per month",
       x = "Calendar year",
       title = "a) Monthly count of RSV tests and hospitalisations")



rate_plot <- outcome_dates %>%
  mutate(birth_year = year(LAPSEN_SYNTYMAPVM)) %>%
  mutate(epidemic_year = ifelse(month(LAPSEN_SYNTYMAPVM) <= 5, birth_year, birth_year +1)) %>%
  group_by(epidemic_year) %>%
  summarise(rate = mean(outcome, na.rm = T),
            n = n()) %>%
  ggplot(., aes(
    x = epidemic_year, 
    y = rate
  )) + 
  geom_col(col = "black",
           fill ="grey") + 
  scale_x_continuous(breaks = pretty_breaks(5),
                     expand = c(0,0),
                     limits = c(1997, 2019)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.032)) +
  theme_bw() +
  labs(y = "Fraction of hospitalised infants",
       x = "Year of the RSV epidemic",
       title = "d) Hospitalisation rates during RSV epidemics")

rate_plot
  
  

# combine plots

a <- pattern_plot
b <- outcome_age_dens
c <- outcome_rsv_test_plot
d <- rate_plot


my_layout <- rbind(c(1, 1), c(3, 4), c(2, 2))
my_layout

my_plotlist <- list(a, d, b, c)

grid.arrange(grobs = my_plotlist, layout_matrix = my_layout)

g <- arrangeGrob(grobs = my_plotlist, layout_matrix = my_layout)

ggsave(plot = g,
       filename = "suppl_outcome_validation_alt.pdf",
       device = "pdf",
       width = 9,
       height = 9,
       dpi = 1200,
       path = "/data/projects/project_pvartiai/rsv/results/plots/")










