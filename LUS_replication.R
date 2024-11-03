## ************************************************************************** ## 
## Project: Gov 2020 - Other Group Replication
## Author: Leah U. Stein
## Date Created: November 3, 2024
## Date Edited: November 3, 2024
## Task: Replicate Partial Draft of Conroy, Jackson, Vazquez (2024)
## ************************************************************************** ## 

## File Set Up ----

# load libraries 
library(tidyverse)
library(readxl)
library(stargazer)

# working directory
data_wd <- "/Users/leah/Library/CloudStorage/Dropbox/Harvard/Year 1/GOV 2020/week 8 - other group replication/representation_replication/Our Data"

## Import Data ----
setwd(data_wd)

# data on party control and share of women in congress (1964-2008)
party_control <- read_csv('party-women_control.csv')

# public opinion and policy outcome data 
policy_df <- read_excel("Main dataset for United States, from Gilens 2012.xlsx")

# Construct Variables ----

# create new main variables 
policy_df <- policy_df %>% mutate(outcome = OUTCOME,
                                  all = PREDALL_SW)
# recode outcome to binary 
policy_df <- policy_df %>% mutate(outcome = case_when(outcome == 0 ~ 0, .default = 1))

# compute preferences for males and females
policy_df <- policy_df %>% mutate(male = (MALE_FAV/(MALE_FAV+MALE_OPP)),
                                  female = (FEMALE_FAV/(FEMALE_FAV+FEMALE_OPP)))
policy_df <- policy_df %>% mutate(male = case_when(SWITCHER==1 ~ 1-male, .default = male),
                                  female = case_when(SWITCHER==1 ~ 1-female, .default = female))

# gender policy different variables
policy_df <- policy_df %>% mutate(genderdiff = abs(male-female),
                                  genderdiff_nonabs = male - female)

# mean centering variables 
policy_df <- policy_df %>% mutate(all_mean = all - mean(all, na.rm = T),
                                  YEAR_mean = YEAR - mean(YEAR, na.rm = T))

# come back to the DK variables if needed 

# Merge Policy Outcomes with Gilens (2012) data ----
main <- policy_df %>%
  left_join(party_control, by = c("YEAR" = "year")) %>%
  select(-ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

# create partisan control scale
main <- main %>%
  mutate(
    # Assign numeric values based on party control for each branch
    pres_score = ifelse(pres_control == "D", 0.5, 0),
    house_score = ifelse(house_control == "D", 0.25, 0),
    senate_score = ifelse(senate_control == "D", 0.25, 0),
    
    # Calculate total control scale
    party_control_scale = pres_score + house_score + senate_score
  )

main <- main %>%
  mutate(OUTCMYEAR = floor(OUTCMYEAR))

## new logic of outcome variable from Gilens
expanded_df <- main %>%
  rowwise() %>%
  mutate(
    outcome_first_year = ifelse(OUTCMYEAR == YEAR, 1, 0),
    outcome_second_year = ifelse(OUTCMYEAR == YEAR + 1, 1, 0)
  ) %>%
  ungroup() %>%
  # Ensure obs_type has no missing values by assigning a default category
  mutate(
    obs_type = case_when(
      outcome_first_year == 1 ~ "first_year",
      outcome_second_year == 1 ~ "second_year",
      TRUE ~ "no_adoption"
    )
  ) %>%
  # Duplicate rows based on obs_type with a default weight of 1 if not second_year
  uncount(weights = ifelse(obs_type == "second_year", 2, 1), .id = "obs_id") %>%
  mutate(
    outcome = case_when(
      obs_id == 1 & obs_type == "first_year" ~ 1,
      obs_id == 1 & obs_type == "second_year" ~ 0,
      obs_id == 2 & obs_type == "second_year" ~ 1,
      TRUE ~ 0
    ),
    YEAR = YEAR + ifelse(obs_id == 2 & obs_type == "second_year", 1, 0),
    weight = ifelse(obs_type == "first_year", 1, 0.5)
  ) %>%
  select(-outcome_first_year, -outcome_second_year, -obs_id, -obs_type)

# Descriptive Analysis ----

# Figure 1: Density of Gender Difference by Policy Outcome (Passed or Not Passed)
figure1 <- ggplot(expanded_df %>% filter(!is.na(outcome)), aes(x = genderdiff_nonabs, 
                                                               fill = factor(outcome))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Gender Difference by Policy Outcome", 
       x = "Gender Difference (Male - Female)", fill = "Policy Outcome") +
  scale_fill_manual(values = c("blue", "green"), 
                    labels = c("Not Passed", "Passed")) +
  theme_minimal()

# Figure 2: Density of Gender Differences with Gender Gap
figure2 <- ggplot(expanded_df %>% filter(!is.na(outcome), genderdiff > 0.1), 
                  aes(x = genderdiff_nonabs, fill = factor(outcome))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Gender Difference by Policy Outcome (with Gender Gap)", 
       x = "Gender Difference (Male - Female)", fill = "Policy Outcome") +
  scale_fill_manual(values = c("blue", "green"), labels = c("Not Passed", "Passed")) +
  theme_minimal()

# Figure 3: Density of Gender Differences with Gender Gap and Party Controls
## Define party control categories with party_control_scale
expanded_df <- expanded_df %>%
  mutate(party_control_category = case_when(
    party_control_scale == 0 ~ "Full Republican",
    party_control_scale == 0.25 ~ "Mostly Republican",
    party_control_scale == 0.5 ~ "Divided",
    party_control_scale == 0.75 ~ "Mostly Democrat",
    party_control_scale == 1 ~ "Full Democrat"
  ))

# Regression Analysis ----
m <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = expanded_df)
stargazer(m, title = "Effect of Party Control on Gender Responsiveness")

# new dataframe to only analyze policies with a gender gap
gendered <- expanded_df %>%
  filter(genderdiff > 0.1)

# Model 1: Treatment Effect
m1 <- glm(outcome ~ genderdiff_nonabs * party_control_scale, data = gendered,
          family = binomial(link = "logit"))

## Regression Model for Specific Issue Areas

#Foreign policy
foreign_policy <- expanded_df %>%
  filter(XL_AREA == "foreign pol")
m30 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = foreign_policy)

#Social welfare
social_welfare <- expanded_df %>%
  filter(XL_AREA == "soc welfare")
m31 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = social_welfare)

#Economic Policy
economic_policy <- expanded_df %>%
  filter(XL_AREA == "econ & labor")
m32 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = economic_policy)


#Religious Issues
religious_issues <- expanded_df %>%
  filter(XL_AREA == "religious")
m33 <- lm(outcome ~ genderdiff_nonabs * party_control_scale, data = religious_issues)
