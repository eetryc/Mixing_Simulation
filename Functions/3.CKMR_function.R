########################################
########### CKMR function ##############
########################################


library(tidyverse)
library(dplyr)
library(lubridate)




#### Define a function to input the sample information into ####

CKMR_est <- function(samples){
### POPs filter 
POpairs <- samples %>%
  rename_with(~ paste0(.x, "_1"), everything()) %>% # add the columns for each indiv
  cross_join(samples %>% rename_with(~ paste0(.x, "_2"), everything())) %>% # remove self-pairings
  # mutate to add the POP possibility (y/n) with the year and reproductive age filters
  mutate(HSPPOP_candidate = ifelse(cohort_1 < cohort_2 & 
                                     (cohort_2 - cohort_1 >= rep.age), "1","0")) %>% 
  #get rid of the ones that are the same comp, eg 1 and 2 vs 2 and 1 
  filter(id_1 != id_2) %>% 
  mutate(PairID = paste(id_1, id_2, sep = "_")) %>%
  mutate(older_first=ifelse(cohort_1 < cohort_2 |(cohort_1 == cohort_2 & id_1 < id_2), T,F) ) %>% 
  filter(older_first) %>%
  select(-older_first) %>% 
  mutate(AgeDif = cohort_2 - cohort_1)




# count numnber of yes or no to get a better idea of how they are changing
POpairs %>% 
  dplyr::count(HSPPOP_candidate)


POPonly <- POpairs %>% 
  filter(HSPPOP_candidate == 1) %>% 
  mutate(RObase=ifelse(sex_1 == "U",2,1)) %>% 
  mutate(Age_dif_exp = 0)




# join the two tables
library(plyr)
possible_pairs <- POPonly  %>% 
  mutate(
    sex_num = case_when(
      sex_1 == "U" ~ NA_real_, # unknown to na
      sex_1 == "0" ~ 1, # male is 1
      sex_1 == "1" ~ 0 # female is 0
    )
  )






#### Data to define for JAGS model ####


# years being estimated (years that actually have potential)
Cohort_years <- possible_pairs %>% 
  group_by(cohort_2) %>% 
  dplyr::summarise(
    Pair_viable_count = sum(HSPPOP_candidate > 0, na.rm = TRUE)
  ) %>% 
  filter(Pair_viable_count > 0) %>%     # keep only cohorts with >0
  arrange(cohort_2)

# extract years (count to give to be i in JAGS)
years <- nrow(Cohort_years)


# group together by cohort year and age dif (differing probabilities)
Cohort_years_cohort_age <- possible_pairs %>%
  group_by(cohort_2, AgeDif) %>%
  dplyr::summarise(
    Pair_viable_count_per_agedif = sum(HSPPOP_candidate > 0, na.rm = TRUE))


# add the counts to each observation
observations <- possible_pairs %>%
  left_join(Cohort_years_cohort_age, by = c("cohort_2", "AgeDif"))

# year index to link kinship to years (just the number to match it, not the actual year)
year_index <- factor(observations$cohort_2, levels = Cohort_years$cohort_2)
observations <- observations %>%
  mutate(
    year_index = as.integer(year_index)
  )




# Count the ones with known sex (U = 0)
# Count females (F = 1)
observations <- observations %>%
  mutate(
    knownSex = as.numeric(!is.na(sex_num)),
    IsFemale = as.numeric(!is.na(sex_num) & (sex_num == 0))  # if sex_num encoded 0 = female
  )




# Match true pairs
observations <- observations %>% 
  mutate(true_HSP = if_else(
    mother_1 == mother_2 | father_1 == father_2, 1, 0),
    true_POP = if_else(
      id_1 == mother_2 | id_1 == father_2, 1, 0),
    TruePairs = if_else(
      true_POP == 1 | true_HSP == 1, 1, 0))



#### Now collapse based on cohort 2, age difference, RObase, if sex is known, and female or male 

group_vars_POP <- c("cohort_2", "knownSex", "IsFemale", "RObase", "Age_dif_exp")


collapsed_POP <- observations %>% 
  filter(RObase == 1) %>% 
  drop_na(mother_1, father_1) %>% 
  group_by(year_index,across(all_of(group_vars_POP))) %>%
  dplyr::summarise(
    Pair_viable_count_per_agedif = n(),                   
    TruePairs = sum(true_POP, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(cohort_2) %>% 
  mutate(AgeDif = 0)


collapsed_POP %>%  
  ggplot(aes(x = year_index, y = TruePairs)) +
  geom_line()

#### Just POP model
data_POP = list( years = years,  # number of cohorts,
                 TruePairs = collapsed_POP$TruePairs,
                 RObase = collapsed_POP$RObase,
                 nobs = nrow(collapsed_POP),
                 Pair_viable_count_per_agedif = collapsed_POP$Pair_viable_count_per_agedif,
                 year=collapsed_POP$year_index)

# Initial values
inits_POP = function() {
  list(
    # surv = runif(1,0.01, 0.99),
    Nadult = pmin(pmax(rnorm(years, mean = 7500, sd = 1000), 0), 10000))
}


# Parameters to follow
params = c("Nadult")

nburn <- 10000
nchains <- 3
niter <- 20000
n.cores = 3


Out_CKMR_sim1_POP = jagsUI::jags(data_POP, inits_POP, params, "HSPPOP.simplest.sim.jags", n.burnin = nburn, n.chains = nchains, n.iter = niter, parallel = T, verbose = T)



#### Plot against true populations and compare
# Extract posterior summary as a data frame
nhat_POP <- as.data.frame(Out_CKMR_sim1_POP$summary)

# Keep only the Nadult rows
nhat_POP <- nhat_POP[grep("^Nadult", rownames(nhat_POP)), ]

# Add a Year index (1, 2, 3, ...)
nhat_POP$Year <- 1:nrow(nhat_POP)

# Select the useful columns
nhat_df_POP <- nhat_POP %>%
  dplyr::select(Year, mean, `2.5%`, `50%`, `97.5%`)
}




