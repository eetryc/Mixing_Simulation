#####################################
##### Single Stock simple sim #######
#####################################


## ^ variable to manipulate

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

######## Generate population at year 0 ########

## Define parameters
init.pop.size <- 5000  ## ^

init.prop.female <- 0.5 


age <- rep(4, times = init.pop.size) # set at 5 so these guys can mate when the loop starts

sex <- sample(c('1','0'), size = init.pop.size,
              prob = c(init.prop.female, 1 - init.prop.female),
              replace = TRUE) 

mother <- rep(NA, init.pop.size)

father <- rep(NA, init.pop.size)

cohort <- rep(0, times = init.pop.size)

number <- seq_len(init.pop.size)

id <- paste(cohort, number, sex, sep = "_")


pop.year0 <- data.frame(id, number, cohort, age, sex, mother, father, stringsAsFactors = FALSE)





######## Set simulation parameters ########
rep.age <- 5 # minimum age to reproduce

mean_offspring <- 200 # mean number of offspring per female ## ^

# survival_prob <- 0.4 # annual survival probability (only have one, change in code to shift by getting rid of the following)
# Chaning to age dependent survival
surv_juv0   <- 0.2  # age 0 ## ^
surv_juv1_4 <- 0.5  # ages 1–4 ## ^
surv_adult  <- 0.6  # age 5+ ## ^
 
p_catch <- 0.1 # percent mortality of catch ## ^ 

K <- 5000  # carrying capacity to stabilize population ## ^ 


years <- 20 # number of years to simulate and extract ## ^

burn.in <- 50 # number of years to run the simulation before counting 

run.years <- burn.in + years # total years to run



######## Initialize storage ########
# Clear if rerunning first 
rm(pop_list, all_fish, current_pop,samples,empty_offspring)

pop_list <- list(pop.year0) # separate table for each year

all_fish <- pop.year0 # running table of all fish

current_pop <- pop.year0 # current population

sampling_window <- (run.years - years + 1):run.years

samples <- data.frame() # create empty data frame

sample_rate <- 0.07 # change percent as needed

empty_offspring <- pop.year0[0, ] %>%
  mutate(
    id      = as.character(id),
    number  = as.integer(number),
    cohort  = as.integer(cohort),
    age     = as.numeric(age),
    sex     = as.character(sex),
    mother  = as.character(mother),
    father  = as.character(father)
  )

######## LOOP OVER YEARS ########
for (yr in 1:run.years) {
  
  # Age everyone
  current_pop <- current_pop %>% mutate(age = age + 1)
  
  ### AGE-STRUCTURED NATURAL SURVIVAL ###
  current_pop$survive_nat <- NA_integer_
  
  # age 0
  idx0 <- current_pop$age == 0
  current_pop$survive_nat[idx0] <- rbinom(sum(idx0), 1, surv_juv0)
  
  # ages 1–4
  idx1_4 <- current_pop$age >= 1 & current_pop$age <= 4
  current_pop$survive_nat[idx1_4] <- rbinom(sum(idx1_4), 1, surv_juv1_4)
  
  # adults 5+
  idx_adult <- current_pop$age >= 5
  current_pop$survive_nat[idx_adult] <- rbinom(
    sum(idx_adult), 1,
    surv_adult * exp(-nrow(current_pop) / (8*K))
  )
  
  ### NATURAL SURVIVORS ###
  alive_after_nat <- current_pop %>% filter(survive_nat == 1)
  
  ### FISHING MORTALITY ###
  alive_after_nat$caught <- rbinom(nrow(alive_after_nat), 1, p_catch)
  
  harvested_this_year <- alive_after_nat %>% filter(caught == 1)
  survivors_to_next_year <- alive_after_nat %>% filter(caught == 0)
  
  ### UPDATE POPULATION ###
  current_pop <- survivors_to_next_year
  # table of the dead fish (natural mort. or fishing mort)
  dead_this_year <- current_pop[current_pop$survive == 0, ]
  
  # ---- SAMPLE  (last 10 years) ----
  if (yr %in% sampling_window) {
    n_sample <- floor(sample_rate * nrow(harvested_this_year))
    
    sampled_fish <- harvested_this_year %>%
      slice_sample(n = min(n_sample, nrow(harvested_this_year)), replace = FALSE) %>%
      mutate(year_sampled = yr)
    
    samples <- bind_rows(samples, sampled_fish)
  }
  
  # Keep only survivors
  current_pop <- current_pop[current_pop$survive == 1, ]
  current_pop$survive <- NULL
  
  # if pop goes extinct
  if(nrow(current_pop) == 0){
    message(paste("Population extinct at year", yr))
    break
  }
  
  # Select mature females and males
  mothers <- current_pop %>%
    filter(sex == '1', 
           age >= rep.age)
  
  if(nrow(mothers) > 0){
    mothers$can_mate <- rbinom(nrow(mothers), 1, 0.8)
    mothers <- mothers[mothers$can_mate == 1, ]
    mothers$can_mate <- NULL
    mothers$num_offspring <- rpois(nrow(mothers), lambda = mean_offspring)
  }
  
  fathers <- current_pop %>%
    filter(sex == '0', age >= rep.age)
  
  # Generate offspring
  if(exists("mothers") & nrow(mothers) > 0 & nrow(fathers) > 0){
    offspring_list <- lapply(1:nrow(mothers), function(i) {
      mom <- mothers$id[i]
      n_off <- mothers$num_offspring[i]
      if(n_off == 0) return(NULL)
      
      dads <- sample(fathers$id, n_off, replace = TRUE)
      sex_off <- sample(c('1','0'), n_off, replace = TRUE, prob = c(init.prop.female, 1 - init.prop.female))
      
      data.frame(
        age = rep(0, n_off),
        sex = sex_off,
        mother = mom,
        father = dads,
        cohort = rep(yr, n_off),
        number = NA,
        stringsAsFactors = FALSE
      )
    })
    
    offspring_df <- do.call(rbind, offspring_list)
    
    # Assign number and ID
    max_number <- max(current_pop$number)
    offspring_df$number <- seq(max_number + 1, length.out = nrow(offspring_df))
    offspring_df$id <- paste(offspring_df$cohort, offspring_df$number, offspring_df$sex, sep = "_")
    
    # Ensure same column order as pop.year0
    offspring_df <- offspring_df[, names(pop.year0)]
    
  } else {
    # Create empty offspring data frame with same column types
    offspring_df <- empty_offspring
  }
  
  # Combine current population + offspring
  current_pop <- bind_rows(current_pop, offspring_df)

  # Store population of this year
  pop_list[[yr + 1]] <- current_pop
  
  # Add new fish to running all_fish table
  all_fish <- bind_rows(all_fish, offspring_df)
  
  # Clean up mothers variable for next iteration
  rm(mothers)
}





#### Inspect the files
names(pop_list) <- 0:(length(pop_list)-1) # rename the year for population so it starts at 0 (cohort will match year)
pop_list 

all_fish 

current_pop

samples 


years_to_estimate <- sort(unique(samples$cohort))

## Population size per year
true_abundance <- lapply(pop_list, function(df){
                          df %>% 
                           filter(age >=5) %>% 
                           nrow()})

true_abundance <- as.data.frame(true_abundance) %>% 
  pivot_longer(cols = everything())

true_abundance # check to make sure there isn't letters in the year names. Re run if true, idk why this is happening


true_abundance$name <- as.integer(sub("X", "",true_abundance$name))
true_abundance$name <- true_abundance$name %>% 
  as.integer()
true_abundance <- true_abundance %>% 
  filter(name %in% years_to_estimate) %>% 
  mutate(year_index = row_number(.))


# Graph
ggplot(true_abundance, aes(x = year_index, y = value, group = 1)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  # scale_y_continuous(limits = c(0, 200)) + 
  labs(
    x = "Year (Cohort index)",
    y = expression(hat(N)[adult])) +
  theme_bw()


