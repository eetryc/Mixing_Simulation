#####################################
######## Stock Sim. Function ########
#####################################

#### 0. Load libraries, init. storage ####
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# rm(pop_list, all_fish, current_pop,samples,empty_offspring) # or
rm(list=ls())



#### 1. Define function and constants ####

##------## Defining constants for all stocks ##------##

init.prop.female  <- 0.5  # assume equal amount of sexes

rep.age <- 5 # minimum age to reproduce

max.age <- 40 # max age they'll live to 

mean_offspring <- 15 # mean number of offspring per female ^



##------## Simulation timing ##------##
years <- 20 # number of years to sample and estiamte ^

burn.in <- 100 # number of years to run the simulation before counting 

run.years <- burn.in + years # total years to run

sampling_window <- (run.years - years + 1):run.years




##------## Mortality related constants ##------##
surv_vec <- c(0.35, 0.6, 0.7, 0.75, 0.8, rep(0.9, max.age - 4))

p_catch <- 0.15 # percent mortality of catch ^ 

K <- 10000  # carrying capacity to stabilize population ^ 




##------## Stock specific information ##------##





## Function

stock.sim <- function(init.pop.size){
  
  ## 0. Init. objects
  ##------## Inits for the starting population ##------##
  age <- rep(4, times = init.pop.size) # set at 4 so these guys can mate when the loop starts post aging
  
  sex <- sample(c('1','0'), size = init.pop.size,
                prob = c(init.prop.female, 1 - init.prop.female),
                replace = TRUE) 
  
  mother <- rep(NA, init.pop.size)
  
  father <- rep(NA, init.pop.size)
  
  cohort <- rep(0, times = init.pop.size)
  
  number <- seq_len(init.pop.size)
  
  id <- paste(cohort, number, sex, sep = "_")
  
  pop.year0 <- data.frame(id, number, cohort, age, sex, mother, father, stringsAsFactors = FALSE) # bind for starting pop table
  
  
  current_pop <- pop.year0
  
  samples     <- data.frame()
  
  pop_list    <- vector("list", run.years + 1)
  
  caught_list <- vector("list", run.years)
  
  pop_list[[1]] <- current_pop
  
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
  
  
  for (yr in 1:run.years) {
    
    offspring_df <- empty_offspring
    
    ## 1. Age everyone
    current_pop <- current_pop %>% mutate(age = age + 1)
    
    ## 2. Select mature females and males
    mothers <- current_pop %>%
      filter(sex == "1", age >= rep.age)
    
    if (nrow(mothers) > 0) {
      mothers$can_mate <- rbinom(nrow(mothers), 1, 0.8)
      mothers <- mothers[mothers$can_mate == 1, ]
      mothers$can_mate <- NULL
      if (nrow(mothers) > 0) {
        mothers$num_offspring <- rpois(nrow(mothers), lambda = mean_offspring)
      }
    }
    
    fathers <- current_pop %>%
      filter(sex == "0", age >= rep.age)
    
    
    ## 3. Generate offspring (safe to handle zero-offspring years)
    if (nrow(mothers) > 0 && nrow(fathers) > 0) {
      offspring_list <- lapply(seq_len(nrow(mothers)), function(i) {
        mom   <- mothers$id[i]
        n_off <- mothers$num_offspring[i]
        if (n_off == 0) return(NULL)
        
        dads    <- sample(fathers$id, n_off, replace = TRUE)
        sex_off <- sample(c('1','0'), n_off, replace = TRUE,
                          prob = c(init.prop.female, 1 - init.prop.female))
        
        data.frame(
          age    = rep(0, n_off),
          sex    = sex_off,
          mother = mom,
          father = dads,
          cohort = rep(yr, n_off),
          number = NA_integer_,
          stringsAsFactors = FALSE
        )
      })
      
      offspring_df <- do.call(rbind, offspring_list)
      
      if (is.null(offspring_df) || nrow(offspring_df) == 0) {
        
        offspring_df <- empty_offspring
        
      } else {
        
        max_number <- if (nrow(current_pop) > 0) max(current_pop$number) else 0
        
        offspring_df$number <- seq_len(nrow(offspring_df)) + max_number
        
        offspring_df$id <- paste(
          offspring_df$cohort,
          offspring_df$number,
          offspring_df$sex,
          sep = "_"
        )
        
        offspring_df <- offspring_df[, names(pop.year0)]
      }
      
    }
    ## 4. Combine current population + offspring
    current_pop <- bind_rows(current_pop, offspring_df)
    
    
    ## 6. Age-structured natural survival
    
    current_pop$survive_nat <- rbinom(
      n = nrow(current_pop),
      size = 1,
      prob = surv_vec[pmin(current_pop$age + 1, length(surv_vec))])
    
    # Take out the olds
    current_pop <- current_pop %>% filter(age <= max.age)
    
    
    
    ## 7. Natural survivors
    alive_after_nat <- current_pop %>% filter(survive_nat == 1)
    
    ## 8. Fishing mortality
    alive_after_nat$caught <- rbinom(nrow(alive_after_nat), 1, p_catch)
    
    harvested_this_year   <- alive_after_nat %>% filter(caught == 1)
    
    caught_list[[yr]] <- harvested_this_year
    
    survivors_to_next_year <- alive_after_nat %>% filter(caught == 0)
    
    
    ## 5. Density dependence applied ONLY to new recruits
    total_before <- nrow(current_pop)
    total_off    <- nrow(offspring_df)
    
    if (total_before + total_off > K && total_off > 0) {
      excess  <- (total_before + total_off) - K
      kill_n  <- min(excess, total_off)
      kill_idx <- sample.int(total_off, size = kill_n, replace = FALSE)
      offspring_df <- offspring_df[-kill_idx, , drop = FALSE]
    }
    
    

    ## 10. STORE POPULATION FOR THIS YEAR
    pop_list[[yr + 1]] <- survivors_to_next_year
    
    ## 11. Update population for next year
    current_pop <- survivors_to_next_year
    current_pop$survive_nat <- NULL
    current_pop$caught <- NULL
    
    ## 12. Extinction check
    if (nrow(current_pop) == 0) {
      message(paste("Population extinct at year", yr))
      break
    }
  }
  # --- RETURN results for this stock ---
  return(list(
    pop_list = pop_list,
    caught_list = caught_list
  ))
  
}














