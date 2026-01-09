#####################################
##### Single Stock simple sim #######
#####################################

######## Generate population at year 0 ########

## Define parameters
init.pop.size <- 100 

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

mean_offspring <- 3 # mean number of offspring per female

survival_prob <- 0.7 # annual survival probability

years <- 10 # number of years to simulate




######## Initialize storage ########
# Clear if rerunning first 
rm(pop_list, all_fish, current_pop)

pop_list <- list(pop.year0) # separate table for each year

all_fish <- pop.year0 # running table of all fish

current_pop <- pop.year0 # current population




######## LOOP OVER YEARS ########
for (yr in 1:years) {
  
  # Age everyone
  current_pop <- current_pop %>% mutate(age = age + 1)
  
  # Apply survival
  current_pop$survive <- rbinom(nrow(current_pop), 1, survival_prob)
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
