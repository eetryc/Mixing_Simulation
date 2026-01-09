###############################
###  Initial simulation LWF ###
###############################


#### Set up functions

## Simulate the stock individuals.
# This will be just the single stock

simulate.stock <- function(init.pop.size, init.prop.female, Nages, mating.periodicity, repro.age, YOY.survival, juvenile.survival, Adult.survival, max.age, num.mates, ff, burn.in, Num.years){
  
  ####---------Set up initial population-----####
  init.pop <- data.frame() # create a blank data frame which will become the initial population
  
  ## vectorizing instead of looping to generate all the individuals at once
  age.x <- rep(1:length(Nages), times = Nages)
  sex <- sample(c('F','M'), size = init.pop.size, prob = c(init.prop.female, 1-init.prop.female), replace = TRUE)
  repro.cycle <- sample(1:mating.periodicity, size = init.pop.size, replace = TRUE)
  indv.name <- replicate(init.pop.size, paste(sample(letters, 20, replace = TRUE), collapse=""))
  mother.x <- rep("xxxxx", init.pop.size)
  father.x <- rep("xxxxx", init.pop.size)
  birth.year <- rep(-1, init.pop.size)

  init.pop <- data.frame(indv.name, birth.year, age.x, mother.x, father.x, sex, repro.cycle)
  
  head(init.pop)
  names(init.pop)[3] <- "age.x"
  summary(as.factor(init.pop$age.x))
  summary(as.factor(init.pop$sex))
  
  ####----------Breeding----------####
  repro.cycle.vec <- rep(1:mating.periodicity, times = 100) # Generate a vector which will be used to determine if it is an even or odd breeding year (or a 1/3 breeding year)
  
  ####---------for year 0 breeding-------####
  
  #------------------------------------Mothers-------------------------------------------#
  mothers <- which(init.pop$sex=='F' & init.pop$age.x>=repro.age)
  
  #-----------------------------------Fathers--------------------------------------------#
  fathers <- which(init.pop$sex=='M' & init.pop$age.x>=repro.age)
  
  YOY.df <- data.frame()
  
  for(j in mothers) {
    num.mates.x <- sample(num.mates, 1)  # number of mates for this female
    fathers.sample <- sample(fathers, num.mates.x, replace = TRUE)  # promiscuous males
    
    for(f in fathers.sample) {
      # determine number of offspring
      xpup <- ceiling(ff) - ff
      num.offspring <- ifelse(runif(1) < xpup, trunc(ff), ceiling(ff))
      if(num.offspring == 0) next
      
      # create offspring
      baby.names <- replicate(num.offspring, paste(sample(letters, 20, replace=TRUE), collapse=""))
      sex <- sample(c('F','M'), num.offspring, replace=TRUE, prob=birth.sex.ratio)
      
      inter.df <- data.frame(
        indv.name = baby.names,
        birth.year = 0,
        age.x = 0,
        mother.x = init.pop[j, "indv.name"],
        father.x = init.pop[f, "indv.name"],
        sex = sex,
        repro.cycle = sample(1:mating.periodicity, num.offspring, replace=TRUE)
      )
      
      YOY.df <- rbind(YOY.df, inter.df)
    }
  }
  
  year.end.pop.0 <- NULL
  
  year.end.pop.0 <- rbind(init.pop, YOY.df) #Combine the YOY data with the other individuals present this year
  
  #Assign a column with whether or not an individual this year will survive to next year. The survival is randomly drawn based on the life stage and the probabilities defined in the parameter section
  year.end.pop.0$Survival <- ifelse(year.end.pop.0$age.x==0, 
                                    sample(c("S","M"), 
                                           size=length(which(year.end.pop.0$age.x==0)), 
                                           prob=c(YOY.survival, 1-YOY.survival), 
                                           replace=T),
                                    ifelse(year.end.pop.0$age.x<repro.age, 
                                           sample(c("S","M"), 
                                                  size=length(which(year.end.pop.0$age.x<repro.age)), 
                                                  prob=c(juvenile.survival, 1-juvenile.survival),
                                                  replace=T),
                                           ifelse(year.end.pop.0$age.x<max.age, 
                                                  sample(c("S","M"), 
                                                         size=length(which(year.end.pop.0$age.x<max.age)), 
                                                         prob=c(Adult.survival, 1-Adult.survival),
                                                         replace=T), "M")))
  
  
  loopy.pop <- year.end.pop.0
  
  # At end of year 0 ...
  print(paste("year 0", "N_mothers=", length(mothers), "N_pups=", nrow(YOY.df), "N_deaths=", sum(loopy.pop$Survival=="M"), "N= ", nrow(loopy.pop[loopy.pop$Survival=="S",]) , sep=" ")) # print the simulation year and the population size in the R console so they can be observed as the simulation is running.
  
  
  ####-------Loop for all other breeding years--------####
  pop.size <- data.frame()
  
  loopy.list <- list() # Make list to store dataframe of population for each year, where each element corresponds to the year e.g. loopy.list[[1]] is the population from the first year
  parents.tibble <- tibble()
  moms.temp = dads.temp <- NULL
  
  for(v in 1:(burn.in + Num.years)){ #loop through all of the years in the simulation - the burn in and the years that matter
    
    #If we are substantially increasing mortality, then we only want to implement this over the last 10 years, otherwise the population will go extinct. Values for Sa and Sj are defined in the main simulation script.
    if(population.growth == "lambda.extreme" & v >= n_yrs-10){
      Adult.survival <- Sa
      juvenile.survival <- Sj
    }
    
    data1 <- loopy.pop[loopy.pop$Survival =="S", -9] #Bring in the data from the previous iteration, but only include those that survive (and leave the column of survival out)
    data1$age.x <- data1$age.x+1 # increase each individuals age by one for the new year - happy birthday survivors!
    
    
    
    #------------------------------------Mothers------------------------------------#
    mothers <- which(data1$sex == "F" & data1$age.x >= repro.age)
    
    #-----------------------------------Fathers--------------------------------------------#
    fathers <- which(data1$sex=='M' & data1$age.x>=repro.age)  #determine which males are available to breed in this year
    
    ####---------------------------------Breeding------------------------####
    YOY.df <- data.frame()
    
    for(j in mothers) {
      num.mates.x <- sample(num.mates, 1)  # number of mates for this female
      fathers.sample <- sample(fathers, num.mates.x, replace = TRUE)  # promiscuous males
      
      for(f in fathers.sample) {
        # determine number of offspring
        xpup <- ceiling(ff) - ff
        num.offspring <- ifelse(runif(1) < xpup, trunc(ff), ceiling(ff))
        if(num.offspring == 0) next
        
        # create offspring
        baby.names <- replicate(num.offspring, paste(sample(letters, 20, replace=TRUE), collapse=""))
        sex <- sample(c('F','M'), num.offspring, replace=TRUE, prob=birth.sex.ratio)
        
        inter.df <- data.frame(
          indv.name = baby.names,
          birth.year = v,
          age.x = 0,
          mother.x = data1[j, "indv.name"],
          father.x = data1[f, "indv.name"],
          sex = sex)
        
        YOY.df <- rbind(YOY.df, inter.df)
      }
    } # end loop over mothers
    
    loopy.pop <- rbind(data1, YOY.df) #Combine the YOY data with the other individuals present this year. YOY go at the bottom.
    
    #Assign a column with whether or not an individual this year will survive to next year. The survival is randomly drawn based on the life stage and the probabilities defined in the parameter section
    loopy.pop$Survival <- ifelse(loopy.pop$age.x==0, 
                                 sample(c("S","M"), 
                                        size=length(which(loopy.pop$age.x==0)), 
                                        prob=c(YOY.survival, 1-YOY.survival), 
                                        replace=T),
                                 ifelse(loopy.pop$age.x<repro.age, 
                                        sample(c("S","M"), 
                                               size=length(which(loopy.pop$age.x<repro.age)), 
                                               prob=c(juvenile.survival, 1-juvenile.survival),
                                               replace=T),
                                        ifelse(loopy.pop$age.x<max.age, 
                                               sample(c("S","M"), 
                                                      size=length(which(loopy.pop$age.x<max.age)), 
                                                      prob=c(Adult.survival, 1-Adult.survival),
                                                      replace=T), "M")))
    
    loopy.list[[v]] <- loopy.pop # Save the current year's population data as a list element, where the index corresponds to the year
    
    #Save number of offspring for each individual parent for each year
    #Mothers
    moms.temp <- YOY.df %>% group_by(mother.x) %>% 
      summarize(num.off = n()) %>% 
      rename(parent = mother.x) %>% 
      mutate(year = v, parent.sex = "mother")
    
    #Fathers
    dads.temp <- YOY.df %>% group_by(father.x) %>% 
      summarize(num.off = n()) %>% 
      rename(parent = father.x) %>% 
      mutate(year = v, parent.sex = "father")
    
    #Combine mother and father info
    parents.tibble <- rbind(parents.tibble, moms.temp, dads.temp)
    
    #Print info to screen for each year of the simulation
    cat(paste("\nyear", v, 
              "\nN_potential_mothers=", length(mothers), 
              "N_actual_mothers=", nrow(moms.temp), 
              "Percent female breeders=", round(nrow(moms.temp)/length(mothers)*100,0),
              "\nN_potential_fathers=", length(fathers), 
              "N_actual_fathers=", nrow(dads.temp), 
              "Percent male breeders=", round(nrow(dads.temp)/length(fathers)*100, 0),
              "\nN_pups=", nrow(YOY.df), 
              "\nN_deaths=", sum(loopy.pop$Survival=="M"), 
              "\nTotal N= ", nrow(loopy.pop[loopy.pop$Survival=="S",]) , sep=" "))
    
    #Save the population size and number of male and females breeders for each year
    pop.size.vec <- cbind.data.frame(year=v, 
                                     population_size=nrow(data1), # 
                                     Male.adult.pop = nrow(data1[data1$sex == "M" & data1$age.x >= repro.age,]), # 
                                     Female.adult.pop = nrow(data1[data1$sex == "F" & data1$age.x >= repro.age,]), #difference between this and number of mothers only if reproductive cycle != 1
                                     Num.mothers = nrow(moms.temp), #The actual number that reproduced this year
                                     Num.fathers = nrow(dads.temp)) #The actual number that reproduced this year
    pop.size <- rbind(pop.size, pop.size.vec)
    
  } # end loop over sim years
  
  #Label the list elements with the year
  names(loopy.list) <- paste0("year.end.pop.", seq(1:(burn.in + Num.years)), "_iteration_", iter)
  
  return(invisible(list(loopy.list, pop.size, parents.tibble)))
}











#### Initialize and run function

#Load packages
library(dplyr)
library(tidyr)
library(stringr) # safe to ignore conflicts with filter() and lag()
library(MASS)
library(popbio)
#library(mpmtools)  # not at CRAN;
# install.packages("devtools")
#devtools::install_github("BruceKendall/mpmtools")
library(ggpubr)
# library(rjags)
# library(R2jags)
# library(jagsUI)
library(Rlab)
library(runjags)
library(postpack)
library(coda)




#################### Set output file locations and labels ####################
temp_location <- ""
output.location <- ""
parents_prefix <- "parents.breakdown"
sample_prefix <- "sample.info"
pop.size.prefix <- "pop.size"
truth.prefix <- "truth"

#################### Simulation parameters ####################
init.adult.pop.size <- 1000 # CHANGED FROM 3000; Initial adult population size
init.prop.female <- 0.5 # proportion of the initial population size that is female
birth.sex.ratio <- c(0.5,0.5) # The probability that each baby is F:M - has to add up to 1
YOY.survival <- 0.7 # CHANGED FROM 0.8; young of year survival
juvenile.survival <- 0.8 # CHANGED FROM 0.9; juvenile survival
Adult.survival <- 0.825 # CHANGED FROM 0.825; Adult survival
repro.age <- 12 # set age of reproductive maturity
max.age <- maxAge <- 50 #set the maximum age allowed in the simulation
num.mates <- c(1:3) #1  #c(1:3) #vector of potential number of mates per mating
f <- (1-Adult.survival)/(YOY.survival * juvenile.survival^11) # adult fecundity at equilibrium if no age truncation

#################### Toggles for  population simulation options ####################
b.schedule <- "annual" #annual or biennial or triennial
input.psi <- 1 #Can use any number here; 1 if wanting every individual to have the same schedule
offcycle.breeding <- "no" #Options are "yes" or "no"
input.popGrowth <- "stable" #Options are "stable", "slight increase", "slight decline", or "severe decline"
#sampling.scheme <- "target.YOY"
#sampling.scheme <- "sample.all.juvenile.ages"
#sampling.scheme <- "sample.ALL.ages"



#################### Breeding schedule ######################
if(b.schedule == "annual"){
  #------------------------------ Annual ------------------------------
  breeding.schedule <- "annual.breeding"
  mating.periodicity <- 1 #number of years between mating; assigned to an individual and sticks with them through their life. So they're either a one or two year breeder.
  non.conformists <- 0
  ff <- f/init.prop.female * mating.periodicity/mean(num.mates) # female fecundity per breeding cycle
  ff
  print("breeding schedule is annual")
  
} else if(b.schedule == "biennial"){
  
  #------------------------------ Biennial ------------------------------
  mating.periodicity <- 2 #number of years between mating; assigned to an individual and sticks with them through their life. So they're either a one or two year breeder.
  print("breeding schedule is biennial")
  
  if(input.psi == 1){
    #============================== psi 1 ==============================
    breeding.schedule <- "biennial.breeding_psi1"
    non.conformists <- 0
    
    print("psi equals 1")
    
    if(offcycle.breeding == "yes"){
      
      breeding.schedule <- "biennial.breeding_psi1_offcycle"
      
      percent.breed_off.cycle <- 0.1 #Percent of off-cycle mothers that will breed each year
      percent.skip_on.cycle <- 0.1 #Percent of on-cycle mothers that will skip breeding each year
      
      print(paste0("There are ", percent.breed_off.cycle*100, "% of females that will breed off-cycle and ", percent.skip_on.cycle*100, "% of females that will fail when on-cycle."))
      
    }
  } else if(input.psi != 1){
    
    breeding.schedule <- paste0("biennial.breeding_psi", input.psi)
    non.conformists <- 1 - input.psi #proportion of off-year breeders to randomly include off their breeding cycle
    
    print(paste0("psi equals ", 1 - input.psi))
  }
  
} else if(b.schedule == "triennial"){
  
  mating.periodicity <- 3 #number of years between mating; assigned to an individual and sticks with them through their life.
  breeding.schedule <- "triennial.breeding_psi1"
  non.conformists <- 0
  
  print("breeding schedule is triennial")
}

if(b.schedule == "biennial" | b.schedule == "triennial"){
  # Adjust fecundity ==============================================================
  ## for effective number of breeders each year, mating cycle, number of mates ====
  #(from liz) ====
  psi <- 1-non.conformists
  ff <- mating.periodicity/(mating.periodicity-psi*mating.periodicity+psi)*f/init.prop.female/mean(num.mates)
  ff
}

#################### Population growth ####################
if(input.popGrowth == "stable"){
  #------------------------------Stable------------------------------
  population.growth <- "lambda.1"
  
  print("population growth is stable")
  
} else if(input.popGrowth == "slight increase"){
  
  #------------------------------Slight increase------------------------------
  population.growth <- "lambda.slight.increase"
  # ff.shift <- ff+0.5 #Increase fecundity to slightly increase population growth - only works for annual breeding
  
  (Ma <- -log(Adult.survival)) #Mortality of adults
  (Mj <- -log(juvenile.survival)) #Mortality of juveniles
  (Sa <- exp(-Ma)) #Survival of adults
  (Sj <- exp(-Mj)) #Survival of juveniles
  
  Mx <- -0.01 #Extra mortality (make negative so it reduces mortality)
  (Sa <- exp(-Ma - Mx)) #Survival of adults
  (Sj <- exp(-Mj - Mx)) #Survival of juveniles
  
  Adult.survival <- Sa
  juvenile.survival <- Sj
  
  cat(paste0("Adult survival is ", round(Adult.survival, 0), ";\nJuvenile survival is ", round(juvenile.survival,0), ";\nPopulation will increase in size"))
  
} else if(input.popGrowth == "slight decline"){
  
  #------------------------------Slight decline------------------------------
  population.growth <- "lambda.slight.decrease"
  # ff.shift <- ff-0.5 #Decrease fecundity to slightly decrease population growth - only works for annual breeding
  (Ma <- -log(Adult.survival)) #Mortality of adults
  (Mj <- -log(juvenile.survival)) #Mortality of juveniles
  (Sa <- exp(-Ma)) #Survival of adults
  (Sj <- exp(-Mj)) #Survival of juveniles
  
  Mx <- 0.01 #Extra mortality
  (Sa <- exp(-Ma - Mx)) #Survival of adults
  (Sj <- exp(-Mj - Mx)) #Survival of juveniles
  
  Adult.survival <- Sa
  juvenile.survival <- Sj
  
  cat(paste0("Adult survival is ", round(Adult.survival, 0), ";\nJuvenile survival is ", round(juvenile.survival,0),";\nPopulation will decrease in size"))
  
} else if(input.popGrowth == "severe decline"){
  
  #------------------------------Substantial decrease------------------------------
  population.growth <- "lambda.extreme" #Mortality will change later inside the loop
  
  cat(paste0("Adult survival is ", round(Adult.survival,0), ";\nJuvenile survival is ", round(juvenile.survival,0),";\nPopulation will decline rapidly"))
}


#-------------------Set date and load seeds----------------------------
today <- format(Sys.Date(), "%d%b%Y") # Store date for use in file name
date.of.simulation <- today

rseeds <- readRDS("rseeds_2022.04.15.rda")
seeds <- "Seeds2022.04.15"
# create some seeds: 
#rseeds <- trunc(1529375630*runif(1000, min=0, max=1 ) )
#rseeds[1]  <- 746160703

#----------------------- DATA-GENERATING MODEL --------------------
# Note on sequencing: Birthdays happen at beginning of each year, followed by mating, then death (i.e. a female who dies in year 10 can still give birth and have surviving pups born in year 10)

#Stable age distribution
props <- rep(NA, max.age+1)
props[1] <- f
props[2] <- f * YOY.survival
for (y in 3:(repro.age+1)) props[y] <- props[y-1] * juvenile.survival #+1 because of age 0 individuals

for (y in (repro.age+2):(max.age+1)) props[y] <- props[y-1] * Adult.survival #+2 because of age 0 individuals
prop.Adult <- sum(props[(repro.age+1):(max.age+1)])/sum(props)
Nages <- round(props[-1] * init.adult.pop.size) 
init.pop.size <- sum(Nages) # all ages except YOYs

#Set length of simulation and estimation year
burn.in <- 40 # number of years to use as simulation burn in period
Num.years <- 50 # The number of years to run in the simulation beyond the burn in
n_yrs <- burn.in + Num.years #Total number of simulation years
estimation.year <- n_yrs - 5 # Set year of estimation for truth calculations


#--------------------- Sampling parameters ---------------------
sample.years <- c(n_yrs - c(3:0)) #For four years of sampling
sample.scheme.vec <- c("target.YOY", "sample.all.juvenile.ages", "sample.ALL.ages")
sample.vec.prop <- c(.5, 1, 1.5, 2)


####-------------- Prep simulation ----------------------
# Moved sampling below so extract different sample sizes from same population
iterations <- 500  # 1 just to look at output     500 #Number of iterations to loop over


# Initialize arrays for saving results
results <- NULL
sims.list.1 <- NULL
sims.list.2 <- NULL
sims.list.3 <- NULL
sims.list.4 <- NULL
sample.info <- NULL
parents.tibble_all <- NULL
pop.size.tibble_all <- NULL
mom.comps.tibble <- NULL
dad.comps.tibble <- NULL
truth.all <- NULL

sim.samples.1 <- paste0(sample.vec.prop[1], "prop.sampled")
sim.samples.2 <- paste0(sample.vec.prop[2], "prop.sampled")
sim.samples.3 <- paste0(sample.vec.prop[3], "prop.sampled")
sim.samples.4 <- paste0(sample.vec.prop[4], "prop.sampled")

#
#---------------------Initialize array from previous checkpoint--------------------------
#Results
#  results <- read_csv(paste0(results_location, results_prefix, "_", date.of.simulation, "_", seeds, "_", purpose, "_iter_", iter, ".csv"))
# # 
# # #Model output for diagnostics
#  sims.list.1 <- readRDS(file = paste0(temp_location, MCMC_prefix, "_", date.of.simulation, "_", seeds, "_", sim.samples.1, "_", MCMC.settings, "_", purpose))
# # 
#  sims.list.2 <- readRDS(file = paste0(temp_location, MCMC_prefix, "_", date.of.simulation, "_", seeds, "_", sim.samples.2, "_", MCMC.settings, "_", purpose))
# # 
#  sims.list.3 <- readRDS(file = paste0(temp_location, MCMC_prefix, "_", date.of.simulation, "_", seeds, "_", sim.samples.3, "_", MCMC.settings, "_", purpose))
# 
# # Detailed info on samples and parents to examine in more detail
#  sample.info <- readRDS(file = paste0(temp_location, sample_prefix, "_", date.of.simulation, "_", seeds, "_", purpose))

####-------------- Start simulation loop ----------------------
for(iter in 1:iterations) {
  
  if(population.growth == "lambda.extreme"){
    juvenile.survival <- 0.8 # Reset at the beginning of each simulation
    Adult.survival <- 0.825 # Reset at the beginning of each simulation
    
    (Ma <- -log(Adult.survival)) #Mortality of adults
    (Mj <- -log(juvenile.survival)) #Mortality of juveniles
    (Sa <- exp(-Ma)) #Survival of adults
    (Sj <- exp(-Mj)) #Survival of juveniles
    
    set.seed(rseeds[iter])
    Mx <- runif(1, min = 0.05, max = 0.1) #Extra mortality
    (Sa <- exp(-Ma - Mx)) #Survival of adults
    (Sj <- exp(-Mj - Mx)) #Survival of juveniles
    
    Adult.survival <- Sa
    juvenile.survival <- Sj
  }
  
  sim.start <- Sys.time()
  rseed <- rseeds[iter]
  set.seed(rseed)
  
  
  #Run individual based simulation
  out <- simulate.pop(init.pop.size = init.pop.size, 
                      init.prop.female = init.prop.female,
                      Nages = Nages,
                      mating.periodicity = mating.periodicity,
                      repro.age = repro.age,
                      YOY.survival = YOY.survival,
                      juvenile.survival = juvenile.survival,
                      Adult.survival = Adult.survival,
                      max.age = max.age,
                      num.mates = num.mates,
                      ff = ff,
                      burn.in = burn.in,
                      Num.years = Num.years)
