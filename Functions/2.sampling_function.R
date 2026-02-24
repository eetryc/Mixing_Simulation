########################################
####### Sampling function ##############
########################################






#### 1. Sample Function ####
sampler <- function(pop.year, yr, sampling_window, rep.age, sample_rate) {
  
  if (!(yr %in% sampling_window)) return(NULL)
  
  adults_alive <- pop.year %>% filter(age >= rep.age)
  
  if (nrow(adults_alive) == 0) return(NULL)
  
  n_sample <- floor(sample_rate * nrow(adults_alive))
  
  sampled_fish <- adults_alive %>%
    slice_sample(n = min(n_sample, nrow(adults_alive)), replace = FALSE) %>%
    mutate(year_sampled = yr)
  
  return(sampled_fish)
}




#### 2. Apply to the pop_lists from the simulated objects list ####

# Define sampling rate per yearly population #

sample_rate <- 0.30 # change percent as needed


# Apply function #
samples <- bind_rows(
  lapply(seq_along(p$pop_list), function(i) {
    sampler(
      pop.year = p$pop_list[[i]],
      yr = i - 1,                   
      sampling_window = sampling_window,
      rep.age = rep.age,
      sample_rate = sample_rate
    )
  })
)


