###########################
######## No Stocks ########
###########################





# 1. sim

sim <- stock.sim(1000)


# 2. Sample

samples <- bind_rows(
  lapply(seq_along(sim$pop_list), function(i) {
    sampler(
      pop.year = sim$pop_list[[i]],
      yr = i - 1,                   
      sampling_window = sampling_window,
      rep.age = rep.age,
      sample_rate = sample_rate
    )
  })
)


# 3. CKMR

ckmr <- CKMR_est(samples)



# 4. Graph 
# Replace p with the simulation object name

#### 1. Graph the true population ####
#### Inspect the files
sim$pop_list 

samples 

# check for presence of pops
sample_ids <- samples$id

samples <- samples %>%
  mutate(
    mother_sampled = mother %in% sample_ids,
    father_sampled = father %in% sample_ids
  )


samples

names(sim$pop_list) <- 0:(length(sim$pop_list)-1) 

years_to_estimate <- sort(unique(samples$cohort))


## Population size per year
true_abundance <- lapply(sim$pop_list, function(df){
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











#### 2. Graph the CKMR estimates
ggplot(ckmr, aes(x = Year, y = mean)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(
    x = "Year (Cohort index)",
    y = expression(hat(N)[adult])) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, fill = "lightblue") +
  theme_bw() +
  # coord_cartesian(xlim = c(20, 40), ylim = c(0,1000)) +
  geom_point(data = true_abundance, aes(x = year_index, y = value, group = 1),color = "red") +
  geom_line(data = true_abundance, aes(x = year_index, y = value, group = 1),color = "red") 
