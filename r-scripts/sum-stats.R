library(tidyverse)
options(
  pillar.print_min = 35,
  pillar.print_max = 35,
  pillar.width = Inf
)

# Load data
data <- read_csv('../data/full-data.csv')
names(data)

data <- data %>%
  select(
    subject,
    enrichment,
    trial_expose,
    session,
    c_IL_sub_pk,
    c_IL_opt_pk
  ) %>%
  filter(session == 11 | session == 22)

# Choice proportions
data$cp <- data$c_IL_sub_pk / (data$c_IL_sub_pk + data$c_IL_opt_pk)

# Arcsine Transformation
data$cp_asin = asin(sqrt(data$cp))

# Collapse across session
data <- data %>%
  group_by(subject, enrichment, trial_expose) %>%
  summarise(
    cp = mean(cp),
    cp_asin = mean(cp_asin)
  )

# Summary stats
data %>% 
  group_by(trial_expose, enrichment) %>% 
  summarise(
    n = length(cp),
    
    # Vanilla
    mean = mean(cp),
    sd = sd(cp),
    
    # Transformed
    mean_asin = mean(cp_asin),
    sd_asin = sd(cp_asin),
    
    # Untransformed
    mean_ut = sin(mean_asin)^2,
    sd_ut = sin(sd_asin)^2
)

data %>% 
  group_by(enrichment) %>% 
  summarise(
    n = length(cp),
    
    # Vanilla
    mean = mean(cp),
    sd = sd(cp),
    
    # Transformed
    mean_asin = mean(cp_asin),
    sd_asin = sd(cp_asin),
    
    # Untransformed
    mean_ut = sin(mean_asin)^2,
    sd_ut = sin(sd_asin)^2
  )

data %>% 
  group_by(trial_expose) %>% 
  summarise(
    n = length(cp),
    
    # Vanilla
    mean = mean(cp),
    sd = sd(cp),
    
    # Transformed
    mean_asin = mean(cp_asin),
    sd_asin = sd(cp_asin),
    
    # Untransformed
    mean_ut = sin(mean_asin)^2,
    sd_ut = sin(sd_asin)^2
  )


