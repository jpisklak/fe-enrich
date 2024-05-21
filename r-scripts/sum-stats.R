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

# Collapse across session
data <- data %>% 
  group_by(subject, enrichment, trial_expose) %>% 
  summarise(cp = mean(cp))

# Summary stats
data %>% 
  group_by(trial_expose, enrichment) %>% 
  summarise(
    n = length(cp),
    mean = mean(cp),
    sd = sd(cp)
  )

data %>% 
  group_by(enrichment) %>% 
  summarise(
    n = length(cp),
    mean = mean(cp),
    sd = sd(cp)
  )

data %>% 
  group_by(trial_expose) %>% 
  summarise(
    n = length(cp),
    mean = mean(cp),
    sd = sd(cp)
  )











