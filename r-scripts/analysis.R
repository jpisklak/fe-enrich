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

# Factor data
data$subject <- factor(data$subject)
data$enrichment <- factor(data$enrichment)
data$trial_expose <- factor(data$trial_expose)

# Choice proportions
data$cp <- data$c_IL_sub_pk / (data$c_IL_sub_pk + data$c_IL_opt_pk)

# Collapse sessions
data <- data %>% 
  group_by(subject, enrichment, trial_expose) %>% 
  summarise(cp = mean(cp))

# Contrasts
levels(data$enrichment)
Enrich_v_Iso <- c(1, -1)
levels(data$trial_expose)
FE_v_Choice <- c(-1, 1)

contrasts(data$enrichment) = Enrich_v_Iso
contrasts(data$trial_expose) = FE_v_Choice

# Classic ANOVA
model <- lm(cp ~ enrichment + trial_expose + enrichment:trial_expose,
  data = data
)
#plot(model)

aov_summary <- anova(model)
aov_summary

# Omega squared
  # Enrichment
w_enrich <- (aov_summary$`Sum Sq`[1] - (2 - 1) * aov_summary$`Mean Sq`[4]) / 
  (sum(aov_summary$`Sum Sq`) + aov_summary$`Mean Sq`[4])

  # Training
w_train <- (aov_summary$`Sum Sq`[2] - (2 - 1) * aov_summary$`Mean Sq`[4]) / 
  (sum(aov_summary$`Sum Sq`) + aov_summary$`Mean Sq`[4])

  # Interaction
w_int <- (aov_summary$`Sum Sq`[3] - (2 - 1) * aov_summary$`Mean Sq`[4]) / 
  (sum(aov_summary$`Sum Sq`) + aov_summary$`Mean Sq`[4])

round(c(w_enrich, w_train, w_int), 2)

