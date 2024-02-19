library(tidyverse)
library(lmPerm)
options(
  pillar.print_max = 500,
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
  filter(session == 22)

# Factor data
data$subject <- factor(data$subject)
data$enrichment <- factor(data$enrichment)
data$trial_expose <- factor(data$trial_expose)

# Choice proportions
data$cp <- data$c_IL_sub_pk / (data$c_IL_sub_pk + data$c_IL_opt_pk)

# Two-way ANOVA
set.seed(2024-02-18)
model <- lmp(cp ~ trial_expose + enrichment + trial_expose:enrichment,
  data = data,
  perm = "Prob"
)

sink("../data/ANOVA-2x2.txt")
anova(model)
sink()

summary(model)

