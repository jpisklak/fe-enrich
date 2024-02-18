library(tidyverse)
source('theme-custom.R')
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

# Plotting values
sum_stats <- data %>%
  group_by(enrichment, trial_expose) %>%
  summarise(
    n = length(cp),
    m = mean(cp),
    df = n - 1,
    alpha = 0.05,
    t_crit = abs(qt(alpha/2, df = df)),
    se = sd(cp) / sqrt(n),
    moe = t_crit * se,
    ci_low = m - moe,
    ci_top = m + moe
    )

sum_stats

# Plot w/ classic confidence intervals  
ggplot(sum_stats, aes(x = trial_expose, y = m, fill = enrichment)) +
  geom_bar(
    stat = "identity",
    colour = 'black',
    position = "dodge"
  ) +
  
  geom_errorbar(
    stat = "identity",
    aes(
      ymin = ci_low,
      ymax = ci_top
    ),
    width = 0.25,
    position = position_dodge(.9)
  ) +
  
  geom_point(data = data,
             size = 4,
             aes(x = trial_expose, 
                 y = cp, 
                 shape = enrichment
                 ),
             position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  xlab('Trial Exposure') +
  ylab('Proportion Suboptimal Choice') +
  labs(fill = 'Environment',
       shape = 'Environment') +
  theme_custom()
  

ggsave('../plots/barplot-classic-ci.png',
       dpi = 400,
       units = 'cm', width = 30, height = 24)
  

  
# Plot w/ bootstrapped confidence intervals  
ggplot(data, aes(x = trial_expose, y = cp, fill = enrichment)) +
  geom_bar(
    stat = "summary",
    fun = mean,
    colour = 'black',
    position = "dodge"
  ) +

  geom_errorbar(
    stat = "summary",
    fun.data = 'mean_cl_boot',
    fun.args = list(conf.int = 0.95),
    width = 0.25,
    position = position_dodge(.9)
  ) +
  
  geom_point(data = data,
             size = 4,
             aes(x = trial_expose, 
                 y = cp, 
                 shape = enrichment
             ),
             position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  xlab('Trial Exposure') +
  ylab('Proportion Suboptimal Choice') +
  labs(fill = 'Environment',
       shape = 'Environment') +
  theme_custom()


ggsave('../plots/barplot-boot-ci.png',
       dpi = 400,
       units = 'cm', width = 30, height = 24)

ggsave('../plots/barplot-boot-ci.svg',
       dpi = 400,
       units = 'cm', width = 30, height = 24)  

ggsave('../plots/barplot-boot-ci.pdf',
       dpi = 400,
       units = 'cm', width = 30, height = 24)  




























