library(tidyverse)
library(effsize)
source('theme-custom.R')
options(
  pillar.print_min = 35,
  pillar.print_max = 35,
  pillar.width = Inf
)

# Load data
data <- read_csv('../data/full-data.csv') %>%
  select(
    subject,
    enrichment,
    trial_expose,
    session,
    c_TLA_pk,
    c_TLB_pk,
    fe_TLA_sub_pks,
    fe_TLB_sub_pks
  )
names(data)

data$subject <- factor(data$subject)

# Obtain TL pecks 
data$TLA_pk <- data$c_TLA_pk + data$fe_TLA_sub_pks
data$TLB_pk <- data$c_TLB_pk + data$fe_TLB_sub_pks

# TL peck proportions
c <- .0001 #constant added to prevent division by 0 if no pecks
data$pk_prop <- (data$TLA_pk + c) / ((data$TLA_pk + c) + (data$TLB_pk + c))

#Arcsine Transformation
data$pk_asin = asin(sqrt(data$pk_prop))

# Plot
tl_plot <- ggplot(data, aes(x = session, y = pk_prop, colour = subject)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(linewidth = 0.75) +
  geom_line(stat = 'summary', fun = mean,
            linewidth = 1.5, colour = 'black') + 
  geom_point(stat = 'summary', fun = mean,
             size = 4, colour = 'black') +
  facet_grid(enrichment ~ trial_expose) +
  xlab('Session') + ylab('Proportion S+') +
  labs(colour = 'Pigeon') +
  theme_bw() +
  theme(
    axis.text = element_text(size = 18, colour = 'black'),
    axis.title = element_text(size = 20, colour = 'black'),
    strip.text = element_text(size = 18, colour = 'black')
  )

tl_plot

# Test against indifference
data_6_10 <- data %>% 
  filter(session > 5 & session < 11) %>% 
  group_by(subject) %>% 
  summarise(
    prop = mean(pk_prop),
    prop_asin = mean(pk_asin)
    
  )

qqnorm(data_6_10$prop_asin)
qqline(data_6_10$prop_asin)

t.test(data_6_10$prop_asin, mu = asin(sqrt(0.5)))
cohen.d(data_6_10$prop_asin, NA, 
        mu = asin(sqrt(0.5)), 
        hedges.correction = TRUE)

