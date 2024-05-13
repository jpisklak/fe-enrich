library(tidyverse)
library(effsize)
source('theme-custom.R')
options(
  pillar.print_max = 500,
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

# Plot
ggplot(data, aes(x = session, y = pk_prop, colour = subject)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(aes(group = subject), linewidth = 0.75) +
  geom_line(stat = 'summary', fun = mean,
            linewidth = 2, colour = 'black') + 
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


# Save
ggsave("../plots/TL_pks.png",
       dpi = 400,
       units = "cm", width = 35, height = 16
)

ggsave("../plots/TL_pks.svg",
       units = "cm", width = 35, height = 16
)

ggsave("../plots/TL_pks.pdf",
       units = "cm", width = 35, height = 16
)


# Test against indifference
data <- data %>% 
  filter(session > 5 & session < 11) %>% 
  group_by(subject) %>% 
  summarise(
    pk_prop = mean(pk_prop)
  )


t.test(data$pk_prop, mu = 0.5)
cohen.d(data$pk_prop, NA, mu = 0.5, hedges.correction = TRUE)






