library(tidyverse)
source("theme-custom.R")
options(
  pillar.print_max = 500,
  pillar.width = Inf
)

# Load data
data <- read_csv("../data/full-data.csv")
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

data$session <- ifelse(data$session == 11, 'Session 11', 'Session 22')

# Choice proportions
data$cp <- data$c_IL_sub_pk / (data$c_IL_sub_pk + data$c_IL_opt_pk)

# Plotting values
sum_stats <- data %>%
  group_by(session, enrichment, trial_expose) %>%
  summarise(
    n = length(cp),
    m = mean(cp),
    df = n - 1,
    alpha = 0.05,
    t_crit = abs(qt(alpha / 2, df = df)),
    se = sd(cp) / sqrt(n),
    moe = t_crit * se,
    ci_low = m - moe,
    ci_top = m + moe
  )

# Line Plot
cols <- as.vector(palette.colors(palette = "Okabe-Ito")[3:2])
dodge <- .15
ggplot(data, aes(
  x = trial_expose, y = cp,
  group = enrichment,
  shape = enrichment,
  linetype = enrichment,
  fill = enrichment
)) +
  geom_hline(yintercept = .5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    linewidth = 1,
    position = position_dodge(dodge)
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se, #' mean_cl_boot',
    # fun.args = list(conf.int = 0.95),
    linewidth = 1,
    width = 0.25,
    position = position_dodge(dodge),
    colour = "black",
    linetype = 1
  ) +
  geom_point(
    stat = "summary", fun = mean,
    size = 7,
    stroke = 2,
    position = position_dodge(dodge)
  ) +
  facet_wrap(~ session) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = cols) +
  coord_cartesian(ylim = c(0, 1)) +
  xlab("Trial Type") +
  ylab("Proportion Suboptimal\nChoice") +
  labs(
    fill = "Environment",
    shape = "Environment",
    linetype = "Environment"
  ) +
  theme_custom() +
  theme(
    axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 30),
    axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 30),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.spacing = unit(3, "lines"),
    legend.key.width = unit(2, "cm")
  )


# Save
ggsave("../plots/lineplot-se.png",
  dpi = 400,
  units = "cm", width = 35, height = 16
)

ggsave("../plots/lineplot-se.svg",
  units = "cm", width = 35, height = 16
)

ggsave("../plots/lineplot-se.pdf",
  units = "cm", width = 35, height = 16
)
