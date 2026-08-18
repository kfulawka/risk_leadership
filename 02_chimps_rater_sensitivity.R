rm(list = ls())

library(data.table)
library(ggplot2)
library(patchwork)

# helper: relative rank within chimp group --------------------------------

rr = function(x) {
  1 - (x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# data --------------------------------------------------------------------

dd = fread('data/chimps.txt')
dd = data.frame(dd)

# exclude Ngamba because hierarchy rank was consensus-based
dd = subset(dd, chimp_group %in% c("old_chimps", "young_chimps"))

# raw ranks

# risk variables
risk_dv = c("general_risk", "food_risk", "snake_risk", 
            "escape_risk", "hierarchy_risk", "strangers_risk")

# rankings
dr = fread('data/hierarchy_Kenya.csv')
dr = data.frame(dr)

# standardize identifiers -------------------------------------------------

dd$subject = tolower(dd$subject)
dr$subject = tolower(dr$subject)

dd$caregiver_no = tolower(dd$caregiver_no)
dr$caregiver_no = tolower(dr$caregiver_no)

rank_caregivers = unique(dr$caregiver_no)

# aggregate hierarchy rank across the three rank caregivers ---------------

rank_agg = aggregate(
  subject_rank ~ chimp_group + subject,
  data = dr,
  FUN = mean,
  na.rm = TRUE
)

rank_agg$relative_rank = ave(
  rank_agg$subject_rank,
  rank_agg$chimp_group,
  FUN = rr
)

# helper: aggregate composite risk across a selected set of caregivers -----

aggregate_risk = function(x) {
  
  x$comp_risk = rowMeans(x[, risk_dv], na.rm = TRUE)
  
  aggregate(
    comp_risk ~ chimp_group + subject,
    data = x,
    FUN = mean,
    na.rm = TRUE
  )
}

# risk based on all 10 caregivers
risk_all = aggregate_risk(dd)

# risk based only on the seven caregivers who did not rate hierarchy
risk_independent = aggregate_risk(
  dd[!dd$caregiver_no %in% rank_caregivers, ]
)

# merge rank and risk -----------------------------------------------------

da_all = merge(
  rank_agg[, c("chimp_group", "subject", "relative_rank")],
  risk_all,
  by = c("chimp_group", "subject")
)

da_independent = merge(
  rank_agg[, c("chimp_group", "subject", "relative_rank")],
  risk_independent,
  by = c("chimp_group", "subject")
)

# correlations ------------------------------------------------------------

cor_all = cor.test(
  da_all$relative_rank,
  da_all$comp_risk,
  method = "spearman",
  exact = FALSE
)

cor_independent = cor.test(
  da_independent$relative_rank,
  da_independent$comp_risk,
  method = "spearman",
  exact = FALSE
)

cor_all
cor_independent

cor_summary = data.frame(
  Risk_raters = c(
    "All caregivers",
    "Non-overlapping caregivers"
  ),
  N = c(nrow(da_all), nrow(da_independent)),
  Spearman_rho = c(
    unname(cor_all$estimate),
    unname(cor_independent$estimate)
  ),
  p_value = c(cor_all$p.value, cor_independent$p.value)
)

print(cor_summary)

# combine plotting data ---------------------------------------------------

da_all$rating_set = "All rank and risk raters"
da_independent$rating_set = "Non-overlapping rank and risk raters"

plot_dat = rbind(da_all, da_independent)

rho_labs = aggregate(
  cbind(relative_rank, comp_risk) ~ rating_set,
  data = plot_dat,
  FUN = length
)

rho_labs$label = c(
  paste0("Spearman's rho = ",
         round(unname(cor_all$estimate), 2)),
  paste0("Spearman's rho = ",
         round(unname(cor_independent$estimate), 2))
)

rho_labs$x = -Inf
rho_labs$y = Inf

# figure ------------------------------------------------------------------

risk_rank_sensitivity_plt = ggplot(
  plot_dat,
  aes(x = relative_rank,
      y = comp_risk,
      # shape = chimp_group
      )
) +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    aes(group = 1)
  ) +
  geom_text(
    data = rho_labs,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = -.1,
    vjust = 1.3
  ) +
  facet_grid(cols = vars(rating_set), 
             axes = 'all',
             axis.labels = 'all') +
  scale_x_continuous(
    "Hierarchy rank",
    limits = c(0, 1),
    breaks = seq(0, 1, .2),
    labels = paste0(seq(0, 1, .2)*100, '%')
  ) +
  ylab("Risk preference") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12, hjust = 0)
  )

risk_rank_sensitivity_plt

ggsave('results/Fig05.jpeg',
       plot = risk_rank_sensitivity_plt,
       device = 'jpeg',
       dpi = 700,
       units = 'cm',
       width = 14,
       height = 6,
       scale = 1.25)