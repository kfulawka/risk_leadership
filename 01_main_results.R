rm(list = ls())

#
library(ggplot2)
library(patchwork)
library(brms)
library(viridis)
library(rockchalk)

source('99_ce_dat_plt.R')
source('99_extract_coefs.R')

# data --------------------------------------------------------------------

# chimp data
d_ch = readRDS('data/dm_chimps.rds')

# human data
d_hu = readRDS('data/dm_hu.rds')

# colors ------------------------------------------------------------------

# cols = list(h = viridis(2)[1], ch = viridis(2)[2])
cols = mako(2, .8, .4, .6)
names(cols) = c('ch', 'h')
sex_cols = viridis(2, .8, .7, .9)
gender_cols = viridis(2, .8, 0, .3)

# descriptive stats -------------------------------------------------------

ct1 = pctable(leadership_b ~ age_b, 
              d_hu, rowpct = TRUE, colpct = TRUE)

ct2 = pctable(leadership_b ~ gender, 
              d_hu, rowpct = TRUE, colpct = TRUE)

ct1b = pctable(income_b ~ age_b, 
              d_hu, rowpct = TRUE, colpct = TRUE)

ct2b = pctable(income_b ~ gender, 
              d_hu, rowpct = TRUE, colpct = TRUE)

ct3 = pctable(rel_rank_b ~ age_b, 
              d_ch, rowpct = TRUE, colpct = TRUE)

ct4 = pctable(rel_rank_b ~ subject_sex, 
              d_ch, rowpct = TRUE, colpct = TRUE)

#
summary(d_hu$risk_preference); sd(d_hu$risk_preference, na.rm = T)
aggregate(risk_preference ~ leadership_b, 
          d_hu, 
          FUN = function(x) c(m = mean(x), sd = sd(x)))
aggregate(risk_preference ~ income_b, 
          d_hu, 
          FUN = function(x) c(m = mean(x), sd = sd(x)))

#
summary(d_ch$comp_risk); sd(d_ch$comp_risk, na.rm = T)
aggregate(comp_risk ~ rel_rank_b, 
          d_ch, 
          FUN = function(x) c(m = mean(x), sd = sd(x)))

# modeling results --------------------------------------------------------

m_ch = readRDS('posteriors/01_chimps_modsMO_b_Z.rds')
m_hu = readRDS('posteriors/02_humans_modsMO_b_Z.rds')
risk_leadership_within_mod = readRDS('posteriors/03_risk_leadership_within_mod.rds')

# unique r2 rank
ch_rank_r2 = bayes_R2(m_ch$comp_risk$m1, summary = F) - bayes_R2(m_ch$comp_risk$co, summary = F)
quantile(ch_rank_r2, c(.5 ,.025, .975))

h_lead_r2 = bayes_R2(m_hu$le, summary = F) - bayes_R2(m_hu$co, summary = F)
quantile(h_lead_r2, c(.5 ,.025, .975))


# coefs
round(fixef(m_ch$comp_risk$m1), 2)
round(fixef(m_hu$inc), 2)

# save into tables
ch_ests = lapply(names(m_ch$general_risk), function(mn) {
  
  mm = m_ch$general_risk[[mn]]
  extract_coefs(mm,
                write_path = paste0('results/model_estimates/chimps_', mn, '.csv'),
                return = T)
  
})

hu_ests = lapply(names(m_hu), function(mn) {
  
  mm = m_hu[[mn]]
  extract_coefs(mm, 
                write_path = paste0('results/model_estimates/humans_', mn, '.csv'),
                return = T)
  
})

# chimp coefs -------------------------------------------------------------

#
co_ch = rbind(fixef(m_ch$comp_risk$co)[c('subject_sex1'
                                         # paste0('age_b', 1:3)
                                         ),],
              fixef(m_ch$comp_risk$m1)['morel_rank_b',],
              fixef(m_ch$comp_risk$int)['morel_rank_b:subject_sex1',])
co_ch = data.frame(co_ch)
co_ch$coef = c('Sex', 'Rank', 'Sex \u00D7\nRank')
co_ch$coef = factor(co_ch$coef,
                    levels = co_ch$coef,
                    ordered = T)  

#
ch_co_plt = ggplot(co_ch,
                   mapping = aes(x = Estimate,
                                 y = coef,
                                 xmin = Q2.5, 
                                 xmax = Q97.5)) +
  geom_vline(xintercept = 0,
             col = 'black') +
  geom_pointrange(col = 'black',
                  size = .4,
                  shape = 1) +
  ggtitle('Chimpanzees') +
  scale_x_continuous('Regression weight',
                     breaks = seq(-1, 1, .4),
                     minor_breaks = seq(-1, 1, .1),
                     limits = c(-1, .8)) +
  ylab('') +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = .5
                                   , size = 8
                                   ),
        # axis.title.x = element_text(color = cols['ch']),
        plot.title = element_text(color = cols['ch'])
        )

# chimps figs -------------------------------------------------------------

# sex effect
ch1a = ce_dat_plt_fun(m_ch$comp_risk$co,
                      ce_var = 'subject_sex',
                      ylab = 'Risk preference',
                      xlab = 'Sex',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['ch'],
                      dat_pt_col = cols['ch'],
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

# age effect
ch1b = ce_dat_plt_fun(m_ch$comp_risk$co,
                      ce_var = 'age_b',
                      ylab = 'Risk preference',
                      xlab = 'Age (in years)',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['ch'],
                      dat_pt_col = cols['ch'],
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

# RR effects
ch1c = ce_dat_plt_fun(m_ch$comp_risk$m1,
                      ce_var = 'rel_rank_b',
                      ylab = 'Risk preference',
                      xlab = 'Hierarchy rank',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['ch'],
                      dat_pt_col = cols['ch'],
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

ch1c$plt = ch1c$plt + scale_x_discrete(labels = c('0\u201320%', 
                                                  '21\u201340%','41\u201360%', 
                                                  '61\u201380%', 
                                                  '81\u2013100%'))

# ch1 = (ch_co_plt|ch1a$plt|ch1c$plt) + 
#   plot_annotation(tag_levels = 'a',
#                   title = 'Risk-taking in chimpanzee dataset') +
#   plot_layout(widths = c(1.5, 1, 3))
# 
# ggsave('results/Fig01a.png',
#        plot = ch1,
#        device = 'png',
#        units = 'cm',
#        width = 16,
#        height = 4,
#        scale = 2)
# 
# ggsave('results/Fig01a.pdf',
#        plot = ch1,
#        device = 'pdf',
#        units = 'cm',
#        width = 16,
#        height = 4,
#        scale = 2)

# CHIMP RANK AND RISK
# 
# model estimates
ch_rr = conditional_effects(m_ch$comp_risk$int,
                            effects = 'rel_rank_b:subject_sex',
                            conditions = data.frame(chimp_group = NA,
                                                    age_b = NA))[[1]]

# hier ranking
ch1c2 = ggplot(data = m_ch$comp_risk$m1$data,
              mapping = aes(x = rel_rank_b,
                            y = y,
                            col = subject_sex)) +
  geom_boxplot(varwidth = T,
               fill = NA,
               outlier.colour = NA,
               show.legend = T) +
  geom_point(position = position_dodge(.5),
             alpha = .7,
             show.legend = F) +
  scale_color_manual('Sex', values = sex_cols) +
  # geom_pointrange(mapping = aes(x = rel_rank_b,
  #                               y = estimate__,
  #                               ymin = lower__,
  #                               ymax = upper__),
  #                 col = rep('black', 10),
  #                 position = position_dodge(.5),
  #                 data = ch_rr,
  #                 size = .4,
  #                 shape = 1) +
  scale_x_discrete('Hierarchy rank',
                   labels = c('0\u201320%', 
                              '21\u201340%','41\u201360%', 
                              '61\u201380%', 
                              '81\u2013100%')) +
  scale_y_continuous('Risk preference',
                     breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  ggtitle('Chimpanzees') +
  theme_bw() 

# # 
# ch1 = (ch_co_plt|ch1a$plt|ch1c2) +
#   plot_annotation(tag_levels = 'a',
#                   title = 'Risk-taking in chimpanzee dataset') +
#   plot_layout(widths = c(2, 2, 6))
# 
# ch1
# 
# ggsave('results/Fig01a2.pdf',
#        plot = ch1,
#        device = 'pdf',
#        units = 'cm',
#        width = 16,
#        height = 4,
#        scale = 2)

# human coefs -------------------------------------------------------------

#
co_hu = rbind(fixef(m_hu$co)['gender1',],
              fixef(m_hu$le)['moleadership_b',],
              fixef(m_hu$le2)['moleadership_b:gender1',],
              fixef(m_hu$inc)['moincome_b',],
              fixef(m_hu$inc2)['moincome_b:gender1',])
co_hu = data.frame(co_hu)
co_hu$coef = c('Gender', 'Leadership', 'Gender \u00D7\nLeadership', 
               'Income', 'Gender \u00D7\nIncome')
co_hu$coef = factor(co_hu$coef,
                    levels = co_hu$coef,
                    ordered = T)  

#
hu_co_plt = ggplot(co_hu,
                   mapping = aes(x = Estimate,
                                 y = coef,
                                 xmin = Q2.5, 
                                 xmax = Q97.5)) +
  geom_pointrange(col = 'black',
                  size = .4,
                  shape = 1) +
  geom_vline(xintercept = 0,
             col = 'black') +
  ylab('') +
  scale_x_continuous('Regression weight',
                     breaks = seq(-1, 1, .4),
                     minor_breaks = seq(-1, 1, .1),
                     limits = c(-1, .8)) +
  theme_bw() +
  ggtitle('Humans') +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = .5
                                   , size = 8
                                   ),
        # axis.title.x = element_text(color = cols['h']),
        plot.title = element_text(color = cols['h']))

# WITHIN-HUMAN DIFFERENCES
id_coefs = data.frame( coef(risk_leadership_within_mod)$id[,,'leadership_c'] )
id_coefs = id_coefs[order(id_coefs$Estimate),]
colnames(id_coefs) = c('e', 'se', 'li', 'ui')
id_coefs$id = 1:nrow(id_coefs)
fx_coefs = fixef(risk_leadership_within_mod)

# the figure
hu_id_co_plt = ggplot() +
  geom_linerange(data = id_coefs,
                 mapping = aes(x = id,
                               ymin = li,
                               ymax = ui),
                 alpha = .1) +
  geom_line(data = id_coefs,
             mapping = aes(x = id, y = e,
                           group = 1),
            col = 'white') +
  scale_x_continuous('Participant',
                     breaks = c(1, 500, 1000, 1500)) +
  scale_y_continuous('Within-person\ndifferences in\nrisk preference',
                     limits = c(-3,3),
                     breaks = -3:3) +
  theme_bw()
# hu_id_co_plt

# humans figs -------------------------------------------------------------

# sex effect
hu1a = ce_dat_plt_fun(m_hu$co,
                      ce_var = 'gender',
                      ylab = 'Risk preference',
                      xlab = 'Gender',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['h'],
                      dat_pt_col = cols['h'],
                      points = F,
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

# age effect
hu1b = ce_dat_plt_fun(m_hu$co,
                      ce_var = 'age_b',
                      ylab = 'Risk preference',
                      xlab = 'Age (in years)',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['h'],
                      dat_pt_col = cols['h'],
                      points = F,
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

# leadership effect
hu1c = ce_dat_plt_fun(m_hu$le,
                      ce_var = 'leadership_b',
                      ylab = 'Risk preference',
                      xlab = 'Leadership (in years)',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['h'],
                      dat_pt_col = cols['h'],
                      points = F,
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

hu1c$plt = hu1c$plt + scale_x_discrete(labels = c('0','1\u20135','6'))

# income effect
hu1d = ce_dat_plt_fun(m_hu$inc,
                      ce_var = 'income_b',
                      ylab = 'Risk preference',
                      xlab = 'Income (monthly average)',
                      m_col = 'black',
                      # ylab_col = cols['ch'],
                      bx_col = cols['h'],
                      dat_pt_col = cols['h'],
                      points = F,
                      ylims = c(-3, 3),
                      ybreaks = seq(-3, 3, 1))

hu1d$plt = hu1d$plt + scale_x_discrete(labels = c('NA & 0\u20AC',
                                                  '1\u20131500\u20AC',
                                                  '1501\u20132500\u20AC',
                                                  '2501\u20134000\u20AC',
                                                  '4001\u20136000\u20AC',
                                                  '>6000\u20AC')) +
  ggtitle(' ')

# des = c('abc
#         add')
# 
# hu12 = (hu_co_plt + hu1a$plt + hu1c$plt + hu1d$plt) +
#   plot_annotation(tag_levels = list(c('d', 'e', 'f', 'g')),
#                   title = 'Risk-taking in human dataset') +
#   plot_layout(widths = c(1.5, 1, 3),
#               design = des)
# hu12 

# ggsave('results/Fig01b.pdf',
#        plot = hu12,
#        device = 'pdf',
#        units = 'cm',
#        width = 16,
#        height = 7,
#        scale = 2)
# 
# ggsave('results/Fig01b.png',
#        plot = hu12,
#        device = 'png',
#        units = 'cm',
#        width = 16,
#        height = 7,
#        scale = 2)

# # leadership AND RISK
# 
# # model estimates
# hu_le = conditional_effects(m_hu$le,
#                             effects = 'leadership_b',
#                             conditions = data.frame(gender = NA,
#                                                     age_b = NA))[[1]]
# 
# hier ranking
hu2c = ggplot(data = d_hu,
              mapping = aes(x = leadership_b,
                            y = risk_preference,
                            col = gender)) +
  geom_boxplot(varwidth = T,
               fill = NA,
               outlier.colour = NA,
               show.legend = T) +
  # geom_point(position = position_dodge(.5),
  #            alpha = .2) +
  scale_color_manual('Gender', values = gender_cols) +
  scale_x_discrete('Leadership (in years)',
                   labels = c('0','1\u20135','6')) +
  scale_y_continuous('Risk preference',
                     breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  ggtitle('Humans') +
  theme_bw() 
# theme(plot.title = element_text(color = cols['h']))
# 
# SALARY AND RISK

# model estimates
hu_rr = conditional_effects(m_hu$inc,
                            effects = 'income_b',
                            conditions = data.frame(gender = NA,
                                                    age_b = NA))[[1]]

# hier ranking
hu2 = ggplot(data = d_hu,
              mapping = aes(x = income_b,
                            y = risk_preference,
                            col = gender)) +
  geom_boxplot(varwidth = T,
               fill = NA,
               outlier.colour = NA,
               show.legend = F) +
  scale_color_manual('Sex', values = gender_cols) +
  xlab('Income level') +
  scale_x_discrete(labels = c('NA & 0\u20AC',
                             '1\u20131500\u20AC',
                             '1501\u20132500\u20AC',
                             '2501\u20134000\u20AC',
                             '4001\u20136000\u20AC',
                             '>6000\u20AC')) +
  scale_y_continuous('Risk preference',
                     breaks = seq(-3, 3, 1),
                     limits = c(-3, 3)) +
  theme_bw()
# 
# 
# des = c('abc
#         ddd')
# 
# hu12 = (hu_co_plt + hu1a$plt + hu1c + hu2) +
#   plot_annotation(tag_levels = list(c('d', 'e', 'f', 'g')),
#                   title = 'Risk-taking in human dataset') +
#   plot_layout(widths = c(2, 2, 6),
#               design = des)
# # hu12 
# 
# ggsave('results/Fig01b.pdf',
#        plot = hu12,
#        device = 'pdf',
#        units = 'cm',
#        width = 16,
#        height = 4,
#        scale = 2)

# final figs --------------------------------------------------------------

des = c('abc
        dee
        fgh')

fin_fig = (hu_co_plt + hu1c$plt + hu1a$plt + 
             free(hu_id_co_plt, type = 'space') + 
             hu1d$plt +
           ch_co_plt + ch1c$plt + ch1a$plt) +
  plot_layout(widths = c(1.5, 3, 1),
              design = des) +
  plot_annotation(tag_levels = 'a')  & 
  theme(plot.tag.position = c(0, 1))

ggsave('results/Fig01.pdf',
       plot = fin_fig,
       device = cairo_pdf,
       units = 'cm',
       width = 16,
       height = 11,
       scale = 1.3)

ggsave('results/Fig01.jpeg',
       plot = fin_fig,
       device = 'jpeg',
       dpi = 700,
       units = 'cm',
       width = 16,
       height = 11,
       scale = 1.3)

# sex / gender interaction fig
ch1c2 = ch1c2 + ggtitle('Chimpanzees') + 
  theme(legend.position = 'bottom')
hu2c = hu2c + ggtitle('Humans') + 
  theme(legend.position = 'bottom')

int_fig = (hu2c|ch1c2) +
  plot_layout(widths = c(3, 5)) 

int_fig_f = int_fig/hu2 +
  plot_annotation(tag_levels = 'a')  &
  theme(plot.tag.position = c(0, 1))

ggsave('results/Fig02.pdf',
       plot = int_fig_f,
       device = cairo_pdf,
       units = 'cm',
       width = 16,
       height = 8,
       scale = 1.3)

ggsave('results/Fig02.jpeg',
       plot = int_fig_f,
       device = 'jpeg',
       dpi = 700,
       units = 'cm',
       width = 16,
       height = 8,
       scale = 1.3)


# age fig
ch1b$plt = ch1b$plt + ggtitle('Chimpanzees') + 
  theme(axis.title.x = element_text(color = 'black'),
        axis.title.y = element_text(color = 'black'))
hu1b$plt = hu1b$plt + ggtitle('Humans') + 
  theme(axis.title.x = element_text(color = 'black'),
        axis.title.y = element_text(color = 'black'))

age_fig = (hu1b$plt | ch1b$plt) +
  plot_annotation(tag_levels = 'a')  &
  theme(plot.tag.position = c(0, 1))

ggsave('results/Fig03.pdf',
       plot = age_fig,
       device = cairo_pdf,
       units = 'cm',
       width = 16,
       height = 4,
       scale = 1.3)

ggsave('results/Fig03.jpeg',
       plot = age_fig,
       device = 'jpeg',
       dpi = 700,
       units = 'cm',
       width = 16,
       height = 4,
       scale = 1.3)


# supplementary res -------------------------------------------------------

# rank and risk in chimps

# RR effects
rr_risks = lapply(names(m_ch[1:6]), function(x){
  
  p = ce_dat_plt_fun(m_ch[[x]]$m1,
                     ce_var = 'rel_rank_b',
                     ylab = 'Risk preference',
                     xlab = 'Hierarchy rank',
                     m_col = 'black',
                     # ylab_col = cols['ch'],
                     bx_col = cols['ch'],
                     dat_pt_col = cols['ch'],
                     ylims = c(-3, 3),
                     ybreaks = seq(-3, 3, 1))$plt
  
  #
  b = fixef(m_ch[[x]]$m1)['morel_rank_b', c(1, 3, 4)]
  b = round(b, 2)
  
  p = p + ggtitle( gsub('_', ' ', x),
                   paste0('\u03B2 = ', b[1], '; 95% BCI: [', b[2], ', ', b[3], ']')
                   ) +
    scale_x_discrete(labels = c('0\u201320%', 
                                '21\u201340%','41\u201360%', 
                                '61\u201380%', 
                                '81\u2013100%'))
  
  return(p)
})

rr_plt = wrap_plots(rr_risks)

ggsave('results/Fig04.jpeg',
       plot = rr_plt,
       device = 'jpeg',
       dpi = 700,
       units = 'cm',
       width = 16,
       height = 10,
       scale = 1.5)