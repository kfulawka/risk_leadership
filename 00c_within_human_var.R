rm(list = ls())

library(data.table)
library(brms)

dd = fread('data/humans.txt')
dd = data.frame( dd[,c(
  "Unveraenderliche_Personennummer", "Erhebungsjahr", "Geschlecht",
  "Geburtsjahr", "Akt_Nettoerwerbseinkommen_EUR", "Fuehrungskraft", 
  "Risikobereitschaft")] )
#
colnames(dd) = c('id', 'year', 'gender', 'birth_year',
                 'income', 'leadership', 'risk_preference')

#
dv = 'risk_preference'

# summary statistics of within score variability --------------------------

da_risk = aggregate(risk_preference ~ id,
                    data = dd, 
                    FUN = function(x) c(r = max(x)-min(x),
                                        sd = sd(x, na.rm = T),
                                        mad = mad(x, na.rm = T),
                                        n = length(x)))
da_risk = do.call(data.frame, da_risk)
#
summary(da_risk$risk_preference.sd)

# leadership change variable prep -----------------------------------------

da = aggregate(leadership ~ id, 
               data = dd,
               FUN = function(x) c(m = mean(x, na.rm = T), 
                                   s = sum(x, na.rm = T),
                                   n = length(x)) )
da = do.call(data.frame, da)
colnames(da) = c('id', 'lead_years_m', 'lead_years_s', 'n_years')

# add leadership variable
# NOTE: if mean is between (0-1) it means there was a transition
da$leadership_b = cut(da$lead_years_m, 
                      breaks = c(0, .01, .99, 1),
                      include.lowest = T,
                      ordered_result = T,
                      labels = c('never', 'occured', 'throught'))

# ids of those who had a leadership position at some point, i.e. 1-5 years
da_lc = da[da$leadership_b=='occured', ] 

# only 1300 individuals 
leadership_change_ids = da_lc$id
length(leadership_change_ids)

# subset main data
df = dd[dd$id %in% leadership_change_ids, ]
#
df = df[!is.na(df$risk_preference),]

rm(da, da_lc, leadership_change_ids, da_risk)

# descriptives -----------------------------------------------------------

# observations per participant
n_obs = table(df$id)

# leader observations per participant
n_lead = aggregate(leadership ~ id, df, sum)

# minimal descriptives
c(
  participants = length(unique(df$id)),
  observations = nrow(df),
  mean_obs = mean(n_obs),
  sd_obs = sd(n_obs),
  min_obs = min(n_obs),
  max_obs = max(n_obs)
)

table(df$leadership)
table(n_lead$leadership)

# leadership within person ------------------------------------------------

# set the contrast
df$leadership_c = ifelse(df$leadership == 0, -.5, .5)

# to z scale
df$risk_preference_z =  scale(df$risk_preference)

#
risk_leadership_within_mod = brm(risk_preference_z ~ leadership_c + (leadership_c|id),
                                 data = df,
                                 family = student(),
                                 cores = 8,
                                 chains = 8,
                                 thin = 2,
                                 iter = 2e3,
                                 warmup = 1e3)

pp_check(risk_leadership_within_mod, 
         ndraws = 100) + xlim(-3, 3)
plot(risk_leadership_within_mod, ask = F)
#
risk_leadership_within_mod
#
saveRDS(risk_leadership_within_mod, 'posteriors/03_risk_leadership_within_mod.rds')

# within person differences -----------------------------------------------

risk_leadership_within_mod = readRDS('posteriors/03_risk_leadership_within_mod.rds')

id_coefs = data.frame( coef(risk_leadership_within_mod)$id[,,'leadership_c'] )
id_coefs = id_coefs[order(id_coefs$Estimate),]
colnames(id_coefs) = c('e', 'se', 'li', 'ui')
id_coefs$id = 1:nrow(id_coefs)
fx_coefs = fixef(risk_leadership_within_mod)

# the figure
id_risk_lead_plt = ggplot() +
  geom_hline(yintercept = 0,
             lty = 2) +
  geom_linerange(data = id_coefs,
                 mapping = aes(x = id,
                               ymin = li,
                               ymax = ui),
                 alpha = .1) +
  geom_point(data = id_coefs,
             mapping = aes(x = id, y = e)) +
  xlab('Participant') +
  ylab('Within-person difference') +
  ylim(-3, 3) +
  theme_bw() +
  theme(axis.text.x = element_blank())
id_risk_lead_plt
