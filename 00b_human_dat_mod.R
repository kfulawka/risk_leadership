library(data.table)
library(brms)
library(bayesplot)

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

# data prep ---------------------------------------------------------------

dd$age = dd$year - dd$birth_year

da = aggregate(cbind(income, age, risk_preference, leadership) ~ 
                 id + gender,
               data = dd,
               FUN = mean,
               na.rm = T,
               na.action = na.pass)

table(is.na(da$income))

quantile(da$income, c(0, .2, .4, .6, .8, 1), na.rm = T)

da$income2 = da$income
da$income2[is.na(da$income2)] = -1

da$income_b = cut(da$income2,
                  breaks = c(-1, 0, 1500, 2500, 4000, 6000, 1700000),
                  include.lowest = T,
                  ordered_result = T)

da$leadership_b = cut(da$leadership, 
                      breaks = c(0, .01, .99, 1),
                      include.lowest = T,
                      ordered_result = T,
                      labels = c('never', 'occured', 'throught'))

da$age_b = cut(da$age, 
               breaks = c(18, 21, 34, 49, 64, 105),
               include.lowest = T,
               ordered_result = T)

#
da$gender = factor(da$gender,
                   levels = 2:1,
                   labels = c('Female', 'Male'),
                   ordered = T)

# modeling data -----------------------------------------------------------

# data for coefficients evaluation
dz = da

# code sex and group
for(i in c('leadership_b', 'gender', 'income_b', 'age_b')) {
  
  dz[,i] = factor(dz[,i])
  contrasts(dz[,i]) = contr.sum(nlevels(dz[,i]))
  
}

#
dz$risk_preference = scale(dz$risk_preference)
# change sex to -.5, and .5 for direct weight comparisons
contrasts(dz$gender) = contrasts(dz$gender)/2

saveRDS(dz, file = 'data/dm_hu.rds')

# model -------------------------------------------------------------------

# formulas to test
bf_forms = list(
  co = bf(risk_preference ~ gender + age_b
          # sigma ~ gender + age_b
  ),
  le = bf(risk_preference ~ gender + age_b + mo(leadership_b)
          # sigma ~ gender + age_b + leadership_b
  ),
  le2 = bf(risk_preference ~ gender * mo(leadership_b) + age_b
           # sigma ~ gender * leadership_b + age_b
  ),
  inc = bf(risk_preference ~ gender + age_b + mo(income_b)
           # sigma ~ gender + age_b + income_b
  ),
  inc2 = bf(risk_preference ~ gender * mo(income_b) + age_b
            # sigma ~ gender * income_b + age_b
  )
)

# BRMS models for coeff eval
mods_z = lapply(bf_forms, function(y) {
  
  m = brm(y,
          data = dz,
          family = student(),
          iter = 2e3,
          warmup = 1e3,
          chains = 4,
          thin = 2,
          cores = 4)
})

saveRDS(mods_z, 'posteriors/02_humans_modsMO_b_Z.rds')

# model diagnostics -------------------------------------------------------

mods_z = readRDS('posteriors/02_humans_modsMO_b_Z.rds')

# parameters to show in diagnostics
# includes fixed effects plus student-t residual parameters
diag_pars = c("^b_", "^sigma$", "^nu$")


for(y in c('le', 'inc')) {
  
  m = mods_z[[y]]
  
  # extract posterior draws
  draws = as_draws_array(m)
  
  # parameter names to plot
  pars_y = grep(
    paste(diag_pars, collapse = "|"),
    dimnames(draws)$variable,
    value = TRUE
  )
  
  # 1. model summary as txt ------------------------------------------------
  
  txt_file = file.path("brms_diagnostics", paste0('humans_', y, "_summary.txt"))
  
  capture.output(
    summary(m),
    file = txt_file
  )
  
  # 2. posterior distributions as PDF --------------------------------------
  
  post_file = file.path("brms_diagnostics", paste0('humans_', y, "_posteriors.pdf"))
  
  pdf(post_file, width = 11, height = 8.5)
  print(
    mcmc_dens(
      draws,
      pars = pars_y
    )
  )
  dev.off()
  
  # 3. trace/chains as PDF -------------------------------------------------
  
  trace_file = file.path("brms_diagnostics", paste0('humans_', y, "_chains.pdf"))
  
  pdf(trace_file, width = 11, height = 8.5)
  print(
    mcmc_trace(
      draws,
      pars = pars_y
    )
  )
  dev.off()
  
  # 4. autocorrelation plots as PDF -----------------------------------------
  
  acf_file = file.path("brms_diagnostics", paste0('humans_', y, "_m1_autocorr.pdf"))
  
  pdf(acf_file, width = 11, height = 7)
  print(
    mcmc_acf(
      draws,
      pars = pars_y,
      lags = 20
    )
  )
  dev.off()
  
}