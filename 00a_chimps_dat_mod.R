rm(list = ls())

library(data.table)
library(brms)
library(bayesplot)

dd = fread('data/chimps.txt')
dd = data.frame(dd)

#
dv = c("general_risk", "food_risk", "snake_risk", 
       "escape_risk", "hierarchy_risk", "strangers_risk")

#
preds = c('chimp_group', 'subject_sex', 'subject_age')

# data prep ---------------------------------------------------------------

# aggregate
da = aggregate(cbind(general_risk, food_risk, snake_risk, 
                     escape_risk, hierarchy_risk, strangers_risk,
                     subject_rank) ~ 
                 chimp_group + subject_sex + subject_age + 
                 subject_age_class + subject,
               data = dd,
               FUN = mean, 
               na.rm = T,
               na.action = 'na.pass')

# age
da$age_b = cut(da$subject_age,
               breaks = c(0, 15, 20, 30, 40),
               include.lowest = T,
               ordered_result = T)


# relative rank
rr = function(x) 1 - (x - min(x)) / (max(x) - min(x))
da$relative_rank = ave(da$subject_rank,
                       da$chimp_group,
                       FUN = rr)

# bins
da$rel_rank_b = cut(da$relative_rank, 
                    breaks = seq(0, 1, length.out = 6),
                    include.lowest = T,
                    ordered_result = T)

# composite risk
da$comp_risk = rowMeans(da[,dv])

dv = c(dv, 'comp_risk')
names(dv) = dv

da$subject_sex = ifelse(da$subject_sex == 'female', 'Female', 'Male')

# modeling data -----------------------------------------------------------

# data for coeffictiens evaluation
dz = da

# standardize cont rel ralnk
dz$relative_rank_z = scale(dz$relative_rank)

# code sex and group
for(i in c('subject_sex', 'chimp_group', 'age_b', 'rel_rank_b')) {
  
  dz[,i] = factor(dz[,i])
  contrasts(dz[,i]) = contr.sum(nlevels(dz[,i]))
  
  # also in the data for plotting
  da[,i] = factor(da[,i])
  contrasts(da[,i]) = contr.sum(nlevels(da[,i]))
  
}

# change sex to -.5, and .5 for direct weight comparisons
contrasts(dz$subject_sex) = contrasts(dz$subject_sex)/2

#
saveRDS(dz, file = 'data/dm_chimps.rds')

# rel rank binned ---------------------------------------------------------

# formulas to test
bf_mo_forms = list(
  co = bf(y ~ subject_sex + age_b + chimp_group),
  m1 = bf(y ~ mo(rel_rank_b) + subject_sex + age_b + chimp_group),
  m2 = bf(y ~ mo(rel_rank_b)),
  int = bf(y ~ mo(rel_rank_b) * subject_sex + age_b + chimp_group),
  int2 = bf(y ~ mo(rel_rank_b) * chimp_group + age_b + subject_sex)
)

# BRMS models for coeff eval
mods_mo = lapply(dv[7], function(y) {
  
  #
  dz$y = scale(dz[,y])
  
  #
  m = lapply(bf_mo_forms, function(yy) {
    
    brm(yy,
        data = dz,
        family = student(),
        iter = 2e3,
        warmup = 1e3,
        chains = 4,
        thin = 2,
        cores = 4)
    
  })
  
})
saveRDS(mods_mo, 'posteriors/01_chimps_modsMO_b_Z.rds')

# rel rank cont -----------------------------------------------------------

# formulas to test
bf_co_forms = list(
  co = bf(y ~ subject_sex + age_b + chimp_group),
  m1 = bf(y ~ relative_rank_z + subject_sex + age_b + chimp_group),
  m2 = bf(y ~ relative_rank_z),
  int = bf(y ~ relative_rank_z * subject_sex + age_b + chimp_group),
  int2 = bf(y ~ relative_rank_z * chimp_group + age_b + subject_sex)
)

# BRMS models for coeff eval
mods_co = lapply(dv, function(y) {
  
  #
  dz$y = scale(dz[,y])
  
  #
  m = lapply(bf_co_forms, function(yy) {
    
    brm(yy,
        data = dz,
        family = student(),
        iter = 2e3,
        warmup = 1e3,
        chains = 4,
        thin = 2,
        cores = 4)
    
  })
  
})
saveRDS(mods_co, 'posteriors/01_chimps_modsCO_b_Z.rds')

# model diagnostics -------------------------------------------------------

# parameters to show in diagnostics
# includes fixed effects plus student-t residual parameters
diag_pars = c("^b_", "^sigma$", "^nu$")

# continuous rank results
mods_co = readRDS('posteriors/01_chimps_modsCO_b_Z.rds')

# binned monotonic
mods_mo = readRDS('posteriors/01_chimps_modsMO_b_Z.rds')

# 
rr = list(continuous = list(mods = mods_co, nm = 'continuous'),
          binned = list(mods = mods_mo, nm = 'binned'))

lapply(rr, function(M) {
  
  for (y in names(M$mods)) {

    message("Saving BRMS diagnostics for: ", y)
    
    m = M$mods[[y]]$m1
    
    # extract posterior draws
    draws = as_draws_array(m)
    
    # parameter names to plot
    pars_y = grep(
      paste(diag_pars, collapse = "|"),
      dimnames(draws)$variable,
      value = TRUE
    )
    
    # 1. model summary as txt ------------------------------------------------
    
    txt_file = file.path("brms_diagnostics", paste0('chimps_', M$nm, '_',
                                                    y, "_m1_summary.txt"))
    
    capture.output(
      summary(m),
      file = txt_file
    )
    
    # 2. posterior distributions as PDF --------------------------------------
    
    post_file = file.path("brms_diagnostics", paste0('chimps_', M$nm, '_',
                                                     y, "_m1_posteriors.pdf"))
    
    pdf(post_file, width = 11, height = 8.5)
    print(
      mcmc_dens(
        draws,
        pars = pars_y
      )
    )
    dev.off()
    
    # 3. trace/chains as PDF -------------------------------------------------
    
    trace_file = file.path("brms_diagnostics", paste0('chimps_', M$nm, '_',
                                                      y, "_m1_chains.pdf"))
    
    pdf(trace_file, width = 11, height = 8.5)
    print(
      mcmc_trace(
        draws,
        pars = pars_y
      )
    )
    dev.off()
    
    # 4. autocorrelation plots as PDF -----------------------------------------
    
    acf_file = file.path("brms_diagnostics", paste0('chimps_', M$nm, '_',
                                                    y, "_m1_autocorr.pdf"))
    
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
  
})