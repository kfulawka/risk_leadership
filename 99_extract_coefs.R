extract_coefs = function(m, 
                         write_path = 'results/mods.csv',
                         return = F) {
  
  # get all model pars
  ff = posterior_summary(as.data.frame(m))
  
  # round up
  ff = as.data.frame(round(ff, 3))
  ff = ff[ !rownames(ff) %in% c('Intercept', 'lprior', 'lp__'), ]
  
  # 
  colnames(ff) = c('Estimate', 'SE', 'BCI_LI_95', 'BCI_UI_95')
  rownames(ff) = gsub('^(b_|bsp_mo)', 'beta_', rownames(ff))

  #
  write.table(ff,
              file = write_path,
              row.names = T,
              dec = ',',
              sep = ';')
  #
  if(return) return(ff)
  
}