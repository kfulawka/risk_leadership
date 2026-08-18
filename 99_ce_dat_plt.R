ce_dat_plt_fun = function(mm,
                          ylims = c(0, 4),
                          ybreaks = c(0:4),
                          ylab = 'risk',
                          xlab = 'Sex',
                          ylab_col = 'black',
                          xlab_col = ylab_col,
                          bx_col = 'black',
                          dat_pt_col = 'black',
                          ce_var,
                          m_col = 'blue',
                          points = T) {
  
  #
  dat = mm$data
  
  colnames(dat)[1] = 'y'
  dat$x = dat[,ce_var]
  
  lvls = levels(dat$x)
  nlvs = length(lvls)
  
  # boxplot with data
  box_plt = ggplot(data = dat,
                   mapping = aes(x = x, y = y)) +
    geom_boxplot(fill = NA,
                 varwidth = T,
                 col = bx_col,
                 outliers = F,
                 alpha = .1) +
    scale_y_continuous(ylab,
                       breaks = ybreaks,
                       limits = ylims) +
    xlab(xlab) +
    theme_bw() +
    theme(axis.title.y = element_text(color = ylab_col),
          axis.title.x = element_text(color = xlab_col))
  
  if(points) {
    box_plt = box_plt + 
      geom_point(alpha = .2,
                 col = dat_pt_col)
  }
  
  # model estimates
  ce1d = mm$data[1,]
  ce1d[1,] = NA
  
  ce1 = conditional_effects(mm,
                            effects = ce_var,
                            conditions = ce1d)[[1]]
  ce1[,'x'] = lvls
  
  plt = box_plt + 
    geom_pointrange(mapping = aes(x = x,
                                  y = estimate__,
                                  ymin = lower__,
                                  ymax = upper__),
                    data = ce1,
                    position = position_nudge(x = 0),
                    col = m_col,
                    size = .4,
                    shape = 1)
  
  
  # output
  return(list(plt = plt,
              ce = ce1))
  
}