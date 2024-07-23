### short script to run for consistent admixture plots
### developed for use with Eric's mega-post-bcf-exploratory-snakeflow
### based off of visFun.R from the ngsAdmix team

plotAdmix <- function(qdata, 
                      metadata,
                      pop, 
                      group,
                      pord,
                      gord,
                      colorpalette = c("aquamarine4", 
                                       "cornflowerblue", 
                                       "indianred3", 
                                       "lightskyblue3", 
                                       "lightslateblue", 
                                       "maroon", 
                                       "sienna2", 
                                       "lightsalmon4", 
                                       "palegreen3", 
                                       "lavender"), 
                      main = paste0("K = ", K, "rep = ", rep), 
                      cex.main = 1.5, 
                      cex.lab = 1, 
                      cex.axis = 0.75, 
                      border = NULL) {
  K <- ncol(qdata) - 1 # if sample names are included in the data file
  
  if(K>length(colorpalette))
    warning("I need more colors!!!")
  data <- qdata %>%
    inner_join(., 
               metadata, 
               by = c("sample" = "NMFS_DNA_ID")) %>%
    mutate(pfact = factor(pop, 
                            levels = pord), 
           gfact = factor(group, 
                            levels = gord)) %>%
    arrange(gfact, pfact)
  
  barplot(t(data[2:K-1]), 
          col = colorpalette, 
          names = data$pop,
          space = 0, 
          border = NA, 
          cex.axis = cex.axis, 
          cex.lab = cex.lab, 
          ylab = "Admixture Proportions", 
          xlab = "", 
          main = main, 
          cex.main = cex.main, 
          xpd = NA)
  
}