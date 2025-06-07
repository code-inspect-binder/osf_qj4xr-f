# PREPARATIONS ##################################################################################################

## Load libraries -----------------------------------------------------------------------------------------------
library(qqplotr)



## Define functions ---------------------------------------------------------------------------------------------

### Computes studentized residuals for svyglm models ------------------------------------------------------------
# So far, there is only a function computing standardized residuals (svystdres from the svydiags package), 
# but we can modify this function as follows:
svystudres <- function (mobj, doplot = F) {
  p <- length(mobj$coefficients)
  e <- mobj$residuals
  mrows <- names(mobj$y)
  mdata <- mobj$data[rownames(mobj$data) %in% mrows, ]
  sighat <- rhohat <- 0
  mbar <- 1
  n <- length(e)
  sighat <- (n - 1) * var(e)/(n - p)
  stdresids <- e/sqrt(sighat)
  stdresids <- e/sqrt(sighat)
  studresids <- stdresids/sqrt((n - p - stdresids^2)/(n - p - 1)) # Here's the crucial adaptation, 
  names(studresids) <- names(e)                                   # inspired by the MASS::lmwork function
  if (doplot) {
    plot(1:length(e), studresids, xlab = "sample element")
    abline(h = c(-3, 0, 3), col = "red", lwd = 2)
  }
  list(studresids = studresids, n = n, mbar = mbar, rtsighat = sqrt(sighat), rhohat = rhohat)
}




### Specify function for distribution checks --------------------------------------------------------------------

distributionchecks <- function(fit) {
  
  # Step 1: Inspect histograms of studentized residuals
  #         For this, we extract fitted values from the svyglm model, compute studentized 
  #         residuals (with the function we defined at the top of the script), attach both to disdf, 
  #         update the svydesign object, and then plot the histograms of studentized residuals
  
  disdf <- data.frame(id = seq.int(nrow(bar2019pop)))
  
  disdf <- fit$fitted.values %>%
    .[match(disdf$id, names(.))] %>%
    mutate(disdf, fitted.values = .)
  
  disdf <- svystudres(fit)$studresids %>%
    .[match(disdf$id, names(.))] %>%
    mutate(disdf, studentized.residuals = .)
  
  
  # Create histogram of studentized residuals
  studreshist <- subset(disdf, !is.na(disdf$studentized.residuals)) %>%
    ggplot(aes(studentized.residuals)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 0.15) + 
    labs(x = "Studentized Residuals", y = "Density") + 
    ggtitle("Histogram of studentized residuals") +
    xlim(-5, 5) + 
    stat_function(fun = dnorm, args = list(mean = mean(disdf$studentized.residuals, na.rm = T)[[1]],
                                           sd   = sd(disdf$studentized.residuals, na.rm = T)[[1]])) + 
    theme_light()
  
  
  # Step 2: Inspect Q-Q plots for studentized residuals of thesvyglm model
  studresqq <- subset(disdf, !is.na(disdf$studentized.residuals)) %>%
    ggplot(aes(sample = studentized.residuals)) +
    stat_qq_band(bandType = "pointwise", fill = "#8DA0CB", alpha = 0.4) +
    stat_qq_line(colour = "#8DA0CB") +
    stat_qq_point() +
    ggtitle("Q-Q plot of studentized residuals") +
    xlab("Normal quantiles") +
    ylab("Studentized residuals") +
    theme_light()
  
  
  # Step 3: Inspect scatterplot of studentized residuals against predicted values of the svyglm model
  studresscatter <- subset(disdf, !is.na(disdf$studentized.residuals)) %>%
    ggplot(aes(fitted.values, studentized.residuals)) + 
    geom_point() + 
    ggtitle("Scatterplot of studentized residuals against predicted values") +
    geom_smooth(method = "loess", colour = "Blue") + 
    labs(x = "Fitted Values", y = "Studentized Residuals") + 
    theme_light()
  
  
  # Step 4: Print plots
  ggarrange(studreshist, studresqq, studresscatter,
            ncol = 2, nrow = 2) %>% 
    annotate_figure(top = text_grob(paste("Non-normality/heteroscedasticity of residuals of", 
                                          deparse(substitute(fit))),
                                    color = "black", 
                                    face = "bold", 
                                    size = 15)) %>% print
  
}


