# SENSITIVITY TESTS FOR TABLE 2 (RERUN OF ANAYLSES WITH TRIMMED WEIGHTS AND BOLLEN SCIPOP SCORE) ################

## Re-run analyses with trimmed weights -------------------------------------------------------------------------

# Preferably, one chooses a trimming approach that does not apply a general rule of thumb but relies on 
# sample-based cut-off values. One such approach is  "the inter-quartile range (IQR) method, which attempts 
# to control the trimming by considering the variation in the weights. When determining a cut-point, this method 
# uses the median of the weights as well as the spread, as measured through the IQR." (van de Kerckhove, 
# Mohadjer, & Krenzke, 2014). This method has been repeatedly used, for example by van de Kerckhove et al. 
# (2014), Potter & Zheng (2015), and Chowdhury, Khare, & Wolter (2007). 
# 
# IQR trimming values are computed as follows: 
#  -	upper cutoff: median of weights + k * IQR
#  -	lower cutoff: median of weights - 1/(k * IQR)
# 
# For k, we choose k = 6, which was also used by Chowdhury et al. (2007), for example. This is a relatively 
# conservative value, which is advisable, because weight trimming can increase nonsampling biases (i.e. the 
# mean squared error of the estimates) and should therefore be used cautiously (Biemer & Christ, 2008).


### Trim weights ------------------------------------------------------------------------------------------------

# Define k
k <- 6


# Compute cutoff
cutoff <- median(bar2019$WT) + k * (quantile(bar2019$WT, 0.75) - quantile(bar2019$WT, 0.25)) %>% as.numeric()


# Trim weights, forcing them to be within the specified interval 
# (with strict = T, which calls the function repeatedly)
bar.design.scipopgoertz.trim <- trimWeights(bar.design.scipopgoertz, upper = cutoff, lower = (1/cutoff), strict = T)
bar.design.scipopppl.trim <- trimWeights(bar.design.scipopppl, upper = cutoff, lower = (1/cutoff), strict = T)
bar.design.scipopeli.trim <- trimWeights(bar.design.scipopeli, upper = cutoff, lower = (1/cutoff), strict = T)
bar.design.scipopdec.trim <- trimWeights(bar.design.scipopdec, upper = cutoff, lower = (1/cutoff), strict = T)
bar.design.scipoptru.trim <- trimWeights(bar.design.scipoptru, upper = cutoff, lower = (1/cutoff), strict = T)


# Inspect weight range:
summary(weights(bar.design.scipopgoertz))
summary(weights(bar.design.scipopgoertz.trim))




### Fit models with trimmed weights -----------------------------------------------------------------------------

# Weighted regression model for SciPop Score (Goertz approach)
m3.scipopgoertz.trim.svyglm.fit <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni +
                                            sciprox.score + urbanity.log + languageregion.ger + 
                                            languageregion.ita + polorientation + religiosity + 
                                            interestscience + sciliteracy + trustscience + trustscientists,
                                          design = bar.design.scipopgoertz.trim, family = gaussian, na.action = na.omit)



# Weighted regression model for Ordinary People subscale score (mean)
m3.ppl.trim.svyglm.fit <- svyglm(scipopppl ~ age + gender + education.comp + education.uni +
                                   sciprox.score + urbanity.log + languageregion.ger + 
                                   languageregion.ita + polorientation + religiosity + 
                                   interestscience + sciliteracy + trustscience + trustscientists,
                                 design = bar.design.scipopppl.trim, family = gaussian, na.action = na.omit)



# Weighted regression model for Academic Elite subscale score (mean)
m3.eli.trim.svyglm.fit <- svyglm(scipopeli ~ age + gender + education.comp + education.uni +
                                   sciprox.score + urbanity.log + languageregion.ger + 
                                   languageregion.ita + polorientation + religiosity + 
                                   interestscience + sciliteracy + trustscience + trustscientists,
                                 design = bar.design.scipopeli.trim, family = gaussian, na.action = na.omit)



# Weighted regression model for Decision-Making subscale score (mean)
m3.dec.trim.svyglm.fit <- svyglm(scipopdec ~ age + gender + education.comp + education.uni +
                                   sciprox.score + urbanity.log + languageregion.ger + 
                                   languageregion.ita + polorientation + religiosity + 
                                   interestscience + sciliteracy + trustscience + trustscientists,
                                 design = bar.design.scipopdec.trim, family = gaussian, na.action = na.omit)



# Weighted regression model for Truth-Speaking subscale score (mean)
m3.tru.trim.svyglm.fit <- svyglm(scipoptru ~ age + gender + education.comp + education.uni +
                                   sciprox.score + urbanity.log + languageregion.ger + 
                                   languageregion.ita + polorientation + religiosity + 
                                   interestscience + sciliteracy + trustscience + trustscientists,
                                 design = bar.design.scipoptru.trim, family = gaussian, na.action = na.omit)





### Model summaries ---------------------------------------------------------------------------------------------

summ(model = m3.scipopgoertz.trim.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.scipopgoertz.trim.svyglm.fit$aic


summ(model = m3.ppl.trim.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.ppl.trim.svyglm.fit$aic


summ(model = m3.eli.trim.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.eli.trim.svyglm.fit$aic


summ(model = m3.dec.trim.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.dec.trim.svyglm.fit$aic


summ(model = m3.tru.trim.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.tru.trim.svyglm.fit$aic



### F tests -----------------------------------------------------------------------------------------------------
m0.scipopgoertz.trim.svyglm.fit <- svyglm(scipopgoertz ~ 1, bar.design.scipopgoertz.trim, family = gaussian)
anova(m0.scipopgoertz.trim.svyglm.fit, m3.scipopgoertz.trim.svyglm.fit, test = "F", method = "Wald")


m0.ppl.trim.svyglm.fit <- svyglm(scipopppl ~ 1, bar.design.scipopppl.trim, family = gaussian)
anova(m0.ppl.trim.svyglm.fit, m3.ppl.trim.svyglm.fit, test = "F", method = "Wald")


m0.eli.trim.svyglm.fit <- svyglm(scipopeli ~ 1, bar.design.scipopeli.trim, family = gaussian)
anova(m0.eli.trim.svyglm.fit, m3.eli.trim.svyglm.fit, test = "F", method = "Wald")


m0.dec.trim.svyglm.fit <- svyglm(scipopdec ~ 1, bar.design.scipopdec.trim, family = gaussian)
anova(m0.dec.trim.svyglm.fit, m3.dec.trim.svyglm.fit, test = "F", method = "Wald")


m0.tru.trim.svyglm.fit <- svyglm(scipoptru ~ 1, bar.design.scipoptru.trim, family = gaussian)
anova(m0.tru.trim.svyglm.fit, m3.tru.trim.svyglm.fit, test = "F", method = "Wald")






## Re-run analyses with Bollenian SciPop Score ------------------------------------------------------------------

# Specify survey design
bar.design.scipoprico <- bar2019pop %>% filter_at(vars(scipoprico, age, gender, education.comp, 
                                                       education.uni, sciprox.score, urbanity.log, 
                                                       languageregion.ger, languageregion.ita, polorientation,
                                                       religiosity, interestscience, sciliteracy, 
                                                       trustscience, trustscientists), 
                                                  all_vars(!is.na(.))) %>%
  svydesign(id = ~0, data = ., weights = ~weight)


# Fit model
m3.scipoprico.svyglm.fit <- svyglm(scipoprico ~ age + gender + education.comp + education.uni +
                                     sciprox.score + urbanity.log + languageregion.ger + 
                                     languageregion.ita + 
                                     polorientation + religiosity + interestscience + sciliteracy + 
                                     trustscience + trustscientists,
                                   design = bar.design.scipoprico, family = gaussian, na.action = na.omit)


# Model summary
summ(model = m3.scipoprico.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.scipoprico.svyglm.fit$aic


# F test
m0.scipoprico.svyglm.fit <- svyglm(scipoprico ~ 1, bar.design.scipoprico, family = gaussian)
anova(m0.scipoprico.svyglm.fit, m3.scipoprico.svyglm.fit, test = "F", method = "Wald")


