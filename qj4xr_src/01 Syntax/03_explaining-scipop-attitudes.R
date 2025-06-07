# ANALYSIS 2: EXPLAINING SCIENCE-RELATED POPULIST ATTITUDES #####################################################

## Preparations -------------------------------------------------------------------------------------------------

### Transform inhabitant count of residence municipality (i.e. urbanity) ----------------------------------------

# Inhabitant count per municipality is not normally distributed but follows a lognormal distribution 
# (see the paper of Decker, Kerkhoff, & Moses, 2007: "Global Patterns of City Size Distributions and Their 
# Fundamental Drivers", p. 4, Figure 4, who proved this for Swiss municipalities). This corresponds actually 
# with Zipf's Law:
svyhist(~urbanity, design = bar.design,
        main = "Histogram of inhabitant counts of residence municipality",
        probability = F, breaks = 300, xlim = c(0, 50000), ylim = c(0, 150))



# Therefore, we transform the urbanity into its log:
bar2019pop %<>% mutate(urbanity.log = log(urbanity))

bar2019pop$urbanity.log %<>% set_label(label = "Urbanity (log)")



# Now the distribution comes closer to a normal distribution:
bar.design <- svydesign(id = ~0, data = bar2019pop, weights = ~weight)

svyhist(~urbanity.log, design = bar.design,
        main = "Histogram of inhabitant counts of residence municipality",
        probability = F, breaks = 50, xlim = c(4, 14))




### Specify survey designs based on data with complete cases ----------------------------------------------------
bar.design.scipopgoertz <- bar2019pop %>% filter_at(vars(scipopgoertz, age, gender, education.comp, 
                                                         education.uni, sciprox.score, urbanity.log, 
                                                         languageregion.ger, languageregion.ita, polorientation,
                                                         religiosity, interestscience, sciliteracy, 
                                                         trustscience, trustscientists), 
                                                    all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopppl <- bar2019pop %>% filter_at(vars(scipopppl, age, gender, education.comp, 
                                                      education.uni, sciprox.score, urbanity.log, 
                                                      languageregion.ger, languageregion.ita, polorientation,
                                                      religiosity, interestscience, sciliteracy, 
                                                      trustscience, trustscientists), 
                                                 all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopeli <- bar2019pop %>% filter_at(vars(scipopeli, age, gender, education.comp, 
                                                      education.uni, sciprox.score, urbanity.log, 
                                                      languageregion.ger, languageregion.ita, polorientation,
                                                      religiosity, interestscience, sciliteracy, 
                                                      trustscience, trustscientists), 
                                                 all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopdec <- bar2019pop %>% filter_at(vars(scipopdec, age, gender, education.comp, 
                                                      education.uni, sciprox.score, urbanity.log, 
                                                      languageregion.ger, languageregion.ita, polorientation,
                                                      religiosity, interestscience, sciliteracy, 
                                                      trustscience, trustscientists), 
                                                 all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipoptru <- bar2019pop %>% filter_at(vars(scipoptru, age, gender, education.comp, 
                                                      education.uni, sciprox.score, urbanity.log, 
                                                      languageregion.ger, languageregion.ita, polorientation,
                                                      religiosity, interestscience, sciliteracy, 
                                                      trustscience, trustscientists), 
                                                 all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)




## Fit models ---------------------------------------------------------------------------------------------------

# Model 1: DV = SciPop Score; IVs: Sociodems
# Model 2: DV = SciPop Score; IVs: Sociodems + Non-science-related atts
# Model 3: DV = SciPop Score; IVs: Sociodems + Non-science-related atts + Science-related atts

### Model 1: DV = SciPop Score; IVs: Sociodems ------------------------------------------------------------------

# IVs: 
# - age, gender, education, proximity to science, urbanity of residence place, Swiss region 

# Weighted regression model for SciPop Score (Goertz approach)
m1.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni +
                                             sciprox.score + urbanity.log + 
                                             languageregion.ger + languageregion.ita,
                                     design = bar.design.scipopgoertz, family = gaussian, na.action = na.omit)



# Weighted regression model for Ordinary People subscale score (mean)
m1.ppl.svyglm.fit <- svyglm(scipopppl ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + 
                                    languageregion.ger + languageregion.ita,
                            design = bar.design.scipopppl, family = gaussian, na.action = na.omit)



# Weighted regression model for Academic Elite subscale score (mean)
m1.eli.svyglm.fit <- svyglm(scipopeli ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + 
                                    languageregion.ger + languageregion.ita,
                            design = bar.design.scipopeli, family = gaussian, na.action = na.omit)



# Weighted regression model for Decision-Making subscale score (mean)
m1.dec.svyglm.fit <- svyglm(scipopdec ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + 
                                    languageregion.ger + languageregion.ita,
                            design = bar.design.scipopdec, family = gaussian, na.action = na.omit)



# Weighted regression model for Truth-Speaking subscale score (mean)
m1.tru.svyglm.fit <- svyglm(scipoptru ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + 
                                    languageregion.ger + languageregion.ita,
                            design = bar.design.scipoptru, family = gaussian, na.action = na.omit)




### Model 2: DV = SciPop Score; IVs: Sociodems + Non-science-related atts ---------------------------------------

# IVs: 
# - age, gender, education, proximity to science, urbanity of residence place, Swiss region
# - political orientation, religiosity

# Weighted regression model for SciPop Score (Goertz approach)
m2.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni +
                                             sciprox.score + urbanity.log + languageregion.ger + 
                                             languageregion.ita + 
                                             polorientation + religiosity,
                                     design = bar.design.scipopgoertz, family = gaussian, na.action = na.omit)



# Weighted regression model for Ordinary People subscale score (mean)
m2.ppl.svyglm.fit <- svyglm(scipopppl ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity,
                            design = bar.design.scipopppl, family = gaussian, na.action = na.omit)



# Weighted regression model for Academic Elite subscale score (mean)
m2.eli.svyglm.fit <- svyglm(scipopeli ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity,
                            design = bar.design.scipopeli, family = gaussian, na.action = na.omit)



# Weighted regression model for Decision-Making subscale score (mean)
m2.dec.svyglm.fit <- svyglm(scipopdec ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity,
                            design = bar.design.scipopdec, family = gaussian, na.action = na.omit)



# Weighted regression model for Truth-Speaking subscale score (mean)
m2.tru.svyglm.fit <- svyglm(scipoptru ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity,
                            design = bar.design.scipoptru, family = gaussian, na.action = na.omit)




### Model 3: DV = SciPop Score; IVs: Sociodems + Non-science-related atts + Science-related atts ----------------

# IVs: 
# - age, gender, education, proximity to science, urbanity of residence place, Swiss region
# - political orientation, religiosity
# - interest in science, scientific literacy, trust in science, trust in scientists

# Weighted regression model for SciPop Score (Goertz approach)
m3.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni +
                                             sciprox.score + urbanity.log + languageregion.ger + 
                                             languageregion.ita + 
                                             polorientation + religiosity + interestscience + sciliteracy + 
                                             trustscience + trustscientists,
                                     design = bar.design.scipopgoertz, family = gaussian, na.action = na.omit)



# Weighted regression model for Ordinary People subscale score (mean)
m3.ppl.svyglm.fit <- svyglm(scipopppl ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity + interestscience + sciliteracy + 
                                    trustscience + trustscientists,
                            design = bar.design.scipopppl, family = gaussian, na.action = na.omit)



# Weighted regression model for Academic Elite subscale score (mean)
m3.eli.svyglm.fit <- svyglm(scipopeli ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity + interestscience + sciliteracy + 
                                    trustscience + trustscientists,
                            design = bar.design.scipopeli, family = gaussian, na.action = na.omit)



# Weighted regression model for Decision-Making subscale score (mean)
m3.dec.svyglm.fit <- svyglm(scipopdec ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity + interestscience + sciliteracy + 
                                    trustscience + trustscientists,
                            design = bar.design.scipopdec, family = gaussian, na.action = na.omit)



# Weighted regression model for Truth-Speaking subscale score (mean)
m3.tru.svyglm.fit <- svyglm(scipoptru ~ age + gender + education.comp + education.uni +
                                    sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                    polorientation + religiosity + interestscience + sciliteracy + 
                                    trustscience + trustscientists,
                            design = bar.design.scipoptru, family = gaussian, na.action = na.omit)




## Inspect model statistics -------------------------------------------------------------------------------------

### SciPop Score models -----------------------------------------------------------------------------------------

# Model 1: Sociodems
summ(model = m1.scipopgoertz.svyglm.fit,
     binary.inputs = "full", # --> Argument to pass to scale_mod() to specify how to treat binary 
     #     variables. Default is "0/1"; "0/1" keeps original scale; "-0.5,0.5" 
     #     rescales 0 as -0.5 and 1 as 0.5; "center" subtracts the mean and 
     #    "full" subtracts the mean and divides by 2 SDs (because n.sd = 2).
     n.sd = 2, # --> Standardization of estimates follows Gelman's (2008) suggestion to rescale the estimates 
     #     by dividing them by two standard deviations instead of just one. Resulting 
     #     coefficients are then directly comparable for untransformed binary predictors.
     #     https://strengejacke.github.io/sjPlot/reference/tab_model.html 
     transform.response = F,
     confint = F, # <- toggle T for confidence intervals
     vifs = T,
     scale = F,   # <- toggle T for standardized regression coefficients
     model.info = T,
     model.fit = T,
     digits = 4)

m1.scipopgoertz.svyglm.fit$aic



# Model 2: Sociodems + Non-science-related atts
summ(model = m2.scipopgoertz.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m2.scipopgoertz.svyglm.fit$aic



# Model 3: Sociodems + Non-science-related atts + science-related atts
summ(model = m3.scipopgoertz.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.scipopgoertz.svyglm.fit$aic




### Ordinary People subscale score models -----------------------------------------------------------------------

# Model 1: Sociodems
summ(model = m1.ppl.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m1.ppl.svyglm.fit$aic



# Model 2: Sociodems + Non-science-related atts
summ(model = m2.ppl.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m2.ppl.svyglm.fit$aic



# Model 3: Sociodems + Non-science-related atts + science-related atts
summ(model = m3.ppl.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.ppl.svyglm.fit$aic




### Academic Elite subscale score models ------------------------------------------------------------------------

# Model 1: Sociodems
summ(model = m1.eli.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m1.eli.svyglm.fit$aic



# Model 2: Sociodems + Non-science-related atts
summ(model = m2.eli.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m2.eli.svyglm.fit$aic



# Model 3: Sociodems + Non-science-related atts + science-related atts
summ(model = m3.eli.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.eli.svyglm.fit$aic




### Decision-Making subscale score models -----------------------------------------------------------------------

# Model 1: Sociodems
summ(model = m1.dec.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m1.dec.svyglm.fit$aic



# Model 2: Sociodems + Non-science-related atts
summ(model = m2.dec.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m2.dec.svyglm.fit$aic



# Model 3: Sociodems + Non-science-related atts + science-related atts
summ(model = m3.dec.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.dec.svyglm.fit$aic




### Truth-Speaking subscale score models ------------------------------------------------------------------------

# Model 1: Sociodems
summ(model = m1.tru.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m1.tru.svyglm.fit$aic



# Model 2: Sociodems + Non-science-related atts
summ(model = m2.tru.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m2.tru.svyglm.fit$aic



# Model 3: Sociodems + Non-science-related atts + science-related atts
summ(model = m3.tru.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = T, scale = F, model.info = T, model.fit = T, digits = 4)

m3.tru.svyglm.fit$aic




## Model comparisons --------------------------------------------------------------------------------------------

# Compare SciPop Score models
m0.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ 1, bar.design.scipopgoertz, family = gaussian)

anova(m0.scipopgoertz.svyglm.fit, m3.scipopgoertz.svyglm.fit, test = "F", method = "Wald")

anova(m1.scipopgoertz.svyglm.fit, m2.scipopgoertz.svyglm.fit, test = "F", method = "Wald")
anova(m2.scipopgoertz.svyglm.fit, m3.scipopgoertz.svyglm.fit, test = "F", method = "Wald")



# Compare ppl Score models
m0.ppl.svyglm.fit <- svyglm(scipopppl ~ 1, bar.design.scipopppl, family = gaussian)

anova(m0.ppl.svyglm.fit, m3.ppl.svyglm.fit, test = "F", method = "Wald")

anova(m1.ppl.svyglm.fit, m2.ppl.svyglm.fit, test = "F", method = "Wald")
anova(m2.ppl.svyglm.fit, m3.ppl.svyglm.fit, test = "F", method = "Wald")



# Compare eli Score models
m0.eli.svyglm.fit <- svyglm(scipopeli ~ 1, bar.design.scipopeli, family = gaussian)

anova(m0.eli.svyglm.fit, m3.eli.svyglm.fit, test = "F", method = "Wald")

anova(m1.eli.svyglm.fit, m2.eli.svyglm.fit, test = "F", method = "Wald")
anova(m2.eli.svyglm.fit, m3.eli.svyglm.fit, test = "F", method = "Wald")



# Compare dec Score models
m0.dec.svyglm.fit <- svyglm(scipopdec ~ 1, bar.design.scipopdec, family = gaussian)

anova(m0.dec.svyglm.fit, m3.dec.svyglm.fit, test = "F", method = "Wald")

anova(m1.dec.svyglm.fit, m2.dec.svyglm.fit, test = "F", method = "Wald")
anova(m2.dec.svyglm.fit, m3.dec.svyglm.fit, test = "F", method = "Wald")



# Compare tru Score models
m0.tru.svyglm.fit <- svyglm(scipoptru ~ 1, bar.design.scipoptru, family = gaussian)

anova(m0.tru.svyglm.fit, m3.tru.svyglm.fit, test = "F", method = "Wald")

anova(m1.tru.svyglm.fit, m2.tru.svyglm.fit, test = "F", method = "Wald")
anova(m2.tru.svyglm.fit, m3.tru.svyglm.fit, test = "F", method = "Wald")







## Model assumption checks --------------------------------------------------------------------------------------

# Do some assumption checks for multiple linear regression (e.g., see Field, 2012, p. 292). These are:

# (1) Multicollinearity
# (2) Non-normality/heteroscedasticity of residuals


### Assumption checks (1): Multicollinearity --------------------------------------------------------------------

# Specify model and inspect GVIFs

# Note: The VIF (usual collinearity diagnostic) may not be applicable to models with dummy regressors 
# constructed from a polytomous categorical variable or polynomial regressors (Fox, 2016: 357). Fox and 
# Monette (1992) introduced generalized variance inflation factor (GVIF) for these cases. As 
# education.uni/education.comp and languageregion.ger/languageregion.ita are such dummy regressors based 
# on polytomous categorial variables (i.e. education and languageregion), we should compute GVIFs. 
# car::vif() does that automatically.


# GVIFs of SciPop Score models
car::vif(m1.scipopgoertz.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m2.scipopgoertz.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m3.scipopgoertz.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))



# GVIFs of ppl Score models
car::vif(m1.ppl.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m2.ppl.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m3.ppl.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))



# GVIFs of eli Score models
car::vif(m1.eli.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m2.eli.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m3.eli.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))



# GVIFs of dec Score models
car::vif(m1.dec.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m2.dec.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m3.dec.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))



# GVIFs of tru Score models
car::vif(m1.tru.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m2.tru.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))
car::vif(m3.tru.svyglm.fit) %>% as.data.frame %>% rename(GVIF = deparse(substitute(.)))


# --> GVIFs are well below even the lowest rule-of-thumb-thresholds (Menard, 1995, suggests a threshold value
#     of 5). This means that we can keep all predictors in the model. See O'Brien (2007) for a discussion 
#     of (G)VIF threshold value rules.




### Assumption checks (2): Non-normality/heteroscedasticity of residuals ----------------------------------------

# Use custom function for that:
source("01 Syntax/04_distribution-checks.R")


# Non-normality/heteroscedasticity of residuals of SciPop Score models
distributionchecks(m1.scipopgoertz.svyglm.fit)
distributionchecks(m2.scipopgoertz.svyglm.fit)
distributionchecks(m3.scipopgoertz.svyglm.fit)



# Non-normality/heteroscedasticity of residuals of ppl Score models
distributionchecks(m1.ppl.svyglm.fit)
distributionchecks(m2.ppl.svyglm.fit)
distributionchecks(m3.ppl.svyglm.fit)



# Non-normality/heteroscedasticity of residuals of eli Score models
distributionchecks(m1.eli.svyglm.fit)
distributionchecks(m2.eli.svyglm.fit)
distributionchecks(m3.eli.svyglm.fit)



# Non-normality/heteroscedasticity of residuals of dec Score models
distributionchecks(m1.dec.svyglm.fit)
distributionchecks(m2.dec.svyglm.fit)
distributionchecks(m3.dec.svyglm.fit)



# Non-normality/heteroscedasticity of residuals of tru Score models
distributionchecks(m1.tru.svyglm.fit)
distributionchecks(m2.tru.svyglm.fit)
distributionchecks(m3.tru.svyglm.fit)



# --> None of the plots suggests severe non-normality or heteroskedasticity of the residuals of the
#     regression model. But...
#     1) The QQ plots of scipopgoertz.svyglm.fit and eli.svyglm.fit hint that there are slightly 
#        more large residuals (> 2 and < -2) than expected. 
#     2) The dec.svyglm.fit seems to have a bit more positive residuals than expected.
#     
#     But we can generally assume that the normality and the homoscedasticity assumptions
#     are not violated for any of the models and do not need to modify them (or estimation methods) 
#     at this point.




# ADDITIONAL ANALYSIS A: TESTING NON-RESPONSE TO POLITICAL ORIENTATION MEASURE ----------------------------------

## Preparations -------------------------------------------------------------------------------------------------

### Build dummy variables ---------------------------------------------------------------------------------------
bar2019pop %<>% mutate(polorientation.left = case_when(polorientation.groups == 1 ~ 1,
                                                       is.na(polorientation) == T ~ 0,
                                                       TRUE ~ 0))

bar2019pop %<>% mutate(polorientation.mdrt = case_when(polorientation.groups == 2 ~ 1,
                                                       is.na(polorientation) == T ~ 0,
                                                       TRUE ~ 0))

bar2019pop %<>% mutate(polorientation.rght = case_when(polorientation.groups == 3 ~ 1,
                                                       is.na(polorientation) == T ~ 0,
                                                       TRUE ~ 0))

bar2019pop %<>% mutate(polorientation.nonr = case_when(is.na(polorientation) == T ~ 1,
                                                       TRUE ~ 0))

bar2019pop %<>% var_labels(polorientation.left = "Political orientation: left-leaning",
                           polorientation.mdrt = "Political orientation: moderate",
                           polorientation.rght = "Political orientation: right-leaning",
                           polorientation.nonr = "Political orientation: no response/don't know")




### Build additional categorical variable which includes no response / don't know -------------------------------
bar2019pop %<>% mutate(polorientation.groups.nonr.c = case_when(polorientation >= 1 & polorientation <= 3 ~ 1,
                                                                polorientation == 4 ~ 2, 
                                                                polorientation >= 5 & polorientation <= 7 ~ 3,
                                                                is.na(polorientation) == T ~ -9))

bar2019pop %<>% var_labels(polorientation.groups.nonr.c = "Political orientation (4 groups incl non-response)")

bar2019pop$polorientation.groups.nonr.c %<>% set_labels(labels = c("left-leaning (1/2/3)" = 1, 
                                                                   "moderate (4)" = 2,
                                                                   "right-leaning (5/6/7)" = 3,
                                                                   "no response / don't know" = -9))




### Build additional binary variable measuring no response / don't know vs any response -------------------------
bar2019pop %<>% mutate(polorientation.groups.nonr.b = case_when(is.na(polorientation) == T ~ 0,
                                                                TRUE ~ 1))

bar2019pop %<>% var_labels(polorientation.groups.nonr.b = "Political orientation (2 groups: non-responders vs responders)")

bar2019pop$polorientation.groups.nonr.b %<>% set_labels(labels = c("non-responders" = 0, 
                                                                   "responders" = 1))




### Update survey design ----------------------------------------------------------------------------------------
bar.design <- svydesign(id = ~0, data = bar2019pop, weights = ~weight)




### Specify survey designs based on data with complete cases for regressions ------------------------------------
bar.design.scipopgoertz.a <- bar2019pop %>% filter_at(vars(scipopgoertz, age, gender, education.comp, 
                                                           education.uni, sciprox.score, urbanity.log, 
                                                           languageregion.ger, languageregion.ita, 
                                                           polorientation.left, polorientation.mdrt,
                                                           polorientation.rght, polorientation.nonr,
                                                           religiosity, interestscience, sciliteracy, 
                                                           trustscience, trustscientists), 
                                                      all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopppl.a <- bar2019pop %>% filter_at(vars(scipopppl, age, gender, education.comp, 
                                                        education.uni, sciprox.score, urbanity.log, 
                                                        languageregion.ger, languageregion.ita,
                                                        polorientation.left, polorientation.mdrt,
                                                        polorientation.rght, polorientation.nonr,
                                                        religiosity, interestscience, sciliteracy, 
                                                        trustscience, trustscientists), 
                                                   all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopeli.a <- bar2019pop %>% filter_at(vars(scipopeli, age, gender, education.comp, 
                                                        education.uni, sciprox.score, urbanity.log, 
                                                        languageregion.ger, languageregion.ita,
                                                        polorientation.left, polorientation.mdrt,
                                                        polorientation.rght, polorientation.nonr,
                                                        religiosity, interestscience, sciliteracy, 
                                                        trustscience, trustscientists), 
                                                   all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipopdec.a <- bar2019pop %>% filter_at(vars(scipopdec, age, gender, education.comp, 
                                                        education.uni, sciprox.score, urbanity.log, 
                                                        languageregion.ger, languageregion.ita,
                                                        polorientation.left, polorientation.mdrt,
                                                        polorientation.rght, polorientation.nonr,
                                                        religiosity, interestscience, sciliteracy, 
                                                        trustscience, trustscientists), 
                                                   all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)

bar.design.scipoptru.a <- bar2019pop %>% filter_at(vars(scipoptru, age, gender, education.comp, 
                                                        education.uni, sciprox.score, urbanity.log, 
                                                        languageregion.ger, languageregion.ita,
                                                        polorientation.left, polorientation.mdrt,
                                                        polorientation.rght, polorientation.nonr,
                                                        religiosity, interestscience, sciliteracy, 
                                                        trustscience, trustscientists), 
                                                   all_vars(!is.na(.))) %>%
        svydesign(id = ~0, data = ., weights = ~weight)





## Descriptive analyses -----------------------------------------------------------------------------------------

### Weighted tables/barplots of SciPop Scores in political orientation groups incl non-response (c) -------------
scipop.pol.nonr.c.df <- get_labels(bar2019pop$polorientation.groups.nonr.c, drop.unused = T) %>% 
        as.data.frame() %>%
        cbind(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups.nonr.c, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(as.data.frame(round(svytable(~polorientation.groups.nonr.c, bar.design), 0))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups.nonr.c, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        set_colnames(c("pol", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
        melt() %>%
        data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
        dcast(factor(pol, levels = unique(pol)) + scale ~ val) %>%
        rename(pol = `factor(pol, levels = unique(pol))`) %>%
        mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")"))

scipop.pol.nonr.c.df


scipopbarplot.pol.nonr.c <- ggplot(scipop.pol.nonr.c.df, aes(x = factor(pol, unique(pol)), M, width = 0.7*(4/5))) + 
        geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "E", end = 0.9)(8)[5]) +
        geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(4/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
        coord_cartesian(ylim = c(1.5, 3)) +  
        labs(title = "Mean Goertzian SciPop scores across political orientations", x = "Political orientation", y = "Mean SciPop Score") +
        scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
              text = element_text(family = "serif"))

scipopbarplot.pol.nonr.c




### Weighted tables/barplots of SciPop Scores in political orientation groups incl non-response (b) -------------
scipop.pol.nonr.b.df <- get_labels(bar2019pop$polorientation.groups.nonr.b, drop.unused = T) %>% 
        as.data.frame() %>%
        cbind(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups.nonr.b, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(as.data.frame(round(svytable(~polorientation.groups.nonr.b, bar.design), 0))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups.nonr.b, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        set_colnames(c("pol", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
        melt() %>%
        data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
        dcast(factor(pol, levels = unique(pol)) + scale ~ val) %>%
        rename(pol = `factor(pol, levels = unique(pol))`) %>%
        mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")"))

scipop.pol.nonr.b.df


scipopbarplot.pol.nonr.b <- ggplot(scipop.pol.nonr.b.df, aes(x = factor(pol, unique(pol)), M, width = 0.7*(2/5))) + 
        geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "E", end = 0.9)(8)[5]) +
        geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(2/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
        coord_cartesian(ylim = c(1.5, 3)) +  
        labs(title = "Mean Goertzian SciPop scores across political orientations", x = "Political orientation", y = "Mean SciPop Score") +
        scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
              text = element_text(family = "serif"))

scipopbarplot.pol.nonr.b




### Weighted tables/barplots of subscale scores in political orientation groups incl non-response (c) -----------
subscale.pol.nonr.c.df <- get_labels(bar2019pop$polorientation.groups.nonr.c) %>% 
        as.data.frame() %>%
        cbind(as.data.frame(svyby(~scipopppl, ~polorientation.groups.nonr.c, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopppl, ~polorientation.groups.nonr.c, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipopeli, ~polorientation.groups.nonr.c, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopeli, ~polorientation.groups.nonr.c, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipopdec, ~polorientation.groups.nonr.c, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopdec, ~polorientation.groups.nonr.c, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipoptru, ~polorientation.groups.nonr.c, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipoptru, ~polorientation.groups.nonr.c, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(round(svytable(~polorientation.groups.nonr.c, bar.design), 0))[-1]) %>%
        set_colnames(c("pol", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                       "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
        mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")")) %>%
        subset(select = -14) %>%
        melt() %>%
        data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
        dcast(factor(pol, levels = unique(pol)) + factor(scale, levels = unique(scale)) ~ val) %>%
        rename(pol = `factor(pol, levels = unique(pol))`, scale = `factor(scale, levels = unique(scale))`)

subscale.pol.nonr.c.df


subscalebarplot.pol.nonr.c <- ggplot(subscale.pol.nonr.c.df, aes(x = factor(pol, levels = unique(pol)), 
                                                                 y = M, group = scale, fill = scale, width = 0.7*(4/5))) + 
        geom_bar(stat = "identity", position = position_dodge(0.7)) +
        geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                      width = 0.3*(4/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
        coord_cartesian(ylim = c(2, 4)) +
        labs(title = "Mean subscale scores across political orientations", x = "Political orientation", y = "Mean subscale score") +
        scale_y_continuous(breaks = seq(1, 4, 0.25)) +
        scale_fill_viridis_d(option = "viridis", begin = 0.4, end = 0.9, name = "Dimension",
                             breaks = c("ppl", "eli", "dec", "tru"),
                             labels = c("Conceptions of the ordinary people", 
                                        "Conceptions of the academic elite",
                                        "Demands for decision-making sovereignty",
                                        "Demands for truth-speaking sovereignty")) + 
        theme_bw() + 
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
              text = element_text(family = "serif"))

subscalebarplot.pol.nonr.c




### Weighted tables/barplots of subscale scores in political orientation groups incl non-response (b) -----------
subscale.pol.nonr.b.df <- get_labels(bar2019pop$polorientation.groups.nonr.b) %>% 
        as.data.frame() %>%
        cbind(as.data.frame(svyby(~scipopppl, ~polorientation.groups.nonr.b, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopppl, ~polorientation.groups.nonr.b, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipopeli, ~polorientation.groups.nonr.b, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopeli, ~polorientation.groups.nonr.b, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipopdec, ~polorientation.groups.nonr.b, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipopdec, ~polorientation.groups.nonr.b, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(svyby(~scipoptru, ~polorientation.groups.nonr.b, bar.design, svymean, na.rm = T))[-1]) %>%
        cbind(sqrt(as.data.frame(svyby(~scipoptru, ~polorientation.groups.nonr.b, bar.design, svyvar, na.rm = T)[[2]]))) %>%
        cbind(as.data.frame(round(svytable(~polorientation.groups.nonr.b, bar.design), 0))[-1]) %>%
        set_colnames(c("pol", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                       "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
        mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")")) %>%
        subset(select = -14) %>%
        melt() %>%
        data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
        dcast(factor(pol, levels = unique(pol)) + factor(scale, levels = unique(scale)) ~ val) %>%
        rename(pol = `factor(pol, levels = unique(pol))`, scale = `factor(scale, levels = unique(scale))`)

subscale.pol.nonr.b.df


subscalebarplot.pol.nonr.b <- ggplot(subscale.pol.nonr.b.df, aes(x = factor(pol, levels = unique(pol)), 
                                                                 y = M, group = scale, fill = scale, width = 0.7*(2/5))) + 
        geom_bar(stat = "identity", position = position_dodge(0.7)) +
        geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                      width = 0.3*(2/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
        coord_cartesian(ylim = c(2, 4)) +
        labs(title = "Mean subscale scores across political orientations", x = "Political orientation", y = "Mean subscale score") +
        scale_y_continuous(breaks = seq(1, 4, 0.25)) +
        scale_fill_viridis_d(option = "viridis", begin = 0.4, end = 0.9, name = "Dimension",
                             breaks = c("ppl", "eli", "dec", "tru"),
                             labels = c("Conceptions of the ordinary people", 
                                        "Conceptions of the academic elite",
                                        "Demands for decision-making sovereignty",
                                        "Demands for truth-speaking sovereignty")) + 
        theme_bw() + 
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
              text = element_text(family = "serif"))

subscalebarplot.pol.nonr.b




## Test Model 3 with dummy-coded political orientation variables ------------------------------------------------

### Fit models --------------------------------------------------------------------------------------------------

# IVs: 
# - age, gender, education, proximity to science, urbanity of residence place, Swiss region
# - political orientation (dummy-coded: left-leaning / right-leaning / no response + don't know), 
#   religiosity
# - interest in science, scientific literacy, trust in science, trust in scientists

# SciPop Score
m3.scipopgoertz.svyglm.fit.a <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni +
                                               sciprox.score + urbanity.log + languageregion.ger + 
                                               languageregion.ita + 
                                               polorientation.left + # Use polorientation.mdrt as reference group
                                               polorientation.rght + polorientation.nonr + 
                                               religiosity + interestscience + sciliteracy + 
                                               trustscience + trustscientists,
                                       design = bar.design.scipopgoertz.a, 
                                       family = gaussian, 
                                       na.action = na.omit)



# Ordinary People subscale score
m3.ppl.svyglm.fit.a <- svyglm(scipopppl ~ age + gender + education.comp + education.uni +
                                      sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                      polorientation.left + # Use polorientation.mdrt as reference group
                                      polorientation.rght + polorientation.nonr + 
                                      religiosity + interestscience + sciliteracy + 
                                      trustscience + trustscientists,
                              design = bar.design.scipopppl.a, 
                              family = gaussian, 
                              na.action = na.omit)



# Academic Elite subscale score
m3.eli.svyglm.fit.a <- svyglm(scipopeli ~ age + gender + education.comp + education.uni +
                                      sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                      polorientation.left + # Use polorientation.mdrt as reference group
                                      polorientation.rght + polorientation.nonr + 
                                      religiosity + interestscience + sciliteracy + 
                                      trustscience + trustscientists,
                              design = bar.design.scipopeli.a, 
                              family = gaussian, 
                              na.action = na.omit)



# Decision-Making subscale score
m3.dec.svyglm.fit.a <- svyglm(scipopdec ~ age + gender + education.comp + education.uni +
                                      sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                      polorientation.left + # Use polorientation.mdrt as reference group
                                      polorientation.rght + polorientation.nonr + 
                                      religiosity + interestscience + sciliteracy + 
                                      trustscience + trustscientists,
                              design = bar.design.scipopdec.a, 
                              family = gaussian, 
                              na.action = na.omit)



# Truth-Speaking subscale score
m3.tru.svyglm.fit.a <- svyglm(scipoptru ~ age + gender + education.comp + education.uni +
                                      sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + 
                                      polorientation.left + # Use polorientation.mdrt as reference group
                                      polorientation.rght + polorientation.nonr + 
                                      religiosity + interestscience + sciliteracy + 
                                      trustscience + trustscientists,
                              design = bar.design.scipoptru.a, 
                              family = gaussian, 
                              na.action = na.omit)




### Inspect model statistics ------------------------------------------------------------------------------------

#### SciPop Score -----------------------------------------------------------------------------------------------
summ(model = m3.scipopgoertz.svyglm.fit.a, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T, model.fit = T, digits = 4)

m3.scipopgoertz.svyglm.fit.a$aic




#### Ordinary People subscale score -----------------------------------------------------------------------------
summ(model = m3.ppl.svyglm.fit.a, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T, model.fit = T, digits = 4)

m3.ppl.svyglm.fit.a$aic




#### Academic Elite subscale score ------------------------------------------------------------------------------
summ(model = m3.eli.svyglm.fit.a, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T, model.fit = T, digits = 4)

m3.eli.svyglm.fit.a$aic




#### Decision-Making subscale score -----------------------------------------------------------------------------
summ(model = m3.dec.svyglm.fit.a, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T, model.fit = T, digits = 4)

m3.dec.svyglm.fit.a$aic




#### Truth-Speaking subscale score ------------------------------------------------------------------------------
summ(model = m3.tru.svyglm.fit.a, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T, model.fit = T, digits = 4)

m3.tru.svyglm.fit.a$aic




# ADDITIONAL ANALYSIS B: TESTING A U-SHAPED RELATIONSHIP BETWEEN SCIPOP AND POLITICAL ORIENTATION ---------------

## 1st test: Quadratic regression on weighted data --------------------------------------------------------------
bar2019pop$polorientation2 <- bar2019pop$polorientation^2

bar.design <- svydesign(id = ~0, data = bar2019pop, weights = ~weight)

quad.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ age + gender + education.comp + education.uni + 
                                               sciprox.score + urbanity.log + languageregion.ger + 
                                               languageregion.ita + polorientation + polorientation2 + 
                                               religiosity + interestscience + sciliteracy + 
                                               trustscience + trustscientists,
                                       design = bar.design, family = gaussian, na.action = na.omit)

summ(model = quad.scipopgoertz.svyglm.fit, binary.inputs = "full", n.sd = 2, transform.response = F,
     confint = F, vifs = F, scale = F, model.info = T,  model.fit = T, digits = 4)

quad.scipopgoertz.svyglm.fit$aic

m0.scipopgoertz.svyglm.fit <- svyglm(scipopgoertz ~ 1, bar.design, family = gaussian, na.action = na.omit)

anova(m0.scipopgoertz.svyglm.fit, quad.scipopgoertz.svyglm.fit, test = "F", method = "Wald")




## 2nd test: Two-lines test on unweighted data (Simonsohn, 2018) ------------------------------------------------
source("01 Syntax/05_two-lines-test-simonsohn-2018.R")

# To test if x has u-shaped effect on y, run: y~x
# To control for x2 linearly, run: y~x+x2
# To test if x2 has a u-shaped effect on y controlling for x, run: y~x2+x
# To control for x1*x2 interaction: run: y~x+x1*x2
twolines(scipopgoertz ~ polorientation + age + gender + education.comp + education.uni +
                 sciprox.score + urbanity.log + languageregion.ger + languageregion.ita + religiosity + 
                 interestscience + sciliteracy + trustscience + trustscientists, 
         graph = 1, link = "gaussian", data = bar2019pop) # Export 7 x 10 inch landscape



