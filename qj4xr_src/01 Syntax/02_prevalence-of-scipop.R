# PREPARATIONS ##################################################################################################

## Load libraries -----------------------------------------------------------------------------------------------
library(data.table)
library(questionr)
library(survey)
library(jtools)
library(lavaan)
library(reshape2)
library(ggpubr)
library(viridis)




## Define functions ---------------------------------------------------------------------------------------------

### Weighted frequency tables for agreement items ---------------------------------------------------------------
freq.table <- function(var, w) {
  wtd.table(var, weights = w) %>%
    as.data.frame %>%
    mutate(Percent = round(100 * Freq/sum(.$Freq), 2)) %>%
    mutate(Label = case_when(Var1 == 1 ~ "1 (fully disagree)", Var1 == 2 ~ "2", Var1 == 3 ~ "3", Var1 == 4 ~ "4",
                             Var1 == 5 ~ "5 (fully agree)")) %>%
    cbind(factor(.$Label, levels = c("1 (fully disagree)", "2", "3", "4", "5 (fully agree)"))) %>%
    setnames(5, "Option") %>%
    select(Option, Freq, Percent)  
}




### Custom function for weighted Likert plot  -------------------------------------------------------------------
panel <- function(...){
  HH::panel.likert(...)
  vals <- list(...)
  df <- data.frame(x = vals$x, y = vals$y, groups = vals$groups)
  
  grps <- as.character(df$groups)
  for(i in 1:length(colnames(likertscipop.df))){
    grps <- sub(paste0('^', colnames(likertscipop.df)[i]), i, grps)
  }
  
  df <- df[order(df$y,grps),]
  
  df$correctX <- ave(df$x, df$y, FUN = function(x){
    x[x < 0] <- rev(cumsum(rev(x[x < 0]))) - x[x < 0]/2
    x[x > 0] <- cumsum(x[x > 0]) - x[x > 0]/2
    return(x)
  })
  
  subs <- sub(' Positive$', '', df$groups)
  collapse <- subs[-1] == subs[-length(subs)] & df$y[-1] == df$y[-length(df$y)]
  df$abs <- abs(df$x)
  df$abs[c(collapse, F)] <- df$abs[c(collapse, F)] + df$abs[c(F, collapse)]
  df$correctX[c(collapse, F)] <- 0
  df <- df[c(T, !collapse),]
  df$perc <- round(ave(df$abs, df$y, FUN = function(x){x/sum(x) * 100}), 1)
  df$perc <- paste0(df$perc,'%')
  df$perc[df$perc == "0%"] <- ""
  
  lattice::panel.text(x = df$correctX, y = df$y, label = df$perc, cex = 1.2, font = 1, col = "white")
}





# ANALYSIS 1: DESCRIPTIVE STATISTICS OF SCIENCE-RELATED POPULIST ATTITUDES ######################################

## Weighted Likert plots of individual items of the SciPop Scale ------------------------------------------------

# Get weighted frequencies per response option for each item and transform them into likert object
likertscipop.df <- rbind(freq.table(bar2019pop$scipop1.ppl, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop2.ppl, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop3.eli, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop4.eli, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop5.dec, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop6.dec, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop7.tru, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1),
                         freq.table(bar2019pop$scipop8.tru, w = bar2019pop$weights) %>% 
                           transpose(make.names = 1) %>% slice(1) %>% round(1))

rownames(likertscipop.df) <- c("What unites the ordinary people is that they\n trust their common sense in everyday life.",
                               "Ordinary people are of good and honest character.",
                               "Scientists are only after their own advantage.",
                               "Scientists are in cahoots with\n politics and business.",
                               "The people should have influence\n on the work of scientists.",
                               "People like me should be involved in decisions\n about the topics scientists research.",
                               "In case of doubt, one should rather trust the life experience\n of ordinary people than the estimations of scientists.",
                               "We should rely more on common sense\n and less on scientific studies.")

likertscipop <- HH::as.likert(likertscipop.df)



# Generate Likert plot
HH::plot.likert(likertscipop.df, as.percent = T, main = "", positive.order = F, key.border.white = F,
                col = c("#bdbdbd", "#8c8c8c", "#656565", "#2a2a2a", "#030303"),
                xlab = list(label = "%", cex = 1.2, hjust = 3.75), ylab = "", rightAxis = F, 
                scales = list(y = list(cex = 1.2), x = list(cex = 1.2, at = seq(-75, 75, 25))),
                auto.key.in = list(cex = 1.2),
                panel = panel) # (Export as 8 x 16 inch landscape PDF)




## Distribution of SciPop Score ---------------------------------------------------------------------------------

# Create weighted frequency table of SciPop Score (grouped)
wtd.table(bar2019pop$scipopgoertz.groups, weights = bar2019pop$weight) %>%
  as.data.frame %>%
  mutate(ValidPercent = round(100 * Freq/sum(.$Freq), 1)) %>%
  mutate(Label = case_when(Var1 == 1 ~ "1.00-2.00", Var1 == 2 ~ "2.01-3.99", Var1 == 3 ~ "4.00-5.00")) %>%
  cbind(factor(.$Label, levels = c("1.00-2.00", "2.01-3.99", "4.00-5.00"))) %>%
  setnames(5, "Option") %>%
  select(Option, Freq, ValidPercent)  




## Weighted means, SDs, and valid Ns of SciPop Score and subscale scores ----------------------------------------

# Descriptive info on survey weights
range(bar2019pop$weight, na.rm = T)
mean(bar2019pop$weight, na.rm = T)
sd(bar2019pop$weight, na.rm = T)



# Specify a survey design using the survey and jtools packages (so that we can run analyses with survey weights)
bar.design <- svydesign(id = ~0, data = bar2019pop, weights = ~weight)



# Compute values
rbind.data.frame(
  cbind(names(svymean(~scipopgoertz, bar.design, na.rm = T)),
        round(svymean(~scipopgoertz, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipopgoertz, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipopgoertz, bar.design)), 0)),
  cbind(names(svymean(~scipopppl, bar.design, na.rm = T)),
        round(svymean(~scipopppl, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipopppl, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipopppl, bar.design)), 0)),
  cbind(names(svymean(~scipop1.ppl, bar.design, na.rm = T)),
        round(svymean(~scipop1.ppl, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop1.ppl, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop1.ppl, bar.design)), 0)),
  cbind(names(svymean(~scipop2.ppl, bar.design, na.rm = T)),
        round(svymean(~scipop2.ppl, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop2.ppl, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop2.ppl, bar.design)), 0)),
  cbind(names(svymean(~scipopeli, bar.design, na.rm = T)),
        round(svymean(~scipopeli, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipopeli, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipopeli, bar.design)), 0)),
  cbind(names(svymean(~scipop3.eli, bar.design, na.rm = T)),
        round(svymean(~scipop3.eli, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop3.eli, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop3.eli, bar.design)), 0)),
  cbind(names(svymean(~scipop4.eli, bar.design, na.rm = T)),
        round(svymean(~scipop4.eli, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop4.eli, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop4.eli, bar.design)), 0)),
  cbind(names(svymean(~scipopdec, bar.design, na.rm = T)),
        round(svymean(~scipopdec, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipopdec, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipopdec, bar.design)), 0)),
  cbind(names(svymean(~scipop5.dec, bar.design, na.rm = T)),
        round(svymean(~scipop5.dec, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop5.dec, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop5.dec, bar.design)), 0)),
  cbind(names(svymean(~scipop6.dec, bar.design, na.rm = T)),
        round(svymean(~scipop6.dec, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop6.dec, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop6.dec, bar.design)), 0)),
  cbind(names(svymean(~scipoptru, bar.design, na.rm = T)),
        round(svymean(~scipoptru, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipoptru, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipoptru, bar.design)), 0)),
  cbind(names(svymean(~scipop7.tru, bar.design, na.rm = T)),
        round(svymean(~scipop7.tru, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop7.tru, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop7.tru, bar.design)), 0)),
  cbind(names(svymean(~scipop8.tru, bar.design, na.rm = T)),
        round(svymean(~scipop8.tru, bar.design, na.rm = T)[[1]], 2),
        round(svysd(~scipop8.tru, bar.design, na.rm = T)[[1]], 2),
        round(sum(svytable(~scipop8.tru, bar.design)), 0))) %>%
  rename(`Score/Item` = V1,
         `M` = V2,
         `SD` = V3,
         `Valid n` = V4) %>%
  set_rownames(c("Science-related populist attitudes", 
                 "Conceptions of the ordinary people", 
                 "'What unites the ordinary people (...).'",
                 "'Ordinary people are of good and (...).'",
                 "Conceptions of the academic elite", 
                 "'Scientists are only after their own (...).'",
                 "'Scientists are in cahoots with politics (...).'",
                 "Demands for decision-making sovereignty",
                 "'The people should have influence (...).'",
                 "'People like me should be involved (...).'",
                 "Demands for truth-speaking sovereignty",
                 "'In case of doubt, one should rather trust (...).'",
                 "'We should rely more on common sense (...).'"))




## Plot SciPop Scale and Subscale score distributions -----------------------------------------------------------
svyhist(~scipopgoertz, design = bar.design,
        main = "Science-related populist attitudes\n(Survey-weighted histogram of SciPop scores)",
        probability = T, breaks = seq(1, 5, by = 0.5))

svyhist(~scipopppl, design = bar.design,
        main = "Conceptions of the ordinary people\n(Survey-weighted histogram of PPL scores)",
        probability = T, breaks = seq(1, 5, by = 0.5))

svyhist(~scipopeli, design = bar.design,
        main = "Conceptions of the academic elite\n(Survey-weighted histogram of ELI scores)",
        probability = T, breaks = seq(1, 5, by = 0.5))

svyhist(~scipopdec, design = bar.design,
        main = "Demands for decision-making sovereignty\n(Survey-weighted histogram of DEC scores)",
        probability = T, breaks = seq(1, 5, by = 0.5))

svyhist(~scipoptru, design = bar.design,
        main = "Demands for truth-speaking sovereignty\n(Survey-weighted histogram of TRU scores)",
        probability = T, breaks = seq(1, 5, by = 0.5))




## Alpha scores of SciPop Scale and subscales -------------------------------------------------------------------
select(bar2019pop, scipop1.ppl:scipop8.tru) %>% alpha() # Cronbach's Alpha full scale: 0.76
select(bar2019pop, scipop1.ppl:scipop2.ppl) %>% alpha() # Cronbach's Alpha ppl subscale: 0.65
select(bar2019pop, scipop3.eli:scipop4.eli) %>% alpha() # Cronbach's Alpha eli subscale: 0.61
select(bar2019pop, scipop5.dec:scipop6.dec) %>% alpha() # Cronbach's Alpha dec subscale: 0.59
select(bar2019pop, scipop7.tru:scipop8.tru) %>% alpha() # Cronbach's Alpha tru subscale: 0.71





## Test if subscale scores are different from each other --------------------------------------------------------

# Use survey-weighted one-sample t tests with Bonferroni correction
# Calculate Bonferroni-corrected p level is .05 / 6 = 0.00833 when we test at alpha = .05 and run 6 tests
#                                        or .01 / 6 = 0.00167 when we test at alpha = .01 and run 6 tests
#                                        or .001 / 6 = 0.00017 when we test at alpha = .001 and run 6 tests
svyttest(I(scipopppl - scipopeli) ~ 0, bar.design, na.rm = T) # p < 2.2e-16 / alpha < 1.32e-15
svyttest(I(scipopppl - scipopdec) ~ 0, bar.design, na.rm = T) # p = 3.738e-13 / alpha = 2.2428e-12
svyttest(I(scipopppl - scipoptru) ~ 0, bar.design, na.rm = T) # p = 0.01681 / alpha = 0.10086
svyttest(I(scipopeli - scipopdec) ~ 0, bar.design, na.rm = T) # p = 0.09618 / alpha = 0.57708
svyttest(I(scipopeli - scipoptru) ~ 0, bar.design, na.rm = T) # p = 1.093e-14 / alpha = 6.558e-14
svyttest(I(scipopdec - scipoptru) ~ 0, bar.design, na.rm = T) # p = 8.134e-07 / alpha = 4.8804e-06





## Apply other aggregation procedures to compute SciPop Scores comparable to other studies ----------------------

### Vehrkamp & Merkel (2020): dichotomous measure plus "mixed" category -----------------------------------------

# Aggregation procedure: "...only someone who either 'strongly' or 'mostly' agrees with all eight statements 
# counts as 'populist'. Respondents who 'strongly disagree' with at least one statement, or who 'mostly 
# disagree' with at least half of the eight statements, are described as being non-populist. All other 
# respondents are neither populist nor non-populist, and fall into the category of 'mixed.'" 
# (henceforth: Bertelsmann approach)

# Step 1: Create additional variable which indicates how often someone reported 2 to an item
bar2019pop$av <- bar2019pop %>% mutate(scipop1.ppl.av = scipop1.ppl == 2,
                                       scipop2.ppl.av = scipop2.ppl == 2,
                                       scipop3.eli.av = scipop3.eli == 2,
                                       scipop4.eli.av = scipop4.eli == 2,
                                       scipop5.dec.av = scipop5.dec == 2,
                                       scipop6.dec.av = scipop6.dec == 2,
                                       scipop7.tru.av = scipop7.tru == 2,
                                       scipop8.tru.av = scipop8.tru == 2) %>% 
  select(scipop1.ppl.av:scipop8.tru.av) %>% rowSums(na.rm = T)



# Step 2: Apply Bertelsmann coding scheme to our data
bar2019pop %<>% mutate(scipopbertel = case_when(is.na(scipop1.ppl) & is.na(scipop2.ppl) &
                                                  is.na(scipop3.eli) & is.na(scipop4.eli) &
                                                  is.na(scipop5.dec) & is.na(scipop6.dec) &
                                                  is.na(scipop7.tru) & is.na(scipop8.tru) ~ NA_real_,
                                                scipop1.ppl >= 4 & scipop2.ppl >= 4 &
                                                  scipop3.eli >= 4 & scipop4.eli >= 4 & 
                                                  scipop5.dec >= 4 & scipop6.dec >= 4 & 
                                                  scipop7.tru >= 4 & scipop8.tru >= 4 ~ 3, 
                                                av >= 4 |
                                                  (scipop1.ppl == 1 | scipop2.ppl == 1 |
                                                     scipop3.eli == 1 | scipop4.eli == 1 |
                                                     scipop5.dec == 1 | scipop6.dec == 1 |
                                                     scipop7.tru == 1 | scipop8.tru == 1) ~ 1,
                                                TRUE ~ 2))



# Step 3: Label score and values
bar2019pop %<>% var_labels(scipopbertel = "SciPop attitudes: SciPop Score (Bertelsmann approach)")

bar2019pop$scipopbertel %<>% set_labels(labels = c("populist" = 1, "mixed" = 2, "non-populist" = 3))




### Rico & Anduiza (2019): Mean across all items ----------------------------------------------------------------

# Aggregation procedure: "Respondents' agreement with each of the statements was measured using a five-point 
# Likert scale, from strongly disagree to strongly agree (see the Appendix for details on question wording and 
# coding). The internal consistency of the resulting composite scale (mean of scores) is satisfactory for the 
# whole sample, with a Cronbach's alpha of 0.83, and across all countries in the survey, with alphas varying 
# between 0.77 (Greece) and 0.87 (France)."

# Step 1: Compute mean across all eight items of the SciPop Scale
bar2019pop$scipoprico <- rowMeans(select(bar2019pop, scipop1.ppl:scipop8.tru), na.rm = T)



# Step 2: Fix NaNs
bar2019pop$scipoprico[is.nan(bar2019pop$scipoprico)] <- NA



# Step 3: Label score
bar2019pop %<>% var_labels(scipoprico = "SciPop attitudes: SciPop Score (Rico & Anduiza approach)")




### van Hauwaert, Schimpf, & Dandoy (2019): CFA scores ----------------------------------------------------------

# Coding scheme: "First, we specify an aggregated populism index to reflect the variance in the sample by 
# region, using averaged factor scores from a confirmatory factor analysis (CFA). Second, we use these 
# region-specific CFA estimates to examine these patterns of variance."

# Step 1: Formulate measurement model with all items loading on a single latent factor:
cfa.1F <- 'scipophsd =~ scipop1.ppl + scipop2.ppl + scipop3.eli + scipop4.eli + 
                       scipop5.dec + scipop6.dec + scipop7.tru + scipop8.tru'



# Step 2: Fit model using polychoric CFA (see Table A3 in appendix of van Hauwaert et al.)
cfa.1F.fit <- cfa(cfa.1F, data = bar2019pop,
                  ordered = c("scipop1.ppl", "scipop2.ppl", "scipop3.eli", "scipop4.eli",
                              "scipop5.dec", "scipop6.dec", "scipop7.tru", "scipop8.tru"))



# Step 3: Get summary statistics
summary(cfa.1F.fit, fit = T)



# Step 4: Extract factor scores
id <- cfa.1F.fit@Data@case.idx[[1]]

cfa.1F.factorscores <- cbind.data.frame(id, lavPredict(cfa.1F.fit))

bar2019pop <- left_join(bar2019pop, cfa.1F.factorscores, by = "id", all.x = T)



# Step 5: Label score
bar2019pop %<>% var_labels(scipophsd = "SciPop attitudes: SciPop Score (van Hauweart et al approach)")



# Step 6: Update survey design
bar.design <- svydesign(id = ~0, data = bar2019pop, weights = ~weight)



# Step 7: Store weighted average factor score per canton in df (i.e., means of "region-specific CFA estimates")
scipophsd.canton <- as.data.frame(svyby(~scipophsd, ~canton, bar.design, svymean, na.rm = T))




### Compare aggregation procedures ------------------------------------------------------------------------------

# Step 1: Weighted means, SDs, and valid Ns for other continuous SciPop Scores (Rico; van Hauwaert et al)
rbind.data.frame(
  cbind(aggregation = names(svymean(~scipopgoertz, bar.design, na.rm = T)),
        M = round(svymean(~scipopgoertz, bar.design, na.rm = T)[[1]], 4),
        SD = round(svysd(~scipopgoertz, bar.design, na.rm = T)[[1]], 4),
        n = round(sum(svytable(~scipopgoertz, bar.design)), 0)),
  cbind(aggregation = names(svymean(~scipoprico, bar.design, na.rm = T)),
        M = round(svymean(~scipoprico, bar.design, na.rm = T)[[1]], 4),
        SD = round(svysd(~scipoprico, bar.design, na.rm = T)[[1]], 4),
        n = round(sum(svytable(~scipoprico, bar.design)), 0)),
  cbind(aggregation = "scipophsd",
        M = round(mean(scipophsd.canton$scipophsd), 4),
        SD = round(sd(scipophsd.canton$scipophsd), 4),
        n = round(sum(svytable(~scipophsd, bar.design)), 0)))



# Step 2: Weighted means, SEs, and valid Ns for SciPop Scores per canton (van Hauwaert et al., 2019)
get_labels(bar2019pop$canton) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipophsd, ~canton, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~canton, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipophsd, ~canton, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("canton", "SciPopHSD.M", "SciPopHSD.SE", "SciPopHSD.n", "SciPopHSD.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(canton, levels = unique(canton)) + scale ~ val) %>%
  rename(canton = `factor(canton, levels = unique(canton))`) %>%
  mutate(canton = paste(.$canton, "\n(n = ", .$n, ")", sep = "")) %>%
  .[order(-.$M),]



# Step 3: Weighted percentages for categorical SciPop Scores (Vehrkamp & Merkel, 2020)
wtd.table(bar2019pop$scipopbertel, weights = bar2019pop$weight.x) %>%
  as.data.frame() %>%
  mutate(Percent = round(100 * Freq/sum(.$Freq), 1)) %>%
  mutate(Label = case_when(Var1 == 1 ~ "non-populist", Var1 == 2 ~ "mixed", Var1 == 3 ~ "populist")) %>%
  cbind(factor(.$Label, levels = c("non-populist", "mixed", "populist"))) %>%
  setnames(5, "Classification") %>%
  select(Classification, Freq, Percent)  




## Weighted tables/barplots of SciPop Scores in population groups -----------------------------------------------

### SciPop Scores: Age ------------------------------------------------------------------------------------------
scipop.age.df <- get_labels(bar2019pop$age.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~age.groups, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("age", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(age, levels = unique(age)) + scale ~ val) %>%
  rename(age = `factor(age, levels = unique(age))`) %>%
  mutate(age = paste0(.$age, "\n(n = ", .$n, ")"))

scipop.age.df


scipopbarplot.age <- ggplot(scipop.age.df, aes(x = factor(age, unique(age)), M, width = 0.7*(4/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[1]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(4/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop Scores across age groups", x = "Age", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.age




### SciPop Scores: Gender ---------------------------------------------------------------------------------------
scipop.gender.df <- get_labels(bar2019pop$gender) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~gender, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("gender", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(gender, levels = unique(gender)) + scale ~ val) %>%
  rename(gender = `factor(gender, levels = unique(gender))`) %>%
  mutate(gender = paste0(.$gender, "\n(n = ", .$n, ")"))

scipop.gender.df


scipopbarplot.gender <- ggplot(scipop.gender.df, aes(x = factor(gender, unique(gender)), M, width = 0.7*(2/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[2]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(2/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop Scores across genders", x = "Gender", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25), position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.gender




### SciPop Scores: Education ------------------------------------------------------------------------------------
scipop.edu.df <- get_labels(bar2019pop$education) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~education, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("education", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(education, levels = unique(education)) + scale ~ val) %>%
  rename(education = `factor(education, levels = unique(education))`) %>%
  mutate(education = paste0(.$education, "\n(n = ", .$n, ")"))

scipop.edu.df


scipopbarplot.edu <- ggplot(scipop.edu.df, aes(x = factor(education, unique(education)), M, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[3]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(3/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop Scores across educations levels", x = "Education", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.edu




### SciPop Scores: Proximity to science -------------------------------------------------------------------------
scipop.sciprox.df <- get_labels(bar2019pop$sciprox.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~sciprox.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~sciprox.groups, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~sciprox.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("sciprox", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(sciprox, levels = unique(sciprox)) + scale ~ val) %>%
  rename(sciprox = `factor(sciprox, levels = unique(sciprox))`) %>%
  mutate(sciprox = paste0(.$sciprox, "\n(n = ", .$n, ")"))

scipop.sciprox.df


scipopbarplot.sciprox <- ggplot(scipop.sciprox.df, aes(x = factor(sciprox, unique(sciprox)), M, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[4]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(3/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop Scores across levels of proximity to science", x = "Proximity to science", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25), position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.sciprox




### SciPop Scores: Urbanity of residence place ------------------------------------------------------------------
scipop.urb.df <- get_labels(bar2019pop$urbanity.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~urbanity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~urbanity.groups, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~urbanity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("urbanity", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(urbanity, levels = unique(urbanity)) + scale ~ val) %>%
  rename(urbanity = `factor(urbanity, levels = unique(urbanity))`) %>%
  mutate(urbanity = paste0(.$urbanity, "\n(n = ", .$n, ")"))

scipop.urb.df


scipopbarplot.urb <- ggplot(scipop.urb.df, aes(x = factor(urbanity, unique(urbanity)), M, width = 0.7*(5/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[5]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(5/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop Scores across municipality sizes", x = "Inhabitants of residence municipality", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.urb




### SciPop Scores: Language region ------------------------------------------------------------------------------
scipop.lanreg.df <- get_labels(bar2019pop$languageregion, drop.unused = T) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~languageregion, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~languageregion, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~languageregion, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("lanreg", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(lanreg, levels = unique(lanreg)) + scale ~ val) %>%
  rename(lanreg = `factor(lanreg, levels = unique(lanreg))`) %>%
  mutate(lanreg = paste0(.$lanreg, "\n(n = ", .$n, ")"))

scipop.lanreg.df


scipopbarplot.lanreg <- ggplot(scipop.lanreg.df, aes(x = factor(lanreg, unique(lanreg)), M, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[6]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(3/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop scores across Swiss language regions", x = "Language region", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25), position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.lanreg




### SciPop Scores: Political orientation ------------------------------------------------------------------------
scipop.pol.df <- get_labels(bar2019pop$polorientation.groups, drop.unused = T) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~polorientation.groups, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~polorientation.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("pol", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(pol, levels = unique(pol)) + scale ~ val) %>%
  rename(pol = `factor(pol, levels = unique(pol))`) %>%
  mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")"))

scipop.pol.df


scipopbarplot.pol <- ggplot(scipop.pol.df, aes(x = factor(pol, unique(pol)), M, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[7]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(3/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop scores across political orientations", x = "Political orientation", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.pol




### SciPop Scores: Religiosity ----------------------------------------------------------------------------------
scipop.rel.df <- get_labels(bar2019pop$religiosity.groups, drop.unused = T) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopgoertz, ~religiosity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(as.data.frame(round(svytable(~religiosity.groups, bar.design), 0))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopgoertz, ~religiosity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  set_colnames(c("rel", "SciPopGoertz.M", "SciPopGoertz.SE", "SciPopGoertz.n", "SciPopGoertz.SD")) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(rel, levels = unique(rel)) + scale ~ val) %>%
  rename(rel = `factor(rel, levels = unique(rel))`) %>%
  mutate(rel = paste0(.$rel, "\n(n = ", .$n, ")"))

scipop.rel.df


scipopbarplot.rel <- ggplot(scipop.rel.df, aes(x = factor(rel, unique(rel)), M, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), fill = viridis_pal(option = "D", end = 0.9)(8)[8]) +
  geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.3*(3/5), size = 0.8, alpha = 1, position = position_dodge(0.9)) + 
  coord_cartesian(ylim = c(1.5, 3)) +  
  labs(title = "Mean Goertzian SciPop scores across levels of religiosity", x = "Religiosity", y = "Mean SciPop Score") +
  scale_y_continuous(breaks = seq(1.5, 4, 0.25), position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        text = element_text(family = "serif"))

scipopbarplot.rel




### Arrange SciPop Score plots ----------------------------------------------------------------------------------
ggarrange(scipopbarplot.age,
          scipopbarplot.gender + rremove("y.title"),
          scipopbarplot.edu,
          scipopbarplot.sciprox + rremove("y.title"),
          scipopbarplot.urb,
          scipopbarplot.lanreg + rremove("y.title"),
          scipopbarplot.pol,
          scipopbarplot.rel + rremove("y.title"),
          ncol = 2, nrow = 4) # Export 13 x 10 inch landscape




## Weighted tables/barplots of subscale scores in population groups ---------------------------------------------

### Subscale scores: Age ----------------------------------------------------------------------------------------
subscale.age.df <- get_labels(bar2019pop$age.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~age.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("age", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(age = paste0(.$age, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(age, levels = unique(age)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(age = `factor(age, levels = unique(age))`, scale = `factor(scale, levels = unique(scale))`)

subscale.age.df


subscalebarplot.age <- ggplot(subscale.age.df, aes(x = factor(age, levels = unique(age)), 
                                                   y = M, group = scale, fill = scale, width = 0.7*(4/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(4/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across age groups", x = "Age", y = "Mean subscale score") +
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

subscalebarplot.age




### Subscale scores: Gender -------------------------------------------------------------------------------------
subscale.gender.df <- get_labels(bar2019pop$gender) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~gender, bar.design), 0))[-1]) %>%
  set_colnames(c("gender", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(gender = paste0(.$gender, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(gender, levels = unique(gender)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(gender = `factor(gender, levels = unique(gender))`, scale = `factor(scale, levels = unique(scale))`)

subscale.gender.df


subscalebarplot.gender <- ggplot(subscale.gender.df, aes(x = factor(gender, levels = unique(gender)), 
                                                         y = M, group = scale, fill = scale, width = 0.7*(4/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(4/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across genders", x = "Gender", y = "Mean subscale score") +
  scale_y_continuous(breaks = seq(1, 4, 0.25), position = "right") +
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

subscalebarplot.gender




### Subscale scores: Education ----------------------------------------------------------------------------------
subscale.edu.df <- get_labels(bar2019pop$education) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~education, bar.design), 0))[-1]) %>%
  set_colnames(c("education", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(education = paste0(.$education, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(education, levels = unique(education)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(education = `factor(education, levels = unique(education))`, scale = `factor(scale, levels = unique(scale))`)

subscale.edu.df


subscalebarplot.edu <- ggplot(subscale.edu.df, aes(x = factor(education, levels = unique(education)), 
                                                   y = M, group = scale, fill = scale, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(3/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across education levels", x = "Education", y = "Mean subscale score") +
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

subscalebarplot.edu




### Subscale scores: Proximity to science -----------------------------------------------------------------------
subscale.sciprox.df <- get_labels(bar2019pop$sciprox.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~sciprox.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~sciprox.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~sciprox.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~sciprox.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~sciprox.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~sciprox.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~sciprox.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~sciprox.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~sciprox.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("sciprox", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(sciprox = paste0(.$sciprox, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(sciprox, levels = unique(sciprox)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(sciprox = `factor(sciprox, levels = unique(sciprox))`, scale = `factor(scale, levels = unique(scale))`)

subscale.sciprox.df


subscalebarplot.sciprox <- ggplot(subscale.sciprox.df, aes(x = factor(sciprox, levels = unique(sciprox)), 
                                                           y = M, group = scale, fill = scale, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(3/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across levels of proximity to science", x = "Proximity to science", y = "Mean subscale score") +
  scale_y_continuous(breaks = seq(1, 4, 0.25), position = "right") +
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

subscalebarplot.sciprox




### Subscale scores: Inhabitant count of residence municipality -------------------------------------------------
subscale.urb.df <- get_labels(bar2019pop$urbanity.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~urbanity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~urbanity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~urbanity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~urbanity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~urbanity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~urbanity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~urbanity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~urbanity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~urbanity.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("urbanity", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(urbanity = paste0(.$urbanity, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(urbanity, levels = unique(urbanity)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(urbanity = `factor(urbanity, levels = unique(urbanity))`, scale = `factor(scale, levels = unique(scale))`)

subscale.urb.df


subscalebarplot.urb <- ggplot(subscale.urb.df, aes(x = factor(urbanity, levels = unique(urbanity)), 
                                                   y = M, group = scale, fill = scale, width = 0.7*(5/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(5/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across municipality sizes", x = "Inhabitants of residence municipality", y = "Mean subscale score") +
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

subscalebarplot.urb




### Subscale scores: Language region ----------------------------------------------------------------------------
subscale.lanreg.df <- get_labels(bar2019pop$languageregion) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~languageregion, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~languageregion, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~languageregion, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~languageregion, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~languageregion, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~languageregion, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~languageregion, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~languageregion, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~languageregion, bar.design), 0))[-1]) %>%
  set_colnames(c("languageregion", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(languageregion = paste0(.$languageregion, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(languageregion, levels = unique(languageregion)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(languageregion = `factor(languageregion, levels = unique(languageregion))`, scale = `factor(scale, levels = unique(scale))`)

subscale.lanreg.df


subscalebarplot.lanreg <- ggplot(subscale.lanreg.df, aes(x = factor(languageregion, levels = unique(languageregion)), 
                                                         y = M, group = scale, fill = scale, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(3/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across language regions", x = "Swiss language region", y = "Mean subscale score") +
  scale_y_continuous(breaks = seq(1, 4, 0.25), position = "right") +
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

subscalebarplot.lanreg




### Subscale scores: Political orientation ----------------------------------------------------------------------
subscale.pol.df <- get_labels(bar2019pop$polorientation.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~polorientation.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~polorientation.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~polorientation.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~polorientation.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~polorientation.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~polorientation.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~polorientation.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~polorientation.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~polorientation.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("pol", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(pol = paste0(.$pol, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(pol, levels = unique(pol)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(pol = `factor(pol, levels = unique(pol))`, scale = `factor(scale, levels = unique(scale))`)

subscale.pol.df


subscalebarplot.pol <- ggplot(subscale.pol.df, aes(x = factor(pol, levels = unique(pol)), 
                                                   y = M, group = scale, fill = scale, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(3/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
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

subscalebarplot.pol




### Subscale scores: Religiosity --------------------------------------------------------------------------------
subscale.rel.df <- get_labels(bar2019pop$religiosity.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipopppl, ~religiosity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopppl, ~religiosity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopeli, ~religiosity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopeli, ~religiosity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipopdec, ~religiosity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipopdec, ~religiosity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipoptru, ~religiosity.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipoptru, ~religiosity.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~religiosity.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("rel", "ppl.M", "ppl.SE", "ppl.SD", "eli.M", "eli.SE", "eli.SD", 
                 "dec.M", "dec.SE", "dec.SD", "tru.M", "tru.SE", "tru.SD", "n")) %>%
  mutate(rel = paste0(.$rel, "\n(n = ", .$n, ")")) %>%
  subset(select = -14) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("scale", "val"))) %>%
  dcast(factor(rel, levels = unique(rel)) + factor(scale, levels = unique(scale)) ~ val) %>%
  rename(rel = `factor(rel, levels = unique(rel))`, scale = `factor(scale, levels = unique(scale))`)

subscale.rel.df


subscalebarplot.rel <- ggplot(subscale.rel.df, aes(x = factor(rel, levels = unique(rel)), 
                                                   y = M, group = scale, fill = scale, width = 0.7*(3/5))) + 
  geom_bar(stat = "identity", position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE, group = scale), 
                width = 0.3*(3/5), size = 0.5, alpha = 1, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(2, 4)) +
  labs(title = "Mean subscale scores across levels of religiosity", x = "Religiosity", y = "Mean subscale score") +
  scale_y_continuous(breaks = seq(1, 4, 0.25), position = "right") +
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

subscalebarplot.rel




### Arrange subscale score plots --------------------------------------------------------------------------------
ggarrange(subscalebarplot.age + rremove("legend"), 
          subscalebarplot.gender + rremove("y.title"),
          subscalebarplot.edu + rremove("legend"),
          subscalebarplot.sciprox + rremove("legend") + rremove("y.title"),
          subscalebarplot.urb + rremove("legend"),
          subscalebarplot.lanreg + rremove("legend") + rremove("y.title"),
          subscalebarplot.pol + rremove("legend"),
          subscalebarplot.rel + rremove("legend") + rremove("y.title"),
          ncol = 2, nrow = 4) # Export 11 x 18 inch landscape





## Weighted tables of SciPop Scale items in population groups ---------------------------------------------------

### SciPop Scale items: Age -------------------------------------------------------------------------------------
items.age.groups.df <- get_labels(bar2019pop$age.groups) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipop1.ppl, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop1.ppl, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop2.ppl, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop2.ppl, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop3.eli, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop3.eli, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop4.eli, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop4.eli, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop5.dec, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop5.dec, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop6.dec, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop6.dec, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop7.tru, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop7.tru, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop8.tru, ~age.groups, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop8.tru, ~age.groups, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~age.groups, bar.design), 0))[-1]) %>%
  set_colnames(c("age", 
                 "ppl1.M", "ppl1.SE", "ppl1.SD", "ppl2.M", "ppl2.SE", "ppl2.SD", 
                 "eli1.M", "eli1.SE", "eli1.SD", "eli2.M", "eli2.SE", "eli2.SD",
                 "dec1.M", "dec1.SE", "dec1.SD", "dec2.M", "dec2.SE", "dec2.SD", 
                 "tru1.M", "tru1.SE", "tru1.SD", "tru2.M", "tru2.SE", "tru2.SD",
                 "n")) %>%
  mutate(age = paste(.$age, "\n(n = ", .$n, ")", sep = "")) %>%
  subset(select = -26) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("item", "val"))) %>%
  dcast(factor(age, levels = unique(age)) + factor(item, levels = unique(item)) ~ val) %>%
  rename(age = `factor(age, levels = unique(age))`, item = `factor(item, levels = unique(item))`)

items.age.groups.df




### SciPop Scale items: Gender ----------------------------------------------------------------------------------
items.gender.df <- get_labels(bar2019pop$gender) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipop1.ppl, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop1.ppl, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop2.ppl, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop2.ppl, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop3.eli, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop3.eli, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop4.eli, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop4.eli, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop5.dec, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop5.dec, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop6.dec, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop6.dec, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop7.tru, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop7.tru, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop8.tru, ~gender, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop8.tru, ~gender, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~gender, bar.design), 0))[-1]) %>%
  set_colnames(c("gender", 
                 "ppl1.M", "ppl1.SE", "ppl1.SD", "ppl2.M", "ppl2.SE", "ppl2.SD", 
                 "eli1.M", "eli1.SE", "eli1.SD", "eli2.M", "eli2.SE", "eli2.SD",
                 "dec1.M", "dec1.SE", "dec1.SD", "dec2.M", "dec2.SE", "dec2.SD", 
                 "tru1.M", "tru1.SE", "tru1.SD", "tru2.M", "tru2.SE", "tru2.SD",
                 "n")) %>%
  mutate(gender = paste(.$gender, "\n(n = ", .$n, ")", sep = "")) %>%
  subset(select = -26) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("item", "val"))) %>%
  dcast(factor(gender, levels = unique(gender)) + factor(item, levels = unique(item)) ~ val) %>%
  rename(gender = `factor(gender, levels = unique(gender))`, item = `factor(item, levels = unique(item))`)

items.gender.df




### SciPop Scale items: Education -------------------------------------------------------------------------------
items.edu.df <- get_labels(bar2019pop$education) %>% 
  as.data.frame() %>%
  cbind(as.data.frame(svyby(~scipop1.ppl, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop1.ppl, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop2.ppl, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop2.ppl, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop3.eli, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop3.eli, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop4.eli, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop4.eli, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop5.dec, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop5.dec, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop6.dec, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop6.dec, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop7.tru, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop7.tru, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(svyby(~scipop8.tru, ~education, bar.design, svymean, na.rm = T))[-1]) %>%
  cbind(sqrt(as.data.frame(svyby(~scipop8.tru, ~education, bar.design, svyvar, na.rm = T)[[2]]))) %>%
  cbind(as.data.frame(round(svytable(~education, bar.design), 0))[-1]) %>%
  set_colnames(c("education", 
                 "ppl1.M", "ppl1.SE", "ppl1.SD", "ppl2.M", "ppl2.SE", "ppl2.SD", 
                 "eli1.M", "eli1.SE", "eli1.SD", "eli2.M", "eli2.SE", "eli2.SD",
                 "dec1.M", "dec1.SE", "dec1.SD", "dec2.M", "dec2.SE", "dec2.SD", 
                 "tru1.M", "tru1.SE", "tru1.SD", "tru2.M", "tru2.SE", "tru2.SD",
                 "n")) %>%
  mutate(education = paste(.$education, "\n(n = ", .$n, ")", sep = "")) %>%
  subset(select = -26) %>%
  melt() %>%
  data.frame(colsplit(.$variable, "\\.", c("item", "val"))) %>%
  dcast(factor(education, levels = unique(education)) + factor(item, levels = unique(item)) ~ val) %>%
  rename(education = `factor(education, levels = unique(education))`, item = `factor(item, levels = unique(item))`)

items.edu.df



