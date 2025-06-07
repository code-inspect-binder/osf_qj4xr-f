# PREPARATIONS ##################################################################################################


## Load libraries -----------------------------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(sjlabelled)
library(htmlTable)
library(readxl)
library(psych)





# DATA WRANGLING ################################################################################################

## Prepare data set ---------------------------------------------------------------------------------------------

# Load data
bar2019 <- read_spss("Data/surveydata.sav")



# Add running ID 
bar2019$id <- seq.int(nrow(bar2019))




## Prepare variables---------------------------------------------------------------------------------------------

# Select relevant items
bar2019pop <- bar2019 %>%
  select(id = id, # ID
         weight = WT, # Survey weights
         languageregion = Region, # Language region
         plz = PLZ, # Postal code
         gender = SEX, # Gender
         age = AGE, # Age
         householdsize = HHGR_ZG, # Household size
         education = F25_3Gruppen, # Education (3 categories)
         religiosity = F26, # Religiosity
         polorientation = F27, # Political orientation
         sciprox1 = F2401A,  # Proximity to science: Are you a scientist yourself?
         sciprox2 = F240201, # Proximity to science: Do you know a scientist personally?
         sciprox3 = F240202, # Proximity to science: Are you professionally involved with science?
         sciprox4 = F240203, # Proximity to science: Do you have family members who have studied or are [...]
         scipop1.ppl = F2107, # Science-related populist attitudes: What unites the ordinary people [...]
         scipop2.ppl = F2108, # Science-related populist attitudes: Ordinary people are of good and [...]
         scipop3.eli = F2104, # Science-related populist attitudes: Scientists are only after their [...]
         scipop4.eli = F2105, # Science-related populist attitudes: Scientists are in cahoots with [...]
         scipop5.dec = F2101, # Science-related populist attitudes: The people should have influence [...]
         scipop6.dec = F1706, # Science-related populist attitudes: People like me should be [...]
         scipop7.tru = F2102, # Science-related populist attitudes: In case of doubt, one should [...]
         scipop8.tru = F2103, # Science-related populist attitudes: We should rely more on common [...]
         interestscience = F0404, # Interest in science
         sl1 = F2201, # Scientific literacy: The continents on which we live have been moving for [...]
         sl2 = F2202, # Scientific literacy: Electrons are smaller than atoms.
         sl3 = F2203, # Scientific literacy: Antibiotics kill both viruses and bacteria.
         sl4 = F2204, # Scientific literacy: The genes of the mother determine whether a child becomes a [...]
         sl5 = F2205, # Scientific literacy: Scientific theories never change.
         trustscientists = F1901, # Trust in university scientists
         trustscience    = F1902) # Trust in science



# Set up variables with more informative labels
bar2019pop %<>% var_labels(id = "ID",
                           languageregion = "Language region",
                           plz = "Postal Code",
                           weight = "Survey weights",
                           gender = "Gender",
                           age = "Age",
                           householdsize = "Household size",
                           education = "Education (3 categories)", 
                           religiosity = "Religiosity",
                           polorientation = "Political orientation",
                           sciprox1 = "Proximity to science: Are you a scientist yourself?",
                           sciprox2 = "Proximity to science: Do you know a scientist personally?",
                           sciprox3 = "Proximity to science: Are you professionally involved with science?",
                           sciprox4 = "Proximity to science: Do you have family members who have studied or are studying?",
                           scipop1.ppl = "SciPop attitudes: What unites the ordinary people is that they trust their common sense in everyday life. (ppl)",
                           scipop2.ppl = "SciPop attitudes: Ordinary people are of good and honest character. (ppl)",
                           scipop3.eli   = "SciPop attitudes: Scientists are only after their own advantage. (eli)",
                           scipop4.eli   = "SciPop attitudes: Scientists are in cahoots with politics and business. (eli)",
                           scipop5.dec  = "SciPop attitudes: The people should have influence on the work of scientists. (dec)",
                           scipop6.dec  = "SciPop attitudes: People like me should be involved in decisions about the topics scientists research. (dec)",
                           scipop7.tru   = "SciPop attitudes: In case of doubt, one should rather trust the life experience of ordinary people than the estimations of scientists. (tru)",
                           scipop8.tru   = "SciPop attitudes: We should rely more on common sense and less on scientific studies. (tru)",
                           interestscience = "Interest in science and research",
                           sl1 = "Scientific literacy: The continents on which we live have been moving for millions of years.",
                           sl2 = "Scientific literacy: Electrons are smaller than atoms.",
                           sl3 = "Scientific literacy: Antibiotics kill both viruses and bacteria.",
                           sl4 = "Scientific literacy: The genes of the mother determine whether a child becomes a boy or a girl.",
                           sl5 = "Scientific literacy: Scientific theories never change.",
                           trustscientists = "Trust in university scientists",
                           trustscience =  "Trust in science")




### Science-related populist attitudes --------------------------------------------------------------------------

# Score built from scipop1.ppl: What unites the ordinary people is that they trust their common  [...]
#                  scipop2.ppl: Ordinary people are of good and honest character.
#                  scipop3.eli: Scientists are only after their own advantage.
#                  scipop4.eli: Scientists are in cahoots with politics and business.
#                  scipop5.dec: The people should have influence on the work of scientists.
#                  scipop6.dec: People like me should be involved in decisions about the topics [...]
#                  scipop7.tru: In case of doubt, one should rather trust the life experience of [...]
#                  scipop8.tru: We should rely more on common sense and less on scientific studies.
#
# The SciPop score is obtained following the 'Goertz approach' (Wuttke, Schimpf, & Schoen, 2020). In a
# first step, unweighted mean values were computed for each of the four 2-item subscales. In a second step,
# the smallest of these four values was used as the SciPop score. Higher values indicate stronger
# science-related populist attitudes.
# The four component scores are unweighted mean values of the 2-item subscales. Higher values indicate 
# favorable conceptions of the ordinary people (ppl score), more unfavorable conceptions of the 
# academic elite (eli score), stronger demands for decision-making sovereignty (dec score), and 
# stronger demands for truth-speaking sovereignty (tru score).

# Step 1: Compute mean values for subscales
bar2019pop$scipopppl <- rowMeans(bar2019pop[ ,c("scipop1.ppl", "scipop2.ppl")], na.rm = T)
bar2019pop$scipopeli <- rowMeans(bar2019pop[ ,c("scipop3.eli", "scipop4.eli")], na.rm = T)
bar2019pop$scipopdec <- rowMeans(bar2019pop[ ,c("scipop5.dec", "scipop6.dec")], na.rm = T)
bar2019pop$scipoptru <- rowMeans(bar2019pop[ ,c("scipop7.tru", "scipop8.tru")], na.rm = T)



# Step 2: Create SciPop Score by taking the minimum mean value as the SciPop score
bar2019pop %<>% transform(scipopgoertz = pmin(scipopppl, scipopeli, scipopdec, scipoptru))



# Step 3: Fix NaNs
bar2019pop$scipopppl[is.nan(bar2019pop$scipopppl)] <- NA
bar2019pop$scipopeli[is.nan(bar2019pop$scipopeli)] <- NA
bar2019pop$scipopdec[is.nan(bar2019pop$scipopdec)] <- NA
bar2019pop$scipoptru[is.nan(bar2019pop$scipoptru)] <- NA
bar2019pop$scipopgoertz[is.nan(bar2019pop$scipopgoertz)] <- NA



# Step 4: Label scores
bar2019pop %<>% var_labels(scipopgoertz = "SciPop attitudes: SciPop Score (Goertzian approach)",
                           scipopppl    = "SciPop attitudes: PPL score (mean)",
                           scipopeli    = "SciPop attitudes: ELI score (mean)",
                           scipopdec    = "SciPop attitudes: DEC score (mean)",
                           scipoptru    = "SciPop attitudes: TRU score (mean)")



# Step 5: Build categorical variable
bar2019pop %<>% mutate(scipopgoertz.groups = case_when(scipopgoertz >= 1 & scipopgoertz <= 2 ~ 1,
                                                       scipopgoertz > 2 & scipopgoertz < 4 ~ 2,
                                                       scipopgoertz >= 4 & scipopgoertz <= 5 ~ 3,
                                                       TRUE ~ NA_real_))


bar2019pop %<>% var_labels(scipopgoertz.groups = "SciPop Score (3 groups)")

bar2019pop$scipopgoertz.groups %<>% set_labels(labels = c("1.00-2.00" = 1, "2.01-3.99" = 2, "4.00-5.00" = 3))




### Age ---------------------------------------------------------------------------------------------------------

# Build categorical variable
bar2019pop %<>% mutate(age.groups = case_when(age >= 15 & age <= 29 ~ 1,
                                              age >= 30 & age <= 44 ~ 2,
                                              age >= 45 & age <= 59 ~ 3,
                                              age >= 60 ~ 4))

bar2019pop %<>% var_labels(age.groups = "Age (4 groups)")

bar2019pop$age.groups %<>% set_labels(labels = c("15-29 years" = 1, 
                                                 "30-44 years" = 2,
                                                 "45-59 years" = 3, 
                                                 "60 years and older" = 4))




### Gender ------------------------------------------------------------------------------------------------------

# Needs to be recoded to 0 = male, 1 = female
bar2019pop$gender %<>% recode(`1` = 0L, `2` = 1L) %>% set_labels(labels = c("male" = 0, "female" = 1))

bar2019pop %<>% var_labels(gender = "Gender")




### Education ---------------------------------------------------------------------------------------------------

# Needs to be dummy-coded
bar2019pop %<>% mutate(education.uni = case_when(education == 3 ~ 1, education == 2 ~ 0, education == 1 ~ 0,
                                                 education == 4 ~ NA_real_),
                       education.sec = case_when(education == 3 ~ 0, education == 2 ~ 1, education == 1 ~ 0,
                                                 education == 4 ~ NA_real_),
                       education.comp = case_when(education == 3 ~ 0, education == 2 ~ 0, education == 1 ~ 1,
                                                  education == 4 ~ NA_real_))

bar2019pop$education.uni  %<>% set_label(label = "Education: University")
bar2019pop$education.sec  %<>% set_label(label = "Education: Secondary education")
bar2019pop$education.comp %<>% set_label(label = "Education: Compulsory schooling")



# Label original variable
bar2019pop$education %<>% set_labels(labels = c("Compulsory schooling" = 1, 
                                                "Secondary education" = 2,
                                                "University" = 3))




### Proximity to science ----------------------------------------------------------------------------------------

# Build score from sciprox1: Are you a scientist yourself?
#                  sciprox2: Do you know a scientist personally?,
#                  sciprox3: Are you professionally involved with science?,
#                  sciprox4: Do you have family members who have studied or are studying?

# Step 1: Recode and label a bunch of variables
bar2019pop$sciprox1 %<>% recode(`1` = 1L, `2` = 0L) %>% set_labels(labels = c("yes" = 1, "no" = 0))
bar2019pop$sciprox2 %<>% recode(`1` = 1L, `2` = 0L) %>% set_labels(labels = c("yes" = 1, "no" = 0))
bar2019pop$sciprox3 %<>% recode(`1` = 1L, `2` = 0L) %>% set_labels(labels = c("yes" = 1, "no" = 0))
bar2019pop$sciprox4 %<>% recode(`1` = 1L, `2` = 0L) %>% set_labels(labels = c("yes" = 1, "no" = 0))

bar2019pop %<>% var_labels(sciprox1 = "Proximity to science: Are you a scientist yourself?",
                           sciprox2 = "Proximity to science: Do you know a scientist personally?",
                           sciprox3 = "Proximity to science: Are you professionally involved with science?",
                           sciprox4 = "Proximity to science: Do you have family members who have studied or are studying?")



# Step 2: Create score: 4 pts = scientist (sciprox1); 1 pt for each "yes" to sciprox2, sciprox3, sciprox4
bar2019pop %<>% mutate(sciprox.score = case_when(sciprox1 == 1 ~ 4,
                                                 sciprox2 + sciprox3 + sciprox4 == 3 ~ 3,
                                                 sciprox2 + sciprox3 + sciprox4 == 2 ~ 2,
                                                 sciprox2 + sciprox3 + sciprox4 == 1 ~ 1,
                                                 sciprox1 == 0 | sciprox2 + sciprox3 + sciprox4 == 0 ~ 0))

bar2019pop %<>% var_labels(sciprox.score = "Proximity to science (score)")



# Step 3: categorical variable
bar2019pop %<>% mutate(sciprox.groups = case_when(sciprox.score == 0 ~ 1,
                                                  sciprox.score >= 1 & sciprox.score <= 3 ~ 2,
                                                  sciprox.score == 4 ~ 3))

bar2019pop %<>% var_labels(sciprox.groups = "Proximity to science (3 groups)")

bar2019pop$sciprox.groups %<>% set_labels(labels = c("no affilation with science" = 1, 
                                                     "know scientist/professionally involved with science/family member studied" = 2, 
                                                     "scientist myself" = 3))




### Urbanity of residence place ---------------------------------------------------------------------------------

# Later, we want to check if the number of inhabitants of the residence municipality (as some sort of proxy for 
# urban vs. rural areas) is a predictor of science-related populist attitudes. For this, we use census data 
# from the Swiss Federal Statistical Office (BfS).
# We need: 
# (1) Census data from the BfS ("Regionalportraets 2019: Kennzahlen aller Gemeinden"), retrieved from:
#     https://www.bfs.admin.ch/bfsstatic/dam/assets/7786544/master
# (2) A correspondence table for matching the municipality numbers of the census data with the postal codes 
#     (PLZs), also from the BfS, retrieved from:
#     https://www.bfs.admin.ch/bfsstatic/dam/assets/7226419/master

# Step 1: Read census data
bfscensus <- read_xlsx("Data/bfs-censusdata.xlsx", sheet = 1, range = "A6:AK2221", col_names = T,
                       na = c("X", "*"), trim_ws = T, .name_repair = "unique") %>%
  slice(4:n()) %>%
  select("Gemeindecode", "Einwohner") %>%
  rename(bfs.m.number      = Gemeindecode,
         bfs.m.inhabitants = Einwohner)



# Step 2: Read correspondence table
bfscorrespontbl <- read_xlsx("Data/bfs-correspondencetable.xlsx", sheet = 2, col_names = T, trim_ws = T,
                             .name_repair = "unique") %>%
  select("PLZ4", "%_IN_GDE", "GDENR", "GDENAMK", "KTKZ") %>%
  rename(plz           = PLZ4,
         bfs.m.plzperc = `%_IN_GDE`,
         bfs.m.number  = GDENR,
         bfs.m.name    = GDENAMK,
         canton        = KTKZ)



# Step 3: Merge census data with correspondence table
bfscensus <- left_join(bfscensus, bfscorrespontbl, by = c("bfs.m.number" = "bfs.m.number"))



# Step 4: See if we could match all postal codes in survey data with postal codes from correspondence table
bar2019pop %>% anti_join(bfscensus, by = c("plz" = "plz")) %>% select(plz)

# --> 7 cases (6 postal codes) can not be matched - Google search shows that:
#     1708 = Fribourg (whose municipality number is 2196)
#     1001 = Lausanne (whose municipality number is 5586)
#     8000 = Zurich (whose municipality number is 261)
#     2001 = Neuch?tel (whose municipality number is 6458)
#     1200 = Genf (whose municipality number is 6621)
#     4000 = Basel (whose municipality number is 2701)

#     So we add the unmatched cases manually:
bfsmissing <- rbind(cbind(bfs.m.number = 2196, 
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 2196, "bfs.m.inhabitants"][1],
                          plz = 1708L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 2196, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 2196, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 2196, "canton"][1]),
                    cbind(bfs.m.number = 5586,
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 5586, "bfs.m.inhabitants"][1],
                          plz = 1001L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 5586, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 5586, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 5586, "canton"][1]),
                    cbind(bfs.m.number = 261,
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 261, "bfs.m.inhabitants"][1],
                          plz = 8000L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 261, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 261, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 261, "canton"][1]),
                    cbind(bfs.m.number = 6458,
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 6458, "bfs.m.inhabitants"][1],
                          plz = 2001L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 6458, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 6458, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 6458, "canton"][1]),
                    cbind(bfs.m.number = 6621,
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 6621, "bfs.m.inhabitants"][1],
                          plz = 1200L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 6621, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 6621, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 6621, "canton"][1]),
                    cbind(bfs.m.number = 2701,
                          bfs.m.inhabitants = bfscensus[bfscensus[, 1] == 2701, "bfs.m.inhabitants"][1],
                          plz = 4000L,
                          bfs.m.plzperc = bfscensus[bfscensus[, 1] == 2701, "bfs.m.plzperc"][1],
                          bfs.m.name = bfscensus[bfscensus[, 1] == 2701, "bfs.m.name"][1],
                          canton = bfscensus[bfscensus[, 1] == 2701, "canton"][1]))

bfscensus <- bfsmissing %>% rbind(bfscensus) %>% arrange(bfs.m.number)



# Step 5: Check if all postal codes match between census and survey data:
bar2019pop %>% anti_join(bar2019pop, bfscensus, by = c("plz" = "plz")) %>% select(plz) 




# Step 6: Now it gets a bit messy. In the survey data, we have data on postal code level. But in the census 
#         data, we have data (= the inhabitant counts) on municipality number level.  Matching postal codes 
#         with municipality numbers (and, accordingly, with inhabitant counts) is tricky though. Generally, 
#         postal codes and municipality numbers have very good correspondence, so most of the postal codes 
#         can be easily matched with municipality numbers (and, accordingly, with inhabitant counts). Yet, 
#         there are cases where one postal code corresponds with two or more municipality numbers (even if 
#         postal code areas are generally smaller than municipalities). Consequently, there are cases for 
#         which we have to decide which municipality number (inhabitant count) to attach to a case.
#
#         For this decision, we use a workaround: We make use of the bfs.m.plzperc variable 
#         in the census dataset. It contains info on how many buildings of a municipality have the postal code 
#         stored in the plz variable. We do the following: For each postal code area we check to which 
#         municipality it "contributes" most, meaning, # in which municipality the 
#         bfs.m.plzperc value is biggest. Then, we take the municipality number (inhabitant 
#         count) of that "most-contributed" municipality and attach it to the case. 
#         For example: If a postal codes belongs to 70% of the buildings in municipality X and to 30% 
#         of the buildings in municipality Y, we preserve the municipality number (inhabitant count) of 
#         municipality X. In other words: We preserve the municipality number that each postal code is "most 
#         affiliated" to, or has "greatest correspondence". Notwithstanding, this has limitations, for example, 
#         when municipality Y has much more builings than muncipality X. But it is a good workaround for 
#         translating the postal code into a municipality number.

# Step 6.1: Create a list of all 3190 postal codes
plzlist <- bfscensus %>%
  group_by(plz) %>%
  summarise(bfs.m.number = first(bfs.m.number)) %>%
  select(plz)


# Step 6.2: Now apply the above-mentioned procedure using a for loop.
#           Unfortunately, however, there is another obstacle in the way: There are cases where one postal 
#           code belongs to 100% of the buildings of two or more municipalities. For these cases, we forego 
#           using the workaround because we simply cannot determine or estimate the municipality number 
#           which may most likely correspond with the postal code. Therefore, we have to set these to NA.
#          
#           What the loop does is:
#           - take each postal code
#           - find all cases that match this postal code
#           - filter the case(s) with the biggest bfs.m.plzperc value
#           - if it is several cases (which indicates that the same postal code belongs to 100% of the 
#             buildings of two or more 
#             municipalities), set bfs.m.number and bfs.m.inhabitants to NA, aggregate 
#             by postal code and store the aggregated case in bfscensus.preserve
#           - if it is a single case store it in bfscensus.preserve
#           - during that, get rid of all variables except bfs.m.number, 
#             bfs.m.inhabitants, and plz
bfscensus.preserve <- c()

for (i in plzlist$plz){
  j <- subset(bfscensus, plz == i) %>% filter(bfs.m.plzperc == max(bfs.m.plzperc))
  
  if (nrow(j) > 1){
    j$bfs.m.number <- NA_integer_
    j$bfs.m.inhabitants <- NA_integer_
    j$bfs.m.name <- NA_integer_
    
    j %<>% select(plz, bfs.m.number, bfs.m.inhabitants, bfs.m.name, canton) %>%
      group_by(plz) %>%
      summarise(bfs.m.number      = first(bfs.m.number),
                bfs.m.inhabitants = first(bfs.m.inhabitants),
                bfs.m.name        = first(bfs.m.name),
                canton            = first(canton))
    
    bfscensus.preserve <- rbind(bfscensus.preserve, j)
  } else {
    j %<>% select(plz, bfs.m.number, bfs.m.inhabitants, bfs.m.name, canton)
    bfscensus.preserve <- rbind(bfscensus.preserve, j)
  }
}


# Step 6.3: Check how many postal codes do not clearly match one municipality number (should be 80)
sum(is.na(bfscensus.preserve$bfs.m.number))


# Step 6.4: Cheer that we finally have a table with each postal code corresponding with only one municipality:
bfscensus.preserve



# Step 7: Match the census data with the survey data
bar2019pop %<>% left_join(bfscensus.preserve, by = c("plz" = "plz"))



# Step 8: Define some labels
bar2019pop$bfs.m.inhabitants %<>% set_label(label = "Inhabitant count of municipality most affiliated to the postal code of the place of residence")
bar2019pop$bfs.m.number      %<>% set_label(label = "Municipality number of municipality most affiliated to the postal code of the place of residence")
bar2019pop$bfs.m.name        %<>% set_label(label = "Name of municipality most affiliated to the postal code of the place of residence")
bar2019pop$canton            %<>% set_label(label = "canton of the place of residence")



# Step 9: Rename bar2019pop$bfs.m.inhabitants to urbanity
bar2019pop %<>% rename(urbanity = bfs.m.inhabitants)



# Step 10: Build categorical variable for urban/rural residence place
bar2019pop %<>% mutate(urbanity.groups = case_when(urbanity <= 1499 ~ 1, 
                                                   urbanity >= 1500 & urbanity <= 4999 ~ 2,
                                                   urbanity >= 5000 & urbanity <= 14999 ~ 3,
                                                   urbanity >= 15000 & urbanity <= 49999 ~ 4,
                                                   urbanity >= 50000 ~ 5))

bar2019pop %<>% var_labels(urbanity.groups = "Inhabitants residence municipality (5 groups)")

bar2019pop$urbanity.groups %<>% set_labels(labels = c("less than 1.500 inhabitants" = 1, 
                                                      "1.500 to 4.999 inhabitants" = 2,
                                                      "5.000 to 14.999 inhabitants" = 3,
                                                      "15.000 to 49.999 inhabitants" = 4,
                                                      "50.000 or more inhabitants" = 5))




### Language region ---------------------------------------------------------------------------------------------

# Needs to be dummy-coded
bar2019pop %<>% mutate(languageregion.ger = case_when(languageregion == 1 ~ 1, languageregion == 2 ~ 0,
                                                      languageregion == 3 ~ 0, languageregion == 4 ~ NA_real_),
                       languageregion.fra = case_when(languageregion == 1 ~ 0, languageregion == 2 ~ 1,
                                                      languageregion == 3 ~ 0, languageregion == 4 ~ NA_real_),
                       languageregion.ita = case_when(languageregion == 1 ~ 0, languageregion == 2 ~ 0,
                                                      languageregion == 3 ~ 1, languageregion == 4 ~ NA_real_))

bar2019pop$languageregion.ger %<>% set_label(label = "Language region: German-speaking part")
bar2019pop$languageregion.fra %<>% set_label(label = "Language region: French-speaking part")
bar2019pop$languageregion.ita %<>% set_label(label = "Language region: Italian-speaking part")



# Label original variable
bar2019pop$languageregion %<>% set_labels(labels = c("German-speaking part" = 1, 
                                                     "French-speaking part" = 2,
                                                     "Italian-speaking part" = 3))




### Political orientation ---------------------------------------------------------------------------------------

# Build categorical variable (1-3 = left-leaning, 4 = moderate, 5-7 = right-leaning)
bar2019pop %<>% mutate(polorientation.groups = case_when(polorientation >= 1 & polorientation <= 3 ~ 1,
                                                         polorientation == 4 ~ 2, 
                                                         polorientation >= 5 & polorientation <= 7 ~ 3))

bar2019pop %<>% var_labels(polorientation.groups = "Political orientation (3 groups)")

bar2019pop$polorientation.groups %<>% set_labels(labels = c("left-leaning (1/2/3)" = 1, 
                                                            "moderate (4)" = 2,
                                                            "right-leaning (5/6/7)" = 3))




### Religiosity  ------------------------------------------------------------------------------------------------

# Build categorical variable (1 = not at all religious, 2-3 = somewhat religious, 4-5 = religious)
bar2019pop %<>% mutate(religiosity.groups = case_when(religiosity == 1 ~ 1,
                                                      religiosity == 2 | religiosity == 3 ~ 2,
                                                      religiosity == 4 | religiosity == 5 ~ 3))


bar2019pop %<>% var_labels(religiosity.groups = "Religiosity (3 groups)")

bar2019pop$religiosity.groups %<>% set_labels(labels = c("not at all religious (1)" = 1, 
                                                         "somewhat religious (2/3)" = 2,
                                                         "religious (4/5)" = 3))




### Scientific literacy -----------------------------------------------------------------------------------------

# Step 1: Recode responses: "definitely correct" answer = 2 pts, "probably correct" answer = 1 pt,
#         "probably" wrong answer = -1 pt, definitely wrong answer = -2 pts, don't know = 0 pts
bar2019pop %<>% mutate(sl1.pt = case_when(sl1==1 ~ -2, sl1==2 ~ -1, sl1==3 ~ 1, sl1==4 ~ 2, is.na(sl1) ~ 0),
                       sl2.pt = case_when(sl2==1 ~ -2, sl2==2 ~ -1, sl2==3 ~ 1, sl2==4 ~ 2, is.na(sl2) ~ 0),
                       sl3.pt = case_when(sl3==1 ~ 2, sl3==2 ~ 1, sl3==3 ~ -1, sl3==4 ~ -2, is.na(sl3) ~ 0),
                       sl4.pt = case_when(sl4==1 ~ 2, sl4==2 ~ 1, sl4==3 ~ -1, sl4==4 ~ -2, is.na(sl4) ~ 0),
                       sl5.pt = case_when(sl5==1 ~ 2, sl5==2 ~ 1, sl5==3 ~ -1, sl5==4 ~ -2, is.na(sl5) ~ 0))


# Step 2: Compute sum score
bar2019pop %<>% mutate(sciliteracy = rowSums(select(bar2019pop, sl1.pt:sl5.pt), na.rm = T))


# Step 3: Label index
bar2019pop %<>% var_labels(sciliteracy = "Scientific literacy (sum score)")





### Set labels for new variables --------------------------------------------------------------------------------

# Tidy up
rm(i, j, bfscensus.preserve, bfsmissing, bfscensus, bfscorrespontbl, plzlist)

bar2019pop %<>% select(-c(sl1.pt:sl5.pt))



# Print variable list
data.frame(Variable = attr(bar2019pop, "names"), Label = as.vector(get_label(bar2019pop))) %>% 
  htmlTable(align = "l")



