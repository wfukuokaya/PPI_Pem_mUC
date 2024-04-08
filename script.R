dataset # full dataset

library(tidyverse)
library(tidylog)
library(lubridate)

dataset_FIXED # full analysis set

labelled::var_label(dataset_FIXED) <- list(FollowUp = "Follow-up period, month",
                                           Progression = "Radiographic progression",
                                           Progression.iRECIST = "Radiographic progression according to iRECIST",
                                           Best.Response = "RECIST-defined best overall response",
                                           Best.Response.iRECIST = "iRECIST-defined best overall response",
                                           TimeToProgression = "Time to progression (RECIST), month",
                                           TimeToProgression.iRECIST = "Time to progression (iRECIST), month",
                                           ProgressionToLastvisit = "Disease progression to last visit, month",
                                           ProgressionToLastvisit.iRECIST = "Disease progression to last visit, month",
                                           ClinicoRadiographic.Progression = "Clinicoradiographic progression",
                                           TimeToDiscont = "Time to treatment discontinuation, month",
                                           TimeToIRAE = "Time to irAE, month",
                                           TimeToBestResponse = "Time to best response, month",
                                           Objective.Response = "Objective response (CR + PR)",
                                           Objective.Response.iRECIST = "Objective response (immune CR + immune PR)",
                                           Pseudoprogression = "Pseudo-progression",
                                           ContinueTreatment = "Continuing pembrolizumab beyond progression", 
                                           Age = "Age, year", 
                                           Smoking = "Smoking history", 
                                           Gender = "Gender",
                                           BMI = "Body mass index",
                                           Karnofsky.PS = "Karnofsky performance status",
                                           PS.Dichotomized.80 = "Karnosfky performance status (<80 or >=80)",
                                           ECOG.Score = "ECOG performance status (0 or >=1)", 
                                           Primary.Location = "Primary location of tumor (lower or upper tract)",
                                           Metastasis.Category = "Location of metastases",
                                           Chemotherapy.Delivered = "Context of most recent therapy",
                                           Patient.Condition = "Reasons for chemotherapy discontinuation",
                                           PD1.Setting.ForMetastaticDis = "Line of therapy",
                                           Number.Prior.Chemotherapy = "Number of prior chemotherapy",
                                           Platinum = "Previous platinum therapy",
                                           Bellmunt.Factor = "Bellmunt risk factor",
                                           irAE = "Immune-related adverse event",
                                           irAE.AfterProgression.iRECIST = "Immune-related adverse event after iRECIST-defined progression",
                                           Neu.Baseline = "Neutrophil count, per uL",
                                           Lym.Baseline = "Lymphocyte count, per uL",
                                           Mo.Baseline = "Monocyte count, per uL",
                                           Eo.Baseline = "Eosinophil count, per uL",
                                           NLR.Baseline = "Neutrophil-to-lymphocyte ratio",
                                           Hb.Baseline = "Hemoglobin concentration, g/dL",
                                           DeRitis.Baseline = "De ritis ratio",
                                           LDH.Baseline = "Lactate dehydrogenase, IU/L",
                                           CRP.Baseline = "C-reactive protein, g/dL",
                                           Bacteriuria = "Bacteriuria",
                                           Refractory.Lesion.Chemotherapy = "Progressive lesion during chemotherapy", 
                                           Objective.Response.Chemotherapy = "Objective response during chemotherapy", 
                                           PPI.Concurrent = "Concurrent PPI use", 
                                           Analgesics.Concurrent = "Analgesic use", 
                                           Opioid.Concurrent = "Opioid Analgesic use")

# CREATE BASELINE CHARACTERISRICS TABLE
# overall population
FactorVariables <- c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median", "Objective.Response.Chemotherapy", "Objective.Response.iRECIST") # factor varables
AllVariables <- c("Age.Dichotomized.Median", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median", "Objective.Response.Chemotherapy", "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", "TimeToProgression.iRECIST", "TreatmentDuration") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, data = dataset_FIXED)
results <- print(tableOne, nonnormal = c("TimeToProgression.iRECIST", "TreatmentDuration"), catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE) # calculate P value by Fisher exact test

# population stratified by variable of interest
FactorVariables <- c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median", "Objective.Response.Chemotherapy", "Objective.Response.iRECIST", "Post.Trt") # factor varables
AllVariables <- c("Age.Dichotomized.Median", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median", "Objective.Response.Chemotherapy", "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", "TimeToProgression.iRECIST", "TreatmentDuration", "Post.Trt") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, strata = "ContinueTreatment", data = dataset_FIXED)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 nonnormal = c("TimeToProgression.iRECIST", "TreatmentDuration"), # show descriptive statistics in median (IQR)
                 exact = c("Age.Dichotomized.Median", "Gender", "Smoking", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median", "Objective.Response.Chemotherapy", "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", "Post.Trt")) # calculate P value by Fisher exact test

write.csv(results, "~/Desktop/baseline.csv") # export table

# BOR RECLASSIFICATION SUMMARY
tableOne <- tableone::CreateTableOne(vars = "Res.Discontinue", factorVars = "Res.Discontinue", data = dataset_FIXED)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE)

# IMMUNE-RELATED ADVERSE EVENT SUMMARY
dataset_irAE <- dataset_FIXED %>% filter(irAE == "Yes")
tableOne <- tableone::CreateTableOne(vars = "AE.Detail", factorVars = "AE.Detail", data = dataset_irAE)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = "AE.Detail") # calculate P value by Fisher exact test

# OBJECTIVE RESPONSE RATE FIGURE
# stacked bar chart for objective response rate
dataset_FIXED %>% count(Best.Response.iRECIST, ContinueTreatment) # calculate number of patients according to the responses
prop.res <- dataset_FIXED %>% group_by(ContinueTreatment, Best.Response.iRECIST) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)*100)
prop.res$Response <- factor(prop.res$Best.Response.iRECIST, levels = c("iCR", 
                                                                       "iPR", 
                                                                       "iSD", 
                                                                       "iUPD", 
                                                                       "iCPD")) # change order
bar <- ggplot2::ggplot() + 
  ggplot2::theme_classic(base_size = 20) +
  ggplot2::geom_bar(ggplot2::aes(y = freq, x = ContinueTreatment, fill = Response), data = prop.res, stat = "identity") + 
  ggsci::scale_fill_jco() + 
  ggplot2::labs(x = "Continuing treatment", y = "Proportion of patients with \n the best overall response")
bar # show stacked bar chart

# ASSOCIATION BETWEEN OBJECTIVE RESPONSE AND VARIABLES
# compare treatment response and the emergence of irAEs
fisher.test(dataset_irAE$ContinueTreatment, dataset_irAE$irAE.AfterProgression.iRECIST)
fisher.test(dataset_FIXED$ContinueTreatment, dataset_FIXED$Objective.Response.iRECIST)

# logistic regression FIXED for predicting objective response
tableone::CreateTableOne(vars = "Objective.Response.iRECIST", data = dataset_FIXED) # describe OR rate summary
tableone::CreateTableOne(vars = "Best.Response.iRECIST", data = dataset_FIXED, includeNA = FALSE) # describe raw data of the radiographic response

# TIME-TO-EVENT FIXED
# overall population
Survfunc.Followup <- survival::survfit(data = dataset_FIXED, 
                                       survival::Surv(FollowUp, Deceased.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # progression -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_FIXED, 
                                       survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # progression -> death
Survfunc.Followup # events, median survival time (95% confidence interval)
# stratified variable of interest
Survfunc.Followup <- survival::survfit(data = dataset_FIXED, 
                                       survival::Surv(ProgressionToLastvisit.iRECIST, Deceased.Binary) ~ 
                                         ContinueTreatment, type = "kaplan-meier", conf.int = 0.95)
Survfunc.Followup # events, median survival time (95% confidence interval)

# calculate the median follow-up
library(pec)
quantile(prodlim::prodlim(Hist(FollowUp, Deceased.Binary) ~ 1, data = dataset_FIXED, reverse = TRUE))

# multivariable Cox regression analysis using finalfit
#1 prepare survival function
dependent_os = "Surv(FollowUp, Deceased.Binary)" # analysis for overall survival (all-cause mortality)
#2 group variable for univariable analysis
Variables = c("Age.Dichotomized.Median", "Gender", "ECOG.Score", "Smoking",
              "Primary.Location", "Metastasis.Category", "Num.Dichotomized.Median",
              "Objective.Response.Chemotherapy", "Objective.Response.iRECIST", "TimeToProgression.iRECIST.Dichotmized.Median", "ClinicoRadiographic.Progression", "ContinueTreatment.iRECIST")

Variables = c("TimeToProgression.iRECIST.Dichotmized.Median", "ClinicoRadiographic.Progression", "ContinueTreatment.iRECIST")

#3 analyze in univariable Cox regression model
results <- dataset_FIXED %>% 
  finalfit::coxphuni(dependent_os, Variables) %>% # Univariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results
#4 analyze in multivariable Cox regression model
results <- dataset_FIXED %>% 
  finalfit::coxphmulti(dependent_os, Variables) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results

write.csv(results, "C:/Users/wfuku/OneDrive/research data/JIKEI/ICI/Cox result.csv") # export table

# test the proportional hazard asumption for a Cox regression model
fit = survival::coxph(survival::Surv(ProgressionToLastvisit, Deceased.Binary) ~ 
                        ContinueTreatment, data = dataset_FIXED)
ftest_pfs <- survival::cox.zph(fit)
hazard <- survminer::ggcoxzph(ftest_pfs)
hazard

# subgroup analysis
# patients without objective response
dataset_FIXED <- dataset_FIXED %>% filter(Objective.Response.iRECIST == "No")

# multivariable Cox regression analysis with time-varying covariates
data_iTime <- dataset_FIXED %>% select(Identifier, Age.Dichotomized.Median, Gender, ECOG.Score, Smoking, Primary.Location, Metastasis.Category, 
                                       Num.Dichotomized.Median, Objective.Response.iRECIST, ClinicoRadiographic.Progression, TimeToProgression.iRECIST.Dichotmized.Median)
data_dTime <- dataset_FIXED %>% select(Identifier, TreatmentDuration.iRECIST, FollowUp, Deceased.Binary)
#1. create start/stop time and time-dependent covariate
spl_dataset <- survival::tmerge(data1 = data_iTime, data2 = data_dTime, 
                                id = Identifier, event = event(FollowUp, Deceased.Binary), ContinueTreatment.iRECIST = tdc(TreatmentDuration.iRECIST)) # tdc = time-dependent covariate
spl_dataset <- spl_dataset %>% mutate(DiscontinueTreatment = case_when(ContinueTreatment.iRECIST == 0 ~ 1, 
                                                                       ContinueTreatment.iRECIST == 1 ~ 0)) # exchange 0 and 1 in spl_dataset
#2. execute multivariable Cox regression analysis
fit = survival::coxph(survival::Surv(tstart, tstop, event) ~ # define survival data (start and stop data)
                        TimeToProgression.iRECIST.Dichotmized.Median + ClinicoRadiographic.Progression + DiscontinueTreatment,
                      cluster = Identifier, data = spl_dataset)
results <- tableone::ShowRegTable(fit, digits = 2, pDigits = 3)

# survival analysis using propensity score
logit.ps <- glm(ContinueTreatment.iRECIST.Binary ~ 
                  ClinicoRadiographic.Progression + TimeToProgression.iRECIST, # factors for calculating propensity scores
                data = dataset_FIXED, family = binomial) # calculate PS using logistic regression model
pscores <- fitted(logit.ps)
dataset_FIXED = dataset_FIXED %>% mutate(
  PS = pscores,# PS = propensity score
  Weight = case_when(ContinueTreatment.iRECIST == "Yes" ~ 1/(PS), TRUE ~ 1/(1-PS))) # Inverse probability of treatment weighting

# PROPENSITY SCORE MATCHING: one-to-one matching
library(MatchIt)
dataset_PS = dataset_FIXED %>% select(FollowUp, Deceased.Binary, Age, Gender, ECOG.Score, Smoking, Primary.Location, Metastasis.Category, Number.Prior.Chemotherapy, 
                                      Objective.Response.iRECIST, ClinicoRadiographic.Progression, TimeToProgression.iRECIST, ContinueTreatment.iRECIST, ContinueTreatment.iRECIST.Binary)
dataset_PS <- na.omit(dataset_PS) # exclude NA
out.matchit <- MatchIt::matchit(formula = ContinueTreatment.iRECIST.Binary ~ 
                                  Age + Gender + ECOG.Score + Smoking + Primary.Location + Metastasis.Category + Number.Prior.Chemotherapy + Objective.Response.iRECIST + ClinicoRadiographic.Progression + TimeToProgression.iRECIST,
                                data = dataset_PS, # must be complete dataset
                                method = "nearest", 
                                caliper = 0.1,
                                distance = "logit",
                                ratio = 1)
summary(out.matchit) # matching result
dataset_MATCHIT <- match.data(out.matchit) # data for further analysis

# multivariable Cox regression analysis using matched cohort
dependent_os = "Surv(FollowUp, Deceased.Binary)" # analysis for overall survival (all-cause mortality)
# group variable for univariable analysis
Variables = c("TimeToProgression.iRECIST", "ClinicoRadiographic.Progression", "ContinueTreatment.iRECIST.Binary")
# analyze in univariable Cox regression model
results <- dataset_MATCHIT %>% 
  finalfit::coxphmulti(dependent_os, Variables) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results # show results

# WEIGHTED analysis: Inverse Probability of Treatment Weight

# IPTW multivariable Cox regression analysis
results <- survival::coxph(survival::Surv(FollowUp, Deceased.Binary) ~ 
                             TimeToProgression.iRECIST + ClinicoRadiographic.Progression + ContinueTreatment.iRECIST,
                           data = dataset_FIXED, weights = Weight) %>% 
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results

# WEIGHTED analysis
# Patient characteristics
# UNWEIGHTED POPULATION
FactorVariables <- c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                     "Number.Prior.Chemotherapy", "Objective.Response.iRECIST") # factor varables
AllVariables <- c("Age", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                  "Number.Prior.Chemotherapy", "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", 
                  "TimeToProgression.iRECIST") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, strata = "ContinueTreatment.iRECIST", data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE) 


# WEIGHTED POPULATION
# overall
dataset_WEIGHT <- survey::svydesign(ids = ~ Identifier, weights = ~ Weight, data = dataset_ANALYSIS)
Weighted.Table <- tableone::svyCreateTableOne(vars = c("Age", "Gender", "ECOG.Score", "Smoking", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                                                       "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", "TimeToProgression.iRECIST"),
                                              factorVars = c("Gender", "ECOG.Score", "Smoking", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                                                             "Objective.Response.iRECIST", "ClinicoRadiographic.Progression"),
                                              data = dataset_WEIGHT, test = FALSE)
results <- print(Weighted.Table, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE)

# stratified
dataset_WEIGHT <- survey::svydesign(ids = ~ Identifier, weights = ~ Weight, data = dataset_ANALYSIS)
Weighted.Table <- tableone::svyCreateTableOne(vars = c("Age", "Gender", "ECOG.Score", "Smoking", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                                                       "Objective.Response.iRECIST", "ClinicoRadiographic.Progression", "TimeToProgression.iRECIST"),
                                              factorVars = c("Gender", "ECOG.Score", "Smoking", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                                                             "Objective.Response.iRECIST", "ClinicoRadiographic.Progression"),
                                              strata = "ContinueTreatment.iRECIST", data = dataset_WEIGHT, test = FALSE)
results <- print(Weighted.Table, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE)

write.csv(results, "C:/Users/wfuku/OneDrive/research data/JIKEI/ICI/baseline.csv") # export table

# calculating differences
library(cobalt)
dataset_WEIGHT <- WeightIt::weightit(ContinueTreatment.iRECIST.Binary ~ TimeToProgression.iRECIST + ClinicoRadiographic.Progression,
                                     data = dataset_ANALYSIS, method = "ps") # calculating IPTW using PS
love.plot(dataset_WEIGHT, thresholds = c(m = .1), binary = "std", which.treat = .all, abs = FALSE) # plotting standardized mean difference
bal.tab(dataset_WEIGHT, un = TRUE) # tabling standardized mean difference