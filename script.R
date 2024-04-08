# import database from excel
dataset # import dataset

library(tidyverse)
library(lubridate)

# DATA SELECTION AND CREATE BASESLINE DATA
dataset_FIXED # full analysis set

labelled::var_label(dataset_ANALYSIS) <- list(FollowUp = "Follow-up period, month",
                                              Progression = "Radiographic progression",
                                              Progression.iRECIST = "Radiographic progression according to iRECIST",
                                              Best.Response = "RECIST-defined best overall response",
                                              Best.Response.iRECIST = "iRECIST-defined best overall response",
                                              TimeToProgression = "Time to RECIST-defined progression, month",
                                              TimeToProgression.iRECIST = "Time to iRECIST-defined progression, month",
                                              TimeToDiscont = "Time to treatment discontinuation, month",
                                              TimeToIRAE = "Time to irAE, month",
                                              TimeToBestResponse = "Time to best response, month",
                                              Objective.Response = "Objective response (CR + PR)",
                                              Objective.Response.iRECIST = "Objective response (immune CR + immune PR)",
                                              Pseudoprogression = "Pseudo-progression",
                                              Age = "Age, year", 
                                              Smoking = "Smoking history", 
                                              Gender = "Gender",
                                              BMI = "Body mass index",
                                              ECOG.Score = "ECOG performance status (0 or >=1)", 
                                              Primary.Location = "Primary location of tumor (lower or upper tract)",
                                              Metastasis.Category = "Location of metastases",
                                              Chemotherapy.Delivered = "Context of most recent therapy",
                                              Patient.Condition = "Reasons for chemotherapy discontinuation",
                                              Number.Prior.Chemotherapy = "Number of prior chemotherapy",
                                              PD1.Setting.ForMetastaticDis = "Line of therapy",
                                              Platinum = "Previous platinum therapy",
                                              Bellmunt.Factor = "Bellmunt risk factor",
                                              irAE = "Immune-related adverse event",
                                              Neu.Baseline = "Neutrophil count, per uL",
                                              Lym.Baseline = "Lymphocyte count, per uL",
                                              Mo.Baseline = "Monocyte count, per uL",
                                              Eo.Baseline = "Eosinophil count, per uL",
                                              NLR.Baseline = "Neutrophil-to-lymphocyte ratio",
                                              Hb.Baseline = "Hemoglobin concentration, g/dL",
                                              LDH.Baseline = "Lactate dehydrogenase, IU/L",
                                              CRP.Baseline = "C-reactive protein, g/dL",
                                              Refractory.Lesion.Chemotherapy = "Progressive lesion during chemotherapy", 
                                              Objective.Response.Chemotherapy = "Objective response during chemotherapy", 
                                              PPI.Concurrent = "Concurrent PPI use", 
                                              Analgesics.Concurrent = "Any analgesics use", 
                                              Opioid.Concurrent = "Opioid Analgesic use")

# numeric -> factor
dataset_ANALYSIS$Number.Prior.Chemotherapy <- as.factor(dataset_ANALYSIS$Number.Prior.Chemotherapy)
dataset_ANALYSIS$LN.Metastasis <- as.factor(dataset_ANALYSIS$LN.Metastasis)

# subgroup analysis
dataset_ANALYSIS = dataset_ANALYSIS %>% filter(Acid.Blocker == "P-CAB" | Acid.Blocker == "PPI")
dataset_ANALYSIS = dataset_ANALYSIS %>% filter(Clinical.Progression == "Yes") %>% mutate(
  Stop = case_when((TimeToDiscont - TimeToClinicalProgression) >= 0.75 ~ "Continue after clinical progression",
                   ((TimeToDiscont - TimeToClinicalProgression) <= 0.75 | (TimeToDiscont - TimeToClinicalProgression) >= -0.75) ~ "Discontinue at clinical progression",
                   TRUE ~ "Discontinue at radiographic progression"))
dataset_ANALYSIS$Stop <- gdata::reorder.factor(dataset_ANALYSIS$Stop, new.order = c("Discontinue at radiographic progression", 
                                                                                    "Discontinue at clinical progression",
                                                                                    "Continue after clinical progression")) # change order of factor

# CREATE BASELINE CHARACTERISRICS TABLE
# overall population
FactorVariables <- c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Analgesics.Concurrent", "Acid.Blocker") # factor varables
AllVariables <- c("Age", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Analgesics.Concurrent", "Acid.Blocker") # numeric and factor variables

# descriptive statistics of all patients
all.patients <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, data = dataset_ANALYSIS) %>% 
  print(., nonnormal = c("Age"), catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE) # calculate P value by Fisher exact test

# descriptive statistics of patients according to the variable of interest
stratified.patients <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, strata = "PPI.Concurrent", data = dataset_ANALYSIS) %>% 
  print(., catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
        nonnormal = c("Age"), # show descriptive statistics in median (IQR)
        exact = c("Gender", "Smoking", "ECOG.Score", "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", "Analgesics.Concurrent")) # calculate P value by Fisher exact test

stratified.patients <- stratified.patients[, -1] # remove the column named "level"
tableone <- cbind(all.patients, stratified.patients)

write.csv(tableone, "~/OneDrive/research data/JIKEI/ICI/export.csv") # export table

# BOR RECLASSIFICATION SUMMARY
tableOne <- tableone::CreateTableOne(vars = "Best.Response", factorVars = "Best.Response", strata = "Progression.iRECIST", data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = "Best.Response") # calculate P value by Fisher exact test

# TREATMENT DISCONTINUATION SUMMARY
dataset_ANALYSIS <- dataset_ANALYSIS %>% filter(Progression == "Yes")
tableOne <- tableone::CreateTableOne(vars = c("Treatment.Discont.RECIST", "Treatment.Discont.iRECIST"), factorVars = c("Treatment.Discont.RECIST", "Treatment.Discont.iRECIST"), data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = c("Treatment.Discont.RECIST", "Treatment.Discont.iRECIST")) # calculate P value by Fisher exact test

# IMMUNE-RELATED ADVERSE EVENT SUMMARY
dataset_irAE <- dataset_FIXED %>% filter(irAE == "Yes")
tableOne <- tableone::CreateTableOne(vars = "AE.Detail", factorVars = "AE.Detail", data = dataset_irAE)
results <- print(tableOne, catDigits = 1, contDigits = 1, pDigits = 3, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE,
                 exact = "AE.Detail") # calculate P value by Fisher exact test

# OBJECTIVE RESPONSE RATE FIGURE
# stacked bar chart for objective response rate
dataset_ANALYSIS %>% count(Best.Response.iRECIST, PPI.Concurrent) # calculate number of patients according to the responses
prop.res <- dataset_ANALYSIS %>% group_by(PPI.Concurrent, Best.Response.iRECIST) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)*100)
prop.res$Response <- factor(prop.res$Best.Response.iRECIST, levels = c("iCR", "iPR", "iSD", "iUPD", "iCPD")) # change order
bar <- ggplot2::ggplot(ggplot2::aes(y = freq, x = PPI.Concurrent, fill = Response, label = round(freq, 1)), data = prop.res) + 
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::geom_text(position = position_stack(vjust = 0.5), color = "white", size = 6) + 
  ggsci::scale_fill_npg() + 
  ggplot2::theme_classic(base_size = 20) +
  ggplot2::labs(x = "Acid blocker", y = "Proportion of patients with the best overall response")
bar # show stacked bar chart

# ASSOCIATION BETWEEN OBJECTIVE RESPONSE AND VARIABLES
fisher.test(dataset_ANALYSIS$PPI.Concurrent, dataset_ANALYSIS$Objective.Response.iRECIST)

# logistic regression analysis for predicting objective response
tableone::CreateTableOne(vars = "Objective.Response.iRECIST", strata = "Acid.Blocker", data = dataset_ANALYSIS) # describe OR rate summary
tableone::CreateTableOne(vars = "Best.Response.iRECIST", data = dataset_ANALYSIS, includeNA = FALSE) # describe raw data of the radiographic response

Explanatory = c("Age", "Gender", "ECOG.Score", "Smoking",
                "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy",
                "Analgesics.Concurrent", "PPI.Concurrent")
Dependent = "Objective.Response.iRECIST.Binary"

#3 analyze in univariable logistic regression model
uni.results <- dataset_ANALYSIS %>% 
  finalfit::glmuni(Dependent, Explanatory) %>% # Univariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "OR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")

#4 analyze in multivariable logistic regression model
multi.results <- dataset_ANALYSIS %>% 
  finalfit::glmmulti(Dependent, Explanatory) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "OR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")

result <- full_join(uni.results, multi.results, by = "Variable")
print(result) # show results

# TIME-TO-EVENT ANALYSIS
# overall population
Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(FollowUp, Deceased.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(TimeToProgression, Progression.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

Survfunc.Followup <- survival::survfit(data = dataset_ANALYSIS, 
                                       survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ 
                                         1, type = "kaplan-meier", conf.int = 0.95) # baseline -> death
Survfunc.Followup # events, median survival time (95% confidence interval)

# stratified variable of interest
Survfunc.Followup <- dataset_ANALYSIS %>% filter() %>%
  survival::survfit(data = ., 
                    survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ 
                      PPI.Concurrent, type = "kaplan-meier", conf.int = 0.95)
Survfunc.Followup # events, median survival time (95% confidence interval)

# calculate the median follow-up
library(pec)
followup <- quantile(prodlim::prodlim(Hist(FollowUp, Deceased.Binary) ~ 1, data = dataset_ANALYSIS, reverse = TRUE))

# pairwise survival difference
pfs.pairwise <- survminer::pairwise_survdiff(survival::Surv(FollowUp, Deceased.Binary) ~ Acid.Blocker, data = dataset_ANALYSIS)
pfs.pairwise

# multivariable Cox regression analysis using finalfit
#1 prepare survival function
dependent_pfs = "Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary)" # analysis for progression-free survival (disease progression)
dependent_os = "Surv(FollowUp, Deceased.Binary)" # analysis for overall survival (all-cause mortality)
#2 group variable for univariable analysis
Variables = c("Age", "Gender", "ECOG.Score", "Smoking",
              "Primary.Location", "Metastasis.Category", "Number.Prior.Chemotherapy", 
              "Statin.Concurrent")

#3 analyze in univariable Cox regression model
uni.results <- dataset_ANALYSIS %>% 
  finalfit::coxphuni(dependent_os, Variables) %>% # Univariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "Univariable | HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")

#4 analyze in multivariable Cox regression model
multi.results <- dataset_ANALYSIS %>% 
  finalfit::coxphmulti(dependent_os, Variables) %>% # Multivariable Cox regression analysis
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "Multivariable | HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")

result <- full_join(uni.results, multi.results, by = "Variable")
print(result) # show results

write.csv(result, "~/OneDrive/research data/JIKEI/ICI/result.csv") # export table

# Kaplan-Meier curves
Survival <- survminer::surv_fit(survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ 
                                  PPI.Concurrent, data = dataset_ANALYSIS)
survminer::ggsurvplot(Survival, risk.table = TRUE, font.tickslab = 14, font.x = 14, font.y = 14,
                      legend = "top", legend.labs = c("PPI = No", "PPI = Yes"),
                      xlab = "Time from treatment initiation, month", ylab = "Progression-free survival probability",
                      palette = "aaas", size = 0.8, censor.shape = "O", censor.size = 4,
                      tables.y.text = FALSE, risk.table.title = "Number at risk",
                      ggtheme = theme_classic(), tables.height = 0.17, risk.table.fontsize = 5,
                      tables.theme = survminer::theme_cleantable())

# Kaplan-Meier curves
Survival <- dataset_ANALYSIS %>% filter() %>%
  survminer::surv_fit(survival::Surv(FollowUp, Deceased.Binary) ~ All.AB,
                      data = .)
survminer::ggsurvplot(Survival, risk.table = TRUE, font.tickslab = 14, font.x = 14, font.y = 14,
                      legend = "top", legend.labs = c("None", "H2 blocker", "PPI", "P-CAB"), legend.title = "Acid blocker", 
                      xlab = "Time from treatment initiation, month", ylab = "Progression-free survival probability",
                      palette = "jama", size = 0.4, censor.shape = "O", censor.size = 3,
                      tables.y.text = FALSE, risk.table.title = "Number at risk", risk.table.y.text = TRUE,
                      ggtheme = theme_classic(), tables.height = 0.17, risk.table.fontsize = 4.5,
                      tables.theme = survminer::theme_cleantable())

# subgroup analysis
dataset_FIXED <- dataset_FIXED %>% filter(Analgesics.Concurrent == "No") # subgroup analysis

# survival analysis using propensity score
logit.ps <- glm(PPI.Concurrent.Binary ~ 
                  Age + Gender + ECOG.Score + Smoking + Primary.Location + Metastasis.Category + 
                  Number.Prior.Chemotherapy + Analgesics.Concurrent + Antibiotics.Concurrent, # factors for calculating propensity scores
                data = dataset_ANALYSIS, family = binomial) # calculate PS using logistic regression model
pscores <- fitted(logit.ps)
dataset_ANALYSIS = dataset_ANALYSIS %>% mutate(
  PS = pscores,# PS = propensity score
  Weight = case_when(PPI.Concurrent == "Yes" ~ 1/(PS), TRUE ~ 1/(1-PS))) # Inverse probability of treatment weighting


# IPTW univariable Cox regression analysis
# risk of progression
results <- dataset_ANALYSIS %>% 
  survival::coxph(survival::Surv(TimeToProgression.iRECIST, Progression.iRECIST.Binary) ~ PPI.Concurrent,
                  weights = Weight, data = .) %>% 
  finalfit::fit2df(explanatory_name = "Variable", 
                   estimate_name = "HR (95% CI, P value)", 
                   p_name = "P", 
                   digits = c(2, 2, 3), 
                   confint_sep = "-")
results

# WEIGHTED ANALYSIS
# Patient characteristics
# UNWEIGHTED POPULATION
FactorVariables <- c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                     "Number.Prior.Chemotherapy", "Analgesics.Concurrent", "Antibiotics.Concurrent") # factor varables
AllVariables <- c("Age", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                  "Number.Prior.Chemotherapy", "Analgesics.Concurrent", "Antibiotics.Concurrent") # numeric and factor variables

tableOne <- tableone::CreateTableOne(vars = AllVariables, factorVars = FactorVariables, strata = "PPI.Concurrent", data = dataset_ANALYSIS)
results <- print(tableOne, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE) 


# WEIGHTED POPULATION
# overall
dataset_WEIGHT <- survey::svydesign(ids = ~ Identifier, weights = ~ Weight, data = dataset_ANALYSIS)
Weighted.Table <- tableone::svyCreateTableOne(vars = c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                                                       "Number.Prior.Chemotherapy", "Analgesics.Concurrent"),
                                              factorVars = c("Age", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                                                             "Number.Prior.Chemotherapy", "Analgesics.Concurrent"),
                                              data = dataset_WEIGHT, test = FALSE)
results <- print(Weighted.Table, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE)

# stratified
dataset_WEIGHT <- survey::svydesign(ids = ~ Identifier, weights = ~ Weight, data = dataset_ANALYSIS)
Weighted.Table <- tableone::svyCreateTableOne(vars = c("Age", "Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                                                       "Number.Prior.Chemotherapy", "Analgesics.Concurrent"),
                                              factorVars = c("Smoking", "Gender", "ECOG.Score", "Primary.Location", "Metastasis.Category", 
                                                             "Number.Prior.Chemotherapy", "Analgesics.Concurrent"),
                                              strata = "PPI.Concurrent", data = dataset_WEIGHT, test = FALSE)
results <- print(Weighted.Table, catDigits = 1, contDigits = 1, explain = TRUE, varLabels = TRUE, noSpaces = TRUE, showAllLevels = TRUE, smd = TRUE)