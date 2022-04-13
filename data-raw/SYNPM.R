library(tidyverse)

set.seed(298)

###-------------------------------------------------------------------------------------------------------
##Simulate some development data to derive a set of 'existing CPMs' - mimics
##those models that might available from the literature
####-------------------------------------------------------------------------------------------------------

#Size of the dataset to simulate
N <- 200000
# set number of observations each year (arbitrary):
M <- c(20100, 18550, 21000, 22200, 19800, 17500, 25698, 16540, 17970); M[10] <- N-sum(M)

#Simulate a population-level dataset:
IPD <- data.frame("Age" = rnorm(N, mean=50, sd=3),
                  "Sex" = factor(ifelse(rbinom(N, 1, 0.55)==1, "M", "F"))) %>%
  mutate("Smoking_Status" = rbinom(N, 1, 0.15),
         "Diabetes" = rbinom(N, 1, 0.1),
         "CKD" = ifelse(Sex=="M",
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.03), rbinom(n(), 1, 0.06)),
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.08), rbinom(n(), 1, 0.13))),
         "X1" = rnorm(N, 0, 1), #will be an unobserved variable
         "Year" = rep(seq(from=2012, to=2021), times=M))

#Simulate the binary outcomes based on a 'true' data-generating model:
TrueModel_LP <- (-2 + ((IPD$Year - 2012)*0.05)) +
  (log(1.01) + ((IPD$Year - 2012)*0.01)) * (IPD$Age-50) +
  log(1.3)*ifelse(IPD$Sex=="M", 1, 0) +
  log(2)*IPD$Smoking_Status +
  (log(1.5) - ((IPD$Year - 2012)*0.03)) * IPD$Diabetes +
  (log(1.8) + ((IPD$Year - 2012)*0.01)) * IPD$CKD +
  log(1.05)*IPD$X1
TrueModel_PR <- exp(TrueModel_LP)/(1+exp(TrueModel_LP))
IPD$Y <- rbinom(N, 1, TrueModel_PR)


## Assume three existing prediction models have been developed on earliest years
## of data, some of which missing key variables (mimicking what would happen in
## practice)

DerivationData <- IPD %>%
  dplyr::mutate(Age = round(Age)) %>%
  dplyr::select(-X1) %>% #remove the unobserved predictor prior to building a set of 'existing CPMs'
  dplyr::filter(Year %in% 2012:2015)

#Existing Model 1
Existing_Mod1 <- glm(Y ~ Age + Sex + Diabetes,
                     data = DerivationData,
                     family = binomial(link = "logit"))

#Existing Model 2
Existing_Mod2 <- glm(Y ~ Age + Sex + Smoking_Status + Diabetes + CKD,
                     data = DerivationData,
                     family = binomial(link = "logit"))

#Existing Model 3
Existing_Mod3 <- glm(Y ~ Age + Sex + CKD,
                     data = DerivationData %>%
                       dplyr::filter(Year %in% c(2012:2016)),
                     family = binomial(link = "logit"))

# Store all the required information about the existing models in a data.frame -
# this mimics each having a published paper, from which we can validate them in
# other datasets
Existing_models <- data.frame("Model_Name" = c("Existing_Mod1",
                                               "Existing_Mod2",
                                               "Existing_Mod3"),
                              "Coeffs" = c(paste(as.character(as.numeric(coef(Existing_Mod1))), collapse = " | "),
                                           paste(as.character(as.numeric(coef(Existing_Mod2))), collapse = " | "),
                                           paste(as.character(as.numeric(coef(Existing_Mod3))), collapse = " | ")),
                              "Variables" = c(paste(as.character(names(coef(Existing_Mod1))), collapse = " | "),
                                              paste(as.character(names(coef(Existing_Mod2))), collapse = " | "),
                                              paste(as.character(names(coef(Existing_Mod3))), collapse = " | ")),
                              "Formula" = c(paste("~", as.character(formula(Existing_Mod1))[3], sep = ' '),
                                            paste("~", as.character(formula(Existing_Mod2))[3], sep = ' '),
                                            paste("~", as.character(formula(Existing_Mod3))[3], sep = ' ')))

####-------------------------------------------------------------------------------------------------------
## Set the alidation data as being the most recent data
####-------------------------------------------------------------------------------------------------------
ValidationData <- IPD %>%
  dplyr::mutate(Age = round(Age)) %>%
  dplyr::select(-X1) %>% #remove the unobserved predictor prior to building a set of 'existing CPMs'
  dplyr::filter(Year %in% 2016:2021)


####-------------------------------------------------------------------------------------------------------
## Save the data for external use within the package
####-------------------------------------------------------------------------------------------------------

SYNPM <- list("Existing_models" = Existing_models,
              "ValidationData" = ValidationData)

usethis::use_data(SYNPM,
                  overwrite = TRUE)

