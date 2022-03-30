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

#Simulate the development dataset:
DerivationData <- tibble("Age" = rnorm(N, mean=50, sd=3),
                         "Sex" = factor(ifelse(rbinom(N, 1, 0.55)==1, "M", "F"))) %>%
  mutate("Smoking_Status" = rbinom(N, 1, 0.15),
         "Diabetes" = rbinom(N, 1, 0.1),
         "CKD" = ifelse(Sex=="M",
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.03), rbinom(n(), 1, 0.06)),
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.08), rbinom(n(), 1, 0.13))),
         "X1" = rnorm(N, 0, 1), #will be an unobserved variable
         "Year" = rep(seq(from=2009, to=2018), times=M))

#Simulate the binary outcomes based on a 'true' data-generating model:
TrueModel_LP <- (-1+(ifelse(DerivationData$Year<2016, 0, ((2016-DerivationData$Year)*0.05)))) +
  ((log(1.1))*(DerivationData$Age-50) + ((DerivationData$Year - 2009)*(0.01)*(DerivationData$Age-50))) +
  log(1.3)*ifelse(DerivationData$Sex=="M", 1, 0) +
  log(2)*DerivationData$Smoking_Status +
  log(1.5)*DerivationData$Diabetes +
  log(1.8)*DerivationData$CKD + ((DerivationData$Year - 2009)*(0.05)*(DerivationData$CKD)) +
  log(1.05)*DerivationData$X1
TrueModel_PR <- exp(TrueModel_LP)/(1+exp(TrueModel_LP))
DerivationData$Y <- rbinom(N, 1, TrueModel_PR)

DerivationData <- DerivationData %>%
  select(-X1) #remove the unobserved predictor prior to building a set of 'existing CPMs'
DerivationData$Age <- round(DerivationData$Age)

#Existing Model 1 - derived on early years of data - misses some important predictors
Existing_Mod1 <- glm(Y ~ Age + Sex + Smoking_Status + Diabetes,
                     data = DerivationData %>%
                       dplyr::filter(Year %in% c(2009:2012)),
                     family = binomial(link = "logit"))

#Existing Model 2 - derived on early years of data, with all the predictors
Existing_Mod2 <- glm(Y ~ Age + Sex + Smoking_Status + Diabetes + CKD,
                     data = DerivationData %>%
                       dplyr::filter(Year %in% c(2009:2012)),
                     family = binomial(link = "logit"))

#Existing Model 3 - middle years of data, but misses important predictors
Existing_Mod3 <- glm(Y ~ Age + Sex + CKD,
                     data = DerivationData %>%
                       dplyr::filter(Year %in% c(2012:2016)),
                     family = binomial(link = "logit"))

#Existing Model 3 - derived on latest years of data, with all the predictors
Existing_Mod4 <- glm(Y ~ Age + Sex + Smoking_Status + Diabetes + CKD,
                     data = DerivationData %>%
                       dplyr::filter(Year %in% c(2015:2018)),
                     family = binomial(link = "logit"))


# Store all the required information about the existing models in a data.frame -
# this mimics each having a published paper, from which we can validate them in
# other datasets

Existing_models <- data.frame("Model_Name" = c("Existing_Mod1",
                                               "Existing_Mod2",
                                               "Existing_Mod3",
                                               "Existing_Mod4"),
                              "Coeffs" = c(paste(as.character(as.numeric(coef(Existing_Mod1))), collapse = " | "),
                                           paste(as.character(as.numeric(coef(Existing_Mod2))), collapse = " | "),
                                           paste(as.character(as.numeric(coef(Existing_Mod3))), collapse = " | "),
                                           paste(as.character(as.numeric(coef(Existing_Mod4))), collapse = " | ")),
                              "Variables" = c(paste(as.character(names(coef(Existing_Mod1))), collapse = " | "),
                                              paste(as.character(names(coef(Existing_Mod2))), collapse = " | "),
                                              paste(as.character(names(coef(Existing_Mod3))), collapse = " | "),
                                              paste(as.character(names(coef(Existing_Mod4))), collapse = " | ")),
                              "Formula" = c(paste("~", as.character(formula(Existing_Mod1))[3], sep = ' '),
                                            paste("~", as.character(formula(Existing_Mod2))[3], sep = ' '),
                                            paste("~", as.character(formula(Existing_Mod3))[3], sep = ' '),
                                            paste("~", as.character(formula(Existing_Mod4))[3], sep = ' ')))

####-------------------------------------------------------------------------------------------------------
## Simulate the (external) validation data
####-------------------------------------------------------------------------------------------------------
N <- 20000
ValidationData <- data.frame("Age" = rnorm(N, mean=55, sd=2),
                             "Sex" = factor(ifelse(rbinom(N, 1, 0.48)==1, "M", "F"))) %>%
  mutate("Smoking_Status" = rbinom(N, 1, 0.16),
         "Diabetes" = rbinom(N, 1, 0.08),
         "CKD" = ifelse(Sex=="M",
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.05), rbinom(n(), 1, 0.08)),
                        ifelse(Age<55,
                               rbinom(n(), 1, 0.07), rbinom(n(), 1, 0.10))),
         "X1" = rnorm(N, 0, 1))

TrueModel_LP <- (-1+((2016-2018)*0.05)) +
  log(1.05)*(ValidationData$Age-55) +
  log(1.1)*ifelse(ValidationData$Sex=="M", 1, 0) +
  log(1.7)*ValidationData$Smoking_Status +
  log(2)*ValidationData$Diabetes +
  log(1.6)*ValidationData$CKD +
  log(1.10)*ValidationData$X1
TrueModel_PR <- exp(TrueModel_LP)/(1+exp(TrueModel_LP))

ValidationData$Y <- rbinom(N, 1, TrueModel_PR)

ValidationData <- ValidationData %>%
  select(-X1)

ValidationData$Age <- round(ValidationData$Age)



####-------------------------------------------------------------------------------------------------------
## Save the data for external use within the package
####-------------------------------------------------------------------------------------------------------

SYNPM <- list("Existing_models" = Existing_models,
              "ValidationData" = ValidationData)

usethis::use_data(SYNPM,
                  overwrite = TRUE)

