library(tidyverse)
library(survival)

set.seed(298)

###-------------------------------------------------------------------------------------------------------
##Simulate some development data across different 'populations' to derive a set of 'existing CPMs' - mimics
##those models that might available from the literature
####-------------------------------------------------------------------------------------------------------

#Size of the dataset to simulate in each population:
N1 <- 14000
N2 <- 10000
N3 <- 12000

#Simulate dataset for each population:
IPD1 <- tibble("Age" = rnorm(N1, mean=50, sd=4),
               "SexM" = ifelse(rbinom(N1, 1, 0.55)==1, 1, 0),
               "Smoking_Status" = rbinom(N1, 1, 0.15),
               "Diabetes" = rbinom(N1, 1, 0.1),
               "Creatinine" = ifelse(SexM==1,
                                   ifelse(Age<55,
                                          rgamma(N1, shape = 9, scale = 0.11), rgamma(N1, shape = 6, scale = 0.17)),
                                   ifelse(Age<55,
                                          rgamma(N1, shape = 8, scale = 0.11), rgamma(N1, shape = 6, scale = 0.13))),
               "X1" = rnorm(N1, 0, 1))

IPD2 <- tibble("Age" = rnorm(N2, mean=45, sd=2),
               "SexM" = ifelse(rbinom(N2, 1, 0.5)==1, 1, 0),
               "Smoking_Status" = rbinom(N2, 1, 0.10),
               "Diabetes" = rbinom(N2, 1, 0.15),
               "Creatinine" = ifelse(SexM==1,
                              ifelse(Age<55,
                                     rgamma(N1, shape = 9, scale = 0.113), rgamma(N1, shape = 6, scale = 0.165)),
                              ifelse(Age<55,
                                     rgamma(N1, shape = 8, scale = 0.11), rgamma(N1, shape = 6, scale = 0.125))),
               "X1" = rnorm(N2, 0, 1))

IPD3 <- tibble("Age" = rnorm(N3, mean=55, sd=3),
               "SexM" = ifelse(rbinom(N3, 1, 0.6)==1, 1, 0),
               "Smoking_Status" = rbinom(N3, 1, 0.12),
               "Diabetes" = rbinom(N3, 1, 0.18),
               "Creatinine" = ifelse(SexM==1,
                              ifelse(Age<55,
                                     rgamma(N1, shape = 9, scale = 0.105), rgamma(N1, shape = 6, scale = 0.175)),
                              ifelse(Age<55,
                                     rgamma(N1, shape = 8, scale = 0.105), rgamma(N1, shape = 6, scale = 0.135))),
               "X1" = rnorm(N3, 0, 1))

#Generate event times
True_coefs_pop1 <- c("Age" = log(1.01),
                     "SexM" = log(1.3),
                     "Smoking_Status" = log(2),
                     "Diabetes" = log(1.5),
                     "Creatinine" = log(1.8),
                     "X1" = log(1.02))
True_coefs_pop2 <- c("Age" = log(1.02),
                     "SexM" = log(1.2),
                     "Smoking_Status" = log(1.7),
                     "Diabetes" = log(1.2),
                     "Creatinine" = log(1.5),
                     "X1" = log(1.01))
True_coefs_pop3 <- c("Age" = log(1.01),
                     "SexM" = log(1.1),
                     "Smoking_Status" = log(1.7),
                     "Diabetes" = log(1.0),
                     "Creatinine" = log(1.75),
                     "X1" = log(1.03))

True_LP_pop1 <- as.numeric(data.matrix(IPD1) %*% True_coefs_pop1)
True_LP_pop2 <- as.numeric(data.matrix(IPD2) %*% True_coefs_pop2)
True_LP_pop3 <- as.numeric(data.matrix(IPD3) %*% True_coefs_pop3)

gen_TTE_data <- function(df, betas, CensoringTime, binary_time, scale, shape) {
  N <- nrow(df)

  U <- runif(N, 0, 1)

  LP <- as.numeric(data.matrix(df) %*% betas)
  df <- df %>%
    mutate(ETime = ((-log(U) / (scale*exp(LP)))^(1/shape)),

           Status = ifelse(ETime < CensoringTime, 1, 0),
           ETime = ifelse(ETime < CensoringTime, ETime, CensoringTime),

           Y = ifelse(ETime <= binary_time & Status == 1, 1, 0))
  df
}

IPD1 <- gen_TTE_data(df = IPD1,
                     betas = True_coefs_pop1,
                     CensoringTime = 5,
                     binary_time = 1,
                     scale = 0.02,
                     shape = 1.01)
IPD2 <- gen_TTE_data(df = IPD2,
                     betas = True_coefs_pop2,
                     CensoringTime = 5,
                     binary_time = 1,
                     scale = 0.04,
                     shape = 1.02)
IPD3 <- gen_TTE_data(df = IPD3,
                     betas = True_coefs_pop3,
                     CensoringTime = 5,
                     binary_time = 1,
                     scale = 0.03,
                     shape = 1.04)

# survminer::ggsurvplot(fit = survfit(Surv(ETime, Status) ~ pop, data = bind_rows(IPD1 %>% mutate(pop = "1"),
#                                                                                 IPD2 %>% mutate(pop = "2"),
#                                                                                 IPD3 %>% mutate(pop = "3"))))


## Develop three existing prediction models for the binary outcome on each dataset in turn:
#Existing Model 1
Existing_logistic_Mod1 <- step(glm(Y ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                   data = IPD1,
                                   family = binomial(link = "logit")),
                               direction = "both")

#Existing Model 2
Existing_logistic_Mod2 <- step(glm(Y ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                   data = IPD2,
                                   family = binomial(link = "logit")),
                               direction = "both")

#Existing Model 3
Existing_logistic_Mod3 <- step(glm(Y ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                   data = IPD3,
                                   family = binomial(link = "logit")),
                               direction = "both")

# Store all the required information about the existing models in a data.frame -
# this mimics each having a published paper, from which we can validate them in
# other datasets
logistic_model_info <- data.frame(matrix(NA, ncol = length(unique(c((names(coef(Existing_logistic_Mod1))),
                                                                    (names(coef(Existing_logistic_Mod2))),
                                                                    (names(coef(Existing_logistic_Mod3)))))),
                                         nrow = 1))
names(logistic_model_info) <- unique(c((names(coef(Existing_logistic_Mod1))),
                                       (names(coef(Existing_logistic_Mod2))),
                                       (names(coef(Existing_logistic_Mod3)))))

logistic_model_info <- logistic_model_info %>%
  bind_rows(as.data.frame(t(coef(Existing_logistic_Mod1)))) %>%
  bind_rows(as.data.frame(t(coef(Existing_logistic_Mod2)))) %>%
  bind_rows(as.data.frame(t(coef(Existing_logistic_Mod3)))) %>%
  filter_all(any_vars(!is.na(.))) %>%
  rename("Intercept" = "(Intercept)")


## Develop three existing prediction models for the time-to-event outcome on each dataset in turn:
#Existing Model 1
Existing_TTE_Mod1 <- step(coxph(Surv(ETime, Status) ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                data = IPD1,
                                x = T,
                                y = T),
                          direction = "both")

#Existing Model 2
Existing_TTE_Mod2 <- step(coxph(Surv(ETime, Status) ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                data = IPD2,
                                x = T,
                                y = T),
                          direction = "both")

#Existing Model 3
Existing_TTE_Mod3 <- step(coxph(Surv(ETime, Status) ~ Age + SexM + Smoking_Status + Diabetes + Creatinine,
                                data = IPD3,
                                x = T,
                                y = T),
                          direction = "both")

# Store all the required information about the existing models in a data.frame -
# this mimics each having a published paper, from which we can validate them in
# other datasets
TTE_model_info <- data.frame(matrix(NA, ncol = length(unique(c((names(coef(Existing_TTE_Mod1))),
                                                                    (names(coef(Existing_TTE_Mod2))),
                                                                    (names(coef(Existing_TTE_Mod3)))))),
                                         nrow = 1))
names(TTE_model_info) <- unique(c((names(coef(Existing_TTE_Mod1))),
                                       (names(coef(Existing_TTE_Mod2))),
                                       (names(coef(Existing_TTE_Mod3)))))

TTE_model_info <- TTE_model_info %>%
  bind_rows(as.data.frame(t(coef(Existing_TTE_Mod1)))) %>%
  bind_rows(as.data.frame(t(coef(Existing_TTE_Mod2)))) %>%
  bind_rows(as.data.frame(t(coef(Existing_TTE_Mod3)))) %>%
  filter_all(any_vars(!is.na(.)))


bh <- basehaz(Existing_TTE_Mod1, centered = FALSE)
TTE_mod1_baseline <- as.data.frame(rbind(bh[(max(which(bh[,2] <= 1))),],
                                         bh[(max(which(bh[,2] <= 2))),],
                                         bh[(max(which(bh[,2] <= 3))),],
                                         bh[(max(which(bh[,2] <= 4))),],
                                         bh[(max(which(bh[,2] <= 5))),])) %>%
  mutate(time = ceiling(time)) %>%
  select(time, hazard)
row.names(TTE_mod1_baseline) <- NULL

bh <- basehaz(Existing_TTE_Mod2, centered = FALSE)
TTE_mod2_baseline <- as.data.frame(rbind(bh[(max(which(bh[,2] <= 1))),],
                                         bh[(max(which(bh[,2] <= 2))),],
                                         bh[(max(which(bh[,2] <= 3))),],
                                         bh[(max(which(bh[,2] <= 4))),],
                                         bh[(max(which(bh[,2] <= 5))),])) %>%
  mutate(time = ceiling(time)) %>%
  select(time, hazard)
row.names(TTE_mod2_baseline) <- NULL

bh <- basehaz(Existing_TTE_Mod3, centered = FALSE)
TTE_mod3_baseline <- as.data.frame(rbind(bh[(max(which(bh[,2] <= 1))),],
                                         bh[(max(which(bh[,2] <= 2))),],
                                         bh[(max(which(bh[,2] <= 3))),],
                                         bh[(max(which(bh[,2] <= 4))),],
                                         bh[(max(which(bh[,2] <= 5))),])) %>%
  mutate(time = ceiling(time)) %>%
  select(time, hazard)
row.names(TTE_mod3_baseline) <- NULL



####-------------------------------------------------------------------------------------------------------
## Generate a validation dataset to mimic data on which we wish to externally validate the existing models,
# and apply model updating/aggregation methods
####-------------------------------------------------------------------------------------------------------
N_val <- 20000
ValidationData <- tibble("Age" = rnorm(N_val, mean=50, sd=3.5),
                         "SexM" = ifelse(rbinom(N_val, 1, 0.55)==1, 1, 0),
                         "Smoking_Status" = rbinom(N_val, 1, 0.13),
                         "Diabetes" = rbinom(N_val, 1, 0.20),
                         "Creatinine" = ifelse(SexM==1,
                                             ifelse(Age<55,
                                                    rgamma(N1, shape = 8, scale = 0.105), rgamma(N1, shape = 7, scale = 0.175)),
                                             ifelse(Age<55,
                                                    rgamma(N1, shape = 7, scale = 0.105), rgamma(N1, shape = 7, scale = 0.135))),
                         "X1" = rnorm(N_val, 0, 1))


True_coefs_valpop <- c("Age" = log(1.04),
                       "SexM" = log(1.4),
                       "Smoking_Status" = log(1.2),
                       "Diabetes" = log(1.2),
                       "Creatinine" = log(1.8),
                       "X1" = log(1.01))

ValidationData <- gen_TTE_data(df = ValidationData,
                               betas = True_coefs_valpop,
                               CensoringTime = 5,
                               binary_time = 1,
                               scale = 0.01,
                               shape = 0.75)

# survminer::ggsurvplot(fit = survfit(Surv(ETime, Status) ~ pop, data = bind_rows(IPD1 %>% mutate(pop = "1"),
#                                                                                 IPD2 %>% mutate(pop = "2"),
#                                                                                 IPD3 %>% mutate(pop = "3"),
#                                                                                 ValidationData %>% mutate(pop = "4"))))

ValidationData <- ValidationData %>%
  select(-X1) %>%
  as.data.frame()

####-------------------------------------------------------------------------------------------------------
## Save the data for external use within the package
####-------------------------------------------------------------------------------------------------------

SYNPM <- list("Existing_logistic_models" = logistic_model_info,
              "Existing_TTE_models" = TTE_model_info,
              "TTE_mod1_baseline" = TTE_mod1_baseline,
              "TTE_mod2_baseline" = TTE_mod2_baseline,
              "TTE_mod3_baseline" = TTE_mod3_baseline,
              "ValidationData" = ValidationData)

usethis::use_data(SYNPM,
                  overwrite = TRUE)

