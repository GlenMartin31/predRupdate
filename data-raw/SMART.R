library(tidyverse)

#The Second Manifestations of ARTerial disease (SMART) dataset was downloaded from:
#http://www.clinicalpredictionmodels.org/doku.php?id=rcode_and_data:start. The
#data was made freely available by Ewout Steyerberg who used the SMART data in
#his book 'Clinical prediction models: A Practical Approach to Development,
#Validation and Updating'. Further details on the data can be found here:
#Simons, P., Algra, A., van de Laak, M. et al. Second Manifestations of ARTerial
#disease (SMART) study: Rationale and design. Eur J Epidemiol 15, 773–781
#(1999). https://doi.org/10.1023/A:1007621514757

SMART <- read_csv(here::here("data-raw", "SMART.csv"))

SMART <- SMART %>%
  dplyr::select(-SYSTH, -DIASTH) %>% #remove the 'by-hand' calculations of SBP/DBP (include the 'automatic' ones)
  dplyr::select(-LENGTHO, -WEIGHTO) %>% #just keep BMI, and remove height and weight separately
  dplyr::mutate(AGE_squared = AGE^2,
                SBP_cat = dplyr::case_when(SYSTBP < 120 ~ 1,
                                           SYSTBP >= 120 & SYSTBP < 130 ~ 2,
                                           SYSTBP >= 130 & SYSTBP < 140 ~ 3,
                                           SYSTBP >= 140 & SYSTBP < 160 ~ 4,
                                           SYSTBP >= 160 ~ 5),
                DBP_cat = dplyr::case_when(DIASTBP < 80 ~ 1,
                                           DIASTBP >= 80 & DIASTBP < 85 ~ 2,
                                           DIASTBP >= 85 & DIASTBP < 90 ~ 3,
                                           DIASTBP >= 90 & DIASTBP < 100 ~ 4,
                                           DIASTBP >= 100 ~ 5),
                max_SBP_DBP_cat = pmax(SBP_cat, DBP_cat),
                Hypertension_Cat = ifelse(SBP_cat == 1 & DBP_cat == 1, 1,
                                          max_SBP_DBP_cat),
                Hypertension_Cat = forcats::fct_recode(factor(Hypertension_Cat),
                                                       "Optimal" = "1",
                                                       "Normal" = "2",
                                                       "High" = "3",
                                                       "Hypertension_S1" = "4",
                                                       "Hypertension_S2" = "5"),
                CHOLO = CHOLO * 38.67, #convert from mmol/L to mg/dL
                HDLO = HDLO * 38.67,
                LDLO = LDLO * 38.67,
                TRIGO = TRIGO * 38.67,

                Total_Chol_cat = factor(dplyr::case_when(CHOLO < 160 ~ "less160",
                                                         CHOLO >= 160 & CHOLO < 200 ~ "160_199",
                                                         CHOLO >= 200 & CHOLO < 240 ~ "200_239",
                                                         CHOLO >= 240 & CHOLO < 280 ~ "240_279",
                                                         CHOLO >= 280 ~ "more280"),
                                        levels = c("160_199", "less160", "200_239", "240_279", "more280")),

                HDL_Chol_cat = factor(dplyr::case_when(HDLO < 35 ~ "less35",
                                                       HDLO >= 35 & HDLO < 45 ~ "35_44",
                                                       HDLO >= 45 & HDLO < 50 ~ "45_49",
                                                       HDLO >= 50 & HDLO < 60 ~ "50_59",
                                                       HDLO >= 60 ~ "more60"),
                                      levels = c("45_49", "less35", "35_44", "50_59", "more60")),

                SEX = factor(dplyr::case_when(SEX == 1 ~ "M",
                                              SEX == 2 ~ "F"),
                             levels = c("F", "M")),

                albumin = factor(dplyr::case_when(albumin == 1 ~ "no",
                                                  albumin == 2 ~ "low",
                                                  albumin == 3 ~ "high")),

                SMOKING = factor(dplyr::case_when(SMOKING == 1 ~ "no",
                                                  SMOKING == 2 ~ "yes",
                                                  SMOKING == 3 ~ "yes")),

                alcohol = factor(dplyr::case_when(alcohol == 1 ~ "no",
                                                  alcohol == 2 ~ "yes",
                                                  alcohol == 3 ~ "yes")),

                Fam_Hist = 0 #missing from data, but needed for PROCAM model: assume no for all
                ) %>%
  dplyr::select(-SBP_cat, -DBP_cat, -max_SBP_DBP_cat,
                -IMTO,
                -packyrs,
                -alcohol) %>%
  as.data.frame()


####-------------------------------------------------------------------------------------------------------
## Enter information about the FRAMINGHAM existing risk models
# as obtained from Wilson et al. https://doi.org/10.1161/01.CIR.97.18.1837
####-------------------------------------------------------------------------------------------------------

Framingham_Male_model_info <- data.frame("AGE" = 0.04826,
                                         "Total_Chol_cat_less160" = -0.65945,
                                         "Total_Chol_cat_200_239" = 0.17692,
                                         "Total_Chol_cat_240_279" = 0.50539,
                                         "Total_Chol_cat_more280" = 0.65713,
                                         "HDL_Chol_cat_less35" = 0.49744,
                                         "HDL_Chol_cat_35_44" = 0.24310,
                                         "HDL_Chol_cat_50_59" = -0.05107,
                                         "HDL_Chol_cat_more60" = -0.48660,
                                         "Hypertension_Cat_Optimal" = -0.00226,
                                         "Hypertension_Cat_High" = 0.28320,
                                         "Hypertension_Cat_Hypertension_S1" = 0.52168,
                                         "Hypertension_Cat_Hypertension_S2" = 0.61859,
                                         "DIABETES" = 0.42839,
                                         "SMOKING_yes" = 0.52337)
Framingham_Male_baseline <- data.frame("t" = 10,
                                       "h" = -log(0.90015))

Framingham_Female_model_info <- data.frame("AGE" = 0.33766,
                                           "AGE_squared" = -0.00268,
                                           "Total_Chol_cat_less160" = -0.26138,
                                           "Total_Chol_cat_200_239" = 0.20771,
                                           "Total_Chol_cat_240_279" = 0.24385,
                                           "Total_Chol_cat_more280" = 0.53513,
                                           "HDL_Chol_cat_less35" = 0.84312,
                                           "HDL_Chol_cat_35_44" = 0.37796,
                                           "HDL_Chol_cat_45_49" = 0.19785,
                                           "HDL_Chol_cat_more60" = -0.42951,
                                           "Hypertension_Cat_Optimal" = -0.53363,
                                           "Hypertension_Cat_High" = -0.06773,
                                           "Hypertension_Cat_Hypertension_S1" = 0.26288,
                                           "Hypertension_Cat_Hypertension_S2" = 0.46573,
                                           "DIABETES" = 0.59626,
                                           "SMOKING_yes" = 0.29246)
Framingham_Female_baseline <- data.frame("t" = 10,
                                         "h" = -log(0.96246))


####-------------------------------------------------------------------------------------------------------
## Enter information about the PROCAM existing risk models
# as obtained from Assmann et al. https://doi.org/10.1161/hc0302.102575
####-------------------------------------------------------------------------------------------------------

PROCAM_model_info <- data.frame("AGE" = 0.103,
                                "LDLO" = 0.013,
                                "SMOKING_yes" = 0.658,
                                "HDLO" = -0.032,
                                "SYSTBP" = 0.010,
                                "Fam_Hist" = 0.382,
                                "DIABETES" = 0.399,
                                "TRIGO" = 0.317)
PROCAM_baseline <- data.frame("t" = c(1:10),
                              "h" = -log(c(0.9944, 0.9885, 0.9827, 0.9761, 0.9697,
                                           0.9629, 0.9573, 0.9513, 0.9444, 0.9369)))

####-------------------------------------------------------------------------------------------------------
## Save the data for external use within the package
####-------------------------------------------------------------------------------------------------------

Existing_models <- Framingham_Male_model_info %>%
  bind_rows(Framingham_Female_model_info) %>%
  bind_rows(PROCAM_model_info)

rownames(Existing_models) <- c("Framingham_Male_model_info",
                               "Framingham_Female_model_info",
                               "PROCAM_model_info")

SMART <- list("Existing_models" = Existing_models,
              "Framingham_Male_baseline" = Framingham_Male_baseline,
              "Framingham_Female_baseline" = Framingham_Female_baseline,
              "PROCAM_baseline" = PROCAM_baseline,
              "SMART_dataset" = SMART)

usethis::use_data(SMART,
                  overwrite = TRUE)


