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
  dplyr::select(-HDLO,
                -LDLO,
                -TRIGO,
                -HOMOCO,
                -GLUTO,
                -CREATO,
                -IMTO,
                -alcohol,
                -packyrs) %>% #remove some variable to keep dataset as simple as possible for examples
  dplyr::mutate(SEX = factor(case_when(SEX == 1 ~ "M",
                                       SEX == 2 ~ "F"),
                             levels = c("F", "M")),

                albumin = factor(case_when(albumin == 1 ~ "no",
                                           albumin == 2 ~ "low",
                                           albumin == 3 ~ "high")),

                SMOKING = factor(case_when(SMOKING == 1 ~ "never",
                                           SMOKING == 2 ~ "former",
                                           SMOKING == 3 ~ "current"))) %>%
  as.data.frame()


####-------------------------------------------------------------------------------------------------------
## Save the data for external use within the package
####-------------------------------------------------------------------------------------------------------

usethis::use_data(SMART,
                  overwrite = TRUE)


