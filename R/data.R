#' SYNthetic Prediction Models (SYNPM) and Validation dataset
#'
#' A list containing: (1) information on some (synthetic) existing prediction models
#' (representing those available/published, which we want to validate in another
#' independent dataset); and (2) a synthetic dataset that we wish to validate the
#' models on.
#'
#' @format A list with two elements.
#' \enumerate{\item{The first element is a data frame with 3
#'   rows and 3 variables:
#'   \describe{
#'       \item{Model_Name}{An (arbitary) name of each of the existing models.}
#'       \item{Coeffs}{Values of the estimated coefficients from each model,
#'                     where each predictor variable (coefficient) is separated
#'                     with a '|'.}
#'       \item{Formula}{A string giving the functional form of the existing
#'                      prediction model}
#'            }
#'            }
#'  \item{
#'  The second element is the (synthetic) validation dataset on which we want to
#'  validate the existing models. The dataset has 118150 rows and 7 variables:
#'  \describe{
#'       \item{Age}{The age of the individual at baseline}
#'       \item{Sex}{The sex of the individual (M = male; F = female)}
#'       \item{Smoking_Status}{Indicates whether the individual was or is a
#'             smoker (1=previous/ current smoker, 0=non-smoker)}
#'      \item{Diabetes}{Indicates whether the individual has diabetes
#'                      (1=diabetic, 0=not diabetic)}
#'      \item{CKD}{Indicates whether the individual has chronic kidney disease
#'                 (1=chronic kidney disease)}
#'      \item{Year}{The year the observation was made (2016-2021)}
#'      \item{Y}{Whether the binary outcome occured (1=yes, 0=no)}
#'            }
#'            }
#' }
#' @source Simulated Data; see \url{https://github.com/GlenMartin31/predRupdate}
"SYNPM"


#' Second Manifestations of ARTerial disease (SMART) dataset
#'
#' The Second Manifestations of ARTerial disease (SMART) study is a
#' single-centre prospective cohort study among patients, newly referred to the
#' hospital with (1) clinically manifest atherosclerotic vessel disease, or (2)
#' marked risk factors for atherosclerosis. Further details on the data can be
#' found here: Simons, P., Algra, A., van de Laak, M. et al. Second
#' Manifestations of ARTerial disease (SMART) study: Rationale and design. Eur J
#' Epidemiol 15, 773–781 (1999). https://doi.org/10.1023/A:1007621514757.
#'
#' Interest is in predicting the occurrence of cardiovascular events; this is
#' defined as time-to-event of the composite endpoint of cardiovascular death,
#' ischemic stroke or myocardial infarction. The dataset includes those patients
#' enrolled in study between September 1996 and March 2006.
#'
#' @format A data with 3873 rows (observations) with 16 variables:
#' \describe{
#'   \item{TEVENT}{Time of event (days)}
#'   \item{EVENT}{Cardiovascular event (0=no, 1=yes)}
#'   \item{SEX}{1 = male, 2 = female}
#'   \item{AGE}{Age (years)}
#'   \item{DIABETES}{History of diabetes (0=no, 1=yes)}
#'   \item{CEREBRAL}{History of cerebrovascular disease (0=no, 1=yes)}
#'   \item{CARDIAC}{History of cardiovascular disease (0=no, 1=yes)}
#'   \item{AAA}{History of abdominal aortic aneurysm (0=no, 1=yes)}
#'   \item{PERIPH}{History of periferal aortic aneurysm (0=no, 1=yes)}
#'   \item{STENOSIS}{History of periferal vascular disease (0=no, 1=yes)}
#'   \item{SYSTBP}{Systolic blood pressure (mm Hg)}
#'   \item{DIASTBP}{Diastolic blood pressure (mm Hg)}
#'   \item{BMIO}{Body mass index (kg/m^2)}
#'   \item{CHOLO}{Cholesterol level (mmol/L)}
#'   \item{albumin}{Albumin in urine (no/low/high)}
#'   \item{SMOKING}{Smoking status (never/former/current)}
#'  }
#'
#' @source \url{http://www.clinicalpredictionmodels.org/doku.php?id=rcode_and_data:start;
#' https://doi.org/10.1023/A:1007621514757}
"SMART"
