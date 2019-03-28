#' @rdname cjpwr_data
#' @title Simple Power Analysis and Sample Size Diagnostic for Conjoint Designs
#' @description Johnson's rule-of-thumb calculation for determining power of conjoint designs.
#' @param data A tidy, long-format conjoint dataframe.
#' @param formula A formula like those passed to cj/amce/mm functions in `cregg`. RHS variables are used to determine max feature levels.
#' @param id A variable, within data, containing respondent IDs. Must be numeric.
#' @return A tibble with columns n (respondents), t (tasks), a (alternatives), c (cells, i.e. largest number of levels per feature), nta_c (nta/c), min_met (whether minimum threshold met y/n), ideal_met (whether ideal threshold met y/n), min_n (minimum n value to meet minimum threshold), and ideal_n (minimum n value to meet ideal threshold).
#' @details \code{cjpwr_data} finds and calculates n, t, a, and c from a tidy conjoint data input (with a formula and id, similar to other tidy conjoint analyses) and divides the product of n, t, and a by c, to give Johnson's rule-of-thumb estimation of conjoint design power. It returns a dataframe containing the inputs and result of this calculation, whether (yes/no) this exceeds the minimal minimum threshold (500) and ideal minimum threshold (1000), and the sample sizes (rounded up) necessary for minimum and ideal power thresholds. {cjpwr_data} uses features of tidyeval which mean variable names can be specified without quoting ("") and without referring back to the dataframe every time (via data$).
#' @export
#' @examples
#' #load example datasets from {cregg} (Leeper 2019)
#' library(cregg)
#' data(immigration)
#' data(taxes)
#' #run cjpwr_data seamlessly on immigration dataset
#' #pre-specify formula
#' f1 <- ChosenImmigrant ~ Gender + Education + LanguageSkills +
#' CountryOfOrigin + Job + JobExperience + JobPlans +
#' ReasonForApplication + PriorEntry
#' cjpwr_data(immigration, f1, CaseID)
#' #same in taxes dataset but without pre-specified formula
#' power_tax <- cjpwr_data(taxes, chose_plan ~ taxrate1 + taxrate2 + taxrate3 +
#' taxrate4 + taxrate5 + taxrate6 + taxrev, ID, profile_no, contest_no)
#' ## include interaction variable for a pair of features
#' library(tidyverse)
#' immigration %>% mutate(ints = interaction(Gender, PriorEntry, sep = "_"))
#' #specify new formula including interaction feature
#' f2 <- ChosenImmigrant ~ Gender + Education + LanguageSkills +
#' CountryOfOrigin + Job + JobExperience + JobPlans +
#' ReasonForApplication + PriorEntry + ints
#' #then just run cjpwr_data with the new formula
#' power_imm <- cjpwr_data(immigration, f2, CaseID)



cjpwr_data <- function(data, formula, id) {

  #check tidyverse installed and load if so
  library("tidyverse")

  #find id arg in data
  id_enq <- enquo(id)

  #extract id column
  id_df <- data %>%
    dplyr::select(!!id_enq)

  #number of respondents
  id_unique <- id_df %>%
    unique() %>%
    nrow()

  #find outcome variable in lhs formula
  outcome <- all.vars(stats::update(formula, . ~ 0))
  if (!length(outcome) || outcome == ".") {
    stop("'formula' is missing a left-hand outcome variable")
  }
  outcome_enq <- enquo(outcome)

  #extract outcome column from data
  outcome <- data %>%
    dplyr::select(!!outcome)

  #total number of tasks
  tasks <- sum(outcome)

  #per respondent
  choices <- tasks/id_unique

  #number of profiles
  profiles <- nrow(id_df)

  #per respondent
  profiles_resp <- profiles/id_unique

  #profiles per task
  profiles_task <- profiles_resp/choices

  #n, t, a
  n <- id_unique
  t <- choices
  a <- profiles_task

  #calculate c
  features <- all.vars(stats::update(formula, 0 ~ .))
  features_df <- dplyr::select(data, one_of(features))
  lengths <- vector("double", ncol(features_df))
  for (i in seq_along(features_df)) {
    lengths[[i]] <- length(unique(features_df[[i]]))
  }

  c <- max(lengths)


  #calculate nta
  nta <- n*t*a

  #calculate nta_c
  design_rep <- nta/c

  #set up calculation to check for necessary n
  ta <- t * a

  min_c <- 500 * c

  max_c <- 1000 * c

  #calculate minimum sample size, rounded up, for minimal threshold
  min_n <- ceiling(min_c / ta)

  #calculate minimum sample size, rounded up, for ideal threshold
  max_n <- ceiling(max_c / ta)

  #generate dataframe of results
  tibble(n = n,
             t = t,
             a = a,
             c = c,
             nta_c = design_rep,
             min_met = ifelse(design_rep >= 500, "yes", "no"),
             ideal_met = ifelse(design_rep >= 1000, "yes", "no"),
             min_n = min_n,
             ideal_n = max_n)
}
