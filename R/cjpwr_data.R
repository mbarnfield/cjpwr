#' @rdname cjpwr_data
#' @title Simple Power Analysis and Sample Size Diagnostic for Conjoint Designs
#' @description Johnson's rule-of-thumb calculation for determining power of conjoint designs.
#' @param data A tidy, long-formate conjoint dataframe.
#' @param formula A formula like those passed to cj/amce/mm functions in `cregg`. RHS variables are used to determine max feature levels.
#' @param id A variable, within data, containing respondent IDs. Must be numeric.
#' @param contest_no A variable, within data, containing contest/choice-task numbers. Must be numeric.
#' @param profile_no A variable, within data, containing within-contest/choice-task profile numbers. Must be numeric.
#' @details \code{cjpwr_data} finds and calculates n, t, a, and c from a tidy conjoint data input (with certain variables specified) and divides the product of n, t, and a by c, to give Johnson's rule-of-thumb estimation of conjoint design power. It returns a dataframe containing the inputs and result of this calculation, whether (yes/no) this exceeds the minimal minimum threshold (500) and ideal minimum threshold (1000), and the sample sizes (rounded up) necessary for minimum and ideal power thresholds. {cjpwr_data} uses features of tidyeval which mean variable names can be specified without quoting ("") and without referring back to the dataframe every time (via data$).
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
#' cjpwr_data(immigration, f1, CaseID, profile, contest_no)
#' ##for taxes dataset, need to create contest_no and profile_no vars
#' #work out how many contests/tasks per respondent
#' tasks_taxes <- sum(taxes$chose_plan)/length(unique(taxes$ID))
#' #work out number of profiles per task
#' profiles_taxes <- nrow(taxes$ID)/length(unique(taxes$ID))
#' profiles_tasks <- profiles_taxes/tasks_taxes
#' #create profile_no and contest_no based on values of profiles_tasks and tasks_taxes
#' taxes$profile_no <- rep(c(1,2))
#' taxes$contest_no <- rep(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8))
#' #run cjpwr_data for power analysis
#' cjpwr_data(taxes, chose_plan ~ taxrate1 + taxrate2 + taxrate3 +
#' taxrate4 + taxrate5 + taxrate6 + taxrev, ID, profile_no, contest_no)
#' #include interaction variable for a pair of features
#' library(tidyverse)
#' immigration$ints <- interaction(immigration$Gender, immigration$PriorEntry)
#' #specify new formula including interaction feature
#' f2 <- ChosenImmigrant ~ Gender + Education + LanguageSkills +
#' CountryOfOrigin + Job + JobExperience + JobPlans +
#' ReasonForApplication + PriorEntry + ints
#' #then just run cjpwr_data with the new formula
#' cjpwr_data(immigration, f2, CaseID, profile, contest_no)



cjpwr_data <- function(data, formula, id, contest_no, profile_no) {

  #check tidyverse installed and load if so
  library("tidyverse")

  #find arguments in data and calculate n, t, and a
  id_enq <- enquo(id)
  n <- data %>%
    dplyr::select(!!id_enq) %>%
    unique() %>%
    nrow()

  contest_enq <- enquo(contest_no)
  t <- data %>%
    dplyr::select(!!contest_enq) %>%
    max()

  pro_enq <- enquo(profile_no)
  a <- data %>%
    dplyr::select(!!pro_enq) %>%
    max()

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
  data.frame(n = n,
             t = t,
             a = a,
             c = c,
             nta_c = design_rep,
             minimum_sufficient = ifelse(design_rep >= 500, "yes", "no"),
             ideal_sufficient = ifelse(design_rep >= 1000, "yes", "no"),
             minimum_n = min_n,
             ideal_n = max_n)
}





