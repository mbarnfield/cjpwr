#' @name cjpwr
#'
#' @rdname cjpwr
#' @title Simple Power Analysis and Sample Size Diagnostic for Conjoint Designs
#' @description Johnson's rule-of-thumb calculation for determining power of conjoint designs.
#' @param n The sample size.
#' @param t The number of choice-tasks per respondent.
#' @param a The number of alternatives per choice task (2 in most designs).
#' @param c The number of analysis cells - equal to largest number of possible levels for any one feature, or the largest product of levels of any two attributes for power of two-way interaction estimates (Johnson and Orme, 2003).
#' @details \code{pwr} divides the product of n, t, and a by c, to give Johnson's rule-of-thumb estimation of conjoint design power. It returns a dataframe containing the inputs and result of this calculation, whether (yes/no) this exceeds the minimal minimum threshold (500) and ideal minimum threshold (1000), and and the necessary sample sizes (rounded up) necessary for minimum and ideal power thresholds.
#' @export
#' @examples
#' #conjoint design with five choice tasks per respondent, two alternative profiles per task, and maximum six levels per feature
#' pwr(n = 1000, t = 5, a = 2, c = 6)
#' #same design but considering interactions, where largest product of levels of any two attributes is 18 (6*3)
#' pwr(n = 1000, t = 5, a = 2, c = 18)
#' #without argument labels, order only matters for c
#' pwr(5, 1000, 2, 18)
#' pwr(5, c = 18, 1000, 2)
#' pwr(c = 18, n = 1000, a = 2, t = 5)
#' @seealso \code{\link{pwr_n}}

cjpwr <-
  function(n, t, a = 2, c) {

    nta <- n*t*a

    #calculate figure to be compared against 500 and 1000
    design_rep <- nta/c

    #set up calculation to check for necessary n
    ta <- t * a

    min_c <- 500 * c

    max_c <- 1000 * c

    #calculate minimum sample size, rounded up, for minimal threshold
    min_n <- ceiling(min_c / ta)

    #calculate minimum sample size, rounded up, for ideal threshold
    max_n <- ceiling(max_c / ta)

    #return dataframe of results
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
