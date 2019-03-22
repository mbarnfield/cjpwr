#' @rdname pwr
#' @title Simple Power Analysis for Conjoint Designs
#' @description Johnson's rule-of-thumb calculation for determining power of conjoint designs.
#' @param n The sample size.
#' @param t The number of choice-tasks per respondent.
#' @param a The number of alternatives per choice task (2 in most designs).
#' @param c The number of analysis cells - equal to largest number of possible levels for any one feature, or the largest product of levels of any two attributes for power of two-way interaction estimates (Johnson and Orme, 2003).
#' @details \code{pwr} divides the product of n, t, and a by c, to give Johnson's rule-of-thumb estimation of conjoint design power. It returns the result of this calculation and statements of whether this exceeds the minimal minimum threshold (500) and ideal minimum threshold (1000).
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

pwr <-
  function(n, t, a, c) {
    nta <- n*t*a
    design_rep <- nta/c
    out1 <- paste("nta/c =", design_rep)
    out2 <- ifelse(design_rep >= 500,
                   "sufficient power for minimum threshold",
                   "insufficient power for minimum threshold")
    out3 <- ifelse(design_rep >= 1000,
                   "sufficient power for ideal threshold",
                   "insufficient power for ideal threshold")
    print(out1)
    print(out2)
    print(out3)
  }
