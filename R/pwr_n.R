#' #' @rdname pwr_n
#' @title Simple Sample Size Diagnostic for Conjoint Designs
#' @description Johnson's rule-of-thumb calculation for determining necessary sample size in conjoint designs.
#' @param t The number of choice-tasks per respondent.
#' @param a The number of alternatives per choice task (2 in most designs).
#' @param c The number of analysis cells - equal to largest number of possible levels for any one feature, or the largest product of levels of any two attributes for power of two-way interaction estimates (Johnson and Orme, 2003).
#' @details \code{pwr_n} calculates the sample size needed for a conjoint design to be sufficiently powered, given the number of choice tasks, profiles and maximum number of levels per feature.
#' @examples
#' #conjoint design with five choice tasks per respondent, two alternative profiles per task, and maximum six levels per feature
#' pwr_n(t = 5, a = 2, c = 6)
#' #same design but considering interactions, where largest product of levels of any two attributes is 18 (6*3)
#' pwr_n(t = 5, a = 2, c = 18)
#' #without argument labels, order only matters for c
#' pwr_n(2, 5, 18)
#' pwr_n(5, c = 18, 2)
#' pwr_n(c = 18, a = 2, t = 5)


pwr_n <- 
  function(t, a, c) {
    ta <- t * a
    min_c <- 500 * c
    max_c <- 1000 * c
    min_n <- min_c / ta
    max_n <- max_c /ta
    out1 <- paste("sample size needed for minimum threshold >", 
                  min_n)
    out2 <- paste("much better sample size would be >", 
                  max_n)
    print(out1)
    print(out2)
  }
