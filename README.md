# cjpwr
`cjpwr` carries out a very simple power analysis for conjoint designs.   


`cjpwr()` takes four arguments: `n` (sample size), `t` (number of choice tasks per respondent), `a` (number of alternatives per choice task), and `c` (number of analysis cells - equal to largest number of possible levels for any one feature, or the largest product of levels of any two attributes for power of two-way interaction estimates). It simply divides `t*n*a` by `c`. The output of `pwr` is a dataframe including the inputs and result of this calculation, whether (yes/no) your design exceeds the minimal minimum threshold (500) and the ideal minimum threshold (1000), and the necessary sample sizes to exceed these thresholds. 

The calculation is based on recommendations from Orme (2010) and Johnson and Orme (2003).
