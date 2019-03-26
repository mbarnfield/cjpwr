# cjpwr
`cjpwr` carries out very simple power analyses for conjoint designs.   


[`cjpwr()`](https://github.com/mbarnfield/cjpwr/blob/master/R/cjpwr.R) takes four arguments: `n` (sample size), `t` (number of choice tasks per respondent), `a` (number of alternatives per choice task), and `c` (number of analysis cells - equal to largest number of possible levels for any one feature, or the largest product of levels of any two attributes for power of two-way interaction estimates). It simply divides `t*n*a` by `c`. The output of `pwr` is a dataframe including the inputs and result of this calculation, whether (yes/no) your design exceeds the minimal minimum threshold (500) and the ideal minimum threshold (1000), and the necessary sample sizes to exceed these thresholds. 

[`cjpwr_data()`](https://github.com/mbarnfield/cjpwr/blob/master/R/cjpwr_data.R) does the same thing but a bit more cleverly. It requires certain variables to exist in your data (profile number, contest/task number and respondent ID - all very advisable inclusions for carrying out other diagnostics anyway) but using these it calculates values of n, t, a, and c itself, and gives the same output as `cjpwr()`. It uses tidy syntax including magrittr piping, so it requires installation of [`tidyverse`](https://www.tidyverse.org) - [GPL-3 licensed](https://github.com/tidyverse/tidyverse/blob/master/LICENSE). 

The calculation is based on recommendations from [Orme (2010)](https://www.sawtoothsoftware.com/download/techpap/samplesz.pdf). Examples in the documentation use datasets from [`cregg`](https://github.com/leeper/cregg) - [MIT licensed](https://choosealicense.com/licenses/mit/). 
