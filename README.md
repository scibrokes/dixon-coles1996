# Example Shiny app
---------------------
I simply write a shiny app to simulate the Dixon & Coles 1996 model :

```{r}
shiny::runGitHub('Dixon-Coles1996', 'englianhu', subdir='knitr')
shiny::runGitHub('Dixon-Coles1996', 'englianhu', subdir='shiny', display.mode='showcase')
```

Reference :

-http://supstat.com.cn/blog/2014/12/03/a-simple-shiny-interface-to-retrieve-stock-information/
-http://cran.r-project.org/web/packages/fbRanks/index.html
-http://wiekvoet.blogspot.nl/2012/09/football-model.html
-http://www.tsrmh.com/2013/08/11/modeling-match-results-in-la-liga-using-a-hierarchical-bayesian-poisson-model-part-three-http://fantasyfootballanalytics.net/2014/06/custom-rankings-and-projections-for-your-league.html#comment-27718
-http://www.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf
