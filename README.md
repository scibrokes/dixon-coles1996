Example Shiny app

This is an example application for Shiny. The main purpose of this example is to illustrate how to run Shiny apps from a remote source. There are many ways to download and run it:

shiny::runGitHub('Dixon-Coles1996', 'englianhu')

library(shiny)

# Easiest way is to use runGitHub
runGitHub("shiny_example", "rstudio")

# Run a tar or zip file directly
runUrl("https://github.com/englianhu/Dixon-Coles1996/archive/master.tar.gz")
runUrl("https://github.com/englianhu/Dixon-Coles1996/archive/master.zip")

Or you can clone the git repository, then use  runApp() :

# First clone the repository with git. If you have cloned it into
# ~/shiny_example, first go to that directory, then use runApp().
setwd("~/Dixon-Coles1996")
runApp() #display.mode='showcase'


Reference:
1) http://supstat.com.cn/blog/2014/12/03/a-simple-shiny-interface-to-retrieve-stock-information/
2) http://cran.r-project.org/web/packages/fbRanks/index.html
3) http://wiekvoet.blogspot.nl/2012/09/football-model.html
4) http://www.tsrmh.com/2013/08/11/modeling-match-results-in-la-liga-using-a-hierarchical-bayesian-poisson-model-part-three/
5) http://fantasyfootballanalytics.net/2014/06/custom-rankings-and-projections-for-your-league.html#comment-27718
6) Whose prediction will be the best? http://fantasyfootballanalytics.net/2014/06/best-fantasy-football-projections-2014.html
7) http://www.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf
