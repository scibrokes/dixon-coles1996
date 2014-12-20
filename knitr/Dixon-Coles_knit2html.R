mydir <- paste0(getwd(),'/GitHub/englianhu/Dixon-Coles1996') 
setwd(mydir)
library(knitr)
library(markdown)

# knitr configuration
opts_knit$set(progress=FALSE) 
opts_chunk$set(echo=TRUE, message=FALSE, tidy=TRUE, comment=NA, 
               fig.path="figure/", fig.keep="high", fig.width=10, fig.height=6, 
               fig.align="center") 
knit2html('knitr/Dixon-Coles1996.Rmd') 
browseURL('knitr/Dixon-Coles1996.html') 
