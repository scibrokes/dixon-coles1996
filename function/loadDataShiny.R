library("shiny")
library("shinyapps")
library("devtools")
library("fbRanks")
library("XML")
library("plyr")
library("dplyr")
source('C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/function/scrapeEPL.R')

# Load Dixon-Coles1996.R to scrap, manage and calculate data instantly (Refer to Dixon-Coles1996.R file)
#' source('C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/Dixon-Coles1996.R')

mydir <- 'C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/data/'
# Load soccer matches dataframe in fbRanks class
scores <- create.fbRanks.dataframes(paste0(mydir,'scores.csv'))
teams <- scores$scores[order(scores$scores$date, decreasing=T) & !duplicated(scores$scores$venue),][c('home.team','venue')]; names(teams)[1] <- 'name'
teamslist <- as.list(teams$name); names(teamslist) <- teams$name

load('C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred1.Rda')
load('C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred2.Rda')
load('C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred3.Rda')

# Dixon & Coles poisson model, we can also add some more effect like weather, pitch condition, home ground advantages etc.
md1 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date), silent=T) #without other effects
md2 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date), add='hdv', silent=T) #with home team advantage
md3 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date), add='venue', silent=T) #with venue effects

#Show the predicted versus actual scores
rsd1 <- residuals(md1)
rsd2 <- residuals(md2)
rsd3 <- residuals(md3)

