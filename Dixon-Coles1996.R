library(shiny)
library('shinyapps')
library('devtools')
library('fbRanks')
library('XML')
library('plyr')
library('dplyr')
#source('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/Dixon-Coles1996.R')

# get the English Premier League 2013/2014 from official website
url = getURL('http://www.premierleague.com/en-gb/matchday/results.html?paramClubId=ALL&paramComp_8=true&paramSeason=2013-2014&view=.scoresSeason')
tbl <- readHTMLTable(htmlParse(url),header='text'); tbl[[length(tbl)]] <- NULL
tbl <- lapply(tbl,function(x) {x$V1 = x$V1[1]; x[-1,]})
scores <- Reduce(function(x, y) merge(x, y, all = T), 
       tbl, accumulate = F)[1:5]
scores$HG <- as.numeric(unlist(lapply(str_split(as.character(scores$V3),'-'),function(x) x[1])))
scores$AG <- as.numeric(unlist(lapply(str_split(as.character(scores$V3),'-'),function(x) x[2])))
scores$V3 <- NULL
names(scores) <- c('date','home.team','away.team','venue','home.score','away.score')
scores$date <- unlist(lapply(str_split(scores$date,' '),function(x) paste(x[-1],collapse='')))
scores$date <- as.Date(scores$date, "%d%B%Y")
attr(scores$home.team,'levels') <- levels(factor(scores$home.team))
attr(scores$away.team,'levels') <- levels(factor(scores$away.team))
scores$venue <- as.character(scores$venue)
teams <- scores[order(scores$date, decreasing=T) & !duplicated(scores$venue),][c('home.team','venue')]
names(teams)[1] <- 'name'
scores$hdv <- ifelse(scores$home.team==teams$name & scores$venue==teams$venue, 1, 0) # scoresa error:only 33 matches home ground among 380 matches
rm(url, tbl, teams)
save(scores, file='C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.Rda')
write.csv(scores, 'C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.csv')

# ===============================================================================
library('shiny')
library('shinyapps')
library('devtools')
library('fbRanks')
library('XML')
library('plyr')
library('dplyr')

# Load soccer matches data
#load('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.Rda')
#scores <- read.csv('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.csv')
scores <- create.fbRanks.dataframes('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.csv')
teams <- scores$scores[order(scores$scores$date, decreasing=T) & !duplicated(scores$scores$venue),][c('home.team','venue')]
names(teams)[1] <- 'name'

# Dixon & Coles poisson model, we can also add some more effect like weather, pitch condition, home ground advantages etc.
md1 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date))#, silent=T) #without other effects
md2 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date), add='hdv')#, silent=T) #with home team advantage
md3 <- rank.teams(scores$scores, min.date=min(scores$scores$date),max.date=max(scores$scores$date), add='venue')#, silent=T) #with venue effects

# -------------------------------------------------------------------------------
# model 1 measure the team attack and defence index.
md1.att <- coef(md1$fit$cluster.1)[1:nrow(md1$teams)]
md1.def <- coef(md1$fit$cluster.1)[-seq(1,nrow(md1$teams))]
md1.tbl <- data.frame(coef(md1))[c(1:nrow(scores$teams)),-1]; names(md1.tbl) <- c('attack','defence')
md1.tbl <- data.frame(team=sort(as.character(teams$name)),md1.tbl)
md1.tbl$attack <- exp(md1.tbl$attack); md1.tbl$defence <- exp(md1.tbl$defence); row.names(md1.tbl) <- NULL
md1.hm <- ddply(scores$scores, "home.team", summarise, scores = sum(home.score), mean = mean(home.score),
                sd = sd(home.score), se = sd/sqrt(nrow(scores$scores)))
md1.aw <- ddply(scores$scores, "away.team", summarise, scores = sum(away.score), mean  = mean(away.score),
                sd = sd(away.score), se = sd/sqrt(nrow(scores$scores)))

# model 2 measure the team attack, defence and home ground advantage index.
md2.att <- coef(md2$fit$cluster.1)[1:nrow(md2$teams)]
md2.def <- coef(md2$fit$cluster.1)[seq(nrow(md2$teams)+2,nrow(md2$teams)*2)-1]
md2.hdv <- coef(md2$fit$cluster.1)[-seq(nrow(md2$teams)*2)+1]
md2.tbl <- data.frame(coef(md2)$coef.list); names(md2.tbl) <- c('attack','defence','hdv')
md2.tbl <- data.frame(team=sort(as.character(teams$name)),md2.tbl)
md2.tbl$attack <- exp(md2.tbl$attack); md2.tbl$defence <- exp(md2.tbl$defence); row.names(md2.tbl) <- NULL
md2.hm <- ddply(scores$scores, "home.team", summarise, scores = sum(home.score), mean = mean(home.score),
                sd = sd(home.score), se = sd/sqrt(nrow(scores$scores)))
md2.aw <- ddply(scores$scores, "away.team", summarise, scores = sum(away.score), mean  = mean(away.score),
                sd = sd(away.score), se = sd/sqrt(nrow(scores$scores)))

# model 3 measure the team attack, defence and venue index.
md3.att <- coef(md3$fit$cluster.1)[1:nrow(md3$teams)]
md3.def <- coef(md3$fit$cluster.1)[seq(nrow(md3$teams)+2,nrow(md3$teams)*2)-1]
md3.hdv <- coef(md3$fit$cluster.1)[-seq(nrow(md3$teams)*2)+1]
md3.tbl <- data.frame(coef(md3))[c(1:nrow(scores$teams)),-1]; names(md3.tbl) <- c('attack','defence')
md3.tbl <- data.frame(team=sort(as.character(teams$name)), md3.tbl)
md3.tbl$attack <- exp(md3.tbl$attack); md3.tbl$defence <- exp(md3.tbl$defence); row.names(md3.tbl) <- NULL
md3.hm <- ddply(scores$scores, "home.team", summarise, scores = sum(home.score), mean = mean(home.score),
                sd = sd(home.score), se = sd/sqrt(nrow(scores$scores)))
md3.aw <- ddply(scores$scores, "away.team", summarise, scores = sum(away.score), mean  = mean(away.score),
                sd = sd(away.score), se = sd/sqrt(nrow(scores$scores)))

# ===============================================================================
# Simulate the venue (home ground advantage and neutral ground), and predict a specific kick-off scores

# Simulate model 1
# Example : predict a match kick-off on last match-day
sim1 <- simulate(md1)
predict(md1, date=as.Date(max(scores$scores$date)))
pred1 <- predict(md1)
save(pred1, file='C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred1.Rda')

# Simulate model 2
# Example : predict a match kick-off at home ground on last match-day
sim2 <- simulate(md2, hdv=1)
predict(md2, hdv=1, date=as.Date(max(scores$scores$date)))
pred2 <- predict(md2, hdv=1)
save(pred2, file='C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred2.Rda')

# Simulate model 3
# Example : predict a match kick-off at Cardiff City Stadium on last match-day
sim3 <- simulate(md3, venue='Cardiff City Stadium')
predict(md3, venue='Cardiff City Stadium', date=as.Date(max(scores$scores$date)))
pred3 <- predict(md3, venue='Cardiff City Stadium')
save(pred3, file='C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/pred3.Rda')

# ===============================================================================
# Preview the models
attributes(pr1)
head(pr1$scores)

attributes(pr2)
head(pr2$scores)

attributes(pr2)
head(pr2$scores)

# ===============================================================================
# Apply anova to compare the models

#Show the predicted versus actual scores
rsd1 <- residuals(md1)
rsd2 <- residuals(md2)
rsd3 <- residuals(md3)
anova (md1,md2,md3, test='Chisq')

attributes(rsd1)
attributes(rsd2)
attributes(rsd3)

# run shiny apps
#runApp('Myapp', display.mode="showcase")
