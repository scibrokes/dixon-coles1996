library(fbRanks)
library(XML)
library(plyr)
# get the English Premier League 2013/2014 from official website
url = getURL('http://www.premierleague.com/en-gb/matchday/results.html?paramClubId=ALL&paramComp_8=true&paramSeason=2013-2014&view=.dateSeason')
tbl <- readHTMLTable(htmlParse(url),header='text'); tbl[[length(tbl)]] <- NULL
tbl <- lapply(tbl,function(x) {x$V1 = x$V1[1]; x[-1,]})
dat <- Reduce(function(x, y) merge(x, y, all = T), 
       tbl, accumulate = F)[1:5]
dat$HG <- as.numeric(unlist(lapply(str_split(as.character(dat$V3),'-'),function(x) x[1])))
dat$AG <- as.numeric(unlist(lapply(str_split(as.character(dat$V3),'-'),function(x) x[2])))
dat$V3 <- NULL
names(dat) <- c('date','home.team','away.team','venue','home.score','away.score')
dat$date <- unlist(lapply(str_split(dat$date,' '),function(x) paste(x[-1],collapse='')))
dat$date <- as.Date(dat$date, "%d%B%Y")
attr(dat$home.team,'levels') <- levels(factor(dat$home.team))
attr(dat$away.team,'levels') <- levels(factor(dat$away.team))
attr(dat$venue,'levels') <- levels(factor(dat$venue))
teams <- dat[order(dat$date, decreasing=T) & !duplicated(dat$venue),][c('home.team','venue')]
names(teams)[1] <- 'name'
dat$hdv <- ifelse(dat$home.team==teams$name & dat$venue==teams$venue, 1, 0) # data error:only 33 matches home ground among 380 matches

#match(dat$home.team,teams$name)
#match(dat$venue,teams$venue)
rm(url, tbl)

# Dixon & Coles poisson model
md1 <- rank.teams(dat, min.date=min(dat$date),max.date=max(dat$date)) #without explanatory variable
md2 <- rank.teams(dat, min.date=min(dat$date),max.date=max(dat$date),add='hdv') #with explanatory variable - home team advantage
md3 <- rank.teams(dat, min.date=min(dat$date),max.date=max(dat$date),add='venue') #with explanatory variable - venue

md1.att <- coef(md1$fit$cluster.1)[1:nrow(md1$teams)]
md1.def <- coef(md1$fit$cluster.1)[-seq(1,nrow(md1$teams))]
md1.df <- data.frame(att=exp(md1.att),def=exp(md1.def))

md2.att <- coef(md2$fit$cluster.1)[1:nrow(md2$teams)]
md2.def <- coef(md2$fit$cluster.1)[seq(nrow(md2$teams)+2,nrow(md2$teams)*2)-1]
md2.hdv <- coef(md2$fit$cluster.1)[-seq(nrow(md2$teams)*2)+1]

md3.att <- coef(md3$fit$cluster.1)[1:nrow(md3$teams)]
md3.def <- coef(md3$fit$cluster.1)[seq(nrow(md3$teams)+2,nrow(md3$teams)*2)-1]
md3.hdv <- coef(md3$fit$cluster.1)[-seq(nrow(md3$teams)*2)+1]

# to be continue
sim <- simulate(md1)
predict(sim)
# apply anova to compare the models

