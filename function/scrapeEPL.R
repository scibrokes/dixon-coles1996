scrapeEPL <- function(season=as.character(season), saveDir){
  library('XML')
  # Example of using this function:
  #   mydir='C:/Users/User/Documents/GitHub/englianhu/Dixon-Coles1996/data/'
  #   scrapeEPL(season='2012-2013', saveDir=mydir)

  # test the input season name
  if(all(unlist(lapply(strsplit(season,'-'),function(x) as.list(nchar(x))==4)))==TRUE){
    cat('season = ',season)
  } else {
    stop('Please enter a correct season name. For example: scrapeEPL(season="2013-2014")')
  }

# get the English Premier League 2013/2014 from official website
  lnk = paste0('http://www.premierleague.com/en-gb/matchday/results.html?paramClubId=ALL&paramComp_8=true&paramSeason=',season,'&view=.scoresSeason')
  dfs <- lnk %>>% html_session() %>>% html_table(); dfs[[length(dfs)]] <- NULL
  num <- as.list(seq(dfs))
  dfs <- lapply(dfs,function(x) {x[,1]=names(x[1]); names(x)=c('date','home.team','score','away.team','venue'); x[,-ncol(x)]})
  dfs <- lapply(num,function(i) data.frame(id=i,dfs[[i]]))
  scores <- Reduce(function(x, y) merge(x, y, all = TRUE),
                   dfs, accumulate = F)
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
  scores$total.goals <- scores$home.score + scores$away.score

  rm(lnk, tble, teams)
  save(scores, file=paste0(saveDir, 'scores.Rda'))
  write.csv(scores, paste(saveDir, 'scores.csv'))
  cat('There are two files have been saved into : \n ',paste0(saveDir,'scores.Rda \n '),paste0(saveDir,'scores.csv'))
}

