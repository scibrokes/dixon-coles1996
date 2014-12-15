library(shiny)
library('fbRanks')
library('XML')
library('plyr')
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

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the expression is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "matches" = matches,
           "teams" = teams,
           "result" = result)
  })
  
  # The output$caption is computed based on a reactive expression that
  # returns input$caption. When the user changes the "caption" field:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive expression below don't 
  # depend on input$caption, those expression are NOT called when 
  # input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive expression, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dat <- datasetInput()
    summary(dat)
  })
  
  # The output$view depends on both the databaseInput reactive expression
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})
