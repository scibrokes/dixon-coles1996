downloadNotInstalled<-function(x){ 
  for(i in x){ 
    if(!require(i,character.only=TRUE)){ 
      install.packages(i,repos="http://cran.r-project.org") 
      library(i,character.only=TRUE) 
    } 
  } 
}
requiredPackages = c("shiny","shinyapps","devtools","fbRanks","XML") 
downloadNotInstalled(requiredPackages) 

#------------------------------------------------
# Load Dixon-Coles1996.R to scrap, manage and calculate data instanatly.
source('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/Dixon-Coles1996.R')

#------------------------------------------------
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  # By declaring datasetInput as a reactive expression we ensure that:
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the expression is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  datasetInput <- reactive({
    switch(input$dataset,
           "teams" = teams,
           "pred1" = pred1$scores,
           "pred2" = pred2$scores,
           "pred3" = pred3$scores,
    )
  })
  
  # The output$caption is computed based on a reactive expression that
  # returns input$caption. When the user changes the "caption" field:
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive expression below don't 
  # depend on input$caption, those expression are NOT called when 
  # input$caption changes.
  output$caption <- renderText({
    input$caption
  })

  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  output$table_variables <- renderPrint({ input$table_variables })

  # The output$summary depends on the datasetInput reactive expression, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    scores <- datasetInput()
    summary(scores)
    scores <- datasetInput()
    summary(scores)
  })
  # The output$view depends on both the databaseInput reactive expression
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})
