# Load Dixon-Coles1996.R to scrap, manage and calculate data instanatly.
source('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/Dixon-Coles1996.R')

#------------------------------------------------
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #ad1d28;
      }
      body {
        background-color: #fbfef1;
      }
    "))
  ),
  tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #CCFF66;
             z-index: 105;
           }
  "),

  headerPanel("Dixon & Coles 1996"),

  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    selectInput("dataset", "Choose a dataset:", 
                choices = c("teams", "pred1", "pred2", "pred3")),
    checkboxGroupInput("table_variables", 
                       label = h3("Residuals"), 
                       choices = list("Model 1" = "rsd1", "Model 2" = "rsd2", "Model 3" = "rsd3"),
                       selected = c()),
    conditionalPanel(
      condition = "input.table_variables.indexOf('rsd1') > -1",
      br(),
      checkboxGroupInput("teams_variables",
                         label = h4("Residuals of model 1"),
                         choices = teamslist,
                         selected = teams$name)),
    conditionalPanel(
      condition = "input.table_variables.indexOf('rsd2') > -1",
      br(),
      checkboxGroupInput("teams_variables",
                         label = h4("Residuals of model 2"),
                         choices = teamslist,
                         selected = teams$name)),
    conditionalPanel(
      condition = "input.table_variables.indexOf('rsd3') > -1",
      br(),
      checkboxGroupInput("teams_variables",
                         label = h4("Residuals of model 3"),
                         choices = teamslist,
                         selected = teams$name)),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),

  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")
  ),

  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 
    verbatimTextOutput("summary"), 
    tableOutput("view")
  )
))
