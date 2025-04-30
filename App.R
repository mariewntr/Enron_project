library(shinydashboard)
library(shiny)

#dashboard of the Enron email analysis
ui <- dashboardPage(
  dashboardHeader(
    title = "Enron Company"
  ),
  #the different page in the dashboard
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
      menuItem("Email flux", tabName = "flux", icon = icon("circle-nodes")),
      menuItem("The most active Enron worker", tabName = "active", icon = icon("sun")),
      menuItem("Email exchange analysis", tabName = "email", icon = icon("envelope")),
      menuItem("Potential fraud actor", tabName = "actor", icon = icon("circle-exclamation"))
    )
  ),
  #the body of the dashboard
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        #name of the tab
        tabName = "dashboard",
              #the title for it
              h4("General information about the company"),
              #1st row with a table box
              fluidRow(
                tabBox(
                  title = "The company",
                  id = "tabset1", height = "250px",
                  tabPanel("Worker per status"), 
                  tabPanel("Email send")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "flux",
              h4("Email flux between Enron worker status")
      ),
      
      # 3rd tab content
      tabItem(tabName = "actor",
              h4("Email send/received by Enron worker knows for being involved in the fiscal fraud and/or the bankruptcy")
    ),
    #4th tab content
    tabItem(tabName = "active",
            h4("The most active email sender of the Enron company, Jeff Dasovich")),
    #5th tab content
    tabItem(tabName = "email",
            h4 ("Analyse of the email subject and content to count how many are related to the Enron event and the core business"))
    
  )))

server <- function(input, output){ }

shinyApp(ui, server)
