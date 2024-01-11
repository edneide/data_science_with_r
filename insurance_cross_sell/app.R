# Imports --------------
library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(DT)

# Load the model --------
model <- readRDS("~/Documents/repos/data_science_with_r/final_model.rds")

# Sidebar -------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Health Insurance Cross-sell", tabName = "health_tab", icon = icon("hospital")),
    menuItem("Group of clients", icon = icon("th"), tabName = "group_tab"),
    menuItem("About", icon = icon("fa fa-sticky-note"), tabName = "about_tab")
  )
)

# Body -------------------------
body <- dashboardBody(
  tabItems(
    
    ## One Client
    tabItem(tabName = "health_tab",
            h1("Single client"),
            box(
              title = "Probability to accept the car insurance", 
              width = 8, solidHeader = TRUE, status = "primary",
              valueBoxOutput("prediction")
            ),
            box(sliderInput("days_associated", label = "Days Associated",
                            min = 10, max = 299, value = 154)),
            box(sliderInput("health_annual_paid", label = "Health Annual Paid",
                            min = 2630, max = 540165, value = 31669)),
            box(selectInput("previously_insured", label = "Previously Insured",
                            choices = c("yes", "no"))),
            box(sliderInput("age", label = "Age",
                            min = 20, max = 85, value = 36)),
            box(selectInput("vehicle_damage", label = "Vehicle Damage",
                            choices = c("yes", "no"))),
            box(sliderInput("region_code", label = "Region Code",
                            min = 0, max = 52, value = 28)),
            box(sliderInput("policy_sales_channel", label = "Policy Channel",
                            min = 1, max = 163, value = 133))),
    ## Group of clients
    tabItem(tabName = "group_tab",
            h1("Group of clients"),
            dashboardBody(
              fileInput("file", "Choose CSV File",
                        accept = c(".csv")
              ),
              tags$hr(),
              numericInput("numRows", "Number of Rows to Display:", 
                           min = 1, max = 20, value = 5),
              actionButton("loadBtn", "Load Data")
              ),
            tags$hr(),
            DTOutput("table")
            ),
    
    ## About the project
    tabItem(tabName = "about_tab",
            h1("About the project"),
            p(HTML("Health Insurance Cross Sell Project ...  
            <br>other line <br> another line <br>
                   and so on")))
  ))

# UI -----------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "Health Insurance App"),
  sidebar,
  body
  
)


# Server -----------------
server <- function(input, output) { 
  
  
  output$prediction <- renderValueBox({
    
    clients <- tibble("days_associated" = input$days_associated,
                      "health_annual_paid" = input$health_annual_paid,
                      "previously_insured" = input$previously_insured,
                      "age" = input$age,
                      "vehicle_damage" = input$vehicle_damage,
                      "region_code" = input$region_code,
                      "policy_sales_channel" = input$policy_sales_channel)
    
    prediction <- predict(model, clients, type = "prob")
    
    pred_yes <- prediction %>% 
      select(.pred_yes) %>% 
      pull()
    
    prediction_color <- case_when(
      pred_yes < 0.25 ~ "red",
      pred_yes >= 0.25 & pred_yes < 0.50 ~ "orange",
      pred_yes >= 0.5 & pred_yes < 0.75 ~ "blue",
      pred_yes > 0.75 ~ "green"
    )
    
    valueBox(
      value = paste0(round(100*pred_yes, 2), "%"),
      subtitle = "Yes probability",
      color = prediction_color,
      icon = icon("hospital")
    )
  })
  
  # Reactive function to read the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  
  # Display the selected number of rows in the table
  output$table <- renderDT({
    datatable(
      data()[1:input$numRows, ],
      extensions = 'Buttons',
      callback = JS('table.page("next").draw(false);'),
      options = list(
        lengthMenu = c(5, 10, 15),
        pageLength = 5,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel')
        
      )
    )
  })

}

# App ----------------------
shinyApp(ui, server)