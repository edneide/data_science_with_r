# Imports ------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)

# Model --------------
model <- readRDS("final_model.rds")


# Dashboard elements ---------------
## HEADER -----------------
header <- dashboardHeader(title = "Car Insurance")

## SIDEBAR ---------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single Client", tabName = "single_client_tab", icon = icon("hospital")),
    menuItem("Group of Clients", tabName = "group_tab", icon = icon("th")),
    menuItem("About", tabName = "about_tab", icon = icon("fa fa-sticky-note"))
    
  )
)

## BODY ------------------
body <- dashboardBody(
  
  ## Single Client ------------
  tabItem(
    tabName = "single_client_tab",
    h1("Single Client"), 
    box(
      title = "Probability to accept the car insurance",
      width = 8, solidHeader = TRUE, status = "primary",
      valueBoxOutput("prediction")
    ),
    box(sliderInput("days_associated", label = h3("Days Associated"), min = 10, 
                    max = 299, value = 154)),
    box(sliderInput("health_annual_paid", label = h3("Health Annual Paid"), min = 2630, 
                    max = 540165, value = 31669)),
    box(selectInput("previously_insured", label = h3("Previously Insured"), 
                    choices = c("yes", "no"), 
                    selected = "yes")),
    box(sliderInput("age", label = h3("Age"), min = 20, max = 85, value = 35)),
    box(selectInput("vehicle_damage", label = h3("Vehicle Damage"), 
                    choices = c("yes", "no"), 
                    selected = "yes")),
    box(sliderInput("region_code", label = h3("Region Code"), min = 0, max = 28, value = 52)),
    box(sliderInput("policy_sales_channel", label = h3("Policy Sales Channel"), min = 1, max = 163, value = 133))
  )
)

# User Interface---------
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Server---------
server <- function(input, output) {
  
  output$prediction <- renderValueBox({
    # Create user table ---------
    clients <- tibble(
      "age" = input$age,                  
      "days_associated" = input$days_associated,      
      "health_annual_paid" = input$health_annual_paid,   
      "region_code" = input$region_code,           
      "policy_sales_channel" = input$policy_sales_channel, 
      "vehicle_damage" = input$vehicle_damage,     
      "previously_insured" = input$previously_insured
    )
    
    # Make prediction ----------
    prediction <- predict(model, clients, type = "prob")
    
    pred_yes <- prediction %>% 
      select(.pred_yes) %>% 
      pull()
    
    prediction_color <- case_when(
      pred_yes < 0.25 ~ "red",
      pred_yes >= 0.25 & pred_yes < 0.50 ~ "orange",
      pred_yes >= 0.5 & pred_yes < 0.75 ~ "green",
      pred_yes >= 0.75 ~ "blue"
    )
    
    valueBox(
      value = paste0(round(100*pred_yes, 2), " %"),
      subtitle = "Yes probability",
      color = prediction_color,
      icon = icon("car")
    )
    
  })
  
}

# App---------
shinyApp(ui, server)