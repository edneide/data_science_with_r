# Imports ----------
library(tidyverse)

# Model --------
# Read the final model
model <- readRDS("final_model.rds")

# Features
vars <- model$pre$mold$predictors %>% 
  colnames() 

vars_sel <- c("age", "days_associated", 
              "health_annual_paid", 
              "region_code",          
              "policy_sales_channel",  
              "vehicle_damage",     
              "previously_insured")   


# Data to use as example
df <- readRDS("df_cleaned.rds")
df_test <- df %>% 
  select(id, one_of(vars_sel))
df_test <- df_test[1:100,]
write.csv(df_test, "df_test.csv", row.names = FALSE)
