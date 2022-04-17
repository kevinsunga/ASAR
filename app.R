# Install and Load Packages ----
packages = c("shiny", "shinydashboard", "tidyverse", "scales", "rstudioapi", 
             "readr", "ggpubr","matrixStats", "MASS","ggplot2", "scales",
             "quantmod","maps","mapproj", "leaflet", "leaflet.extras", "DT",
             "shinythemes", "pivottabler", "lubridate", "rpivotTable", "dplyr")

for (p in packages){
 if(!require(p,character.only = T)){
   install.packages(p)
 }
  library(p, character.only = T)
}

# Read data ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ks_data <- read.csv("data/kickstarter_baseline_dataset_0222.csv")

# Used for graphs and prediction
ks_data$launch_month <- factor(ks_data$launch_month,
                               levels=c("Jan","Feb","Mar","Apr",
                                        "May","Jun","Jul","Aug",
                                        "Sep","Oct","Nov","Dec"))
ks_data$launch_day <- factor(ks_data$launch_day,
                             levels=c("Mon","Tue","Wed","Thu","Fri",
                                      "Sat","Sun"))
ks_data$state <- factor(ks_data$state,
                             levels=c("failed","successful"))


#kickstarter logo and hyperlink ----
dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='https://www.kickstarter.com/about',
                                           tags$img(src='kickstarter-logo-white.png',
                                                    height='22',width='200'))

# Define UI ----
ui <- dashboardPage(
  
  #Header content
  dbHeader,
  
  #Sidebar content
  dashboardSidebar(
    sidebarMenu(
      skin = "green",
      menuItem("Overall Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("th")),
      menuItem("Multiple Linear Regression", tabName = "mlr", icon = icon("chart-line")),
      menuItem("Logistics Regression Staff Pick", tabName = "logit", icon = icon("clipboard-check")),
      menuItem("Logistics Regression Success", tabName = "logit2", icon = icon("check-double"))
    )
  ),
  
  #Body content
  dashboardBody(
    
    tabItems(
      #dashboard
      tabItem(tabName ="dashboard",
        h1("Kick Starter Global Campaign Metrics for 2016-2020"),
      
        fluidRow(align = "center",
          
          box(
            title = "Successful Projects", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("global_success"))
          ),
          
          box(
            title = "Failed Projects", width = 2, status = "danger", align = "center",
            solidHeader = TRUE, h2(textOutput("global_fail"))
          ),
          
          box(
            title = "Number of Projects", width = 2, status = "success",align = "center",
            solidHeader = TRUE, h2(textOutput("global_n_projects"))
          ),
          
          box(
            title = "Median USD Pledged", width = 2, status = "success",align = "center",
            solidHeader = TRUE, h2(textOutput("global_median_pledged"))
          ),
          
          box(
            title = "Median # of Backers", width = 2, status = "success",align = "center",
            solidHeader = TRUE, h2(textOutput("global_median_backers"))
          ),
          
          box(
            title = "Median Duration (days)", width = 2, status = "success",align = "center",
            solidHeader = TRUE, h2(textOutput("global_median_duration"))
          )
        ), #fluidRow Global Metrics
        
        
        fluidRow(
          box(
            title = "Top 15 Countries with Most Number of Projects", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_map"),
            p("The color and size of the dots represent total number of projects over the past 5 years - bigger dots and darker color mean higher number of projects. 
              United States has the largest size, and the Europe shows more densely populated dots, which indicates majority of the kickstarter projects came from these two regions. 
              For Top 15 countries, there are less projects that originated from Asia, Africa, South America and Oceania.")
          ),
          
          box(
            title = "Number of Projects by Country", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_country"),
            p("The United States has the greatest number of projects launched over the past 5 years, 
              which is significantly larger than the rest of the countries, followed by United Kingdom, Canada and Australia. 
              Most of the top 15 countries are western countries except Hong Kong, Singapore and Japan which are Asian countries.")
          )
        ), #fluidRow Map
        
        fluidRow(
          box(
            title = "Success Rate by Country", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_success_country"),
            p("Over 2016 to 2020, the top 3 countries in terms of success rate are Hong Kong (75.9%), United Kingdom (67.8%) and Japan (67.0%).
              Most of the countries with high success rate during this period are from Asia and the Europe.")
          ),
          
          box(
            title = "Success Rate by Main Category", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_success_category"),
            p("Comics, games, publishing, design and dance are the top fields that have the highest success rates.
              Projects in technology, food, and journalism are at the bottom with the lowest success rates.")
          )
        ), #fluidRow success rate
        
        
        fluidRow(
          box(
            title = "Success Rate by Launch Date", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_success_date"),
            p("Overall trend has been increasing especially during 2020 where there was a sharp spike at the middle of the year. 
              Seasonality can also be observed with lows during December and peaks at the start of the year.")
          ),
          
          box(
            title = "Median Goal and Pledge by Launch Date ", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_median"),
            p("The median goal amount has been gradually decreasing with spikes 2-3 times a year.
               The median pledged amount however has seen increasing trend over the years especially during 2020.")
          )
        ),  #fluidRow median
        
        
        fluidRow(
          box(
            title = "Proportion of Total Projects from United States vs. Other Countries", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_us_foreign"),
            p("Over the past 5 years, Kickstarter platform was dominated by projects from the United States, over 50%, whereas the remaining are from other countries. 
              However, the proportion of projects from the US has been gradually decreasing over the years.")
          ),
          
          box(
            title = "Sum of Pledged (USD) of Projects from United States vs. Other Countries", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("global_sum_pledged"),
            p("The area graph shows that the sum of pledged amount for projects from the United states were generally higher than that from other countries.
               The total amount for both US and foreign countries increased sharply during 2020.")
          )
        )  #fluidRow us vs. foreign
      ), #dashboard
    
      #eda
      tabItem(tabName = "eda",
        h1("Detailed Level Exploratory Data Analysis"),
        fluidRow(
          box(
            title = "Set EDA Parameters:", status = "primary", width = 12,
            solidHeader = TRUE,
            
            column(12, uiOutput("country_selector")),
            column(12, uiOutput("category_selector")),
            column(6, uiOutput("date_selector")), 
            column(12, actionButton("button_metrics","Generate Campaign Metrics!", icon("hand-point-right")))
          )
        ),
        
        h3(textOutput("country_year")),
        fluidRow(align = "center",
          box(
            title = "Successful Projects", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("country_success"))
          ),

          box(
            title = "Failed Projects", width = 2, status = "danger", align = "center",
            solidHeader = TRUE, h2(textOutput("country_fail"))
          ),

          box(
            title = "Number of Projects", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("country_n_projects"))
          ),

          box(
            title = "Median USD Pledged", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("country_median_pledged"))
          ),

          box(
            title = "Median # of Backers", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("country_median_backers"))
          ),

          box(
            title = "Median Duration (days)", width = 2, status = "success", align = "center",
            solidHeader = TRUE, h2(textOutput("country_median_duration"))
          )
        ), #fluidRow Country Metrics
        
        h3("Attributes of Successful Projects"),
        fluidRow(
          box(
            title = "Select Attribute:", status = "primary", width = 6,
            solidHeader = TRUE, 
            uiOutput("attribute_selector"),
            h4(strong("Top Attributes Sorted by Success Rate")),
            tableOutput("success_table"),style = "overflow-x: scroll;"
          ),
          
          box(
            title = "Select Grouping:", status = "primary", width = 6,
            solidHeader = TRUE, 
            
            selectInput(inputId="selected_grouping",
                        label=NULL,
                        choices=c("Main Category"="category_parent_name_recode", 
                                  "Country"="location_expanded_country"),
                        selected="category_parent_name_recode"),
            
            plotOutput("plot_grouping")
          )
        ),
        
        h3("Comparison of Successful vs. Failed Projects"),
        fluidRow(
          box(
            title = "Frequency (count) based on Selected Grouping", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("plot_category")
          ),
          
          box(
            title = "Number of Projects by Creator (proportion)", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("plot_numcreator")
          )
        ),
        
        fluidRow(
          box(
            title = "Launch Month (proportion)", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("plot_month")
          ),
          
          box(
            title = "Launch Day (proportion)", width = 6, status = "success",
            solidHeader = TRUE, plotOutput("plot_day")
          )
        ), #fluidRow launch date
        
        fluidRow(
          box(
            title = "Successful Projects (proportion) by Staff Pick", width = 12, status = "success",
            solidHeader = TRUE, plotOutput("plot_staffpick")
          )
        ), #fluidRow Staff Pick
        
        fluidRow(
          box(
            title = "Campaign Duration (days)", width = 12, status = "success",
            solidHeader = TRUE, plotOutput("plot_duration")
          )
        ), #fluidRow number of projects
        
        fluidRow(
          
          box(
            title = "Pivot Tables and Graphs", width = 12, status = "success",
            solidHeader = TRUE, rpivotTableOutput("pivot_table"), style = "height:700px; overflow-x: scroll;"
          )
        ),
        
        fluidRow(
          box(
            title = "Examples of Successful Projects", width = 12, status = "success",
            solidHeader = TRUE, dataTableOutput("table_with_link"),style = "overflow-x: scroll;"
          )
        )
      ), #eda
    
      #mlr
      tabItem(tabName = "mlr",
        h1("Regression for Pledged Amount"),
        tabsetPanel(
          tabPanel("Generate Regression Model",
              fluidRow(
                box(
                  title = "Multiple Linear Regression Parameters", width = 12, status = "primary", solidHeader = TRUE,
                  column(12, selectInput("mlr_variables", "Select Independent Variables:",
                              choices = c("Main Category"="category_parent_name_recode", 
                                          "Country of Origin"="location_country",
                                          "Duration (in days)"="campaign_duration",
                                          "Number of Backers" = "backers_count",
                                          "Name Length (no. of words)"="name_word_len",
                                          "Description Length (no. of words)"="blurb_word_len",
                                          "Launch Month"="launch_month", 
                                          "Launch Day"="launch_day",
                                          "Staff Pick"="staff_pick",
                                          "Number of Projects by Creator"="num_project_creator"),
                              multiple = TRUE,selected = c("category_parent_name_recode",
                                                           "location_country",
                                                           "campaign_duration",
                                                           "backers_count",
                                                           "launch_day",
                                                           "launch_month",
                                                           "name_word_len",
                                                           "blurb_word_len",
                                                           "staff_pick",
                                                           "num_project_creator")
                  )),
                  column(12, selectInput("mlr_direction", "Select Regression Direction:",
                              choices =  list("Forward Selection"  = "forward", 
                                              "Backward Selection" = "backward", 
                                              "Stepwise Selection" = "both"),
                              selected = "both"
                  )),
                  
                  column(6, dateRangeInput(inputId="mlr_selected_date",
                                 label="Select Launch Date of Past Projects to be used for Training the Model:",
                                 start  = "2016-01-01",
                                 end    = "2020-12-31",
                                 min    = "2016-01-01",
                                 max    = "2020-12-31",
                                 startview = "year",
                                 separator = " to ", 
                                 format = "dd/m/yyyy")
                  ),
                  column(12, actionButton(inputId="button_mlr",label="Run Multiple Linear Regression!", icon("hand-point-right"))
                  )
                ),
                
                box(
                  title = "Regression Results Interpretation", width = 12, status = "primary", solidHeader = TRUE,
                  p("There are 3 different regression directions (Backward, Forward, Stepwise) to choose from.",
                    strong("The best model will the last model in the regression summary."),
                    "The overall", strong("p-value of less than 0.05 indicates that the model is significant and useful."),
                    "Adjusted R-squared indicates that the model is well explained by the input variables, its always between 0 to 1 hence the higher the better. 
                    The coefficient estimate of each input variable gives you the expected change in pledged amount when the input variables increase by one unit. 
                    If you have generated a model with", strong("lower than 0.05 overall p-value and high adjusted R-squared"), 
                    ", you are good to proceed to the next tab, select and adjust the variables and predict the pledged amount.")
                ), 
                
                box(
                  title = "Regression Summary", width = 12, status = "success", solidHeader = TRUE,
                  h4("Total number of past projects used to train the model:"),
                  h4(strong(textOutput("mlr_num_projects"))),
                  br(),
                  h4("Statistics Results:"), 
                  verbatimTextOutput("mlr_summary")
                )
              ) #fluidRow
          ),
          tabPanel("Predict Pledged Amount",
              fluidRow(
                   box(
                     title = "Select Parameter Values", width = 12, status = "primary", solidHeader = TRUE,
                     
                     checkboxInput("staff_pick", "Staff Pick (tick the box if selected by Kickstater)", FALSE),

                     selectInput("category_parent_name_recode", "Main Category:",
                                 choices = list("Art",
                                                "Comics",
                                                "Crafts",
                                                "Dance",
                                                "Design",
                                                "Fashion",
                                                "Film & Video",
                                                "Food",
                                                "Games",
                                                "Journalism",
                                                "Music",
                                                "Photography",
                                                "Publishing",
                                                "Technology",
                                                "Theater"),
                                 selected = "Art"
                     ),

                     selectInput("location_country", "Country of Origin:",
                                 choices =  list(
                                   "Australia"= "AU",
                                   "Canada" = "CA",
                                   "Denmark"= "DK",
                                   "France" = "FR",
                                   "Germany" = "DE",
                                   "Hong Kong" = "HK",
                                   "Italy" = "IT",
                                   "Japan" = "JP",
                                   "Mexico" = "MX",
                                   "Netherlands" = "NL",
                                   "Singapore"= "SG",
                                   "Spain" ="ES",
                                   "Sweden" = "SE",
                                   "United Kingdom"= "GB",
                                   "United States" = "US"),
                                 selected = "US"
                     ),

                     selectInput("launch_day", "Launch Day (of week):",
                                 choices =  list("Mon",
                                                 "Tue",
                                                 "Wed",
                                                 "Thu",
                                                 "Fri",
                                                 "Sat",
                                                 "Sun"),
                                 selected = "Mon"
                     ),

                     selectInput("launch_month", "Launch Month:",
                                 choices =  list("Jan",
                                                 "Feb",
                                                 "Mar",
                                                 "Apr",
                                                 "May",
                                                 "Jun",
                                                 "Jul",
                                                 "Aug",
                                                 "Sep",
                                                 "Oct",
                                                 "Nov",
                                                 "Dec"),
                                 selected = "Jan"
                     ),

                     sliderInput("campaign_duration", "Duration (in days):", min = 1, max = 100, value = 30, step = 1),

                     sliderInput("backers_count", "Number of Backers:", min = 0, max = 100000, value = 100, step = 1),

                     sliderInput("name_word_len", "Name Length (no. of words):", min = 1, max = 20, value = 10, step = 1),

                     sliderInput("blurb_word_len", "Description Length (no. of words):", min = 1, max = 50, value = 25, step = 1),

                     sliderInput("num_project_creator", "Number of Projects by Creator:", min = 1, max = 50, value = 1, step = 1)
                   ),
                   
                   box(
                     title = "Prediction Results:", width = 12, status = "success", solidHeader = TRUE,
                     h4("Predicted Pledged Amount in USD"),
                     verbatimTextOutput("mlr_predict")
                   )
              ) #fluidRow
            ) #tabPanel predict
          ) #tabSetPanel Regression Pledged Amount
      ), #mlr
    
      #logit
      tabItem(tabName = "logit",
        h1("Regression for Staff Pick"),
        tabsetPanel(
          tabPanel("Generate Regression Model",
                   fluidRow(
                     box(
                       title = "Logistics Regression Parameters", width = 12, status = "primary", solidHeader = TRUE,
                       
                       column(12, selectInput("logit_category", "Select Main Category to build Regression Model:",
                                   choices = list("Art",
                                                  "Comics",
                                                  "Crafts",
                                                  "Dance",
                                                  "Design",
                                                  "Fashion",
                                                  "Film & Video",
                                                  "Food",
                                                  "Games",
                                                  "Journalism",
                                                  "Music",
                                                  "Photography",
                                                  "Publishing",
                                                  "Technology",
                                                  "Theater"),
                                   selected = "Art"
                       )),
                       
                       column(12, selectInput("logit_variables", "Select Independent Variables:",
                                   choices = c("Country of Origin"="location_country",
                                               "Duration (in days)"="campaign_duration",
                                               "USD Goal" = "usd_goal",
                                               "Name Length (no. of words)"="name_word_len",
                                               "Description Length (no. of words)"="blurb_word_len",
                                               "Launch Month"="launch_month",
                                               "Launch Day"="launch_day",
                                               "Number of Projects by Creator"="num_project_creator"),
                                   multiple = TRUE,selected = c("location_country",
                                                                "campaign_duration",
                                                                "usd_goal",
                                                                "launch_day",
                                                                "launch_month",
                                                                "name_word_len",
                                                                "blurb_word_len",
                                                                "num_project_creator")
                       )),
                       
                       column(6, dateRangeInput(inputId="logit_selected_date",
                                                label="Select Launch Date of Past Projects to be used for Training the Model:",
                                                start  = "2016-01-01",
                                                end    = "2020-12-31",
                                                min    = "2016-01-01",
                                                max    = "2020-12-31",
                                                startview = "year",
                                                separator = " to ", 
                                                format = "dd/m/yyyy")
                       ),
                       column(12, actionButton(inputId="button_logit",label="Run Logistics Regression!", icon("hand-point-right")))
                     ),
                     
                     box(
                       title = "Regression Results Interpretation", width = 12, status = "primary", solidHeader = TRUE,
                       p("Logistic regressions give predictions based on main project categories.
                          The", strong("coefficients estimate and respective p-value"), "at the last column tells you the direction of the change in predicted outcome and whether the variable is significant in determining the target variable. 
                          Positive coefficient indicates one unit increase in the variable will lead to increase in the probability of being staff picked or success rate. 
                          If the respective", strong("p value is less than 0.05 this variable is significant determinant in the prediction."), 
                          "The difference between null deviance and residual deviance (seen at the bottom of summary) tells you how good the model fit is. 
                          The bigger the difference, the better the model is. Finally, lower AIC values also indicate a better-fit model.")
                     ), 

                     box(
                       title = "Regression Summary", width = 12, status = "success", solidHeader = TRUE,
                       
                       h4("Total number of past projects used to train the model:"),
                       h4(strong(textOutput("logit_num_projects"))),
                       br(),
                       h4("Statistics Results:"), 
                       verbatimTextOutput("logit_summary")
                     )
                   ) #fluidRow
          ),
          tabPanel("Predict Staff Pick",
                   fluidRow(
                     box(
                       title = "Select Parameter Values", width = 12, status = "primary", solidHeader = TRUE,

                       selectInput("logit_location_country", "Country of Origin:",
                                   choices =  list(
                                     "Australia"= "AU",
                                     "Canada" = "CA",
                                     "Denmark"= "DK",
                                     "France" = "FR",
                                     "Germany" = "DE",
                                     "Hong Kong" = "HK",
                                     "Italy" = "IT",
                                     "Japan" = "JP",
                                     "Mexico" = "MX",
                                     "Netherlands" = "NL",
                                     "Singapore"= "SG",
                                     "Spain" ="ES",
                                     "Sweden" = "SE",
                                     "United Kingdom"= "GB",
                                     "United States" = "US"),
                                   selected = "US"
                       ),

                       selectInput("logit_launch_day", "Launch Day (of week):",
                                   choices =  list("Mon",
                                                   "Tue",
                                                   "Wed",
                                                   "Thu",
                                                   "Fri",
                                                   "Sat",
                                                   "Sun"),
                                   selected = "Mon"
                       ),

                       selectInput("logit_launch_month", "Launch Month:",
                                   choices =  list("Jan",
                                                   "Feb",
                                                   "Mar",
                                                   "Apr",
                                                   "May",
                                                   "Jun",
                                                   "Jul",
                                                   "Aug",
                                                   "Sep",
                                                   "Oct",
                                                   "Nov",
                                                   "Dec"),
                                   selected = "Jan"
                       ),

                       sliderInput("logit_campaign_duration", "Duration (in days):", min = 1, max = 100, value = 30, step = 1),

                       sliderInput("logit_name_word_len", "Name Length (no. of words):", min = 1, max = 20, value = 10, step = 1),

                       sliderInput("logit_blurb_word_len", "Description Length (no. of words):", min = 1, max = 50, value = 25, step = 1),

                       sliderInput("logit_num_project_creator", "Number of Projects by Creator:", min = 1, max = 50, value = 1, step = 1),

                       #max amount 50M, historically Pebble Time (2015) 20.34 M
                       numericInput("logit_usd_goal", "USD Goal:", min = 1, value = 10000)
                      ),
                   
                      box(
                        title = "Prediction Results:", width = 12, status = "success", solidHeader = TRUE,
                        h4("Predicted to be Staff Pick if response value is",strong(">=0.5")),
                        verbatimTextOutput("logit_predict")
                      )
                   ) #fluidRow
          ) #tabPanel predict
        ) #tabSetPanel Regression for Staff Pick
      ), #logit
      
      #logit2
      tabItem(tabName = "logit2",
              h1("Regression for Campaign State (Success or Failure)"),
              tabsetPanel(
                tabPanel("Generate Regression Model",
                         fluidRow(
                           box(
                             title = "Logistics Regression Parameters", width = 12, status = "primary", solidHeader = TRUE,
                             
                             column(12, selectInput("logit2_category", "Select Main Category to build Regression Model:",
                                         choices = list("Art",
                                                        "Comics",
                                                        "Crafts",
                                                        "Dance",
                                                        "Design",
                                                        "Fashion",
                                                        "Film & Video",
                                                        "Food",
                                                        "Games",
                                                        "Journalism",
                                                        "Music",
                                                        "Photography",
                                                        "Publishing",
                                                        "Technology",
                                                        "Theater"),
                                         selected = "Art"
                             )),
                             
                             column(12, selectInput("logit2_variables", "Select Independent Variables:",
                                         choices = c("Country of Origin"="location_country",
                                                     "Duration (in days)"="campaign_duration",
                                                     "USD Goal" = "usd_goal",
                                                     "Name Length (no. of words)"="name_word_len",
                                                     "Description Length (no. of words)"="blurb_word_len",
                                                     "Launch Month"="launch_month",
                                                     "Launch Day"="launch_day",
                                                     "Staff Pick"="staff_pick",
                                                     "Number of Projects by Creator"="num_project_creator"),
                                         multiple = TRUE,selected = c("location_country",
                                                                      "campaign_duration",
                                                                      "usd_goal",
                                                                      "launch_day",
                                                                      "launch_month",
                                                                      "name_word_len",
                                                                      "blurb_word_len",
                                                                      "staff_pick",
                                                                      "num_project_creator")
                             )),
                             
                             column(6, dateRangeInput(inputId="logit2_selected_date",
                                                      label="Select Launch Date of Past Projects to be used for Training the Model:",
                                                      start  = "2016-01-01",
                                                      end    = "2020-12-31",
                                                      min    = "2016-01-01",
                                                      max    = "2020-12-31",
                                                      startview = "year",
                                                      separator = " to ",
                                                      format = "dd/m/yyyy")
                             ),
                             
                             column(12, actionButton(inputId="button_logit2",label="Run Logistics Regression!", icon("hand-point-right")))
                           ),
                           
                           box(
                             title = "Regression Results Interpretation", width = 12, status = "primary", solidHeader = TRUE,
                             p("Logistic regressions give predictions based on main project categories.
                                The", strong("coefficients estimate and respective p-value"), "at the last column tells you the direction of the change in predicted outcome and whether the variable is significant in determining the target variable. 
                                Positive coefficient indicates one unit increase in the variable will lead to increase in the probability of being staff picked or success rate. 
                                If the respective", strong("p value is less than 0.05 this variable is significant determinant in the prediction."), 
                                "The difference between null deviance and residual deviance (seen at the bottom of summary) tells you how good the model fit is. 
                                The bigger the difference, the better the model is. Finally, lower AIC values also indicate a better-fit model.")
                           ), 
                           
                           box(
                             title = "Regression Summary", width = 12, status = "success", solidHeader = TRUE,
                             h4("Total number of past projects used to train the model:"),
                             h4(strong(textOutput("logit2_num_projects"))),
                             br(),
                             h4("Statistics Results:"),
                             verbatimTextOutput("logit2_summary")
                           )
                         ) #fluidRow
                ),
                tabPanel("Predict Campaign Success",
                         fluidRow(
                           box(
                             title = "Select Parameter Values", width = 12, status = "primary", solidHeader = TRUE,
                             
                             checkboxInput("logit2_staff_pick", "Staff Pick (tick the box if selected by Kickstater)", FALSE), 
                             
                             selectInput("logit2_location_country", "Country of Origin:",
                                         choices =  list(
                                           "Australia"= "AU",
                                           "Canada" = "CA",
                                           "Denmark"= "DK",
                                           "France" = "FR",
                                           "Germany" = "DE",
                                           "Hong Kong" = "HK",
                                           "Italy" = "IT",
                                           "Japan" = "JP",
                                           "Mexico" = "MX",
                                           "Netherlands" = "NL",
                                           "Singapore"= "SG",
                                           "Spain" ="ES",
                                           "Sweden" = "SE",
                                           "United Kingdom"= "GB",
                                           "United States" = "US"),
                                         selected = "US"
                             ),
                             
                             selectInput("logit2_launch_day", "Launch Day (of week):",
                                         choices =  list("Mon",
                                                         "Tue",
                                                         "Wed",
                                                         "Thu",
                                                         "Fri",
                                                         "Sat",
                                                         "Sun"),
                                         selected = "Mon"
                             ),
                             
                             selectInput("logit2_launch_month", "Launch Month:",
                                         choices =  list("Jan",
                                                         "Feb",
                                                         "Mar",
                                                         "Apr",
                                                         "May",
                                                         "Jun",
                                                         "Jul",
                                                         "Aug",
                                                         "Sep",
                                                         "Oct",
                                                         "Nov",
                                                         "Dec"),
                                         selected = "Jan"
                             ),
                             
                             sliderInput("logit2_campaign_duration", "Duration (in days):", min = 1, max = 100, value = 30, step = 1),
                             
                             sliderInput("logit2_name_word_len", "Name Length (no. of words):", min = 1, max = 20, value = 10, step = 1),
                             
                             sliderInput("logit2_blurb_word_len", "Description Length (no. of words):", min = 1, max = 50, value = 25, step = 1),
                             
                             sliderInput("logit2_num_project_creator", "Number of Projects by Creator:", min = 1, max = 50, value = 1, step = 1),
                             
                             #max amount 1M, historically Pebble Time (2015) 20.34 M
                             #sliderInput("logit2_usd_goal", "USD Goal:", min = 1, max = 1000000, value = 10000, step = 1000)
                             numericInput("logit2_usd_goal", "USD Goal:", min = 1, value = 10000)
                           ),
                           
                           box(
                             title = "Prediction Results:", width = 12, status = "success", solidHeader = TRUE,
                             h4("Predicted to be Successful if response value is ",strong(">=0.5")),
                             verbatimTextOutput("logit2_predict")
                           )
                         ) #fluidRow
                ) #tabPanel predict
              ) #tabSetPanel Regression for Staff Pick
      ) #logit2
    ) #tabItems
  ), #dashboardBody
  
  #UI/UX style and customization
  skin = "green",
  
  tags$head(
    tags$style(HTML('* {font-family: "Monaco"};')),
    tags$style(HTML(".btn { display:block; 
                            border: SteelBlue
                            border-radius: 4px;
                            background-color:DarkSlateGray;
                            font-size: 16px;
                            color: white;
                            :hover
                    }")),
    tags$head(tags$style(HTML('
                .content-wrapper {
                    background-color: WhiteSmoke;
                }
                .main-sidebar {
                    background-color: DarkSlateGray !important;
                }')))
  )
  
) #ui

# Define server logic ----
server <- function(input, output) {
  
  #Create country filter
  country_list  <- sort(unique(ks_data$location_expanded_country))
  category_list <- sort(unique(ks_data$category_parent_name_recode))
  ks_data$launched_at_utc <- as.Date(ks_data$launched_at_utc,"%d/%m/%Y", "%m/%d/%Y")
  
  
  #selector
  output$country_selector <- renderUI({
    selectInput(inputId="selected_country",
                label="Select Country:",
                multiple = TRUE,
                choices=country_list,
                selected="United States")
  })
  
  output$date_selector <- renderUI({
    dateRangeInput(inputId="selected_date",
                label="Select Launch Date:",
                start  = "2016-01-01",
                end    = "2020-12-31",
                min    = "2016-01-01",
                max    = "2020-12-31",
                startview = "year",
                separator = " to ", 
                format = "dd/m/yyyy")
  })
  
  output$category_selector <- renderUI({
    selectInput(inputId="selected_category",
                label="Select Main Category:",
                multiple = TRUE,
                choices = category_list,
                selected = category_list)
  })
  

  #print info
  output$country_year <- renderText({
    paste("Campaign Metrics for the period of ",input$selected_date[1], " to ",
          input$selected_date[2])
  })
  
  filtered_data <- eventReactive(input$button_metrics,{
    req(input$selected_country)
    ks_data %>% filter(location_expanded_country  %in% input$selected_country)%>%
                filter(launched_at_utc >= input$selected_date[1] &
                       launched_at_utc <= input$selected_date[2]) %>%
                filter(category_parent_name_recode %in% input$selected_category)
  })
  
  
  #Global Metrics
  output$global_success <- renderText({
    n_success <- (ks_data %>%
                    filter(state=="successful") %>%
                    summarize(count=n()))[, 1]
    n_total <- (ks_data %>%
                  summarize(count=n()))[, 1]
    pct_success <- round(n_success/n_total, 3)
    paste(format(n_success, big.mark=","),
          gsub(" ", "", paste("(", label_percent(accuracy=.1)(pct_success), ")")))
  })
  
  output$global_fail <- renderText({
    n_fail <- (ks_data %>%
                 filter(state=="failed") %>%
                 summarize(count=n()))[, 1]
    n_total <- (ks_data %>%
                  summarize(count=n()))[, 1]
    pct_fail <- round(n_fail/n_total, 3)
    paste(format(n_fail, big.mark=","),
          gsub(" ", "", paste("(", label_percent(accuracy=.1)(pct_fail), ")")))
  })
  
  output$global_n_projects <- renderText({
    n_total <- (ks_data %>% dplyr::summarize(count=n()))[, 1]
    format(n_total, big.mark=",")
  })
  
  output$global_median_pledged <- renderText({
    usd_pledged <- ks_data$usd_pledged
    med_pledged <- median(usd_pledged)
    dollar_format()(med_pledged)
  })
  
  output$global_median_backers <- renderText({
    n_backers <- ks_data$backers_count
    med_backers <- median(n_backers)
    format(med_backers, big.mark=",")
  })
  
  output$global_median_duration <- renderText({
    duration <- ks_data$campaign_duration
    med_duration <- median(duration)
    format(med_duration)
  })
  
  
  #Global Plots
  global_color <- "forestgreen"
  us_color <- "lightblue3"
  
  #Maps coordinates
  world <- map_data("world")
  maps_data <- read.csv("data/kickstarter_maps.csv")
  
  output$global_map <- renderPlot({
    ggplot() +
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "gray88", size = 0.1
      ) + 
      geom_point(
        data = maps_data,
        aes(long, lat, color = total_projects, size = total_projects), alpha = 0.6) +
      scale_size_continuous(range=c(1,50)) + 
      scale_color_gradient(low = "green3", high = "darkgreen") + 
      theme_void() + theme(legend.position="none")
  })
  
  output$global_country <- renderPlot({
    ks_data %>%
      group_by(location_expanded_country) %>%
      summarize(total_count=n()) %>%
      ggplot(aes(x=reorder(location_expanded_country, -total_count),y=total_count)) +
      geom_bar(stat="identity", fill = global_color) +
      geom_text(aes(label=comma(total_count)), color="black", face="bold", size=4, vjust=-.5) +
      scale_y_continuous(labels = comma) + 
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
            axis.text.x = element_text(angle=90, size=12),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank(),
            axis.text.y = element_blank())
  })
  
  output$global_success_country <- renderPlot({
    ks_data %>%
      group_by(location_expanded_country) %>%
      summarize(success_rate=sum(ifelse(state=="successful", 1, 0))/n()) %>%
      ggplot(aes(x=reorder(location_expanded_country, success_rate), y=success_rate)) +
      geom_bar(stat="identity", fill = global_color) +
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes(label=label_percent(accuracy=.1)(success_rate)), hjust=1.2, color="white", face="bold", size=3.5) +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank(),
            axis.text.y = element_text(face="bold"))
  })
  
  output$global_success_category <- renderPlot({
    ks_data %>%
      group_by(category_parent_name_recode) %>%
      summarize(success_rate=sum(ifelse(state=="successful", 1, 0))/n()) %>%
      ggplot(aes(x=reorder(category_parent_name_recode, success_rate), y=success_rate)) +
      geom_bar(stat="identity", fill = global_color) +
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes(label=label_percent(accuracy=.1)(success_rate)), hjust=1.2, color="white", face="bold", size=3.5) +
      coord_flip() +
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank(),
            axis.text.y = element_text(face="bold"))
  })
  
  output$global_success_date <- renderPlot({
    ks_data %>%
      group_by(date=floor_date(launched_at_utc, "month")) %>%
      summarize(success_rate=sum(ifelse(state=="successful", 1, 0))/n()) %>%
      ggplot(aes(x=date, y=success_rate, group=1))+
      geom_line(color = global_color, size=1.25)+
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
  })
  
  output$global_median <- renderPlot({
    ks_data %>%
      group_by(date=floor_date(launched_at_utc, "month")) %>%
      summarize(median_usd_goal=median(usd_goal),
                median_usd_pledge=median(usd_pledged)) %>%
      ggplot(aes(x=date))+
      geom_line(aes(y=median_usd_goal, color="Goal"), size=1.25)+
      geom_line(aes(y=median_usd_pledge, color="Pledge"), size=1.25)+
      scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))+
      theme_minimal()+
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
            legend.title=element_blank(), legend.position = "right",
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            legend.text = element_text(size=12)) +
      scale_color_manual(values = c("Goal"= us_color, "Pledge"= global_color))
  })
  
  output$global_us_foreign <- renderPlot({
    ks_data %>%
      mutate(date=floor_date(launched_at_utc, "month"),
             isUS=if_else(country=="US", "United States", "Other Countries")) %>%
      group_by(date, isUS) %>%
      summarise(count=n()) %>%
      mutate(pct=count/sum(count)*100) %>%
      ggplot(aes(x=date, y=pct, group=isUS, fill=isUS))+
      geom_area(position = 'stack') +
      scale_fill_manual(breaks = c("United States", "Other Countries"), 
                        values=c(us_color,global_color)) +
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(), 
            legend.title=element_blank(), legend.position = "right",
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            legend.text = element_text(size=12))
  })
  
  output$global_sum_pledged <- renderPlot({
    ks_data %>%
      mutate(date=floor_date(launched_at_utc, "month"),
             isUS=if_else(country=="US", "United States", "Other Countries")) %>%
      group_by(date, isUS) %>%
      summarise(sum_pledged=sum(usd_pledged)) %>%
      ggplot(aes(x=date, y=sum_pledged, group=isUS, fill=isUS))+
      geom_area()+
      scale_y_continuous(labels = scales::dollar_format(scale = .000001, suffix = "M"))+
      theme_minimal()+
      scale_fill_manual(breaks = c("United States", "Other Countries"), 
                        values=c(us_color,global_color)) +
      theme_minimal() +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(), 
            legend.title=element_blank(), legend.position = "right",
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            legend.text = element_text(size=12))
  })
  
  #Country Metrics
  output$country_success <- renderText({
    n_success <- (filtered_data() %>%
                    filter(state=="successful") %>%
                    summarize(count=n()))[, 1]
    n_total <- (filtered_data() %>%
                  summarize(count=n()))[, 1]
    pct_success <- round(n_success/n_total, 3)
    paste(format(n_success, big.mark=","),
          gsub(" ", "", paste("(", label_percent(accuracy=.1)(pct_success), ")")))
  })
  
  output$country_fail <- renderText({
    n_fail <- (filtered_data() %>%
                 filter(state=="failed") %>%
                 summarize(count=n()))[, 1]
    n_total <- (filtered_data() %>%
                  summarize(count=n()))[, 1]
    pct_fail <- round(n_fail/n_total, 3)
    paste(format(n_fail, big.mark=","),
          gsub(" ", "", paste("(", label_percent(accuracy=.1)(pct_fail), ")")))
    
  })
  
  output$country_n_projects <- renderText({
    n_total <- (filtered_data() %>% dplyr::summarize(count=n()))[, 1]
    format(n_total, big.mark=",")
  })
  
  output$country_median_pledged <- renderText({
    usd_pledged <- filtered_data()$usd_pledged
    med_pledged <- median(usd_pledged)
    dollar_format()(med_pledged)
  })
  
  output$country_median_backers <- renderText({
    n_backers <- filtered_data()$backers_count
    med_backers <- median(n_backers)
    format(med_backers, big.mark=",")
  })
  
  output$country_median_duration <- renderText({
    med_duration <- median(filtered_data()$campaign_duration)
    format(med_duration)
  })
  
  #Create success rate table pick list
  #This feature will be used as the "group by" column
  output$attribute_selector <- renderUI({
    selectInput(inputId="selected_attribute",
                label=NULL,
                choices=c("Main Category"="category_parent_name_recode", 
                          "Subcategory"="category_name",
                          "Campaign Duration (Days)"="campaign_duration",
                          "Staff Pick"="staff_pick", 
                          "Number of Projects by Creator"="num_project_creator",
                          "Launch Month"="launch_month", 
                          "Launch Day"="launch_day",
                          "Country"="location_expanded_country",
                          "Location State"="location_state",
                          "USD Pledged"="usd_pledged", 
                          "Number of Words in the Project Name"="name_word_len",
                          "Language used for the Project Name"="name_detected_language",
                          "Number of Words in the Description"="blurb_word_len",
                          "Language used for the Description"="blurb_detected_language"
                          ),
                selected="category_parent_name_recode")
  })
  
  #Success rate table
  output$success_table <- renderTable({
    filtered_data() %>%
      group_by(!! sym(input$selected_attribute)) %>%
      mutate(
        is_success=ifelse(state=="successful", 1, 0),
        is_fail=ifelse(state=="failed", 1, 0)
      ) %>%
      summarize(
        n_success=sum(is_success),
        n_fail=sum(is_fail),
        n_total=n()
      ) %>%
      mutate(
        n_success=as.integer(n_success),
        n_fail=as.integer(n_fail),
        n_total=as.integer(n_total),
        success_rate=label_percent(accuracy=.1)(n_success/n_total),
        fail_rate=label_percent(accuracy=.1)(n_fail/n_total)
      ) %>%
      top_n(n=10,wt=n_success) %>%
      arrange(desc(success_rate)) %>%
      rename("Selected Attribute" = input$selected_attribute,
             "# Successes" = "n_success",
             "# Fails" =  "n_fail",
             "# Total" = "n_total",
             "Success Rate" = "success_rate",
             "Fail Rate" = "fail_rate")
  })
  
  #Country Plots
  success_color <- "palegreen3"
  fail_color    <- "indianred"
  group_color   <- "deepskyblue3"
  
  output$plot_grouping <- renderPlot({
    if (input$selected_grouping == "category_parent_name_recode") {
      filtered_data() %>%
        group_by(category_parent_name_recode) %>%
        summarize(total_count=n()) %>%
        ggplot(aes(x=reorder(category_parent_name_recode, -total_count),y=total_count)) +
        geom_bar(stat="identity", fill = group_color) +
        geom_text(aes(label=comma(total_count)), color="black", face="bold", size=4, vjust=-.5) +
        scale_y_continuous(labels = comma) + 
        theme_minimal() +
        theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
              axis.text.x = element_text(angle=90, size=12),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank(),
              axis.text.y = element_blank())
    } else {
      filtered_data() %>%
        group_by(location_expanded_country) %>%
        summarize(total_count=n()) %>%
        ggplot(aes(x=reorder(location_expanded_country, -total_count),y=total_count)) +
        geom_bar(stat="identity", fill = group_color) +
        geom_text(aes(label=comma(total_count)), color="black", face="bold", size=4, vjust=-.5) +
        scale_y_continuous(labels = comma) + 
        theme_minimal() +
        theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
              axis.text.x = element_text(angle=90, size=12),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank(),
              axis.text.y = element_blank())
    }
  })
  
  output$plot_category <- renderPlot({
     if (input$selected_grouping == "category_parent_name_recode") {
      filtered_data() %>%
         group_by(category_parent_name_recode)%>%
         ggplot(aes(x=category_parent_name_recode, fill = state)) +
         geom_bar(position="dodge") +
         scale_fill_manual(breaks = c("failed", "successful"),
                           values=c(fail_color,success_color)) +
         coord_flip() +
         theme(axis.text.x = element_text(angle=90, size=12),
               axis.title.x=element_blank(),axis.title.y=element_blank(),
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               plot.background=element_blank(),
               legend.title=element_blank(), legend.position = "right",
               legend.text = element_text(size=12))
    } else {
      filtered_data() %>%
        group_by(location_expanded_country)%>%
        ggplot(aes(x=location_expanded_country, fill = state)) +
        geom_bar(position="dodge") +
        scale_fill_manual(breaks = c("failed", "successful"),
                          values=c(fail_color,success_color)) +
        coord_flip() +
        theme(axis.text.x = element_text(angle=90, size=12),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.title=element_blank(), legend.position = "right",
              legend.text = element_text(size=12))
    }
  })
  
  output$plot_duration <- renderPlot({
    if (input$selected_grouping == "category_parent_name_recode") {
      filtered_data() %>%
        ggplot(aes(x= as.factor(category_parent_name_recode),
                   y=campaign_duration,
                   fill = state)) +
        geom_boxplot()+
        scale_fill_manual(breaks = c("failed", "successful"),
                          values=c(fail_color,success_color)) +
        theme(axis.text.x = element_text(angle=90, size=12),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.title=element_blank(), legend.position = "right",
              legend.text = element_text(size=12))
    } else {
      filtered_data() %>%
        ggplot(aes(x= as.factor(location_expanded_country),
                   y=campaign_duration,
                   fill = state)) +
        geom_boxplot()+
        scale_fill_manual(breaks = c("failed", "successful"),
                          values=c(fail_color,success_color)) +
        theme(axis.text.x = element_text(angle=90, size=12),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.title=element_blank(), legend.position = "right",
              legend.text = element_text(size=12))
    }
  })
  
  output$plot_staffpick <- renderPlot ({
    if (input$selected_grouping == "category_parent_name_recode") {
      filtered_data() %>%
        group_by(staff_pick) %>%
        ggplot(aes(x = staff_pick, fill = state )) +
        geom_bar(position = "fill") +
        facet_grid(~category_parent_name_recode, scale="free") +
        scale_fill_manual(breaks = c("failed", "successful"),
                          values=c(fail_color,success_color)) +
        theme(axis.text.x = element_text(size=12,angle=90),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.title=element_blank(), legend.position = "right",
              legend.text = element_text(size=12))
    } else {
      filtered_data() %>%
        group_by(staff_pick) %>%
        ggplot(aes(x = staff_pick, fill = state )) +
        geom_bar(position = "fill") +
        facet_grid(~location_expanded_country, scale="free") +
        scale_fill_manual(breaks = c("failed", "successful"),
                          values=c(fail_color,success_color)) +
        theme(axis.text.x = element_text(size=12,angle=90),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              legend.title=element_blank(), legend.position = "right",
              legend.text = element_text(size=12))
      }
  })
  
  output$plot_numcreator <- renderPlot({
    filtered_data() %>%
      group_by(num_project_creator)%>%
      ggplot(aes(x=num_project_creator, fill = state))+
      geom_bar(position="fill") +
      coord_flip() +
      scale_fill_manual(breaks = c("failed", "successful"), 
                        values=c(fail_color,success_color)) +
      theme(axis.text.x = element_text(size=12),
            axis.title.x=element_blank(),axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.title=element_blank(), legend.position = "right",
            legend.text = element_text(size=12))
  })
  
  output$plot_month <- renderPlot({
    
    filtered_data() %>%
      group_by(launch_month)%>%
      ggplot(aes(x=launch_month, fill = state))+
      geom_bar(position="fill") +
      scale_fill_manual(breaks = c("failed", "successful"), 
                        values=c(fail_color,success_color)) +
      theme(axis.text.x = element_text(size=12),
            axis.title.x=element_blank(),axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.title=element_blank(), legend.position = "right",
            legend.text = element_text(size=12))
  })
  
  output$plot_day <- renderPlot({
    filtered_data() %>%
      group_by(launch_day)%>%
      ggplot(aes(x=launch_day, fill = state))+
      geom_bar(position="fill") +
      scale_fill_manual(breaks = c("failed", "successful"), 
                        values=c(fail_color,success_color)) +
      theme(axis.text.x = element_text(size=12),
            axis.title.x=element_blank(),axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.title=element_blank(), legend.position = "right",
            legend.text = element_text(size=12))
  })
  
  pt_table <- reactive({ 
    filtered_data()%>%
    dplyr::mutate(
      country=location_expanded_country,
      main_category=category_parent_name_recode,
      subcategory=category_name,
      count=1,
      number_of_backer=backers_count,
      name_length=name_word_len,
      description_length=blurb_word_len,
      number_of_projects_by_creator=num_project_creator,
      launch_date=launched_at_utc
    ) %>%
    dplyr::select(
      country,
      location_state,
      main_category,
      subcategory,
      count,
      state,
      number_of_backer,
      name_length,
      description_length,
      number_of_projects_by_creator,
      campaign_duration,
      backers_count,
      launch_date,
      launch_year,
      launch_month,
      launch_day,
      staff_pick,
      usd_pledged,
      usd_goal
    )
  })
  
  output$pivot_table<-renderRpivotTable({
    rpivotTable(data=pt_table(),
                rows="state",
                cols="main_category",
                rendererName="Heatmap")
  })
  
  #Table with Links
  output$table_with_link <- renderDataTable({
    
    table <- filtered_data() %>%
      filter(state=="successful") %>%
      mutate(pledge_vs_goal=label_percent(accuracy=1,big.mark=",")(usd_pledged/usd_goal),
             usd_pledged_per_backer=dollar_format()(usd_pledged/backers_count),
             usd_pledged=dollar_format()(usd_pledged),
             usd_goal=dollar_format()(usd_goal),
             link=paste0("<a href='", url, "'>Link to Project</a>")) %>%
      arrange(desc(pledge_vs_goal)) %>%
      dplyr::select(name,
                    launched_at_utc,
                    location_expanded_country,
                    location_state,
                    category_parent_name_recode,
                    category_name,
                    usd_pledged,
                    usd_goal,
                    pledge_vs_goal,
                    backers_count,
                    usd_pledged_per_backer,
                    link) %>%
      rename("Project Name"="name",
             "Launch Date"= "launched_at_utc",
             "Country"="location_expanded_country",
             "State"="location_state",
             "Main Category"="category_parent_name_recode",
             "Sub Category"="category_name",
             "USD Pledged"="usd_pledged",
             "USD Goal"="usd_goal",
             "Pledge / Goal"="pledge_vs_goal",
             "# Backers"="backers_count",
             "Avg. USD Pledged per Backer"="usd_pledged_per_backer",
             "Link"="link"
      )
    
    return(table)
    
  }, escape = FALSE)
  
  
  #mlr pledged amount
  mlr_run <- eventReactive(input$button_mlr, {
    validate(
      need(input$mlr_variables != "", "Please select at least one Independent Variable to generate a Regression Model")
    )
    
    variablesm<-c("usd_pledged",input$mlr_variables)
    
    mlr_date_range <- ks_data%>%filter(launched_at_utc >= input$mlr_selected_date[1] &
                                       launched_at_utc <= input$mlr_selected_date[2])
    mlr_data = mlr_date_range[variablesm]
    mlr_data<-na.omit(mlr_data)
    
    
    fit1<-lm(usd_pledged ~.,data=mlr_data)
    fit2<-lm(usd_pledged ~1,data=mlr_data)
    
    if (input$mlr_direction == "backward") {
      step<- stepAIC(fit1,direction = "backward")
      
    } else if (input$mlr_direction == "forward") {
      step<- stepAIC(fit2,direction = "forward",scope = list(upper = fit1,lower = fit2))
      
    } else {
      step<- stepAIC(fit2,direction = "both",scope = list(upper = fit1,lower = fit2))
    }
  })
  
  output$mlr_num_projects <- renderText({
    n_total <- (ks_data%>%filter(launched_at_utc >= input$mlr_selected_date[1] &
                                 launched_at_utc <= input$mlr_selected_date[2])%>% dplyr::summarize(count=n()))[, 1]
    format(n_total, big.mark=",")
  })
  
  output$mlr_summary <- renderPrint({
    req(mlr_run())
    summary(mlr_run())
  })
  
  mlr_predict <- reactive ({
    mlr_data_frame <- data.frame("category_parent_name_recode" = input$category_parent_name_recode,
                        "location_country" = input$location_country,
                        "campaign_duration" = input$campaign_duration,
                        "backers_count" = input$backers_count,
                        "launch_day" = input$launch_day,
                        "launch_month" = input$launch_month,
                        "name_word_len" = input$name_word_len,
                        "blurb_word_len" = input$blurb_word_len,
                        "staff_pick" = input$staff_pick,
                        "num_project_creator" = input$num_project_creator)
    
    mlr_results <- predict(mlr_run(), newdata = mlr_data_frame)
    cat(mlr_results)
  })
  
  output$mlr_predict <- renderPrint({
    mlr_predict()
  })
  
  
  #logit staff pick
  logit_run <- eventReactive(input$button_logit, {
    validate(
      need(input$logit_variables != "", "Please select at least one Independent Variable to generate a Regression Model")
    )
    variablesl<-c("staff_pick",input$logit_variables)
    
    logit_by_category <- ks_data%>%filter(category_parent_name_recode == input$logit_category)%>%
                                   filter(launched_at_utc >= input$logit_selected_date[1] &
                                          launched_at_utc <= input$logit_selected_date[2])
    logit_data = logit_by_category[variablesl]
    logit_data<-na.omit(logit_data)
    
    logit<-glm(staff_pick ~., data=logit_data, family = binomial(link = "logit"))
  })
  
  output$logit_num_projects <- renderText({
    n_total <- (ks_data%>%filter(category_parent_name_recode == input$logit_category)%>% 
                          filter(launched_at_utc >= input$logit_selected_date[1] &
                                 launched_at_utc <= input$logit_selected_date[2])%>% dplyr::summarize(count=n()))[, 1]
    format(n_total, big.mark=",")
  })
  
  output$logit_summary <- renderPrint({
    req(logit_run())
    summary(logit_run())
  })
  
  logit_predict <- reactive ({
    logit_data_frame <- data.frame("location_country" = input$logit_location_country,
                                 "campaign_duration" = input$logit_campaign_duration,
                                 "launch_day" = input$logit_launch_day,
                                 "launch_month" = input$logit_launch_month,
                                 "name_word_len" = input$logit_name_word_len,
                                 "blurb_word_len" = input$logit_blurb_word_len,
                                 "usd_goal" = input$logit_usd_goal,
                                 "num_project_creator" = input$logit_num_project_creator)
    
    logit_results <- predict(logit_run(), newdata = logit_data_frame, type="response")
    cat(logit_results)
  })
  
  output$logit_predict <- renderPrint({
    validate(
      need(input$logit_usd_goal != "", "Please set a valid USD Goal")
    )
    logit_predict()
  })
  
  
  #logit state
   logit2_run <- eventReactive(input$button_logit2, {
     validate(
       need(input$logit2_variables != "", "Please select at least one Independent Variable to generate a Regression Model")
     )

     variablesls<-c("state",input$logit2_variables)

     logit2_by_category <- ks_data%>%filter(category_parent_name_recode == input$logit2_category)%>%
                                     filter(launched_at_utc >= input$logit2_selected_date[1] &
                                            launched_at_utc <= input$logit2_selected_date[2])

     logit2_data = logit2_by_category[variablesls]
     logit2_data<-na.omit(logit2_data)

     logit2<-glm(state ~., data=logit2_data, family = binomial(link = "logit"))
   })

   output$logit2_num_projects <- renderText({
     n_total <- (ks_data%>%filter(category_parent_name_recode == input$logit2_category)%>%
                           filter(launched_at_utc >= input$logit2_selected_date[1] &
                                  launched_at_utc <= input$logit2_selected_date[2])%>% dplyr::summarize(count=n()))[, 1]
     format(n_total, big.mark=",")
   })
  
  output$logit2_summary <- renderPrint({
    req(logit2_run())
    summary(logit2_run())
  })
  
  logit2_predict <- reactive ({
    logit2_data_frame <- data.frame("location_country" = input$logit2_location_country,
                                   "campaign_duration" = input$logit2_campaign_duration,
                                   "launch_day" = input$logit2_launch_day,
                                   "launch_month" = input$logit2_launch_month,
                                   "name_word_len" = input$logit2_name_word_len,
                                   "blurb_word_len" = input$logit2_blurb_word_len,
                                   "usd_goal" = input$logit2_usd_goal,
                                   "staff_pick" = input$logit2_staff_pick,
                                   "num_project_creator" = input$logit2_num_project_creator)
    
    logit2_results <- predict(logit2_run(), newdata = logit2_data_frame, type="response")
    cat(logit2_results)
  })
  
  output$logit2_predict <- renderPrint({
    validate(
      need(input$logit2_usd_goal != "", "Please set a valid USD Goal")
    )
    logit2_predict()
  })
  
} #server

# Run the app ----
shinyApp(ui = ui, server = server)