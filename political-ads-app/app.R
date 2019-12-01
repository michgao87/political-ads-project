#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(broom)
library(plotly)
library(scales)
library(directlabels)
library(gt)
library(dotwhisker)
library(arm)
library(GGally)
library(tidyverse)

ads <- read_rds("ads.rds")
advertisers <- read_rds("advertisers.rds")
advertisers_weekly_spend <- read_rds("advertisers_weekly_spend.rds")
ad_campaigns <- read_rds("ad_campaigns.rds")
primaries <- read_rds("primaries.rds")
poll_ads_weekly <- read_rds("poll_ads_weekly.rds")


# Define UI for application 
ui <- fluidPage(
  navbarPage("Google Political Ads",
  tabPanel("Model",
          tabsetPanel(
            tabPanel("Model",
                     h3("Regression Models Across Democratic Candidates"),
                     h5("Independent Variable: Primary Poll Results"),
                     h5("Aggregated by Week"),
                     plotOutput("full_model_graph"),
                     plotOutput("spend_model_graph"),
                     plotOutput("ad_number_model_graph")),
            tabPanel("By Candidate",
                     h3("Regression Models by Candidate"),
                     h5("Independent Variable: Primary Poll Results"),
                     h5("Aggregated by Week"),
                     plotOutput("spend_coef_by_cand_graph"),
                     plotOutput("ad_number_coef_by_cand_graph"),
                     plotOutput("interaction_coef_by_cand_graph")),
            tabPanel("Variables",
                     includeHTML("explanation.Rhtml"))
          )),    
  tabPanel("Visualizations",
           tabsetPanel(
             tabPanel("Overview",
                      h3("Ad Spending & Polling Results over 2019"),
                      plotOutput("spend_and_poll_plot")),
             tabPanel("Polling",
                      h3("Polling Results over 2019"),
                      plotOutput("polling_plot")),
             tabPanel("Ad Spending",
                      h3("Ad Spending by Candidate"),
                      plotOutput("spend_plot"),
                      br(),
                      plotOutput("number_of_ads_plot"))
           )),
  tabPanel("About",
          mainPanel(
            includeHTML("about.Rhtml")
          ))
  )
)

# Define server logic 
server <- function(input, output) {
  
  october_debate_cands <- c("Biden", "Booker", "Buttigieg", "Castro", "Gabbard",
                            "Harris", "Klobuchar", "O'Rourke", "Sanders",
                            "Steyer", "Warren", "Yang")
  
  november_debate_cands <- c("Biden", "Booker", "Buttigieg", "Gabbard",
                             "Harris", "Klobuchar", "Sanders",
                             "Steyer", "Warren", "Yang")
  
  december_debate_cands <- c("Biden", "Buttigieg", "Harris", 
                             "Klobuchar", "Sanders","Warren")
  
  # Visualizations
   
  output$spend_and_poll_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% november_debate_cands) %>% 
      ggplot(aes(x = end_week)) +
      geom_line(aes(y = Spend_USD, color = "Ad Spending")) +
      geom_line(aes(y = avg_pct * 10000, color = "Percent")) +
      scale_y_continuous(label = comma, sec.axis = sec_axis(~./10000, name = "Percent")) +
      facet_wrap(~answer) +
      labs(x = "Week", y = "Ad Spending (USD)", sec.axis = "Polling %",
           title = "Ad Spending and Polling Results over 2019",
           subtitle = "Democratic candidates qualified for November debate",
           color = "Color")
  })
  
  output$polling_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% november_debate_cands) %>% 
      ggplot(aes(x = end_week, y = avg_pct, group = answer, color = answer)) +
      geom_line() +
      scale_color_discrete(guide = "none") +
      coord_cartesian(clip = "off") +
      geom_dl(aes(label = answer), method = list(dl.combine("last.points"))) +
      labs(title = "Primary Polling Results over 2019",
           subtitle = "Democratic candidates qualified for November debate",
           x = "Week", y = "Avg. Poll Result (%)",
           caption = "Avg. Poll Result calculated by averaging the results of all primary polls ending that week, as collected by FiveThirtyEight")
  })
  
  output$spend_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% november_debate_cands) %>% 
      ggplot(aes(x = end_week, y = Spend_USD, group = answer, color = answer)) +
      geom_line() +
      scale_y_continuous(label = comma) +
      labs(x = "Week", y = "Ad Spending (USD)", 
           title = "Ad Spending per Week",
           subtitle = "Democratic candidates qualified for November debate",
           color = "Candidate")
  })
  
  output$number_of_ads_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% november_debate_cands) %>% 
      ggplot(aes(x = end_week, y = ad_number, fill = answer)) +
      geom_col() +
      labs(x = "Week", y = "Number of Ads", 
           title = "Number of Ads Run per Week",
           subtitle = "Democratic candidates qualified for November debate",
           color = "Candidate")
  })
  
  # Regression models
  
  november_poll_ads_weekly <- poll_ads_weekly %>% 
    filter(answer %in% november_debate_cands)
  
  full_model = lm(avg_pct ~ Spend_USD + ad_number + text_ads 
                  + above_10k_impressions + age_targeting 
                  + gender_targeting, data = november_poll_ads_weekly)
  
  spend_model = lm(avg_pct ~ Spend_USD, data = november_poll_ads_weekly)
  
  ad_number_model = lm(avg_pct ~ ad_number, data = november_poll_ads_weekly)
  
  output$full_model_graph <- renderPlot({
   ggcoef(full_model, conf.level = 0.9, 
          exclude_intercept = T,
          color = "dark green", sort = "ascending") +
      labs(title = "Full Model Coefficients",
           x = "Coefficient Estimate",
           y = "Predictor Variable")
  })
  
  output$spend_model_graph <- renderPlot({
    ggcoef(spend_model, conf.level = 0.9, 
           exclude_intercept = T,
           color = "dark green", sort = "ascending") +
      labs(title = "Spend Model Coefficients",
           x = "Coefficient Estimate",
           y = "Predictor Variable")
  })
  
  output$ad_number_model_graph <- renderPlot({
    ggcoef(ad_number_model, conf.level = 0.9, 
           exclude_intercept = T,
           color = "dark green", sort = "ascending") +
      labs(title = "Ad Volume Model Coefficients",
           x = "Coefficient Estimate",
           y = "Predictor Variable")
  })
  
  # Regression models by candidate
  
  spend_model_by_cand <- november_poll_ads_weekly %>% 
    group_by(answer) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(avg_pct ~ Spend_USD, data = .x))) %>% 
    mutate(spend_coef = map_dbl(model, ~coef(.x) %>% pluck("Spend_USD"))) 
  
  output$spend_coef_by_cand_graph <- renderPlot({
    spend_model_by_cand %>% 
      ggplot(aes(x = answer, y = spend_coef)) +
        geom_col() +
        labs(title = "Spend Model Coefficients",
           x = "Candidate",
           y = "Ad Spend Coefficient")
  })
  
  ad_number_model_by_cand <- november_poll_ads_weekly %>% 
    group_by(answer) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(avg_pct ~ ad_number, data = .x))) %>% 
    mutate(ad_number_coef = map_dbl(model, ~coef(.x) %>% pluck("ad_number"))) 
  
  output$ad_number_coef_by_cand_graph <- renderPlot({
    ad_number_model_by_cand %>% 
      ggplot(aes(x = answer, y = ad_number_coef)) +
      geom_col() +
      labs(title = "Ad Volume Model Coefficients",
           x = "Candidate",
           y = "Ad Volume Coefficient")
  })
  
  interaction_model_by_cand <- november_poll_ads_weekly %>% 
    group_by(answer) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(avg_pct ~ ad_number*Spend_USD, data = .x))) %>% 
    mutate(interaction_coef = map_dbl(model, ~coef(.x) %>% pluck("ad_number:Spend_USD"))) 
  
  output$interaction_coef_by_cand_graph <- renderPlot({
    interaction_model_by_cand %>% 
      ggplot(aes(x = answer, y = interaction_coef)) +
      geom_col() +
      labs(title = "Interaction Model Coefficients",
           x = "Candidate",
           y = "Spend_USD * Ad Volume Coefficient",
           subtitle = "Predicting Primary Poll Results")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

