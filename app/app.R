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
                     dataTableOutput("spend_by_cand_model"),
                     tableOutput("number_by_cand_model"),
                     tableOutput("full_spend_model"),
                     tableOutput("full_numb_model"),
                     tableOutput("full_spend_numb_model"))
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
   
  output$spend_and_poll_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week)) +
      geom_line(aes(y = Spend_USD, color = "Ad Spending")) +
      geom_line(aes(y = avg_pct * 10000, color = "Percent")) +
      scale_y_continuous(label = comma, sec.axis = sec_axis(~./10000, name = "Percent")) +
      facet_wrap(~answer) +
      labs(x = "Week", y = "Ad Spending (USD)", sec.axis = "Polling %",
           title = "Ad Spending and Polling Results over 2019",
           subtitle = "Democratic candidates qualified for December debate",
           color = "Color")
  })
  
  output$polling_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = avg_pct, group = answer, color = answer)) +
      geom_line() +
      scale_color_discrete(guide = "none") +
      coord_cartesian(clip = "off") +
      geom_dl(aes(label = answer), method = list(dl.combine("last.points"))) +
      labs(title = "Primary Polling Results over 2019",
           subtitle = "Democratic candidates qualified for December debate",
           x = "Week", y = "Avg. Poll Result (%)",
           caption = "Avg. Poll Result calculated by averaging the results of all primary polls ending that week, as collected by FiveThirtyEight")
  })
  
  output$spend_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = Spend_USD, group = answer, color = answer)) +
      geom_line() +
      scale_y_continuous(label = comma) +
      labs(x = "Week", y = "Ad Spending (USD)", 
           title = "Ad Spending per Week",
           subtitle = "Democratic candidates qualified for December debate",
           color = "Candidate")
  })
  
  output$number_of_ads_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = ad_number, fill = answer)) +
      geom_col() +
      labs(x = "Week", y = "Number of Ads", 
           title = "Number of Ads Run per Week",
           subtitle = "Democratic candidates qualified for December debate",
           color = "Candidate")
  })
  
  # Regression models
  
  output$full_spend_model <- renderTable({
    lm(avg_pct ~ Spend_USD, data = poll_ads_weekly) %>% 
      tidy()
  })
  
  output$full_numb_model <- renderTable({
    lm(avg_pct ~ ad_number, data = poll_ads_weekly) %>% 
      tidy()
  })
  
  output$full_text_model <- renderTable({
    lm(avg_pct ~ Spend_USD*text_ads, data = poll_ads_weekly) %>% 
      tidy()
  })
  
  output$full_spend_numb_model <- renderTable({
    lm(avg_pct ~ Spend_USD * ad_number, data = poll_ads_weekly) %>% 
      tidy()
  })

  output$spend_by_cand_model <- renderDataTable({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      group_by(answer) %>% 
      nest() %>% 
      mutate(model = map(data, ~lm(avg_pct ~ Spend_USD, data = .x))) %>% 
      mutate(spend_coef = map_dbl(model, ~coef(.x) %>% pluck("Spend_USD"))) %>% 
      mutate(rsquared = map_dbl(model, ~glance(.x) %>% pluck("r.squared"))) %>% 
      #bind_cols(list(c("Biden", "Sanders", "Warren", "Harris", "Klobuchar", "Buttigieg"))) %>% 
      select(answer, spend_coef, rsquared) %>%
      as.data.frame() #%>% 
      #gt()# %>% 
      #fmt_number(columns = vars(spend_coef), decimals = 5)
  })
  
  output$number_by_cand_model <- renderTable({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      group_by(answer) %>% 
      nest() %>% 
      mutate(model = map(data, ~lm(avg_pct ~ ad_number, data = .x))) %>% 
      mutate(number_coef = map_dbl(model, ~coef(.x) %>% pluck("ad_number"))) %>% 
      mutate(rsquared = map_dbl(model, ~glance(.x) %>% pluck("r.squared"))) %>% 
      select(-c(model, data)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

