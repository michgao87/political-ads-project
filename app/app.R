#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

ads <- read_rds("ads.rds")
advertisers <- read_rds("advertisers.rds")
advertisers_weekly_spend <- read_rds("advertisers_weekly_spend.rds")
ad_campaigns <- read_rds("ad_campaigns.rds")
primaries <- read_rds("primaries.rds")
poll_ads_weekly <- read_rds("poll_ads_weekly.rds")


# Define UI for application 
ui <- fluidPage(
  navbarPage("Political Web Ads",
  tabPanel("Model",
          tabsetPanel(
            tabPanel("Model"))),    
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
            h3("Data"),
            h6("The data comes from the Google Transparency Political Ad Project. "),
            h3("Me"),
            h6("I am a Harvard junior studying government and statistics.")
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
      geom_line(aes(y = Spend_USD, color = "Spend")) +
      geom_line(aes(y = avg_pct * 10000, color = "Percent")) +
      scale_y_continuous(sec.axis = sec_axis(~./10000, name = "Percent")) +
      facet_wrap(~answer)
  })
  
  output$polling_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = avg_pct, group = answer, color = answer)) +
      geom_line()
  })
  
  output$spend_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = Spend_USD, group = answer, color = answer)) +
      geom_line()
  })
  
  output$number_of_ads_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% december_debate_cands) %>% 
      ggplot(aes(x = end_week, y = ad_number, fill = answer)) +
      geom_col()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

