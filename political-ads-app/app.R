# Shiny app

library(shiny)
library(shinythemes)
library(broom)
library(scales)
library(gt)
library(tidyverse)

# Read in RDS file with cleaned data 

poll_ads_weekly <- read_rds("poll_ads_weekly.rds")

# Create vectors of candidates based on their success
# as defined by qualification for Democratic debates
# where each month the qualification threshold increased
# I will use these vectors as input options in my Shiny app

all_cands <- c("Biden", "Booker", "Buttigieg", "Castro", "Gabbard",
                          "Harris", "Klobuchar", "O'Rourke", "Sanders",
                          "Steyer", "Warren", "Williamson", "Yang")

november_debate_cands <- c("Biden", "Booker", "Buttigieg", "Gabbard",
                           "Harris", "Klobuchar", "Sanders",
                           "Steyer", "Warren", "Yang")

december_debate_cands <- c("Biden", "Buttigieg", "Harris", 
                           "Klobuchar", "Sanders", "Warren")

# Define UI for application 

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title describes the overarching question of my project
  navbarPage("Performance of Political Ads on Google",
             
  # I created a "Visualizations" page to show different candidates' ad strategies
  # as a first pass at finding a relationship between ad spend and poll results
  
  tabPanel("Visualizations",
           tabsetPanel(
             
             # The "Overview" panel looks at the relationship between ad spend 
             # and polling
             
             tabPanel("Overview",
                      
                      # Plot with double axes: left = ad spend, right = poll result
                      # This is a very easy way to see if there's a general correlation
                      # between changes in ad spending and poll results
                      # I facetted by candidate to make the relationship easier to see
                      # But there is not a very good relationship -- the lines don't
                      # generally match up
                      
                      mainPanel(h3("Ad Spending & Polling Results over 2019"),
                                plotOutput("spend_and_poll_plot")),
                      
                      # Allow users to pick which candidates they want to see
                      # The default is the candidates qualified for the December
                      # debate, since they are the most prominent. The options range
                      # to candidates who had qualified for October, since those are
                      # still in the race.
                      
                      sidebarPanel(
                        checkboxGroupInput("overview_cands","Select Democratic Presidential Candidates",
                                           choices = all_cands,
                                           selected = december_debate_cands))),
             
             # The "Ads" panel looks specifically at candidates' ad strategies
             
             tabPanel("Ads",
                      
                      # Allow users to pick which candidates they want to investigate
                      
                      sidebarPanel(
                        checkboxGroupInput("ad_cands","Select Democratic Presidential Candidates",
                                           choices = all_cands,
                                           selected = december_debate_cands)),
                      mainPanel(
                        h3("Ad Strategies by Candidate"),
                        
                        # Use more tabs to show different facets of the ad strategies:
                        # how much they spend, how many ads they buy
                        
                        tabsetPanel(
                          tabPanel("Ad Spend", plotOutput("spend_plot")),
                          tabPanel("Number of Ads", plotOutput("number_of_ads_plot"))))),
             
             # The "Polling" tab shows just polling results over 2019,
             # again allowing users to choose candidates to focus on
             
             tabPanel("Polling",
                      mainPanel(h3("Polling Results over 2019"),
                                plotOutput("polling_plot")),
                      sidebarPanel(
                        checkboxGroupInput("polling_cands","Select Democratic Presidential Candidates",
                                           choices = all_cands,
                                           selected = december_debate_cands)))
           )),
  
  # The "Model" page focuses on the regression between ad spend and poll result
  
  tabPanel("Model",
           tabsetPanel(
             
             # "Full Model" is based on the regression across all candidates 
             
             tabPanel("Full Model",
                      h3("Relationship between Ad Spending & Polling Results"),
                      
                      # Scatterplot of ad spend by polling results
                      # I added a geom_line of the linear regression, 
                      # with some confidence interval shading
                      # You can see the relationship is positive, as candidates would hope
                      # More ad spending is associated with higher poll results
                      
                      plotOutput("regress_spend_graph"),
                      hr(),
                      
                      # Table of regression coefficients and their confidence intervals
                      # Again you can see the 90% confidence interval for the ad spend
                      # coefficient is fully positive, so it is very likely the relationship
                      # is indeed positive 
                      
                      gt_output("spend_coef_table"),
                      hr(),
                      includeHTML("fullmodel_explanation.Rhtml")),
             
             # The "By Candidate" panel shows the relationship between ad spend and poll results
             # by different candidates. I did this because I suspected that candidates with
             # different national exposures might benefit differently from Google ads. 
             # I hypothesized candidates that were less well known could benefit more.
             
             tabPanel("By Candidate",
                      mainPanel(
                        
                        # Bar chart of the ad spend coefficient by candidate. 
                        # As expected, some lesser known candidates like Buttigieg and Yang
                        # had high coefficients. Somewhat surprisingly to me, Warren had the 
                        # highest coefficient. 
                        
                        h3("Relationship between Ad Spending & Polling Results by Candidate"),
                        plotOutput("spend_coef_plot"),
                        hr(),
                        includeHTML("by_candidate_explanation.Rhtml")),
                      sidebarPanel(
                        checkboxGroupInput("regress_spend_cands", 
                                           "Select Democratic Presidential Candidates",
                                           choices = all_cands,
                                           selected = december_debate_cands))),
             
             # "Model Info" provides a description of how I created the regression
             
             tabPanel("Model Info",
                      includeHTML("explanation.Rhtml"))
           )),  
  
  # "About" page describing my project
  
  tabPanel("About",
          mainPanel(
            includeHTML("about.Rhtml")
          ))
  )
)

# Define server logic 

server <- function(input, output) {
  
  # VISUALIZATIONS
  
  # Line graph with double axes to show both ad spend and poll results over 2019
   
  output$spend_and_poll_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% input$overview_cands) %>% 
      ggplot(aes(x = end_week)) +
      geom_line(aes(y = Spend_USD, color = "Ad Spending")) +
      
      # Scale up percentages so you can see the relationship well
      
      geom_line(aes(y = avg_pct * 10000, color = "Percent")) +
      scale_y_continuous(label = comma, 
                         sec.axis = sec_axis(~./10000, name = "Percent")) +
      
      # Facet wrap by candidate to see each person's trends separately
      
      facet_wrap(~answer) +
      labs(x = "Week", y = "Ad Spending (USD)", sec.axis = "Polling %",
           color = "Color")
  })
  
  # Line graph of polling results over 2019, different lines by candidate
  
  output$polling_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% input$polling_cands) %>% 
      ggplot(aes(x = end_week, y = avg_pct, group = answer, color = answer)) +
      geom_line() +
      labs(x = "Week", y = "Avg. Poll Result (%)",
           color = "Candidate",
           caption = "Avg. Poll Result calculated by averaging the results of all primary polls ending that week, as collected by FiveThirtyEight")
  })
  
  # Bar graph of candidates' ad spend per week
  # One bar per week, but candidates are delineated by color
  # I wanted to show both how general ad spending trended over the year
  # (progressively more spending by everyone) as well as individuals' spending

  output$spend_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% input$ad_cands) %>% 
      ggplot(aes(x = end_week, y = Spend_USD, group = answer, fill = answer)) +
      geom_col() +
      scale_y_continuous(label = comma) +
      labs(x = "Week", y = "Ad Spending (USD)", 
           title = "Ad Spending per Week",
           fill = "Candidate")
  })
  
  # Bar graph of how many ads were taken out by candidates per week
  # Again, one bar per week, with colors delineating different candidates
  # You can see how the number of ads increased over the year, 
  # and how candidates' magnitudes changed. Warren seems to have been the 
  # dominant candidate by far throughout the year
  
  output$number_of_ads_plot <- renderPlot({
    poll_ads_weekly %>% 
      filter(answer %in% input$ad_cands) %>% 
      ggplot(aes(x = end_week, y = ad_number, fill = answer)) +
      geom_col() +
      labs(x = "Week", y = "Number of Ads", 
           title = "Number of Ads Run per Week",
           fill = "Candidate")
  })
  
  # REGRESSION
  
  # Create regression data
  
  regression_data <- poll_ads_weekly %>%
    
    # Because so much money is involved in ad spending, 
    # to make the regression clearer to see, scale Spend_USD by 1000
    # so that in regression, the association is based on an increase
    # in ad spend by $1000, not just $1
    
    mutate(Spend_USD = Spend_USD / 1000)
  
  # Full regression model: predict avg poll result by ad spend in that week
  # across all candidates
  
  regress_spend = lm(avg_pct ~ Spend_USD, data = regression_data)
  
  # Scatterplot of ad spend vs. polling result by week
  # adding the linear regression line with confidence interval shading
  
  output$regress_spend_graph <- renderPlot({
    regression_data %>% 
      ggplot(aes(x = Spend_USD * 1000, y = avg_pct)) +
        geom_point() + 
        geom_smooth(method = "lm") + 
        scale_x_continuous(label = comma) +
        labs(title = "Ad Spending vs. Poll Results",
             subtitle = "Across All Democratic Candidates",
             x = "Ad Spending (USD)", 
             y = "Avg. Poll Result (%)")
  })
  
  # Table of the regression coefficients and 90% confidence interval
  # to further demonstrate how the relationship is very likely positive 
  
  output$spend_coef_table <- render_gt({
    regress_spend %>% 
      tidy() %>% 
      bind_cols(confint_tidy(regress_spend, conf.level = .9)) %>%
      select(term, estimate, conf.low, conf.high) %>% 
      mutate(term = recode(term, Spend_USD = "Ad Spend")) %>% 
      gt() %>% 
      tab_header(title = "Ad Spending vs. Polling Results",
                 subtitle = "Across All Democratic Candidates") %>% 
      cols_label(term = "", estimate = "Coefficient",
                 conf.low = "5th Percentile", conf.high = "95th Percentile") %>%
      tab_source_note("Ad Spend in units of $1000") %>% 
      fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 3)
  })
  
  # Create linear regression models for each candidate separately
  # and pluck the ad spend coefficient to compare
  
  regress_spend_by_cand <- regression_data %>%
    group_by(answer) %>%
    nest() %>%
    mutate(model = map(data, ~lm(avg_pct ~ Spend_USD, data = .x))) %>%
    mutate(spend_coef = map_dbl(model, ~coef(.x) %>% pluck("Spend_USD")))
  
  # Bar graph of the ad spend coefficient by candidate
  
  output$spend_coef_plot <- renderPlot({
    regress_spend_by_cand %>%
      filter(answer %in% input$regress_spend_cands) %>% 
      ggplot(aes(x = answer, y = spend_coef)) + 
        geom_col() +
      
        # Add line representing the ad spend coefficient in the full model
        # as benchmark to compare whether candidates are getting more for their money
        # than their competitors
      
        geom_hline(yintercept = summary(regress_spend)$coef[2,1],
                   linetype = "dashed", color = "dark green") +
        labs(title = "Spend Model Coefficients by Candidate",
           x = "Candidate",
           y = "Ad Spend Coefficient",
           caption = "Dashed line represents ad spend coefficient for model including all candidates \n Ad Spend in units of $1000")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

