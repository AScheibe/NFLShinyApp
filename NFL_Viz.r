install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

packages <- c("DT", "shiny", "tidyverse", "ggplot2")
lapply(packages, install_if_missing)

# Load necessary libraries
library(DT)
library(shiny)
library(tidyverse)
library(ggplot2)

# Read dataset
team_name_mapping <- c(
  "Washington Redskins" = "Washington Commanders",
  "Washington Football Team" = "Washington Commanders",
  "San Diego Chargers" = "Los Angeles Chargers",
  "St. Louis Rams" = "Los Angeles Rams",
  "Oakland Raiders" = "Las Vegas Raiders"
)

# Read dataset
team_stats <- read_csv("https://alexscheibe.com/assets/files/team_stats_2003_2023.csv")

# Apply the mapping to update team names
team_stats <- team_stats %>%
  mutate(team = recode(team, !!!team_name_mapping))

# Define UI for the application
# Define UI for the application
ui <- fluidPage(
  
  titlePanel("NFL Team Performance: 2003-2023"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Select the type of visualization
      selectInput("visualization", "Select Visualization Type:",
                  choices = c("Single Team Performance", "Team Comparisons", "Points Scored/Points Allowed")),
      helpText("Choose the visualization type for team statistics."),
      
      # Inputs for Single Team Performance
      conditionalPanel(
        condition = "input.visualization == 'Single Team Performance'",
        selectInput("team", "Select a Team:",
                    choices = unique(team_stats$team), selected = "New England Patriots"),
        helpText("Select a team to display its performance over the years."),
        selectInput("metric", "Select Metric:",
                    choices = c("Win Percentage" = "win_loss_perc",
                                "Points Scored" = "points",
                                "Points Allowed" = "points_opp",
                                "Rushing Yards per Attempt" = "rush_yds_per_att",
                                "Penalties" = "penalties")),
        helpText("Select a statistical metric to visualize."),
        sliderInput("years", "Select Year Range:",
                    min = min(team_stats$year), max = max(team_stats$year),
                    value = c(min(team_stats$year), max(team_stats$year)), sep = ""),
        helpText("Use the slider to choose the time range for the data.")
      ),
      
      # Inputs for Team Comparisons
      conditionalPanel(
        condition = "input.visualization == 'Team Comparisons'",
        selectInput("comparisonMetric", "Select Comparison Metric:",
                    choices = c("Win Percentage", "Points Scored vs. Allowed", 
                                "Margin of Victory", "Rushing Yards per Attempt", 
                                "Penalties", "Turnover Percentage", "Expected Points")),
        helpText("Select a metric to compare between the selected teams."),
        sliderInput("comparison_years", "Select Year Range:",
                    min = min(team_stats$year), max = max(team_stats$year),
                    value = c(min(team_stats$year), max(team_stats$year)), sep = ""),
        helpText("Adjust the slider to select a year range for comparing teams."),
        
        checkboxGroupInput("teams", "Select Teams to Display:",
                           choices = unique(team_stats$team),
                           selected = unique(team_stats$team)), # All teams selected by default
        helpText("Choose which teams to display in the comparison."),
        
        actionButton("selectAll", "Select All"),
        actionButton("unselectAll", "Unselect All")
      ),
      
      # Inputs for Brushing Plot
      conditionalPanel(
        condition = "input.visualization == 'Points Scored/Points Allowed'",
        sliderInput("brushing_years", "Select Year Range:",
                    min = min(team_stats$year), max = max(team_stats$year),
                    value = c(min(team_stats$year), max(team_stats$year)), sep = ""),
        helpText("Use the slider to filter data by year for brushing.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization",
                 # Outputs for Single Team Performance
                 conditionalPanel(
                   condition = "input.visualization == 'Single Team Performance'",
                   plotOutput("teamPlot", height = "1000px")
                 ),
                 
                 # Outputs for Team Comparisons
                 conditionalPanel(
                   condition = "input.visualization == 'Team Comparisons'",
                   plotOutput("comparisonPlot", height = "1000px")
                 ),
                 
                 # Outputs for Brushing
                 conditionalPanel(
                   condition = "input.visualization == 'Points Scored/Points Allowed'",
                   plotOutput("brushingPlot", height = "1000px", brush = brushOpts(id = "plot_brush")),
                   DT::dataTableOutput("formatted_table")  # Add the table output here
                 )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on user inputs for Single Team Performance
  filtered_data <- reactive({
    team_stats %>%
      filter(team == input$team, year >= input$years[1], year <= input$years[2])
  })
  
  # Select All button functionality
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "teams", 
                             choices = unique(team_stats$team),  # Ensure to pass choices again
                             selected = unique(team_stats$team))  # Select all teams
  })
  
  # Unselect All button functionality
  observeEvent(input$unselectAll, {
    updateCheckboxGroupInput(session, "teams", 
                             choices = unique(team_stats$team),  # Pass choices again
                             selected = NULL)  # Unselect all teams
  })
  
  metric_labels <- c(
    "win_loss_perc" = "Win Percentage",
    "points" = "Points Scored",
    "points_opp" = "Points Allowed",
    "rush_yds_per_att" = "Rushing Yards per Attempt",
    "penalties" = "Penalties"
  )
  
  output$teamPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = year, y = .data[[input$metric]])) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      labs(
        title = paste("Performance of", input$team, "over the Years"),
        x = "Year",
        y = metric_labels[[input$metric]]  # Use the mapping for properly formatted metric labels
      ) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render comparison plot for Team Comparisons
  output$comparisonPlot <- renderPlot({
    comparison_data <- team_stats %>%
      filter(year >= input$comparison_years[1], year <= input$comparison_years[2], 
             team %in% input$teams) # Filter by selected teams
    
    if (input$comparisonMetric == "Win Percentage") {
      ggplot(comparison_data, aes(x = year, y = win_loss_perc, color = team)) +
        geom_line(size = 1) +
        labs(title = "Win Percentage Comparison",
             x = "Year",
             y = "Win Percentage",
             color="Team")
      
    } else if (input$comparisonMetric == "Points Scored vs. Allowed") {
      ggplot(comparison_data, aes(x = points_opp, y = points, color = team)) +
        geom_point(size = 3) +
        labs(title = "Points Scored vs Points Allowed",
             x = "Points Allowed",
             y = "Points Scored",
             color="Team")
      
    } else if (input$comparisonMetric == "Margin of Victory") {
      ggplot(comparison_data, aes(x = team, y = mov)) +
        geom_boxplot() +
        labs(title = "Margin of Victory (MOV) Comparison",
             x = "Team",
             y = "Margin of Victory") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    } else if (input$comparisonMetric == "Rushing Yards per Attempt") {
      ggplot(comparison_data, aes(x = team, y = rush_yds_per_att)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Rushing Yards per Attempt Comparison",
             x = "Team",
             y = "Rushing Yards per Attempt") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    } else if (input$comparisonMetric == "Penalties") {
      ggplot(comparison_data, aes(x = year, y = penalties_yds, color = team)) +
        geom_line() +
        labs(title = "Penalties Yards Over Time",
             x = "Year",
             y = "Penalty Yards")
      
    } else if (input$comparisonMetric == "Turnover Percentage") {
      ggplot(comparison_data, aes(x = year, y = turnover_pct, color = team)) +
        geom_line() +
        labs(title = "Turnover Percentage Over Time",
             x = "Year",
             y = "Turnover Percentage",
             color="Team")
      
    } else if (input$comparisonMetric == "Expected Points") {
      ggplot(comparison_data, aes(x = team, y = exp_pts_tot)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        labs(title = "Expected Points Total Comparison",
             x = "Team",
             y = "Expected Points") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
  })
  
  output$brushingPlot <- renderPlot({
    brushed_data <- team_stats %>%
      filter(year >= input$brushing_years[1], year <= input$brushing_years[2])
    
    ggplot(brushed_data, aes(x = points_opp, y = points, color = team)) +
      geom_point(size = 3) +
      labs(title = "Interactive Points Scored vs Points Allowed (Brushable)",
           x = "Points Allowed",
           y = "Points Scored",
           color = "Team")
  })
  
  # Render formatted table for brushed data
  output$formatted_table <- DT::renderDataTable({
    # Filter and select only the 10 most important columns
    brushed_points <- brushedPoints(team_stats, input$plot_brush) %>%
      select(year, team, wins, losses, win_loss_perc, points, points_opp, points_diff, rush_yds_per_att, turnover_pct)
    
    # Rename columns for proper capitalization and spacing
    colnames(brushed_points) <- c("Year", "Team", "Wins", "Losses", "Win Percentage", 
                                  "Points Scored", "Points Allowed", "Points Differential", 
                                  "Rushing Yards per Attempt", "Turnover Percentage")
    
    # Format the table
    datatable(brushed_points, 
              options = list(
                pageLength = 10,
                autoWidth = TRUE, 
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE) %>%
      formatPercentage(columns = "Win Percentage", digits = 2) %>%
      formatRound(columns = "Turnover Percentage", digits = 2) %>% 
      formatStyle('Points Differential', color = styleInterval(c(0), c('red', 'green')))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
