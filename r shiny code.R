library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("U.S. Chronic Disease Indicators (CDI) Dashboard"),
  
  # Survey and dataset description
  fluidRow(
    column(12,
           h3("Survey and Dataset Description"),
           p("This dataset is based on indicators described in MMWR 'Indicators for Chronic Disease Surveillance — United States, 2013' before 2024 CDI refresh. It provides a cross-cutting set of 124 indicators that allow states and territories to uniformly define, collect, and report chronic disease data that are important to public health practice and available for states and territories."),
           p("For more information, visit the CDC Chronic Disease Indicators (CDI) website:"),
           a("CDC CDI Website", href = "https://www.cdc.gov/cdi/overview.html", target="_blank")
    )
  ),
  
  # States participated in the survey
  fluidRow(
    column(12,
           h3("States Participated in the Survey"),
           verbatimTextOutput("location_desc_list")
    )
  ),
  
  # Most counted topic bar chart
  fluidRow(
    column(12,
           h3("Most Important Topics"),
           plotOutput("bar_chart")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load the necessary library
  library(readr)
  library(dplyr)
  library(ggplot2)
  
  # Set the path to your CSV file
  file_path <- "C:/Users/ns14555/Downloads/chronic_disease_data.CSV"
  
  # Load the CSV file into a data frame
  data <- read_csv(file_path)
  
  # States participated in the survey
  output$location_desc_list <- renderPrint({
    distinct(data, LocationDesc) %>% 
      pull(LocationDesc) %>% 
      paste(collapse = ", ")
  })
  
  # Most common topic
  # Count the values in the 'Topic' column
  topic_counts <- data %>%
    count(Topic) %>%
    arrange(desc(n))
  
  # Render the bar chart
  output$bar_chart <- renderPlot({
    ggplot(topic_counts, aes(x = reorder(Topic, -n), y = n, fill = Topic)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Most Important Topics",
           x = "Topic",
           y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +  # Use a nice color palette
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })
}

# Run the application
shinyApp(ui = ui, server = server)
