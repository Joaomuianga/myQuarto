library(shiny)
library(DT)
library(bslib)
library(ggplot2)

# Function to generate the sample data
generate_data <- function() {
  set.seed(123)  # For reproducibility
  
  regions <- c("AFRICA", "AMERICA", "ASIA", "EUROPE")
  years <- 2000:2020
  data <- expand.grid(region = regions, year = years)
  
  data$doses <- sample(100000:1000000, nrow(data), replace = TRUE)
  data$population <- sample(1000000:10000000, nrow(data), replace = TRUE)
  data$coverage <- (data$doses / data$population) * 100
  
  return(data)
}

# Generate the data
data <- generate_data()

# Define UI for application
ui <- page_sidebar(
  title = "Vaccination Coverage Data",
  
  sidebar = sidebarPanel(
    width = 30,
    selectInput("region", "Select Region:", 
                choices = c("All", "AFRICA", "AMERICA", "ASIA", "EUROPE"), 
                selected = "All",
                multiple = FALSE),
    sliderInput("yearRange", "Select Year Range:", 
                min = 2000, max = 2020, 
                value = c(2000, 2020), step = 1)
  ),
  
  navbarPage(
  title = "Vaccination Coverage Data",
  
    # Example of a tab with a plot (additional feature)
  tabPanel(
    "Plot",
    h3("Coverage Plot"),
    plotOutput("coveragePlot")
  )
  )
)
# Define server logic required to display the table and other components
server <- function(input, output) {
  # Reactive expression to filter data based on selected region and year range
  filtered_data <- reactive({
    data_filtered <- data
    
    if (input$region != "All") {
      data_filtered <- subset(data_filtered, region == input$region)
    }
    
    data_filtered <- subset(data_filtered, year >= input$yearRange[1] & year <= input$yearRange[2])
    
    return(data_filtered)
  })

  # Render a plot of coverage
  output$coveragePlot <- renderPlot({
    library(ggplot2)
    ggplot(filtered_data(), aes(x = year, y = coverage, color = region)) +
      geom_line() +
      geom_point()+
      labs(
        title = "Vaccination Coverage Over Time",
        x = "Year",
        y = "Coverage (%)"
      ) +
       scale_color_manual(name = "Region",
                         values = c("AFRICA" = "#E88D67",
                                    "AMERICA" = "#DA7297",
                                    "ASIA" = "#006989",
                                    "EUROPE" ="#729762"))+
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
