#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny)
library(shiny)
library(TripleD)

# Define UI for application that plots abundance, and summarizes some data.
ui <- navbarPage( # page with tabs to navigate to different pages
  "NIOZ TripleD Data",
  # Page with interactive North Sea map
  tabPanel("Map"),

  # Page with histograms
  tabPanel(
    "Histograms",
    sidebarLayout(
      sidebarPanel(
        selectInput("histogram_data", h3("Histogram of:"),
                    choices = list("Water depth" = "Water_depth_m", "Track length" = "Track_dist_m",
                                          "Taxon count" = "Count"), selected = 1)
      ),
      mainPanel(
        plotOutput("histoPlot")
      )
    )
  ),
  tabPanel(
    "Some plots",
    # Application title
    titlePanel("NIOZ TripleD Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins","Number of bins:", min = 5, max = 50, value = 30)
      ),

    # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  tabPanel("About",
           p("This interactive page is created by Danielle de Jonge."),
           img(src = "Diagram_TripleD.png"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Histogram plots
  output$histoPlot <- renderPlot({
    data_to_plot <- database[,input$histogram_data]
    hist(data_to_plot)
  })

  output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- database$Water_depth_m
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'orange')
   })
}

# Run the application
shinyApp(ui = ui, server = server)

