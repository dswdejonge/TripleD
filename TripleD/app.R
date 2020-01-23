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
library(ggplot2)
library(mapdata)
library(dplyr)

# -----------------------
# Define fixed parameters
# -----------------------
# North Sea boundaries lat and lon
lats <- c(50, 62)
lons <- c(-5, 10)


# ---------------
# User interface
# ---------------
ui <- navbarPage( # page with tabs to navigate to different pages
  "NIOZ TripleD Data",

  # ------------------------------------
  # Page with interactive North Sea map
  # ------------------------------------
  tabPanel(
    "Map",
    plotOutput("North_Sea_map")
  ),

  # --------------------
  # Page with histograms
  # --------------------
  tabPanel(
    "Histograms",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "histogram_data",
          label = h3("Histogram of:"),
          choices = list(
            "Water depth" = "Water_depth_m",
            "Track length" = "Track_length_m",
            "Taxon count" = "Count"),
          selected = 1)
      ),
      mainPanel(
        plotOutput("histoPlot")
      )
    )
  ),

  # -----------
  # About page
  # -----------
  tabPanel("About",
           p("This interactive page is created by Danielle de Jonge."),
           img(src = "Diagram_TripleD.png"))
)

# ---------------------------------------
# Server, i..e R-code that renders output
# ---------------------------------------
server <- function(input, output) {

  # --------------
  # North Sea map
  # --------------
  output$North_Sea_map <- renderPlot({
    # Get coastal outlines
    reg <- ggplot2::map_data("world2Hires")
    reg <- reg %>%
      dplyr::filter(
        long > lons[1] &
        long < lons[2] &
        lat > lats[1] &
        lat < lats[2]
      )
    # make plot
    map_samples <- ggplot() +
      # add coastline
      geom_polygon(data = reg, aes(x = long, y = lat, group = group),
                   fill = "darkgrey", color = NA) +
      # add points
      geom_point(data = stations_final, aes(x = Lon_DD, y = Lat_DD),
                 fill = "red", colour = "black",
                 size = 1, shape = 21) +
      # configure projection and plot domain
      coord_map(xlim = lons, ylim = lats) +
      # formatting
      ylab("") + xlab("")+
      theme_bw()
    map_samples
  })

  # --------------
  # Histogram plots
  # --------------
  output$histoPlot <- renderPlot({
    data_to_plot <- pull(database, input$histogram_data)
    hist(data_to_plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

