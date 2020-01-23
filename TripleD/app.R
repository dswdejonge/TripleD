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

# Get coastal outlines
reg <- ggplot2::map_data("world2Hires")
reg <- reg %>%
  dplyr::filter(
    region == "UK" |
    region == "Netherlands" |
    region == "Belgium" |
    region == "France" |
    region == "Denmark" |
    region == "Norway" |
    region == "Germany")

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
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "taxonomic_level",
          label = h3("Select a taxonomic group:"),
          choices = as.list(
            c("all", unique(database$phylum))),
          selected = 1)
      ),
      mainPanel(
        plotOutput("North_Sea_map")
      )
    )
  ),

  # ------------------------------------
  # Page with database summary
  # ------------------------------------
  tabPanel(
    "Summary",

    h1("Factsheet"),
    p(paste0("This database contains ",dim(database)[1]," entries, collected over
             ",length(unique(database$StationID))," sample stations,
             during ",length(unique(database$CruiseID))," cruises.")),
    p(paste0("The oldest sample was taken on ",min(database$Date),
             " and the most recent sample was taken on ",max(database$Date),".")),
    p(paste0("Total suface area that has been sampled with the TripleD is ",
             sum(stations_final$Sample_area_m2), "m2 and the total volume of sediment
             sampled is ", sum(stations_final$Sample_volume_m3), "m3.")),
    p(paste0("In total ", sum(as.integer(database$Count)), " specimens have been counted from ",
             length(unique(database$valid_name))," taxa.")),
    hr(),

    h1("Missing data"),
    p("Not all raw data attributes are mandatory,
      which means optional fields might not always be filled.
      These graphs show the percentage of entries per attribute that are
      provided in the raw data."),
    radioButtons(
      "missing_data",
      label = "Show missing data for:",
      choices = list("Stations", "Species")),
    plotOutput("missingDataPlot"),

    h1("Water depth accuracy"),
    p("In order to check the accuracy of water depth derived from bathymetry at the track midpoint,
      it is compared to water depth measured onboard if this data exists."),
    plotOutput("waterDepthAccuracy")
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
           img(src = "Diagram_TripleD.png", height = 400))
)

# ---------------------------------------
# Server, i..e R-code that renders output
# ---------------------------------------
server <- function(input, output) {

  # --------------
  # North Sea map
  # --------------
  output$North_Sea_map <- renderPlot({
    # Subset data based on input
    if(input$taxonomic_level == "all"){
      my_data <- database %>%
        group_by(StationID, Lat_DD, Lon_DD) %>%
        summarise(Density = sum(Density_nr_per_m2))
    }else{
      my_data <- database %>%
        filter(phylum == input$taxonomic_level) %>%
        rename(Density = Density_nr_per_m2)
    }
    # make plot
    ggplot() +
      # add 20m contour
      geom_contour(data = bathymetry,
                   aes(x = x, y = y, z = z),
                   breaks = c(-20),
                   size = c(0.3),
                   colour = "grey") +
      # add 50m contour
      geom_contour(data = bathymetry,
                   aes(x = x, y = y, z = z),
                   breaks = c(-50),
                   size = c(0.3),
                   colour ="grey") +
      # add 100m contour
      geom_contour(data = bathymetry,
                   aes(x = x, y = y, z = z),
                   breaks = c(-100),
                   size = c(0.3),
                   colour = "grey") +
      # add coastline
      geom_polygon(data = reg, aes(x = long, y = lat, group = group),
                   fill = "darkgrey", color = NA) +
      # add points
      geom_point(data = my_data, aes(x = Lon_DD, y = Lat_DD, colour = Density),
                 size = 2) +
      # configure projection and plot domain
      coord_map(xlim = lons, ylim = lats) +
      scale_colour_gradientn(colours = rainbow(4)) +
      # formatting
      ylab("") + xlab("")+
      theme_bw()
  })

  # ------------
  # Summary page
  # ------------
  # Missing data plot
  output$missingDataPlot <- renderPlot({
    # stations
    stations_perc_complete <- stations %>%
      sapply(.,
             function(x){sum(length(which(!is.na(x))))/dim(stations)[1]}
      ) %>%
      sort(., decreasing = T) %>%
      as.data.frame() %>%
      as_tibble(rownames = "Attribute")
    colnames(stations_perc_complete)[2] <- "Percentage"
    stations_perc_complete$Attribute <- factor(
      stations_perc_complete$Attribute, levels = stations_perc_complete$Attribute)
    # species
    species_perc_complete <- species %>%
      sapply(.,
             function(x){sum(length(which(!is.na(x))))/dim(species)[1]}
      ) %>%
      sort(., decreasing = T) %>%
      as.data.frame() %>%
      as_tibble(rownames = "Attribute")
    colnames(species_perc_complete)[2] <- "Percentage"
    species_perc_complete$Attribute <- factor(
      species_perc_complete$Attribute, levels = species_perc_complete$Attribute)
    # plot
    if(input$missing_data == "Stations"){
      my_data <- stations_perc_complete
    }else if(input$missing_data == "Species"){
      my_data <- species_perc_complete
    }
    ggplot(my_data) +
      geom_col(aes(x = Attribute, y = Percentage)) +
      labs(title = input$missing_data, y = "% complete i.e. not NA") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })

  # Water depth accuracy
  output$waterDepthAccuracy <- renderPlot({
    differences <- na.omit(abs(stations_additions$Water_depth_m_cruise - stations_additions$Water_depth_m_Bathy))
    avg_diff <- sum(differences) / length(differences)
    ggplot(stations_additions,
           aes(x = Water_depth_m_cruise, y = Water_depth_m_Bathy)) +
      geom_point() +
      geom_abline() +
      annotate(
        "text",
        x = 50, y = 175,
        label = paste0("average difference = ", round(avg_diff, digits = 1)," m.")) +
      labs(
        title = "Difference between observed and bathymetry water depth",
        x = "Water depth measured on site (m)",
        y = "Water depth from NOAA bathymetry resolution 1' (m)") +
      xlim(0, max(c(stations_additions$Water_depth_m, stations_additions$Water_depth_m_Bathy), na.rm = T)) +
      ylim(0, max(c(stations_additions$Water_depth_m, stations_additions$Water_depth_m_Bathy), na.rm = T)) +
      coord_fixed() +
      theme_bw()
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

