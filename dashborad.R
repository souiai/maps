library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(readxl)
library(RColorBrewer)

data1 <- read_excel("C:/Users/HP/Downloads/Recombinants_country (1).xlsx")

# UI
ui <- fluidPage(
  titlePanel("Distribution of Variants by Country"),
  sidebarLayout(
    sidebarPanel(
      # Add legend in the sidebar
      verbatimTextOutput("legend")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    # Group data by country
    country_counts <- data1 %>%
      group_by(Country, Variant) %>%
      summarise(count = n()) %>%
      group_by(Country) %>%
      mutate(percent = count / sum(count))
    
    # Get the coordinates for each country
    country_coords <- map_data("world") %>%
      group_by(region) %>%
      summarise(long = mean(long), lat = mean(lat))
    
    # Merge the data with coordinates
    country_data <- inner_join(country_counts, country_coords, by = c("Country" = "region"))
    
    # Create the pie charts on the map
    m <- leaflet(country_data) %>%
      addProviderTiles("OpenStreetMap", options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    for (i in seq_along(country_data$Country)) {
      country <- country_data$Country[i]
      variant <- country_data$Variant[i]
      count <- country_data$count[i]
      long <- country_data$long[i]
      lat <- country_data$lat[i]
      
      chart <- ggplot(data = country_data %>% filter(Country == country), aes(x = "", y = percent, fill = Variant)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Dark2")  # Use the "Dark2" color palette
      
      chart_file <- paste0(country, ".png")
      ggsave(chart_file, chart, width = 3, height = 3, dpi = 300)
      
      m <- m %>% addMarkers(
        lng = long, lat = lat,
        icon = makeIcon(
          iconUrl = chart_file,
          iconWidth = 30, iconHeight = 30
        ),
        label = paste0(country, "<br>Count: ", count)
      )
    }
    # Define the legend colors
    legend_colors <- unique(country_data$Variant)
    legend_palette <- colorFactor(palette = "Dark2", domain = legend_colors)
    
    # Add a legend with the same colors as on the map
    m <- m %>% addLegend(
      pal = legend_palette,
      values = ~Variant,
      title = "Legend"
    )
    
    m
    m
  })
  
  # Define the legend text
  output$legend <- renderPrint({
    country_counts <- data1 %>%
      group_by(Country, Variant) %>%
      summarise(count = n()) %>%
      group_by(Country) %>%
      mutate(percent = count / sum(count))
    
    paste("Legend:",
          paste(paste(country_counts$Variant, ":",
                      scales::percent(country_counts$percent),
                      sep = " "), collapse = ", "))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
