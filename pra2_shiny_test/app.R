#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Llibreries ####
if (!require(tidyverse)) install.packages("tidyverse", dependencies = T); library (tidyverse)
if (!require(leaflet)) install.packages("leaflet", dependencies = T); library (leaflet)
if (!require(sf)) install.packages("sf", dependencies = T); library (sf)
if (!require(gstat)) install.packages("gstat", dependencies = T); library (gstat)
if (!require(raster)) install.packages("raster", dependencies = T); library (raster)
if (!require(sp)) install.packages("sp", dependencies = T); library (sp)
if (!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = T); library (RColorBrewer)
if (!require(shiny)) install.packages("shiny", dependencies = T); library (shiny)
if (!require(shinydashboard)) install.packages("shinydashboard", dependencies = T); library (shinydashboard)
if (!require(shinycssloaders)) install.packages("shinycssloaders", dependencies = T); library (shinycssloaders)
if (!require(plotly)) install.packages("plotly", dependencies = T); library (plotly)

#### Carreguem arxius i els tractem ####

# Airbnb info #

airbnb_dir <- "datasets/airbnb"
list_datasets_airbnb <- list.files(airbnb_dir, pattern = ".csv")

df <- tibble()

for (dataset in list_datasets_airbnb){
  df_temp <- read_csv(paste(airbnb_dir,
                            dataset, sep = "/"))
  df_temp <- df_temp %>%
    mutate(city = str_split(dataset, "_")[[1]][1]) %>%
    mutate(weekend = str_split(str_split(dataset, "_")[[1]][2],
                               "[.]")[[1]][1])
  
  df <- rbind(df, df_temp)
  rm(df_temp)
}

df_unique <- df %>%
  group_by(lat, lng)  %>%
  mutate_at(.vars = vars(realSum, person_capacity, cleanliness_rating, guest_satisfaction_overall,
                         bedrooms, dist, metro_dist, attr_index, attr_index_norm, rest_index,
                         rest_index_norm),
            .funs = mean) %>%
  ungroup() %>%
  distinct(lat, lng, .keep_all = T)

df_weekdays <- df %>%
  filter(weekend == "weekdays") %>%
  distinct(lat, lng, .keep_all = T)

df_weekends <- df %>%
  filter(weekend == "weekends") %>%
  distinct(lat, lng, .keep_all = T)

# Metro info #

underground_dir <- "datasets/underground"

# Llegim dataframe de les ciutats
cities <- read_csv(paste(underground_dir,"cities.csv",sep = "/"))
cities <- cities %>%
  filter(url_name %in% df$city) %>%
  dplyr:::select(id, coords, name = url_name) %>%
  st_as_sf(wkt = "coords", crs = 4326)


# Seleccionem les estacions de les ciutats que tenim
stations <- read_csv(paste(underground_dir,"stations.csv",sep = "/"))
stations <- stations %>%
  filter(city_id %in% cities$id) %>%
  dplyr:::select(id, name, coords = geometry, city_id) %>%
  left_join(data.frame(id = cities$id, city = cities$name),
            by = c("city_id" = "id")) %>%
  st_as_sf(wkt = "coords", crs = 4326)

stations_sp  <- st_coordinates(stations) %>%
  SpatialPoints()

#### UI Part ####

ui <- dashboardPage(
  dashboardHeader(title = "PRA2 - Visualització de Dades (UOC) - Albert Salvador"),
  dashboardSidebar(
    
    selectInput("ciutat",
                "Ciutat:",
                unique(df$city)),
    selectInput("variable_representar",
                "Variable:",
                c("realSum", "person_capacity", "dist",
                  "metro_dist", "attr_index_norm", "rest_index_norm")),
    selectInput("weekend",
                "Laborables / Cap de setmana:",
                c("Tots","Laborables", "Cap de setmana")),
    checkboxInput("metro", "Estacions de metro", value = FALSE),
    actionButton("reset", "Reiniciar mapa")
    
  ),
  dashboardBody(
    
    fluidRow(
      box(title = "Mapa amb la ciutat i variable indicades",
          leafletOutput("leaflet_var") %>%
            withSpinner(color="#0dc5c1"),
          width = 12
          ),
      box(title = "Preu segons les diferents variables",
          plotlyOutput("distribution")  %>%
            withSpinner(color="#0dc5c1"),
          width = 6
          ),
      box(title = "Distribució segons variable",
          plotlyOutput("piechart")  %>%
            withSpinner(color="#0dc5c1"),
          selectInput("var_pie",
                      "Variable:",
                      c("room_type", "person_capacity", "host_is_superhost")),
          width = 6
      )
      )
    )
)

#### SERVER Part ####

server <- function(input, output) {
  
  # Adaptem el dataset segons el periode a observar
  df_airbnb <- reactive({
    if (input$weekend == "Tots"){
      df_unique
    } else if (input$weekend == "Laborables") {
      df_weekdays
    } else {
      df_weekends
    }
  })
  
  # Creem una funció de mapa inicial
  leaflet_reset <- function(){ 
    output$leaflet_var <- renderLeaflet(
      leaflet() %>% 
        addTiles() %>% setView(lat = 47.656501, 
                               lng = 7.634668,
                               zoom = 4)
    )
  }
  
  # Creem el mapa al inici
  leaflet_reset()
  
  # Reiniciem el mapa al clicar el botó
  observeEvent(input$reset,{
    leaflet_reset()
  })
  
  
  output$distribution <- output$piechart <- renderPlotly({
    ggplotly(
      ggplot(data.frame()) +
        theme_minimal()
    )
  })
  
  toListen <- reactive({
    list(input$ciutat,
         input$variable_representar,
         input$weekend)
  })
  
  observeEvent(toListen(),{
    output$leaflet_var <- renderLeaflet({
      # Interpolem un mapa per la ciutat i variable indicada
      
      df_airbnb <- df_airbnb() %>%
        filter(city == input$ciutat)
      
      # df amb informació per coordenades
      df_airbnb_coords <- st_as_sf(df_airbnb,
                                   coords = c("lng", "lat"),
                                   crs = 4326)
      # Interpolació KRIGEAJE
      gstat.parametros <- gstat(formula = get(input$variable_representar)~1,
                                locations = df_airbnb_coords)
      variograma.df <- variogram(gstat.parametros, width = 100)
      var.teorico <- fit.variogram(variograma.df,
                                   vgm(c("Exp", "Ste", "Sph", "Mat", "Gau", "Spl")))
      # Extraïem un les coordenades per definir un raster
      coords <- bbox(SpatialPoints(
        cbind(df_airbnb %>%
                dplyr:::select(lng, lat))))
      ext = extent(coords)
      # Es limita la resolució per afavorir rapidesa en la web
      rst_city <- raster(ext = ext,
                         res = c((ext[2]-ext[1])/30,
                                 (ext[4]-ext[3])/30))
      df_coords <- df_airbnb
      coordinates(df_coords) <- ~lng + lat
      proj4string(df_coords) <- CRS("+proj=longlat +datum=WGS84")
      
      df.ordkg <- krige(formula = get(input$variable_representar)~1,
                        locations = df_coords,
                        newdata = as(rst_city, "SpatialGrid"),
                        model = var.teorico, 
                        nmax = 100)
      
      # Obtenim els valors de la interpol·lació per definir els
      # colors  del raster i la llegenda
      valors_raster <- df.ordkg %>% raster() %>% values()
      valors_raster <- seq(min(valors_raster, na.rm = T),
                           max(valors_raster, na.rm = T),
                           length.out = 5)
      
      # Mostrem el mapa
      p <- leaflet() %>% 
        addTiles() %>%
        addRasterImage(x = df.ordkg %>% raster(),
                       col = brewer.pal(5, "Spectral"),
                       opacity = 0.7) %>%
        addLegend(colors = brewer.pal(5, "Spectral"),
                  labels = valors_raster %>% round(2),
                  title = input$variable_representar)
      
      if (input$metro) { # Seleccionem representar les estacions de metro
        
        if (input$ciutat != "athens"){ # No tenim dades d'atenes
          
          undergroundIcon <- makeIcon(
            iconUrl = "https://upload.wikimedia.org/wikipedia/commons/4/41/Underground.svg",
            iconWidth = 30, iconHeight = 80,
            iconAnchorX = 15, iconAnchorY = 40
          )
          
          stations_crop <- raster::crop(stations_sp, df.ordkg)
          
          p %>% addMarkers(data = stations_crop,
                           icon = undergroundIcon)
        } else {
          p
        }
      } else {
        p
      }
    })
    
    output$distribution <- renderPlotly({
      
      df_airbnb <- df_airbnb() %>%
        filter(city == input$ciutat)
      
      var_representar <- input$variable_representar
      
      cut_list <- 
        switch(var_representar,
               "dist" = c(0, 1, 2, 3, 4, 5, 10, Inf),
               "realSum" = c(0, 200, 400, 500, 600, 800, Inf),
               "person_capacity" = c(0, 2, 3, 4, 5, 6),
               "metro_dist" = c(0, 1, 2, 3, 4, 5, Inf),
               "attr_index_norm" = c(0, 10, 20, 30, 40, 50, 100),
               "rest_index_norm" = c(0, 10, 20, 30, 40, 50, 100))
      
      x_lab <- switch(var_representar,
                      "dist" = "Distància al centre (km)",
                      "realSum" = "Preu per nit (€)",
                      "person_capacity" = "Aforament",
                      "metro_dist" = "Distància al metro més proper (km)",
                      "attr_index_norm" = "Índex de punts turístics propers",
                      "rest_index_norm" = "Índex de punts de restauració propers")
      
      df_plot <- df_airbnb %>% 
        mutate(var_bin = cut(!!as.symbol(var_representar),
                             cut_list))
      p <- df_plot %>%
        ggplot() +
        geom_boxplot(aes(x = var_bin,
                         y = realSum,
                         fill = var_bin),
                     outlier.shape = NA,
                     varwidth = T) +
        scale_y_continuous(limits = quantile(df_plot$realSum, c(0.1, 0.9))) +
        xlab(x_lab) + ylab("Preu per nit (€)") +
        theme_linedraw() +
        theme(text = element_text(size = 20),
              legend.position = "none")
      
      ggplotly(p)
        
    })
  }, ignoreInit = T)
  
  observeEvent(toListen(),{
    output$piechart <- renderPlotly({
      
      df_airbnb <- df_airbnb() %>%
        filter(city == input$ciutat)
      
      if (input$var_pie == "person_capacity") {
        df_pie <- tibble(unique(round(df_airbnb[,input$var_pie])))
        df_pie$frequency <- table(round(df_airbnb[,input$var_pie]))
      } else {
        df_pie <- tibble(unique(df_airbnb[,input$var_pie]))
        df_pie$frequency <- table(df_airbnb[,input$var_pie])
      }
      
      df_pie <- df_pie %>%
        arrange(desc(get(input$var_pie))) %>%
        mutate(prop = frequency / sum(df_pie$frequency) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      plot_ly(data = df_pie, values = ~frequency,
              labels = ~get(input$var_pie), type = "pie",
              textinfo = 'label+percent',
              hole = 0.3)
    })
  }, ignoreInit = T)
}

# Run the application 
shinyApp(ui = ui, server = server)
