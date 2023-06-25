#
# PRA2 - Visualització de dades - UOC
# ***********************************
#
# Autor: Albert Salvador Yuste
# Última data actualització: 25/06/2023
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
if (!require(shinyjs)) install.packages("shinyjs", dependencies = T); library (shinyjs)
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

df <- df %>%
  mutate(host_is_superhost = case_when(
    host_is_superhost == F ~ 'Host',
    T ~ 'Super Host')) %>%
  mutate(room_type = case_when(
    room_type == "Private room" ~ "Habitació privada",
    room_type == "Shared room" ~ "Habitació compartida",
    T ~ "Casa - Apartament"
  ))

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
  dashboardHeader(title = "UOC"),
  dashboardSidebar(
    useShinyjs(),
    selectInput("ciutat",
                "Ciutat:",
                c("Amsterdam" = "amsterdam",
                  "Atenes" = "athens",
                  "Barcelona" = "barcelona",
                  "Berlín" = "berlin",
                  "Budapest" = "budapest",
                  "Lisboa" = "lisbon",
                  "Londres" = "london",
                  "París" = "paris",
                  "Roma" = "rome",
                  "Viena" = "vienna")),
    selectInput("variable_representar",
                "Variable:",
                c("Preu" = "realSum", 
                  "Aforament" = "person_capacity", 
                  "Distància al centre" = "dist",
                  "Distància al metro més proper" = "metro_dist",
                  "Índex de punts turístics propers" = "attr_index_norm",
                  "Índex de punts de restauració propers" = "rest_index_norm",
                  "Tipus d'immoble" = "room_type",
                  "Superhost" = "host_is_superhost",
                  "Puntuació neteja" = "cleanliness_rating",
                  "Puntuació global" = "guest_satisfaction_overall")),
    
    selectInput("weekend",
                "Laborables / Cap de setmana:",
                c("Tots","Laborables", "Cap de setmana")),
    checkboxInput("metro", "Estacions de metro", value = FALSE),
    actionButton("reset", "Reiniciar mapa")
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").
        append(\'<span class="myClass"> PRA2 - Visualització de Dades - Albert Salvador </span>\');
      })
     ')),
    
    h1(em("Anuncis d'Airbnb al llarg de diferents ciutats Europees"), align = "center"),
    h5(em("Informació derivada a través d'anuncis publicats a Airbnb"), align = "center"),
    fluidRow(
      box(id = "mapa_box",
          title = "Representació geogràfica",
          leafletOutput("leaflet_var") %>%
            withSpinner(color="#0dc5c1"),
          width = 12),
      box(id = "num_anuncis_box",
          title = "Nombre d'anuncis per ciutat",
          plotlyOutput("num_anuncis")  %>%
            withSpinner(color="#0dc5c1"),
          width = 12),
      box(id = "distribution_box",
          title = "Variació del preu",
          plotlyOutput("distribution")  %>%
            withSpinner(color="#0dc5c1"),
          width = 6),
      box(id = "piechart_box",
          title = "Distribució",
          plotlyOutput("piechart")  %>%
            withSpinner(color="#0dc5c1"),
          width = 6)
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
    
    shinyjs::hide(id = "distribution_box")
    shinyjs::hide(id = "piechart_box")
    shinyjs::show(id = "num_anuncis_box")
    
    output$leaflet_var <- renderLeaflet(
      leaflet() %>% 
        addTiles() %>% setView(lat = 47.656501, 
                               lng = 7.634668,
                               zoom = 4) %>%
        addMarkers(data = cities)
    )
    
    output$num_anuncis <- renderPlotly({
      p <- ggplot(df_airbnb() %>% 
                    mutate(city = str_to_title(city))) +
        geom_bar(aes(x=reorder(city,city,function(x)length(x)),
                     fill = city)) +
        xlab("Ciutat") + ylab("Nombre anuncis") +
        theme_linedraw() +
        theme(text = element_text(size = 20),
              legend.position = "none")
      
      ggplotly(p)
    })
  }
  
  # Creem el mapa al inici
  leaflet_reset()
  
  # Reiniciem el mapa al clicar el botó
  observeEvent(input$reset,{
    leaflet_reset()
  })
  
  toListen <- reactive({
    list(input$ciutat,
         input$variable_representar,
         input$weekend)
  })
  
  observeEvent(toListen(),{
    
    shinyjs::hide(id = "num_anuncis_box")
    
    output$leaflet_var <- renderLeaflet({
      
      df_airbnb <- df_airbnb() %>%
        filter(city == input$ciutat)
      
      # Extraïem un les coordenades per definir un (potencial) raster
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
      
      if (input$variable_representar == "room_type") {
        
        roomicon <- makeIcon(
          iconUrl = ifelse(df_airbnb$room_type == "Habitació privada",
                           "https://static.thenounproject.com/png/1047372-200.png", #Habitació privada
                           ifelse(df_airbnb$room_type == "Casa - Apartament",
                                  "https://cdn-icons-png.flaticon.com/512/25/25694.png", #Casa
                                  "https://static.thenounproject.com/png/1190848-200.png")), #Habitació compartida
          iconWidth = 30, iconHeight = 30,
          iconAnchorX = 15, iconAnchorY = 15)
        
        html_legend <-
        "<img src='https://static.thenounproject.com/png/1047372-200.png'
        style='width:30px;height:30px;'>Habitació privada<br/>
        <img src='https://static.thenounproject.com/png/1190848-200.png' 
        style='width:30px;height:30px;'>Habitació compartida<br/>
        <img src='https://cdn-icons-png.flaticon.com/512/25/25694.png' 
        style='width:30px;height:30px;'>Casa - Apartament"
        
        p <- leaflet() %>%
          addTiles() %>%
          addMarkers(data = df_airbnb,
                     icon = roomicon) %>%
          addControl(html = html_legend, position = "topright")
        
      } else if (input$variable_representar == "host_is_superhost"){
        hosticon <- makeIcon(
          iconUrl = ifelse(df_airbnb$host_is_superhost == "Host",
                           "https://cdn-icons-png.flaticon.com/512/1654/1654220.png", #Host
                           "https://seeklogo.com/images/A/airbnb-superhost-logo-1E4451535F-seeklogo.com.png"), #Super Host
          iconWidth = 30, iconHeight = 30,
          iconAnchorX = 15, iconAnchorY = 15)
        
        html_legend <-
          "<img src='https://cdn-icons-png.flaticon.com/512/1654/1654220.png'
        style='width:30px;height:30px;'>Host<br/>
        <img src='https://seeklogo.com/images/A/airbnb-superhost-logo-1E4451535F-seeklogo.com.png' 
        style='width:30px;height:30px;'>Super Host"
        
        p <- leaflet() %>%
          addTiles() %>%
          addMarkers(data = df_airbnb,
                     icon = hosticon) %>%
          addControl(html = html_legend, position = "topright")
      } else {
        # Interpolem un mapa per la ciutat i variable indicada
        
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
        
        legend_title <- switch(input$variable_representar,
                               "dist" = "Distància al centre (km)",
                               "realSum" = "Preu per nit (€)",
                               "person_capacity" = "Aforament",
                               "metro_dist" = "Distància al metro més proper (km)",
                               "attr_index_norm" = "Índex de punts turístics propers",
                               "rest_index_norm" = "Índex de punts de restauració propers",
                               "room_type" = "Tipus d'immoble",
                               "host_is_superhost" = "Superhost",
                               "weekend" = "Laborable / Cap de setmana",
                               "cleanliness_rating" = "Puntuació neteja",
                               "guest_satisfaction_overall" = "Puntuació global")
        
        # Mostrem el mapa
        p <- leaflet() %>% 
          addTiles() %>%
          addRasterImage(x = df.ordkg %>% raster(),
                         col = brewer.pal(5, "Spectral"),
                         opacity = 0.7) %>%
          addLegend(colors = brewer.pal(5, "Spectral"),
                    labels = valors_raster %>% round(2),
                    title = legend_title)
      }
      
      
      
      if (input$metro) { # Seleccionem representar les estacions de metro
        
        if (input$ciutat != "athens"){ # No tenim dades d'atenes
          
          undergroundIcon <- makeIcon(
            iconUrl = "https://upload.wikimedia.org/wikipedia/commons/4/41/Underground.svg",
            iconWidth = 30, iconHeight = 80,
            iconAnchorX = 15, iconAnchorY = 40
          )
          
          stations_crop <- raster::crop(stations_sp, df_coords)
          
          p %>% addMarkers(data = stations_crop,
                           icon = undergroundIcon)
        } else {
          p
        }
      } else {
        p
      }
    })
    
    shinyjs::show(id = "distribution_box")
    shinyjs::show(id = "piechart_box")
    
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
               "rest_index_norm" = c(0, 10, 20, 30, 40, 50, 100),
               "guest_satisfaction_overall" = c(0, 50, 60, 70, 80, 90, 100),
               "cleanliness_rating" = c(0, 5, 6, 7, 8, 9, 10))
      
      x_lab <- switch(var_representar,
                      "dist" = "Distància al centre (km)",
                      "realSum" = "Preu per nit (€)",
                      "person_capacity" = "Aforament",
                      "metro_dist" = "Distància al metro més proper (km)",
                      "attr_index_norm" = "Índex de punts turístics propers",
                      "rest_index_norm" = "Índex de punts de restauració propers",
                      "room_type" = "Tipus d'immoble",
                      "host_is_superhost" = "Superhost",
                      "weekend" = "Laborable / Cap de setmana",
                      "cleanliness_rating" = "Puntuació neteja",
                      "guest_satisfaction_overall" = "Puntuació global")
      
      # Amaguem el gràfic si es representa el preu, doncs no ens interessa
      if (var_representar == "realSum"){
        shinyjs::hide(id = "distribution_box")
      } else {
        shinyjs::show(id = "distribution_box")
      }
      
      if (var_representar %in% c("dist", "realSum", "person_capacity", "metro_dist",
                                 "attr_index_norm", "rest_index_norm",
                                 "cleanliness_rating", "guest_satisfaction_overall")){
        df_plot <- df_airbnb %>% 
          mutate(var_bin = cut(!!as.symbol(var_representar),
                               cut_list))
      } else {
        df_plot <- df_airbnb %>%
          mutate(var_bin = !!as.symbol(var_representar))
      }
      
      p <- df_plot %>%
        ggplot()
      
      if (var_representar %in% c("dist", "realSum", "person_capacity", "metro_dist",
                                 "attr_index_norm", "rest_index_norm",
                                 "cleanliness_rating", "guest_satisfaction_overall")){
        p <- p +
          geom_boxplot(aes(x = var_bin,
                           y = realSum,
                           fill = var_bin),
                       outlier.shape = NA,
                       varwidth = T)
      } else {
        p <- p + 
          geom_boxplot(aes(x = reorder(!!as.symbol(var_representar), realSum),
                           y = realSum,
                           fill = !!as.symbol(var_representar)),
                       outlier.shape = NA)
      }
      
      p <- p +
        scale_y_continuous(limits = quantile(df_plot$realSum, c(0.1, 0.9))) +
        xlab(x_lab) + ylab("Preu per nit (€)") +
        theme_linedraw() +
        theme(text = element_text(size = 20),
              legend.position = "none")
      
      ggplotly(p)
        
    })
    
    # Representem el piechart
    output$piechart <- renderPlotly({
      
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
               "rest_index_norm" = c(0, 10, 20, 30, 40, 50, 100),
               "guest_satisfaction_overall" = c(0, 50, 60, 70, 80, 90, 100),
               "cleanliness_rating" = c(0, 5, 6, 7, 8, 9, 10))
      
      if (var_representar %in% c("dist", "realSum", "person_capacity", "metro_dist",
                                 "attr_index_norm", "rest_index_norm",
                                 "cleanliness_rating", "guest_satisfaction_overall")){
        df_plot <- df_airbnb %>% 
          mutate(var_bin = cut(!!as.symbol(var_representar),
                               cut_list))
        df_pie <- tibble(var_bin = sort(levels(df_plot$var_bin)))
      } else {
        df_plot <- df_airbnb %>%
          mutate(var_bin = !!as.symbol(var_representar))
        df_pie <- tibble(var_bin = sort(unique(df_plot$var_bin)))
      }
      
      df_pie$frequency <- table(df_plot$var_bin)
      
      df_pie <- df_pie %>%
        arrange(desc(var_bin)) %>%
        mutate(prop = frequency / sum(df_pie$frequency) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop )
      
      plot_ly(data = df_pie, values = ~frequency,
              labels = ~var_bin, type = "pie",
              textinfo = 'label+percent',
              hole = 0.3)
    })
  }, ignoreInit = T)
}

# Run the application 
shinyApp(ui = ui, server = server)
