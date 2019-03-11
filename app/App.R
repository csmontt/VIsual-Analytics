
library(shinydashboard)
library(shiny)
library(rgdal)
library(leaflet)
library(dplyr)
library(maptools)
library(rgeos)
library(ggplot2)
library(ggExtra)
library(grid)
library(GGally)
library(broom)
library(car)
options(digits = 1)
options(shiny.sanitize.errors = TRUE)
options(scipen=999)

hex_spdf <- readRDS("./data/hexGrid.rds")
hex_spdf$gse.num <- 6 - hex_spdf$gse.num
hex_spdf$gse.cat <- as.factor(round(hex_spdf$gse.num))

hex_spdf <- hex_spdf[!is.na(hex_spdf@data$ID),]

hex_spdf$sector5 <- as.factor(hex_spdf$sector5 )

# Eliminate tildes
hex_spdf$comuna <- gsub("Á", "A", hex_spdf$comuna)
hex_spdf$comuna <- gsub("É", "E", hex_spdf$comuna)
hex_spdf$comuna <- gsub("Í", "I", hex_spdf$comuna)
hex_spdf$comuna <- gsub("Ó", "O", hex_spdf$comuna)
hex_spdf$comuna <- gsub("Ú", "U", hex_spdf$comuna)
hex_spdf$comuna <- gsub("Ñ", "N", hex_spdf$comuna)

# Create size of electorate for each round
hex_spdf$electorate_1 <- hex_spdf$total_population * hex_spdf$part
hex_spdf$electorate_2 <- hex_spdf$total_population * hex_spdf$part2

# Determine variables to map
choices_num_vars <- names(hex_spdf)[c(3:length(names(hex_spdf)))]
choices_num_vars <- c("residuals", choices_num_vars)

sector_polys <- unionSpatialPolygons(hex_spdf, ID=hex_spdf@data$sector5)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Chilean Elections 2017"),
  dashboardSidebar(selectInput(inputId = "variableselected", 
                  label = "Variable map",
                  choices = c(choices_num_vars)),
      selectInput("Borough", "Remove Boroughs", 
                   choices= sort(unique(hex_spdf$comuna)), multiple =TRUE),
      selectInput(inputId = 'xCol', label = 'X', 
                  choices = names(hex_spdf)[-1], multiple = TRUE,
                  selected = c("women_ratio", "rest_adults")),
      selectInput(inputId = 'yCol', label = 'Y', choices = names(hex_spdf)[-1],
                  selected = "right_perc_1"),
      selectInput(inputId = 'group', label = 'Color Points by', selected = "sector5",
                  choices = c("None", "sector5"))),
  dashboardBody(
           fluidRow(column(width = 12,
      box(width = 5, solidHeader = TRUE,
          title = "Map, VIF Scores and Model Summary",
        leafletOutput(outputId = "map",  height = 390),
        verbatimTextOutput(outputId = "vif_scores"),
        verbatimTextOutput(outputId = "RegSum"),
    tags$head(tags$style("#RegSum{color: black;
                                 font-size: 11px;
                                 font-style: calibri;
                                 }"
                         )
              )
      ) ,
      box(width = 7,
       plotOutput(outputId = "plot", height = 750),
       plotOutput(outputId = "regCoeff")
         )
       )
           )
)
)      

# server() ------------------------------------------------------------
server <- function(input, output){
  
    spdf <- reactive({
                datos <- hex_spdf[!(hex_spdf$comuna %in% input$Borough), ]
                return(datos)
    })      
    
    residuals_model <- reactive({
            resids_vect <- resid(lm(paste(input$yCol, " ~ ", paste(input$xCol, collapse = "+"), sep = ""), 
                                data = spdf()))
            return(resids_vect)
            
    })
  
  output$map <- renderLeaflet({
        hex_spdf <- spdf()
        residuals <- residuals_model()
        hex_spdf$residuals <- residuals
        if(input$variableselected == "residuals"){
                maxValue <- max(abs(min(hex_spdf@data[, input$variableselected])), 
                                max(hex_spdf@data[, input$variableselected]))
                variablePlot <- c(-maxValue, maxValue)
                pal <- colorNumeric(
                palette = "RdBu",
                domain = variablePlot)
        } else {
        variablePlot <- hex_spdf@data[, input$variableselected]
        pal <- colorNumeric(
                palette = "YlOrRd",
                domain = variablePlot)
        }
        
        labels <- sprintf("%g", hex_spdf@data[, input$variableselected]) %>% lapply(htmltools::HTML)
        labels_sector <- c("Center", "East", "North", "South", "West")
        
        leaflet() %>% addPolygons(data = sector_polys, fillColor = "transparent", opacity = 1,
                            stroke = TRUE, color = "black", weight = 1,
                            highlightOptions = highlightOptions(color = "black", 
                                                                weight = 2, 
                                                                bringToFront = FALSE),
                            label = labels_sector) %>%
                addPolygons(data = hex_spdf,
                            fillColor = ~pal(hex_spdf@data[, input$variableselected]),
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.8,
                            weight = 0.2,
                            label = labels) %>%
                            addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%

                addLegend(pal = pal, values = variablePlot, opacity = 0.7,
                         title = "Values", position = "bottomleft")
  })
  
  data_plots <- reactive({
                datos <- hex_spdf[!(hex_spdf$comuna %in% input$Borough), ]
                datos <- datos@data[, c(input$xCol, input$yCol, "sector5")]
                return(datos)
    })      
  
  output$plot <- renderPlot({ 
         data_plots <- data_plots()
         if(input$group == "sector5"){
         
         lowerFn <- function(data, mapping, method = "lm", ...) {
                p <- ggplot(data = data_plots, mapping = mapping) +
                     geom_point(aes(colour = data[, "sector5"])) +
                     geom_smooth(method = method, color = "red", ...) 
                p
         }
         
         pgg <- ggpairs(
                data_plots, aes(colour = sector5, alpha = 0.7), 
                lower = list(continuous = "smooth_lm")
         )
                  pgg + ggplot2::theme(axis.text.x = element_text(angle = 60, hjust = 1),
                                       text = element_text(size=18))
         }else{
                 data_plots$sector5 <- NULL
                 pgg <- ggpairs(data_plots, 
                         columns = 1:length(input$xCol)+1,
                         lower = list(continuous = "smooth_lm"))
                 pgg + ggplot2::theme_grey(base_size = 18)

         }
  })
  
  
  lm1 <- reactive({lm(paste(input$yCol, " ~ ", paste(input$xCol, collapse = "+"), sep = ""), 
                      data = spdf())})
  
  # can use just lm1 instead of creating another reactive object
  vif1 <- reactive({vif(lm(paste(input$yCol, " ~ ", paste(input$xCol, collapse = "+"), sep = ""), 
                      data = spdf()))})
  
  output$regCoeff <- renderPlot({ggcoef(lm1(), 
                                        mapping = aes(x = estimate, y = term, size = p.value)) +
                  scale_size_continuous(trans = "reverse")})
  
  output$vif_scores <- renderPrint({(vif1())})
  output$RegSum <- renderPrint({summary(lm1())})
  
}
 
# shinyApp()
shinyApp(ui = ui, server = server)
