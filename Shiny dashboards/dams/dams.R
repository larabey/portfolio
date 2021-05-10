#load libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(here)
library(leaflet.extras)
library(shinyWidgets)
library(leafpop)
library(waiter)
library(shinyalert)


#load data
damdata <- read_csv(here("damdata.csv")) %>%
  mutate(purpose = fct_relevel(purpose, c("Hydroelectricity",
                                          "Irrigation",
                                          "Irrigation & Hydroelectricity",
                                          "Irrigation & Water supply")),
         drought = fct_relevel(drought, c("Low drought risk", "High drought risk")))

damspat <- read_csv(here("damspats.csv")) %>%
  mutate(purpose = fct_relevel(purpose, c("Hydroelectricity",
                                          "Irrigation",
                                          "Irrigation & Hydroelectricity",
                                          "Irrigation & Water supply")),
         district = as_factor(district),
         drought = as_factor(drought))

load(here("popmaps.RData")) #this is called "maps" in the environment
#because that's what it was called in leafpop.Rmd and somehow R knows


#create static pieces
content <- paste("<b>", damspat$reservoir_name, "Dam", "</b></br>",
                 "River:", damspat$river, "</br>",
                 "District:", damspat$district, "</br>",
                 "Purpose:", damspat$purpose, "</br>",
                 "Effective storage capacity:", 
                 round(damspat$effective_storage_capacity_109m3, digits = 2), "BCM")

waiting_screen <- tagList(
  img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Mahuli_Fort_From_Pivali_End.JPG/1200px-Mahuli_Fort_From_Pivali_End.JPG",
      height=600,
      id = "myImage"),
  spin_flower(),
  h4("Loading Maharashtra Dam Statistics..."))



# User interface
ui <- fluidPage(
  
  use_waiter(),
  waiter_preloader(),
  
  
  titlePanel(title = "Dams and Droughts in Maharashtra"),
  tabsetPanel(
    
    
    tabPanel("Map", 
             verticalLayout(
               
               useShinyalert(),
               
               p(h4(
                 "Maharashtra Dams")),
               
               leafletOutput("map", height = "550px"),
               
               actionButton(inputId = "welcome",
                            label = "Show welcome information again"),
               
               uiOutput("information"))
             
    ),
    
    
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 
                 checkboxGroupInput(inputId = "damtype",
                                    label = "Dam use",
                                    choices = levels(damdata$purpose),
                                    selected = levels(damdata$purpose)),
                 
                 pickerInput(inputId = "damdist",
                             label = "District",
                             # choices = levels(damdata$distlabel),
                             choices = list(
                               `High risk areas` = c("Hingoli - High drought risk", "Jalgaon - High drought risk",
                                                     "Parbhani - High drought risk", "Pune - High drought risk", 
                                                     "Satara - High drought risk"),
                               `Low risk areas` = c("Ahmadnagar - Low drought risk", "Aurangabad - Low drought risk",
                                                    "Kolhapur - Low drought risk", "Nagpur - Low drought risk", 
                                                    "Nashik - Low drought risk", "Pune - Low drought risk", 
                                                    "Satara - Low drought risk", "Thane - Low drought risk")
                             ),
                             selected = c("Satara - High drought risk", "Pune - High drought risk",
                                          "Pune - Low drought risk", "Thane - Low drought risk"),
                             options = list(`actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 2"
                             ),
                             multiple = TRUE),
                 
                 dateRangeInput(inputId = "dates",
                                label = "Date range",
                                start = as_date("2015-01-01"), end = as_date("2016-12-01"),
                                min = as_date("2015-01-01"), max = as_date("2020-12-01")),
                 
                 textOutput("dateinfo"),
                 
                 p(tags$br(),
                   h4("Drought years in Maharashtra:"),
                   "07/2015 - 07/2016", tags$br(),
                   "07/2018 - 06/2019", tags$br(),
                   tags$br(),
                   "Note how dams in high drought risk areas are more likely to
                   run out of water, do so earlier in the year, and stay empty 
                   for longer than dams in low drought risk areas.")
               ),
               
               mainPanel(
                 h3("Water storage over time"),
                 plotOutput("plot"))
             )),
    
    
    tabPanel("Data",
             sidebarLayout(
               
               sidebarPanel(position = "right",
                            p(h4("App authors"),
                              "Aditya Gadkari", tags$br(),
                              "Lauren Rabe", tags$br(),
                              tags$br(),
                              h4("Data sources"),
                              tags$a(href = "https://indiawris.gov.in/wris/#/", 
                                     target = "_blank", 
                                     "India Water Resources Information System"), tags$br(),
                              tags$a(href="https://en.wikipedia.org/wiki/List_of_dams_and_reservoirs_in_Maharashtra", 
                                     target = "_blank",
                                     "Wikipedia: List of dams and reservoirs in Maharashtra")
                            )
               ),
               
               mainPanel(
                 uiOutput("adinfo")
               )
             )
    )
  )
)




# Server function
server <- function(input, output, session){
  
  
  waiter_show(html = waiting_screen)
  Sys.sleep(3) # do something that takes time
  waiter_hide()
  
  
  
  damreact <- reactive({
    damdata %>%
      filter(purpose %in% input$damtype, 
             distlabel %in% input$damdist,
             date <= input$dates[2], date >= input$dates[1])
  })
  
  
  
  dampal <- colorFactor(palette = c("#564040", "#0033FF"),
                        domain = damspat$drought)
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      setView(lat = 19.2, lng = 75, zoom = 7) %>%
      setMaxBounds(lat1 = 10, lng1 = 62.2,
                   lat2 = 29, lng2 = 90.2) %>%
      addCircleMarkers(data = damspat,
                       lat = ~lat, lng = ~long,
                       stroke = FALSE, fillOpacity = 0.65,
                       color = ~dampal(drought),
                       radius = ~effective_storage_capacity_109m3*10,
                       group = "Depletion.Curve",
                       label = unique(damspat$reservoir_name)) %>%
      addPopupGraphs(maps, group = "Depletion.Curve", width = 400, height = 300) %>%
      addCircleMarkers(data = damspat,
                       lat = ~lat, lng = ~long,
                       stroke = FALSE, fillOpacity = 0.65,
                       color = ~dampal(drought),
                       radius = ~effective_storage_capacity_109m3*10,
                       group = "Text.Info",
                       label = unique(damspat$reservoir_name),
                       popup = content) %>%
      addLegend(position = "bottomright", pal = dampal, values = damdata$drought,
                opacity = 0.65) %>%
      addLayersControl(baseGroups = c("Depletion.Curve", "Text.Info"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright") %>%
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\"><u>Toggle Popups</u></label>');
        }
    ")
    
  })
  
  
  
  output$dateinfo <- renderText({
    "The data are at the monthly level and range from January 2015 to December 2020. 
    We recommend subsetting the data to avoid overplotting."
  })
  
  
  
  output$plot <- renderPlot({
    
    ggplot(damreact(),
           aes(x = date, y = storage_bcm, color = purpose, group = reservoir_name,
               shape = drought)) +
      geom_point(alpha = 0.6,
                 aes(size = damreact()$effective_storage_capacity_109m3)) +
      geom_line(alpha = 0.2, color = "black") +
      theme_minimal() +
      labs(y = bquote("Water storage"~(10^9~m^3)), color = "Use",
           shape = "Drought risk",
           size = bquote("Effective storage capacity"~(10^9~m^3))) +
      theme(axis.title.x = element_blank()) +
      guides(color = guide_legend(order = 1),
             size = guide_legend(order = 2)) +
      scale_color_manual(breaks = c("Hydroelectricity",
                                    "Irrigation",
                                    "Irrigation & Hydroelectricity",
                                    "Irrigation & Water supply"),
                         values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")) +
      scale_shape_manual(breaks = c("Low drought risk", "High drought risk"),
                         values = c(16, 17))
  })
  
  
  
  output$information <- renderUI({
    HTML(paste("<b>", h4("The state of Maharashtra in India has more than 1,200 dams, but it also has one 
of the highest farmer suicide rates anywhere in the world. How is this possible?"), "</b>",
               
               "<b>",  "The Monsoons", "</b></br>",
               "The monsoon season in the Indian subcontinent is a weather pattern that brings rain 
to the subcontinent every year, generally between June and September. The predominant monsoon winds enter the 
subcontinent from the south west. Having travelled across the Indian Ocean, these winds carry with 
them water-saturated clouds and deliver over 50% of India's total annual rainfall.", 
               "</br> </br>",
               
               "<b>",  "The Western Ghats", "</b></br>",
               
               "The Western Ghats are a mountain range that run down the western coast of the Indian Peninsula. 
They act as a natural barrier that slows down the incoming monsoon winds and thus facilitate heavy 
precipitation across the mountain range in the monsoon season. The precipitation is so great that 
all of peninsular India's major rivers originate from a 50 km stretch of the northern 
Western Ghats.", "</br>", "</br>",
               
               "<b>",  "The Deccan Plateau", "</b></br>",
               "Once the monsoon winds cross the Western Ghats, they enter the Deccan plateau of central
India where there are minimal physical barriers to slow down the winds. As a result, these 
clouds do not deposit as much rain in this region until they slow down again as they approach 
the Eastern Ghats, a comparatively minor mountain range on the eastern coast.", "</br> </br>",
               
               "<b>",  "Drought", "</b></br>",
               
               "Dams in the Western Ghats fill up more and retain water for longer than those in the Deccan,
even if the monsoons are particularly weak in a certain year. As a result, this central region in the Deccan known 
as Vidhabha is extremely drought prone. In drought years like 2018, the reservoirs of these 
dams dry up as early as March. This is part of the reason that this region faces massive crop 
failures which have ultimately led many farmers to take their own lives. 
Climate change has made monsoons even more erratic which makes the threat to these communities 
even more severe going into the future.", "</br> </br>"))
  })
  
  
  
  output$adinfo <- renderUI({
    HTML(paste("</br>", h4("Background on the data"),
               "We noted that there were over 1,200 dams in Maharashtra, yet only provided data for 17 of them
      in our map and depletion curve graph. This is because these are the only dams for which we could
      get consistent water storage data. The Wikipedia article provided the dam characteristics in our 
      Text.info map popups, and the Water Resources Information System (WRIS) provided the actual time 
      series data of water storage within the dams. The WRIS reports this data at the yearly, monthly, 
      and daily level; we found the monthly data to be the most helpful for understanding the Monsoon 
      and drought seasons."))
  })
  
  
  
  observeEvent(waiter_preloader(), {
    shinyalert(
      title = "Welcome!",
      text = "This dashboard allows users to compare the depletion rates of 
              dams in high drought risk and low drought risk districts in the 
              state of Maharashtra. Dams that are in the drought prone 
              central region of the state run out of water much sooner than 
              ones that are in the low drought risk mountains. Users can 
              click on the dams in the map tab to view information about the 
              particular dam or the depletion rate across 2019 and 2020 
              (where 2019 was a drought year). In the plot tab, users can 
              view the depletion curves of many dams at once, for any 
              months between January 2015 and December 2020.",
      confirmButtonText = "Let's go!",
      immediate = TRUE
    )
  })
  
  
  observeEvent(input$welcome, {
    shinyalert(
      title = "Welcome, again!",
      text = "This dashboard allows users to compare the depletion rates of 
              dams in high drought risk and low drought risk districts in the 
              state of Maharashtra. Dams that are in the drought prone 
              central region of the state run out of water much sooner than 
              ones that are in the low drought risk mountains. Users can 
              click on the dams in the map tab to view information about the 
              particular dam or the depletion rate across 2019 and 2020 
              (where 2019 was a drought year). In the plot tab, users can 
              view the depletion curves of many dams at once, for any 
              months between January 2015 and December 2020.",
      confirmButtonText = "Okay!",
      immediate = TRUE
    )
  })
  
}



# Create app
shinyApp(ui = ui, server = server)