# load libraries
library(shiny)
library(tidyverse)
library(palmerpenguins)
library(DT)
library(glue)


# simplify data
penguins2 <- penguins %>%
  select(-year) %>% #(we can't map a continuous variable to shape)
  drop_na() #(these just clutter the plots)


# approximate the `palmerpenguins` palette from github
ppal <- c("#50A6A6", "#B455F3", "#FFA449")


# create app user interface
ui <- fluidPage(
  titlePanel(title = p("Plotting", 
                       tags$a(href = "https://allisonhorst.github.io/palmerpenguins/",
                              tags$code("palmerpenguins"),
                              target = "_blank"),
                       tags$a(href = "https://allisonhorst.github.io/palmerpenguins/",
                              tags$img(src = "https://allisonhorst.github.io/palmerpenguins/reference/figures/logo.png",
                                       width = "80px", style = "float:right"),
                              target = "_blank"))),
  tabsetPanel(
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 h4("Pick your variables to plot!"),
                 radioButtons(inputId = "yvar", 
                              label = "Y Variable",
                              choices = names(penguins2), 
                              selected = "bill_length_mm"),
                 
                 radioButtons(inputId = "xvar", 
                              label = "X Variable",
                              choices = names(penguins2), 
                              selected = "flipper_length_mm"),
                 
                 radioButtons(inputId = "byvar", 
                              label = "By",
                              choices = list("species", "island", "sex"), 
                              selected = "species")
               ),
               mainPanel(
                 plotOutput("plot"),
                 br(),
                 h4("Summary Statistics:"),
                 textOutput("text"),
                 dataTableOutput("table")
               )
             )
    ),
    tabPanel("Info",
             mainPanel(br(),
                       p("These data are from the", 
                         tags$code("palmerpenguins"),
                         "package in R, developed by",
                         tags$a(href = "https://www.allisonhorst.com/",
                                "Dr. Allison Horst,", target = "_blank"),
                         tags$a(href = "https://alison.rbind.io/",
                                "Dr. Alison Hill,", target = "_blank"), "and",
                         tags$a(href = "https://www.uaf.edu/cfos/people/faculty/detail/kristen-gorman.php",
                                "Dr. Kristen Gorman", target = "_blank"),
                         "under the CC-0 license.",
                         "As per the", 
                         tags$a(href = "https://allisonhorst.github.io/palmerpenguins/",
                                tags$code("palmerpenguins"), "GitHub,"),
                         "data were collected and made available by Dr. Kristen Gorman and the", 
                         tags$a(href = "https://pal.lternet.edu/",
                                "Palmer Station, Antarctica LTER", target = "_blank"),
                         ", a member of the",
                         tags$a(href = "https://lternet.edu/",
                                "Long Term Ecological Research Network.")),
                       p("While the main purpose of the", tags$code("palmerpenguins"),
                         "package is to allow the user to explore data visualization with",
                         "a fun dataset, the purpose of this app is rather the antithesis of that:",
                         "to allow the user to explore a fun dataset without needing to learn",
                         "data visualization. That being said, I hope this app inspires you to",
                         "explore data viz or even", 
                         tags$a(href = "https://shiny.rstudio.com/gallery/",
                                tags$code("shiny"), "apps", target = "_blank"), 
                         "themselves!"),
                       p("App created by Lauren Rabe using the", tags$code("shiny"), "package. :)")
             ))
  )
)



# create server function
server <- function(input, output){
  
  axislabs <- reactive({
    penguins2 %>%
      mutate(
        yaxlab = case_when(
          input$yvar == "species" ~ "Species",
          input$yvar == "island" ~ "Island",
          input$yvar == "bill_length_mm" ~ "Bill length (mm)",
          input$yvar == "bill_depth_mm" ~ "Bill depth (mm)",
          input$yvar == "flipper_length_mm" ~ "Flipper length (mm)",
          input$yvar == "body_mass_g" ~ "Body mass (g)",
          input$yvar == "sex" ~ "Sex"
        ),
        xaxlab = case_when(
          input$xvar == "species" ~ "Species",
          input$xvar == "island" ~ "Island",
          input$xvar == "bill_length_mm" ~ "Bill length (mm)",
          input$xvar == "bill_depth_mm" ~ "Bill depth (mm)",
          input$xvar == "flipper_length_mm" ~ "Flipper length (mm)",
          input$xvar == "body_mass_g" ~ "Body mass (g)",
          input$xvar == "sex" ~ "Sex"
        ),
        bylab = case_when(
          input$byvar == "species" ~ "Species",
          input$byvar == "island" ~ "Island",
          input$byvar == "sex" ~ "Sex"
        )
      )
  })
  
  
  output$plot <- renderPlot({
    ggplot(penguins2, 
           aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
               color = .data[[input$byvar]], shape = .data[[input$byvar]])) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal() +
      scale_color_manual(values = ppal) +
      labs(y = axislabs()$yaxlab, x = axislabs()$xaxlab,
           color = axislabs()$bylab, shape = axislabs()$bylab,
           title = paste("Penguin", axislabs()$yaxlab, "and", axislabs()$xaxlab),
           subtitle = paste("By", axislabs()$bylab))
  })
  
  
  
  output$text <- renderText({
    
    glue("Averages by ", input$byvar)
    
  })
  
  # Reactive table headers. cause mischief :(
  
  # tablabs <- reactive({
  #   penguins2 %>%
  #     mutate(
  #       bylab = case_when(
  #         input$byvar == "species" ~ "Species",
  #         input$byvar == "island" ~ "Island",
  #         input$byvar == "sex" ~ "Sex"
  #       ),
  #       xlab = case_when(
  #         input$xvar == "species" ~ "Species",
  #         input$xvar == "island" ~ "Island",
  #         input$xvar == "bill_length_mm" ~ "Mean bill length (mm)",
  #         input$xvar == "bill_depth_mm" ~ "Mean bill depth (mm)",
  #         input$xvar == "flipper_length_mm" ~ "Mean flipper length (mm)",
  #         input$xvar == "body_mass_g" ~ "Mean body mass (g)",
  #         input$xvar == "sex" ~ "Sex"
  #       ),
  #       ylab = case_when(
  #         input$yvar == "species" ~ "Species",
  #         input$yvar == "island" ~ "Island",
  #         input$yvar == "bill_length_mm" ~ "Mean bill length (mm)",
  #         input$yvar == "bill_depth_mm" ~ "Mean bill depth (mm)",
  #         input$yvar == "flipper_length_mm" ~ "Mean flipper length (mm)",
  #         input$yvar == "body_mass_g" ~ "Mean body mass (g)",
  #         input$yvar == "sex" ~ "Sex"
  #       )
  #     )
  # })
  # 
  # 
  # 
  # tablabs2 <- reactive({
  #   as.character(as.vector(tablabs()[1, 8:10]))
  #     
  # })
  
  
  
  stats <- reactive({
    penguins2 %>%
      select(input$byvar, input$xvar, input$yvar) %>%
      group_by(.data[[input$byvar]]) %>%
      summarize(across(where(is.numeric), mean))
    
  })
  
  
  numcols <- reactive({
    stats() %>%
      select(where(is.numeric)) %>%
      colnames()
  })
  
  
  output$table <- renderDataTable({
    datatable(stats(),
              options = list(paging = FALSE,
                             searching = FALSE,
                             ordering = FALSE,
                             dom = "t"),
              rownames = FALSE) %>%
      formatRound(columns = numcols(),
                  digits = 1)
  })
  
}



# create app
shinyApp(ui = ui, server = server)