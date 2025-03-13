#Loading needed libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(DT)

# Load dataset with the cleaned graduate data
survey_data <- read.csv("cleaned_survey.csv")


ui <- dashboardPage( skin = "blue",
  dashboardHeader(title = "Eduvos Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guide to use Dashboard", tabName = "guide", icon = icon("book")),
      menuItem("Eduvos Graduates", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Survey Data", tabName = "data_table", icon = icon("table"))
    )
  ),
  #Creating dashboard
  dashboardBody(
    tabItems(
      #Creating Graduates tabitem.
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Select Tool Category", status = "primary",
                  solidHeader = TRUE, width = 4,
                  selectInput("tool_option", "Select option:",
                              choices = c("Programming Languages" = "ProgLang",
                                          "Databases" = "Databases",
                                          "AI Search Tools" = "AISearch",
                                          "AI Tools" = "AITool",
                                          "Web Frameworks" = "WebFramework",
                                          "Platforms" = "Platform"),
                              selected = "ProgLang")
                ),
                box(
                  title = "Plot of selection", status = "primary",
                  solidHeader = TRUE, width = 8,
                  plotOutput("dynamicPlot", height = 400)
                )
              )
      ),
      #Creating User guide
      tabItem("guide",
              fluidRow(
                box(
                  title = "Goal of the dashboard.", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 8,
                  "The goal of this dashboard is to analyse the tools that graduates from eduvos are currently using in their fields of work."
                )
              ),
              fluidRow(
                box(
                  title = "How to use the Eduvos dashboard.",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 10,
                  HTML("<ul>
          <li>Step 1: If the sidebar is not displayed, click on the three stripes in the header of the webpage.</li>
          <li>Step 2: Once the sidebar is open you can either click on the Eduvos Graduates or the Survey Data option.</li>
          <br>
          <li>Eduvos Graduates: In the dropdown list in the select tool category box you can select which tool you would like to view.
          The plot on the right hand side displays the plotted data of your selection.</li>
          <br>
          <li>Survey Data: On this option you could look at a table of the data that was used to create the plots previously mentioned.</li>
       </ul>")
                )
              )),
      #Creating table with the clean dataset that was used.
      tabItem(tabName = "data_table",  # New tab for displaying the table
              fluidRow(
                box(
                  title = "Survey Data Table", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  dataTableOutput("surveyTable")  # DataTable output
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  display_names <- c(
    "Programming Languages" = "ProgLang",
    "Databases" = "Databases",
    "AI Search Tools" = "AISearch",
    "AI Tools" = "AITool",
    "Web Frameworks" = "WebFramework",
    "Platforms" = "Platform"
  )
  
  output$dynamicPlot <- renderPlot({
    selected_column <- input$tool_option
    #Plot data
    top_data <- survey_data %>%
      filter(!is.na(.data[[selected_column]])) %>%
      mutate(tech = str_split(.data[[selected_column]], ";")) %>%
      unnest(tech) %>%
      group_by(tech) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_max(count, n = 10)
    
    # Plot titles
    plot_title <- names(display_names[display_names == selected_column])
    
    ggplot(top_data, aes(x = reorder(tech, count), y = count, fill = tech)) +
      geom_col() +
      labs(title = paste("Top", plot_title),  
           x = plot_title, 
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      guides(fill = guide_legend(title = plot_title))  
  })
  #Datatable
  output$surveyTable <- renderDataTable({
    datatable(survey_data, 
              options = list(
                pageLength = 10,  
                autoWidth = TRUE,  
                scrollX = TRUE     
              ))
  })
}

# Running  app 
shinyApp(ui, server)
