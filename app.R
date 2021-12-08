library(shiny)
library(plotly)
library(DT)
library(shinythemes)

covid = read.csv("covid_data.csv", sep = ';')
covid$Date = as.Date(covid$Date)
covid$Country = as.factor(covid$Country)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
 
  
  navbarPage(
    title = "Covid-19 Data",
    id = "nav",
    tabPanel("Data", value = "Data",
  
      sidebarLayout(
        sidebarPanel(
          h2("COVID-19 Data in South East Asia"),
          selectInput(inputId = "dv", label = "Category",
                      choices = c("Total_Cases","New_Cases", "Total_Death","New_Death"),
                      selected = "New_Cases"),
          selectInput(inputId = "Country", "Country(s)",
                      choices = levels(covid$Country),
                      multiple = TRUE,
                      selected = c("Malaysia")),
          dateRangeInput(inputId = "date", "Date range",
                         start = min(covid$Date),
                         end   = max(covid$Date)),
          downloadButton(outputId = "download_data", label = "Download"),
    ),
      mainPanel(
        plotlyOutput(outputId = "plot"), br(),
        em("COVID-19 CASES IN SOUTH EAST ASIA"),
        br(), br(), br(),
        DT::dataTableOutput(outputId = "table")
    )
  )),
  
  tabPanel("Vaccinnation", value = "Vaccination",
),

  tabPanel("Economic", value = "Economic",
),

  tabPanel("About", value = "About",
           sidebarLayout(
             sidebarPanel("PREPARED BY"),
              mainPanel(""),
))))

server <- function(input, output) {
  filtered_data <- reactive({
    subset(covid,
           Country %in% input$Country &
             Date >= input$date[1] & Date <= input$date[2])})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="Date", y=input$dv, color="Country")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("Total Number")
      
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
