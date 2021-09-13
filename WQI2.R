#Pennsylvania WQI Dashboard
#STA419-02
#Cleyde Murillo Jr, Jacob Weishun, Taira Vogt
library(shiny)
library(ggplot2)
library(data.table)
library(DT)
library(shinyWidgets)
# library(readr)
# PAwqi <- read_csv("GVSU/STA419/Data/RawWQD/PAwqi.csv")

ui <- fluidPage(
  
    titlePanel("Pennsylvania Water Quality Index"),

        sidebarLayout(
          sidebarPanel(
            
            h3("Data Table Filters"),
            h6("School name can be searched using the Search bar on the top right"),
            
            selectInput('site',
                        'Site Number:',
                         c('All',
                            unique(as.numeric(PAwqi$Site)))),
            
            dateRangeInput('daterange',
                label = 'Select Date Range:',
                start = "2000-01-01", 
                end = '2020-04-01',
                min = min(PAwqi$Date),
                max = max(PAwqi$Date)),
            
            h3('Graph Selection Filter'),
            
            selectInput('grphsite',
                        'Site Number:',
                        (unique(as.character(models$Site)))),
            
            selectInput('grphvar',
                        'Comparison Variable:',
                        names(PAwqi[,c(5:13)])),
            
            
            h6('*Site Numbers for both filters should match in order to
               view data from graph'),
            h4('Table of Variables with ranges'),
            h6('WQI - Water Quality Index (0-100)'),
            h6('pH - Acidity/Basic Scale (0-14)'),
            h6('NO3-N - Nitrate Nitrogen (0-2.6) mg/L'),
            h6('P - Phosphorous (0-0.43) mg/L'),
            h6('Turbidity - (0-45) JTU'),
            h6('DO - Dissolved Oxygen (6-14) mg/L'),
            h6('TDS - Total Dissolved Solids (60-460) mg/L'),
            h6('Alkalinity - (20-200) mg/L'),
            h6('Temperature - Celsius (C)'),
            
            width = 2
           ),
         
    
           mainPanel(
             DT::dataTableOutput('table'),
             plotOutput('plot1', height = 550,
                        dblclick = 'plot1_dblclick',
                        brush = brushOpts(
                          id = 'plot1_brush',
                          resetOnNew = TRUE
                        )
             ),
             
             width = 10
           )
        ))


server <- function(input, output) {
    
    output$plot1 <- renderPlot({
      grphdata <- models[[2]][[which(models$Site == input$grphsite, arr.ind=TRUE)]]
      datechar <- 'Date'
      grphdata %>%
        tail(100) %>%
        ggplot(aes_string(x = datechar, y = input$grphvar)) +
        geom_point() +
        geom_smooth(method = loess, color = 'red', se=TRUE)
    })
    
    output$startdate <- renderText({
        as.character(input$daterange[1])
    })
    output$enddate <- renderText({
        as.character(input$daterange[2])
    })
    output$range <- renderText({
        paste('Selected date range is', input$daterange[1], 'to', input$daterange[2])
    })
    output$table <- DT::renderDataTable(DT::datatable({
        data <- PAwqi
        if(input$site != "All") {
            data <- data[data$Site == input$site,]
        }
        data = subset(data, data$Date >= input$daterange[1] & data$Date <= input$daterange[2])
        data
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)