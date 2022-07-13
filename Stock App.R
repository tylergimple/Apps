#Packages:
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(rsconnect)
library(tidyquant)

#Creating the UI.
ui<-dashboardPage(skin="red",
                  dashboardHeader(title = "Tyler's Dashboard"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Stock", tabName = "Stock", icon = icon("money-bill-alt"))
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      tabItem("Stock",
                              box(plotlyOutput("stock_plot"), width=10),
                              box(
                                dateInput("start_date", "Start Date:", value = Sys.Date()-(365*10)),
                                dateInput("end_date", "End Date:", value = Sys.Date()),
                                selectInput("measure", "Measure",
                                            c("open", "high","low","close")),
                                textInput("ticker1", "1st Ticker", "AAPL"),
                                textInput("ticker2", "2nd Ticker", "MSFT"),
                                width = 2)
                      )
                    ),
                  )
)

#Creating the Server
server<- function(input, output){
  output$stock_plot <- renderPlotly({
                
                data1<-tq_get(input$ticker1,
                              from = "1900-01-01",
                              to = Sys.Date(),
                              get = "stock.prices")
                
                fdata1 <- data1[which(data1$date>input$start_date & data1$date<input$end_date), ]
                
                
                data2<-tq_get(input$ticker2,
                              from = "1900-01-01",
                              to = Sys.Date(),
                              get = "stock.prices")
                
                fdata2 <- data2[which(data2$date>input$start_date & data2$date<input$end_date), ]
                
                
                cdata<-as.data.frame(rbind(fdata1,fdata2))
    
    plot_ly(cdata, x = ~date, y = ~cdata[[input$measure]], color=~symbol, type = 'scatter', mode = 'lines') %>%
      layout(title = 'Stock Price Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Price'))%>%
      layout(
        yaxis = list(
          tickformat = "$"
        ))
  })
}

shinyApp(ui, server)
