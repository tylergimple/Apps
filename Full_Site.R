library(shiny)
library(shiny)
library(shinydashboard)
library(plotly)
library(rsconnect)
library(tidyquant)
library(DataCombine)
library(formattable)
library(data.table)
library(stats)
library(dplyr)

ui<-dashboardPage(skin="red",
                  dashboardHeader(title = "Tyler's Website"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("home")),
                      menuItem("Money", tabName = "money", icon = icon("money"),
                               menuSubItem("General Investment Growth", tabName = "gen_grow", icon = icon("dollar")),
                               menuSubItem("Past Stock Performance", tabName = "past_stock_perf", icon = icon("line-chart")),
                               menuSubItem("Two Stock Viewer", tabName = "two_stock", icon = icon("line-chart")),
                               menuSubItem("Mortgage Calculator", tabName = "mort_calc", icon = icon("home"))),
                      menuItem("Chaos Theory", tabName = "chaos", icon = icon("snowflake-o"),
                               menuSubItem("Sierpinski Triangle", tabName = "s_tri", icon = icon("codepen")),
                               menuSubItem("Barnsley Fern", tabName = "b_fern", icon = icon("leaf")),
                               menuSubItem("Collatz Conjecture", tabName = "c_conj", icon = icon("xing")))
                    )
                  ),
                  dashboardBody(
                  tabItems(
                    tabItem("home",
                            fluidPage(
                              h1("Welcome to my Website of Random Mathematical Awesomeness!"),
                              h4("The entire site is built in R primarily using the Plotly and Shiny Packages. Contact me at tylergimple@gmail.com if you would like to learn more about what you see here or if you have any cool ideas about future projects to add to this site. Have fun exploring!")
                            )),
                    tabItem("gen_grow",
                            h4("Adjust the sliders to change the length of investment, initial investment, the amount being invested per month, or the annual rate of return. Note how the graph changes as you adjust the variables!"),
                            box(plotlyOutput("gen_grow_plot"), width=10),
                            box(
                              sliderInput("years", "Number of Years", min = 0, max = 100, value = 30),
                              numericInput("initial", "Initial Amount:", 10000, min = 1, max = 1000000000),
                              numericInput("monthly_investment", "Monthly Investment:", 1000, min = 1, max = 1000000000),
                              numericInput("yearly_return", "Annual Return %", 7, min = 0, max = 100),
                              width = 2),
                            box(textOutput("gen_grow_print"), width=2),
                    ),
                    tabItem("past_stock_perf",
                            h4("The figure below examines how an investment would perform if left in a non-interest earning account vs. being invested in a stock or mutual fund. Adjust any of the variables to see how you would have done!"),
                            box(plotlyOutput("past_stock_plot"), width=10),
                            box(
                              textInput("ticker", "Ticker", "AAPL"),
                              dateInput("start_date", "Start Date:", value = Sys.Date()-(365*10)),
                              dateInput("end_date", "End Date:", value = Sys.Date()),
                              numericInput("initial_past", "Initial Deposit", 10000, min = 0, max = 1000000000),
                              numericInput("amount", "Investment Amount", 100, min = 1, max = 10000000),
                              numericInput("freq", "Investment Frequency in Days", 30, min = 1, max = 365),
                              width = 2),
                            box(textOutput("past_stock_print"))
                    ),
                    tabItem("two_stock",
                            h4("See how two different stocks or mutual funds performed vs. one another."),
                            box(plotlyOutput("two_stock_plot"), width=10),
                            box(
                              dateInput("start_date1", "Start Date:", value = Sys.Date()-(365*10)),
                              dateInput("end_date1", "End Date:", value = Sys.Date()),
                              selectInput("measure", "Measure",
                                          c("open", "high","low","close")),
                              textInput("ticker1", "1st Ticker", "AAPL"),
                              textInput("ticker2", "2nd Ticker", "MSFT"),
                              width = 2)
                    ),
                    tabItem("mort_calc",
                            h4("Examine how a mortgage is paid off over time. Adjust any of the variables to see how it will change."),
                            box(plotlyOutput("mort_calc_plot"), width=10),
                            box(
                              numericInput("cost", "Cost of Property", 500000, min = 0, max = 1000000000),
                              numericInput("down_payment", "Down Payment in % of Total Property Cost:", 20, min = 0, max = 100),
                              numericInput("interest_rate", "Mortgage Interest Rate", 3, min = 0, max = 40),
                              sliderInput("length", "Length of Mortgage in Years", min = 0, max = 100, value = 30),
                              width = 2),
                            box(textOutput("mort_calc_print"))
                            ),
                    tabItem("chaos",
                              fluidPage(
                                h1("Chaos")
                              )),
                    tabItem("s_tri",
                            h4("The Sierpinski Triangle is a fractal attractive fixed set with the overall shape of an equilateral triangle, subdivided recursively into smaller equilateral triangles. There are many different ways to construct the triangle, however, the way here uses a chaotic iterated function system. Use the sliders to adjust the positions of the corners of the triangle, the beginning point, or the number of dots to see how the figure reacts!"),
                            box(plotlyOutput("s_tri_plot"), width=8),
                            box(
                              sliderInput("corn_ax", "Corner A X-Coordinate", min = 0, max = 100, value = 0),
                              sliderInput("corn_ay", "Corner A Y-Coordinate", min = 0, max = 100, value = 0),
                              sliderInput("corn_bx", "Corner B X-Coordinate", min = 0, max = 100, value = 50),
                              sliderInput("corn_by", "Corner B Y-Coordinate", min = 0, max = 100, value = 60),
                              sliderInput("corn_cx", "Corner C X-Coordinate", min = 0, max = 100, value = 100),
                              width = 2),
                            box(
                              sliderInput("corn_cy", "Corner C Y-Coordinate", min = 0, max = 100, value = 0),
                              sliderInput("start_x", "Starting X-Coordinate", min = 0, max = 100, value = 30),
                              sliderInput("start_y", "Starting Y-Coordinate", min = 0, max = 100, value = 30),
                              sliderInput("its", "Dots", min = 1, max = 15000, value = 7500),
                              selectInput("color", "Color",
                                          c("blue", "pink","red","green", "black", "yellow","brown","")),
                              width = 2)
                            ),
                    tabItem("b_fern",
                            h4("The Barnsley fern is a fractal and mathematically generated pattern that can be reproducible at any magnification or reduction. It is constructed using four affine transformations. Use the sliders to adjust the starting X and Y Points as well as the number of dots appearing on the screen and see how the fern reacts!"),
                            box(plotlyOutput("b_fern_plot"), width=10),
                            box(
                              sliderInput("beg_x", "Starting X-Coordinate", min = 0, max = 20, value = 0),
                              sliderInput("beg_y", "Starting Y-Coordinate", min = 0, max = 20, value = 1),
                              sliderInput("iters", "Dots", min = 0, max = 150, value = 75),
                              width = 2)
                    ),
                    tabItem("c_conj",
                            h4("The Collatz Conjecture is one of the most famous unsolved problems in mathematics. The conjecture asks whether repeating two simple arithmetic operations will eventually transform every positive integer into 1. It concerns sequences of integers in which each term is obtained from the previous term as follows: if the number is even, the next number is one half of the previous term. If the previous number is odd, the next term is 3 times the previous number plus 1. The conjecture states that these sequences always reach 1, no matter which positive integer is chosen to start the sequence. Input a beginning number and watch it fall to 1 or see if you can find a counter-example!"),
                            box(plotlyOutput("c_conj_plot"), width=10),
                            box(
                              numericInput("int_num", "Number to Test", 27, min = 0, max = 99999999999999999999999999999999999),
                              width = 2),
                            box(textOutput("c_conj_print"), width=2))
                  )
                 )
                )
server<- function(input, output){
  output$two_stock_plot <- renderPlotly({
    data1<-tq_get(input$ticker1,
                  from = "1900-01-01",
                  to = Sys.Date(),
                  get = "stock.prices")
    
    fdata1 <- data1[which(data1$date>input$start_date1 & data1$date<input$end_date1), ]
    
    
    data2<-tq_get(input$ticker2,
                  from = "1900-01-01",
                  to = Sys.Date(),
                  get = "stock.prices")
    
    fdata2 <- data2[which(data2$date>input$start_date1 & data2$date<input$end_date1), ]
    
    
    cdata<-as.data.frame(rbind(fdata1,fdata2))
    
    plot_ly(cdata, x = ~date, y = ~cdata[[input$measure]], color=~symbol, type = 'scatter', mode = 'lines') %>%
      layout(title = 'Stock Price Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Price'))%>%
      layout(
        yaxis = list(
          tickformat = "$"
        ))
  })
  
  output$gen_grow_plot <- renderPlotly({
    Years<-input$years
    Initial<-input$initial
    Monthly_Deposit<-input$monthly_investment
    return<-input$yearly_return
      
    Return <- return/100 #Input the Yearly Return
    Yearly_Investment <- Monthly_Deposit*12
    Iyear<-(((Initial)*(1+Return))+Yearly_Investment)
    twoyear<-(((Iyear)*(1+Return))+Yearly_Investment)
    new_year<-(((Initial)*(1+Return))+Yearly_Investment)
    i=0
    x <- vector(length=Years)
    while (i < Years) {
      new_year=(((new_year)*(1+Return))+Yearly_Investment)
      x[i] <- new_year
      i = i+1
    }
    
    Results<-data.frame(x)
    tworesults<-InsertRow(Results, NewRow = twoyear, RowNum = 1)
    IResults <- InsertRow(tworesults, NewRow = Iyear, RowNum = 1)
    resizeIresults<-data.frame(IResults[-c(Years+1,Years+2),])
    FinalValue<-resizeIresults[Years, 1]
    fFinalValue<-currency(FinalValue)
    
    STotal_Deposit<-c(1:(nrow(resizeIresults)))
    SYear<-c(1:(Years))
    SInterest<-c(1:(Years))
    Sstarting<-c(Initial)
    Spread <- data.frame(SYear, resizeIresults, Sstarting, STotal_Deposit, SInterest)
    
    M1Spread <- Spread %>% mutate(STotal_Deposit = SYear * Yearly_Investment)
    M2Spread <- M1Spread %>% mutate(SInterest = (M1Spread$IResults..c.Years...1..Years...2.... - (STotal_Deposit+Sstarting)))
    
    Interest<-M2Spread$SInterest
    Deposit<-M2Spread$STotal_Deposit
    Initial<-M2Spread$Sstarting
    DF<-cbind(Interest,Deposit,Initial)
    resize <- as.data.table(DF[-c(Years+1,Years+2),])
    MDF<-melt(resize)
    plotly_data<-cbind(resize, SYear)
    df_plotly_data <- as.data.frame(plotly_data)
    
    plot_ly(df_plotly_data, x = ~SYear, y = ~Initial, type = 'bar', name = 'Initial') %>% add_trace(y = ~Deposit, name = 'Deposit') %>% add_trace(y = ~Interest, name = 'Interest') %>% layout(yaxis = list(title = 'Dollars'), barmode = 'stack') %>% layout(title = "Investment Growth",
                                                                                                                                                                                                                                                              xaxis = list(title = "Year"),
                                                                                                                                                                                                                                                              yaxis = list(title = "Dollars"))
      

  })
  
  output$gen_grow_print <- renderPrint({
    Years<-input$years
    Initial<-input$initial
    Monthly_Deposit<-input$monthly_investment
    return<-input$yearly_return
    
    Return <- return/100 #Input the Yearly Return
    Yearly_Investment <- Monthly_Deposit*12
    Iyear<-(((Initial)*(1+Return))+Yearly_Investment)
    twoyear<-(((Iyear)*(1+Return))+Yearly_Investment)
    new_year<-(((Initial)*(1+Return))+Yearly_Investment)
    i=0
    x <- vector(length=Years)
    while (i < Years) {
      new_year=(((new_year)*(1+Return))+Yearly_Investment)
      x[i] <- new_year
      i = i+1
    }
    
    Results<-data.frame(x)
    tworesults<-InsertRow(Results, NewRow = twoyear, RowNum = 1)
    IResults <- InsertRow(tworesults, NewRow = Iyear, RowNum = 1)
    resizeIresults<-data.frame(IResults[-c(Years+1,Years+2),])
    FinalValue<-resizeIresults[Years, 1]
    l<-paste("Your Final Value is:", (currency(FinalValue)))
    print(l)
  })
  
  output$past_stock_plot <- renderPlotly({
    Ticker<-input$ticker
    Investment_Start<-input$start_date
    Investment_End<-input$end_date
    Initial_Deposit<-input$initial_past
    Investment_Frequency<-input$freq
    Investment_Amount<-input$amount
    
    Stock_Table <- tq_get(Ticker,                    
                          from = Investment_Start,
                          to = Investment_End,
                          get = "stock.prices")
    Stock_Table_df<-as.data.frame(Stock_Table)
    Stock_Table_df_cut <- Stock_Table_df %>%
      filter(row_number() %% Investment_Frequency == 1)
    
    deposit <- as.data.frame(seq(Investment_Amount, Investment_Amount, length = count(Stock_Table_df_cut)))
    
    Stock_Table_wDeposit<-as.data.frame(c(Stock_Table_df_cut,deposit))
    Stock_Table_wDeposit[1, 9] = Initial_Deposit
    colnames(Stock_Table_wDeposit)[9] <- "Deposit"
    
    M1Spread <- Stock_Table_wDeposit %>% mutate(Shares_Purchased = Deposit / close)
    M2Spread <- M1Spread %>% mutate(Cumulative_Shares = cumsum(Shares_Purchased))
    M3Spread <- M2Spread %>% mutate(Cumulative_Deposit = cumsum(Deposit))
    M4Spread <- M3Spread %>% mutate(Value = (Cumulative_Shares*close))
    
    clean_spread<-as.data.table(M4Spread[, c("Cumulative_Deposit", "Value")])
    colnames(clean_spread)[1] <- "Deposit"
    colnames(clean_spread)[2] <- "Investment"
    m_clean_spread<-melt(clean_spread)
    colnames(m_clean_spread)[1] <- "Money_Type"
    colnames(m_clean_spread)[2] <- "Value"
    final_spread<-cbind(m_clean_spread,M4Spread$date)
    colnames(final_spread)[3] <- "Date"
    
    plot_ly(data = final_spread, x = ~Date, y = ~Value, color = ~Money_Type, mode = 'lines')
  })
  
  output$past_stock_print <- renderPrint({
    Ticker<-input$ticker
    Investment_Start<-input$start_date
    Investment_End<-input$end_date
    Initial_Deposit<-input$initial_past
    Investment_Frequency<-input$freq
    Investment_Amount<-input$amount
    
    Stock_Table <- tq_get(Ticker,                    
                          from = Investment_Start,
                          to = Investment_End,
                          get = "stock.prices")
    Stock_Table_df<-as.data.frame(Stock_Table)
    Stock_Table_df_cut <- Stock_Table_df %>%
      filter(row_number() %% Investment_Frequency == 1)
    
    deposit <- as.data.frame(seq(Investment_Amount, Investment_Amount, length = count(Stock_Table_df_cut)))
    
    Stock_Table_wDeposit<-as.data.frame(c(Stock_Table_df_cut,deposit))
    Stock_Table_wDeposit[1, 9] = Initial_Deposit
    colnames(Stock_Table_wDeposit)[9] <- "Deposit"
    
    M1Spread <- Stock_Table_wDeposit %>% mutate(Shares_Purchased = Deposit / close)
    M2Spread <- M1Spread %>% mutate(Cumulative_Shares = cumsum(Shares_Purchased))
    M3Spread <- M2Spread %>% mutate(Cumulative_Deposit = cumsum(Deposit))
    M4Spread <- M3Spread %>% mutate(Value = (Cumulative_Shares*close))
    
    clean_spread<-as.data.table(M4Spread[, c("Cumulative_Deposit", "Value")])
    colnames(clean_spread)[1] <- "Deposit"
    colnames(clean_spread)[2] <- "Investment"
    m_clean_spread<-melt(clean_spread)
    colnames(m_clean_spread)[1] <- "Money_Type"
    colnames(m_clean_spread)[2] <- "Value"
    final_spread<-cbind(m_clean_spread,M4Spread$date)
    colnames(final_spread)[3] <- "Date"
    
    Fin<-as.character(count(M4Spread))
    
    Final_Value <- M4Spread[Fin,13]
    Final_Deposit_two <- M4Spread[Fin,12]
    Final_Difference <- Final_Value-Final_Deposit_two
    z<-paste("Your Final Value is:",currency(Final_Value), "Your Final Deposit is:", currency(Final_Deposit_two),"For a Total Gain/Loss of:",currency(Final_Difference))
    print(z)
  })
  
  output$mort_calc_plot <- renderPlotly({
    Cost_of_Property<-input$cost
    Percent_Down_Payment<-input$down_payment
    Percent_Interest_Rate<-input$interest_rate
    Mortgage_Length<-input$length
    
    Down_Payment <- Percent_Down_Payment/100 #Enter Down Payment in %
    
    Interest_Rate <- Percent_Interest_Rate/100 #Enter the Interest Rate in %
    
    Monthly_Interest_Rate<-Interest_Rate/12
    Mortgage_Periods <- Mortgage_Length*12
    Mortgage_Amount <- Cost_of_Property-(Cost_of_Property*Down_Payment)
    Monthly_Payment <- Mortgage_Amount*((Monthly_Interest_Rate*(1+Monthly_Interest_Rate)^Mortgage_Periods)/(((1+Monthly_Interest_Rate)^Mortgage_Periods)-1))
    one_year <- ((Mortgage_Amount)-(Monthly_Payment-(Mortgage_Amount*Monthly_Interest_Rate)))
    new_year <- Mortgage_Amount
    i=0
    x <- vector(length=Mortgage_Periods)
    
    while (i < Mortgage_Periods) {
      new_year=(((new_year)-(Monthly_Payment-(new_year*Monthly_Interest_Rate))))
      x[i] <- new_year
      i = i+1
    }
    
    Results<-data.frame(x)
    Balance <- InsertRow(Results, NewRow = one_year, RowNum = 1)
    resizeBalance<-data.frame(Balance[-c(Mortgage_Periods),])
    
    
    Payment <- rep(Monthly_Payment, Mortgage_Periods)
    Principal <- c(1:(Mortgage_Periods))
    Period <- c(1:(Mortgage_Periods))
    Interest <- c(1:(Mortgage_Periods))
    Spread <- as.data.frame(cbind(Period,Payment,Principal,Interest,resizeBalance))
    colnames(Spread)[5] <- "Balance"
    Spread2 <- rbind(c(0,0,0,0,Mortgage_Amount), Spread)
    M1Spread <- Spread %>% mutate(Interest = lag(Balance, n=1L) * Monthly_Interest_Rate)
    M1Spread[1, 4] = Mortgage_Amount*Monthly_Interest_Rate
    M2Spread <- M1Spread %>% mutate(Principal = Payment - Interest)
    
    Slim_Spread <- as.data.table(cbind(M2Spread$Principal, M2Spread$Interest))
    colnames(Slim_Spread)[1] <- "Principal"
    colnames(Slim_Spread)[2] <- "Interest"
    mSpread <- cbind(melt(Slim_Spread),Period)
    plotlydat <- as.data.frame(cbind(Slim_Spread,Period,Payment))
    
    plot_ly(plotlydat, x = ~Period, y = ~Principal, type = 'bar', name = 'Principal') %>% add_trace(y = ~Interest, name = 'Interest') %>% layout(yaxis = list(title = 'Dollars'), barmode = 'stack')
  })
  
  output$mort_calc_print <- renderPrint({
    Cost_of_Property<-input$cost
    Percent_Down_Payment<-input$down_payment
    Percent_Interest_Rate<-input$interest_rate
    Mortgage_Length<-input$length
    Down_Payment <- Percent_Down_Payment/100 #Enter Down Payment in %
    Interest_Rate <- Percent_Interest_Rate/100 #Enter the Interest Rate in %
    Monthly_Interest_Rate<-Interest_Rate/12
    Mortgage_Periods <- Mortgage_Length*12
    Mortgage_Amount <- Cost_of_Property-(Cost_of_Property*Down_Payment)
    Monthly_Payment <- Mortgage_Amount*((Monthly_Interest_Rate*(1+Monthly_Interest_Rate)^Mortgage_Periods)/(((1+Monthly_Interest_Rate)^Mortgage_Periods)-1))
    q<-paste("Your Monthly Payment is",currency(Monthly_Payment), "For a total mortgage payment of", currency((Monthly_Payment*Mortgage_Periods)+(Cost_of_Property*Down_Payment)), "over",Mortgage_Length,"Years")
    print(q)
  })
  
  output$s_tri_plot <- renderPlotly({
    corn_Ax<-input$corn_ax
    corn_Bx<-input$corn_bx
    corn_Cx<-input$corn_cx
    Start_x<-input$start_x
    Start_y<-input$start_y
    corn_Ay<-input$corn_ay
    corn_By<-input$corn_by
    corn_Cy<-input$corn_cy
    its<-input$its
    color<-input$color
    
    new_point<-((Start_x+corn_Cx)/2)
    y <- as.numeric(round(runif(its, min = 1, max = 3),0))
    z <-as.data.frame(capture.output(for (val in y) {
      if(val == 3)
        new_point = ((new_point+corn_Cx)/2)
      if(val == 2)
        new_point = ((new_point+corn_Bx)/2)
      if(val == 1)
        new_point = ((new_point+corn_Ax)/2)
      print(new_point)
    }))
    colnames(z) <- c("xvals")
    cleanz = as.data.frame(as.numeric(substring(z$xvals,5)))
    colnames(cleanz)<- c("xvals")
    
    p<-as.data.frame(capture.output(for (val in y) {
      if(val == 3)
        new_point = ((new_point+corn_Cy)/2)
      if(val == 2)
        new_point = ((new_point+corn_By)/2)
      if(val == 1)
        new_point = ((new_point+corn_Ay)/2)
      print(new_point)
    }))
    colnames(p) <- c("yvals")
    cleanp<-as.data.frame(as.numeric(substring(p$yvals,5)))
    colnames(cleanp) <- c("yvals")
    
    
    graph<-as.data.frame(cbind(cleanz,cleanp))
    colnames(graph) <- c("xvals", "yvals")
    
    
    plot_ly(data = graph, x = ~xvals, y = ~yvals, type = "scatter", mode = "markers", marker = list(size = 1, color = color))
  })

  output$b_fern_plot <- renderPlotly({
    begx<-input$beg_x
    begy<-input$beg_y
    iters1<-input$iters
    
    new_pointx<-(begx*0.85)+(begy*0.04)
    new_pointy<-(begx*-0.04)+(begy*0.85)+1.6
    one <- (as.numeric(round(runif((iters1*1), min = 1, max = 1),0)))
    two <- (as.numeric(round(runif((iters1*73), min = 2, max = 2),0)))
    three <- (as.numeric(round(runif((iters1*13), min = 3, max = 3),0)))
    four <- (as.numeric(round(runif((iters1*11), min = 4, max = 4),0)))
    y<-sample(c(one,two,three,four))
    
    z<-as.data.frame(capture.output(for (val in y) {
      if(val == 1){
        new_pointx = 0
        new_pointy = (new_pointy*0.16)
        print(new_pointx)
        print(new_pointy)}
      if(val == 2){
        new_pointx = (new_pointx*0.85)+(new_pointy*0.04)
        new_pointxi=(1.17647058823529*(new_pointx))-(0.0470588235294118*new_pointy)
        new_pointy = (new_pointxi*-0.04)+(new_pointy*0.85)+1.6
        print(new_pointx)
        print(new_pointy)}
      if(val == 3){
        new_pointx = (new_pointx*-0.15)+(new_pointy*0.28)
        new_pointxi = ((-6.66667*new_pointx)+(1.86667*new_pointy))
        new_pointy = (new_pointxi*0.26)+(0.24*new_pointy)+1.6
        print(new_pointx)
        print(new_pointy)}
      if(val == 4){
        new_pointx = (new_pointx*0.2)-(new_pointy*0.26)
        new_pointxi = ((5*new_pointx)+(1.3*new_pointy))
        new_pointy = (new_pointxi*0.23)+(0.2*new_pointy)+0.44
        print(new_pointx)
        print(new_pointy)}
    }))
    colnames(z)<- c("vals")
    cleanz<-as.data.frame(as.numeric(substring(z$vals,4)))
    colnames(cleanz)<- c("vals")
    
    
    grabx<-seq(1, nrow(cleanz), by=2)
    xpoint<-as.data.frame(cleanz$vals[c(grabx)])
    colnames(xpoint) <- c("xvals")
    
    graby<-seq(2, nrow(cleanz), by=2)
    ypoint<-as.data.frame(cleanz$vals[c(graby)])
    colnames(ypoint) <- c("yvals")
    
    graph<-cbind(xpoint,ypoint)
    
    
    graph %>%
      plot_ly(
        x = ~xvals, 
        y = ~yvals,
        frame = ~iters1, 
        text = ~xvals,
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 1.75, color = "green"))

  })

  output$c_conj_plot <- renderPlotly({
    intnum<-input$int_num
    
    onenum<-as.data.frame(intnum)
    colnames(onenum)<- c("yvals")
    new_num<-if (intnum %% 2 ==0) {
      intnum/2
    } else {
      (intnum*3)+1
    }
    twonum<-as.data.frame(new_num)
    colnames(twonum)<- c("yvals")
    its<-1000
    y<-seq(1:its)
    
    z<-as.data.frame(capture.output(for (val in y) {
      if(new_num %% 2 == 0){
        new_num = ((new_num)/2)
        print(new_num)}
      if(new_num %% 2 != 0){
        new_num = ((new_num*3)+1)
        print(new_num)}
    }))
    
    
    colnames(z) <- c("yvals")
    cleanz <- as.data.frame(as.numeric(substring(z$yvals,5)))
    colnames(cleanz)<- c("yvals")
    cleanz<-as.data.frame(rbind(onenum,twonum,cleanz))
    
    df1 <- as.data.frame(cleanz[cleanz$yvals != 1, ])
    colnames(df1)<- c("yvals")
    df2 <- as.data.frame(df1[df1$yvals != 2, ])
    colnames(df2)<- c("yvals")
    df3 <- as.data.frame(df2[df2$yvals != 4, ])
    colnames(df3)<- c("yvals")
    j<-as.data.frame(as.numeric(c(4,2,1)))
    colnames(j)<- c("yvals")
    
    df4<-as.data.frame(rbind(df3,j))
    
    q<-as.data.frame(seq(1:nrow(df4)))
    colnames(q)<-c("xvals")
    
    graph<-as.data.frame(cbind(df4,q))
    
    plot_ly(data = graph, x = ~xvals, y = ~yvals, type = "scatter", mode = "lines", marker = list(size = 1, color = "black"))
  })
  
  output$c_conj_print <- renderPrint({
    intnum<-input$int_num
    
    onenum<-as.data.frame(intnum)
    colnames(onenum)<- c("yvals")
    new_num<-if (intnum %% 2 ==0) {
      intnum/2
    } else {
      (intnum*3)+1
    }
    twonum<-as.data.frame(new_num)
    colnames(twonum)<- c("yvals")
    its<-1000
    y<-seq(1:its)
    
    z<-as.data.frame(capture.output(for (val in y) {
      if(new_num %% 2 == 0){
        new_num = ((new_num)/2)
        print(new_num)}
      if(new_num %% 2 != 0){
        new_num = ((new_num*3)+1)
        print(new_num)}
    }))
    
    
    colnames(z) <- c("yvals")
    cleanz <- as.data.frame(as.numeric(substring(z$yvals,5)))
    colnames(cleanz)<- c("yvals")
    cleanz<-as.data.frame(rbind(onenum,twonum,cleanz))
    
    df1 <- as.data.frame(cleanz[cleanz$yvals != 1, ])
    colnames(df1)<- c("yvals")
    df2 <- as.data.frame(df1[df1$yvals != 2, ])
    colnames(df2)<- c("yvals")
    df3 <- as.data.frame(df2[df2$yvals != 4, ])
    colnames(df3)<- c("yvals")
    j<-as.data.frame(as.numeric(c(4,2,1)))
    colnames(j)<- c("yvals")
    
    df4<-as.data.frame(rbind(df3,j))
    
    q<-as.data.frame(seq(1:nrow(df4)))
    colnames(q)<-c("xvals")
    
    graph<-as.data.frame(cbind(df4,q))
    
    b<-paste(intnum, "falls to 1 in", nrow(graph)-1, "steps")
    print(b)
  })
}

shinyApp(ui, server)
