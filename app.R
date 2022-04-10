library(tidyverse)
library(shiny)
library(tidyquant)
library(treemap)
library(dygraphs)
library(timetk)
library(PortfolioAnalytics)
library(ggplot2)
library(bslib)
library(plotly)
library(ggplot2)
library(TTR)
library(pander)
library(RCurl)
library(RJSONIO)
library(ggtext)
library(roll)
library(RColorBrewer)
library(DT)
library(shinyvalidate)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)



header <- 
  dashboardHeader( title = HTML("Project name"), 
                   disable = FALSE, 
                   titleWidth  = 550)



siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      
      ## 1st tab show the Main dashboard -----------
      menuItem( "Input", tabName = 'input',
                icon = icon('glyphicon glyphicon-log-in	',lib = "glyphicon")),
      
      ## 2nd Second tab shows the country/region level tab --------------
      menuItem("Portfolio Management", tabName = 'portfolio',
               icon = icon('glyphicon glyphicon-piggy-bank',lib = "glyphicon")),
      
      ## 3rd tab shows commodity intel ----------
      menuItem( "World distribution", tabName = 'world',
                icon = icon('globe')),
      
      ## 5th tab Data source, definition , i.e., help ---------------
      menuItem( "Sector Peformace", tabName = 'sector',
                icon = icon('glyphicon glyphicon-stats',lib = "glyphicon") ),
      
      ## 6th tab monthly update ----------------------
      menuItem( "Portfolio Optimizer", tabName = 'optimizer',
                icon = icon('glyphicon glyphicon-education',lib = "glyphicon"),
                badgeLabel = "new", badgeColor = "green" )
    )
  )



body <- dashboardBody(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  
  tabItems(
    
    #--------------------------------------------------------------------
    # INPUT TAB
    #--------------------------------------------------------------------
    tabItem(
      tabName = 'input',
      headerPanel(title = 'Stock input'),
      
      sidebarLayout(
        sidebarPanel(
          
          headerPanel(title = 'Input pannel'),
          h6('Please add the stocks that you have in your portfolio currently'),
          
          verbatimTextOutput('ticker'),
          
          textInput('ticker', 
                    label = 'Ticker', value = 'NA'),
          
          sliderInput('slider1',
                      label = 'Zoom slider',
                      min = Sys.Date()- 10*365,
                      max = Sys.Date(),
                      value = Sys.Date()-365,
                      step = 30),
          
          dateInput('date1',
                    label = 'Date purchased',
                    value = Sys.Date()),
          
          numericInput('quantity',
                       label = 'Quantity of stock', min = 0, value = 0 ),
          
          actionButton('add_portfolio',
                       label = 'add',
                       icon = icon('glyphicon glyphicon-plus',lib = "glyphicon"),
                       class = "btn-success"),
          
          actionButton('remove_portfolio',
                       label = 'remove',
                       icon = icon('glyphicon glyphicon-minus',lib = "glyphicon"),
                       class = "btn-danger")),
        
        mainPanel(
          br(),
          plotlyOutput('stock_plot'),
          br(),
          div(DT::dataTableOutput("df_data_out"))
          
        )
      )
    ),
    
    #--------------------------------------------------------------------
    # PORTFOLIO TAB
    #--------------------------------------------------------------------
    tabItem(
      tabName = 'portfolio',
      headerPanel(title = 'Portfolio management'),
      
      sidebarLayout(
        sidebarPanel(
          
          h3('My Portfolio'),
          h4(textOutput('portfolio_value')),
          h6(htmlOutput('profit_loss_1')),
          
          sliderInput('slider2',
                      label = 'Zoom slider',
                      min = Sys.Date()- 10*365,
                      max = Sys.Date(),
                      value = Sys.Date()-365,
                      step = 30
          ),
          
          
          materialSwitch('snp500',
                         label = 'S&P 500 as benchmark'),
          
          materialSwitch('portfolio_only',
                         label = 'Only Portfolioㅤㅤㅤㅤ'),
          
          br(),
          
          h6(textOutput('portfolio_mean')),
          h6(textOutput('portfolio_sd')),
          
          h5('Portfolio allocation'),
          
          plotOutput('tree_map_portfolio_allocation'),
          
        ),
        
        #--------------------------------------------------------------------
        mainPanel(
          tabsetPanel(
            type = 'tabs',
            tabPanel('Portfolio Returns',
                     br(),
                     dygraphOutput('portfolio_returns_chart')),
            
            tabPanel('Portfolio Value',
                     br(),
                     dygraphOutput('portfolio_value_chart')),
            
            tabPanel('Risk distribution',
                     br(),
                     plotlyOutput('risk_dist'))
          ),
          h3('My portfolio'),
          div(DT::dataTableOutput("Stock_peformance"), style = "font-size: 80%")
          
          
        )
        
        
        
      )
    ),
    
    tabItem(
      tabName = 'world'
    ),
    
    
    
    #--------------------------------------------------------------------
    # SECTOR ALLOCATION
    #--------------------------------------------------------------------
    tabItem(
      tabName = 'sector',
      headerPanel(title = 'Sector Peformace'),
      sidebarLayout(
        sidebarPanel(
          h3('Sector peformance'),
          h6("Percentage tickers sma 50 above sma 200",),
          plotOutput('moving_ave')
        ),
        
        #--------------------------------------------------------------------
        mainPanel(
          plotlyOutput('sector_pie_chart'),
          div(DT::dataTableOutput("sector_allocation_table"))
          
        )
      ),
    ),
    
    
    #--------------------------------------------------------------------
    # PORTFOLIO OPTIMISATION
    #--------------------------------------------------------------------
    tabItem(
      tabName = 'optimizer',
      headerPanel(title = 'Portfolio optimizer'),
      
      sidebarLayout(
        sidebarPanel(
          h3('Optimisations'),
          h6('Please choose how you would like to optimise your portfolio'),
          
          actionButton('min_risk',
                       label = 'Minimum risk',
                       class = "btn-success"),
          
          actionButton('max_return',
                       label = 'Maximise return',
                       class = "btn-danger"),
          
          br(),
          br(),
          br(),
          br(),
          
          h4('Proposed Portfolio'),
          h6(textOutput('opti_portfolio_mean')),
          h6(textOutput('opti_portfolio_sd'))
          
        ),
        
        #--------------------------------------------------------------------
        mainPanel(
          tabsetPanel(
            tabPanel('Asset allocation', br(), plotlyOutput('piechart1')),
            tabPanel('Efficient frontier', br(), plotOutput('eff_front')),
            tabPanel('Optimised risk', br(), plotlyOutput('opti_risk'))
            
          ),
          div(DT::dataTableOutput("table1"))
          
        ),
        
        
      )
    )
  )
)

ui <- dashboardPage(header, siderbar, body )







server <- function(input, output, session) {
  
  #--------------------------------------------------------------------
  #INPUT TAB
  #--------------------------------------------------------------------
  
  #input page error validation 
  iv <- InputValidator$new()
  #input validataion for correct ticker
  iv$add_rule("ticker", function(value) {
    if (tryCatch(getSymbols(input$ticker), error = function(x){F},warning = function(x){F}) == F){
      "Please input a valid symbol"
    }
  })
  #input validation for correct quantitiy 
  iv$add_rule("quantity", function(value) {
    if (value <= 0) {
      "Quantity must be more than 1"
    }
  })
  
  iv$enable()
  
  
  #input page chart plot
  output$stock_plot <- renderPlotly({
    
    shiny::validate(
      need(input$ticker != 'NA', ""),
      need(input$quantity, "")
    )
    
    charting <- getSymbols(input$ticker, 
                           from = input$slider1,
                           to = Sys.Date(),
                           auto.assign = F)
    
    dat <- as.data.frame(charting)
    dat$date <- index(charting)
    dat <- subset(dat, date >= "2016-01-01")
    
    names(dat) <- sub(".*\\.", "", names(dat))
    
    dat
    
    
    fig <- plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open,
                   colors = c("red", "forestgreen"), hoverinfo = "none") 
    fig <- fig %>% add_segments(y = ~Low, yend = ~High, size = I(1)) 
    fig <- fig %>% add_segments(y = ~Open, yend = ~Close, size = I(3)) 
    fig <- fig %>% layout(showlegend = FALSE, title = toupper(as.character(input$ticker))) 
    fig <- fig %>% layout(yaxis = list(title = 'Price'),
                          xaxis = list(title = 'Date'))
    
    fig
    
    
  })
  
  
  #input page add stock button 
  portfolio <- reactiveValues(df_data = NULL)
  
  
  observeEvent(input$add_portfolio , {
    
    if (tryCatch(getSymbols(input$ticker), error = function(x){F},warning = function(x){F}) == F){
      return()
    }
    
    if (input$quantity == 0){
      return()
    }
    
    
    temp <- portfolio$data
    prices <- getSymbols(input$ticker, auto.assign = F)
    
    
    #weekend causing crashes 
    
    tdy <- input$date1
    day <- weekdays(tdy)
    
    if (day == 'Saturday' | day == 'Sunday') {
      showNotification("The date you chose is a weekend. The stock market is not open on weekends.
                       The purchase price used will be the closing price on Friday.",
                       type = 'warning')
    }
    
    pur_date <- case_when(day == "Saturday" ~ tdy - 1, 
                     day == "Sunday" ~ tdy - 2, 
                     TRUE ~ tdy)
    
    
    
    if (length(portfolio$data[,'ticker'])==0){
      print('1')
      
      portfolio$data <- data.frame(ticker = toupper(input$ticker),
                                         quantity = input$quantity,
                                         last_price = as.numeric(prices[dim(prices)[1],6]),
                                         purchase_date = input$date1,
                                         purchase_price = as.numeric(prices[pur_date,6][1])
                              )
      
    }
    
    else if ((toupper(input$ticker) %in% unlist(as.vector(portfolio$data[,'ticker'])))){
      print('2')
      temp <- portfolio$data
      r <- match(toupper(input$ticker),unlist(as.vector(temp[,'ticker'])))
      temp[r,'quantity'] <- temp[r,'quantity'] + input$quantity
      portfolio$data <- temp
    }
    
    else {
      print('3')
      portfolio$data <- rbind(temp,
                              data.frame(ticker = toupper(input$ticker),
                                         quantity = input$quantity,
                                         last_price = as.numeric(prices[dim(prices)[1],6]),
                                         purchase_date = input$date1,
                                         purchase_price = as.numeric(prices[pur_date,6][1])
                              )
      )
    }
    
    
    
    
    
  })
  
  
  #input page delete stock button
  observeEvent(input$remove_portfolio,{
    
    if (length(portfolio$data[,'ticker'])==0) {}
    
    else if ((toupper(input$ticker) %in% unlist(as.vector(portfolio$data[,'ticker'])))){
      temp <- portfolio$data
      temp[match(toupper(input$ticker),unlist(as.vector(portfolio$data[,'ticker']))),'quantity'] <- temp[match(toupper(input$ticker),unlist(as.vector(portfolio$data[,'ticker']))),'quantity'] - input$quantity
      
      if (temp[match(toupper(input$ticker),unlist(as.vector(portfolio$data[,'ticker']))),'quantity'] <= 0){
        temp <- temp[-match(toupper(input$ticker),unlist(as.vector(portfolio$data[,'ticker']))),]
      }
      portfolio$data <- temp
    }
    
    else {}
  })
  
  
  #Table
  output$df_data_out <- renderDataTable({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- portfolio$data %>% mutate(last_price = round(last_price, digits = 2),
                                      purchase_price = round(purchase_price, digits = 2)) %>% 
      transmute(Ticker = ticker,
                Position = quantity, 
                Last = last_price,
                "Date Purchased" = purchase_date,
                Cost = purchase_price)
    
    return(temp)
    
    })
  
  
  
  
  #--------------------------------------------------------------------
  #PORTFOLIO TAB
  #--------------------------------------------------------------------
  
  #portfolio tab value and gain
  output$portfolio_value <- renderText({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- portfolio$data
    
    out <- temp %>% transmute(weight = quantity * last_price) %>% sum(.$weight) %>%
      round(.,digits = 2) %>% as.character() %>% paste0('$', .)
    
    return(out)
    
  })
  
  
  #portfolio tab gain 
  output$profit_loss_1 <- renderText({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- portfolio$data
    
    current <- temp %>% transmute(weight = quantity * last_price) %>% sum(.$weight)
    
    past <- temp %>% transmute(weight = quantity * purchase_price) %>% sum(.$weight) 
    
    out <- current - past
    out <- out %>% round(.,digits = 2)
    
    if (out == 0) 
      return(paste0("<span style=\"color:black\">", 
                    as.character(icon('glyphicon glyphicon-triangle-right', lib = "glyphicon")),
                    as.character(out),'</span>'))
    else if (out > 0)
      return(paste0('<span style=\"color:green\">',
                    as.character(icon('glyphicon glyphicon-triangle-top',lib = "glyphicon")),
                    as.character(out), '</span>' ))
    else
      return(paste0('<span style=\"color:red\">',
                    as.character(icon('glyphicon glyphicon-triangle-bottom',lib = "glyphicon")),
                    as.character(out),'</span>' ))
    
    
    
    return(out)
  })
  
  
  #portfolio tab tree chart 
  output$tree_map_portfolio_allocation <- renderPlot({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- portfolio$data %>% mutate(investment = last_price * quantity)
    
    return(treemap(temp,
                   
                   index = 'ticker',
                   vSize = 'investment',
                   
                   title = '',
                   palette = 'Dark2',
                   
                   border.col = c("black"),             
                   border.lwds = 1,
                   
                   fontsize.labels=3,
                   fontcolor.labels="white",
                   fontface.labels=1,            
                   bg.labels=c("transparent"),              
                   align.labels=c("left", "top"),                                  
                   overlap.labels=0.5,
                   inflate.labels=T
    )
    )
  })
  
  
  #portfolio tab stock chart
  output$portfolio_value_chart <- renderDygraph({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    # merge.xts for all stocks
    ticker_name <- portfolio$data[,1] %>% as.vector()
    
    temp <- list()
    
    for (i in 1:length(ticker_name)){
      temp[[i]] <- assign(paste0('stock',as.character(i)), 
                          na.omit(getSymbols(ticker_name[i], 
                                             #change to slider 
                                             from = input$slider2,
                                             to = Sys.Date(),
                                             auto.assign = F
                          ))[,6]
      )
    }
    
    if(length(ticker_name)==1){
      portfolio_returns <- na.omit(getSymbols(ticker_name[1],
                                              #change to slider 
                                              from = input$slider2,
                                              to = Sys.Date(),
                                              auto.assign = F))[,6]}
    
    else{
      portfolio_returns <- do.call('merge.xts',temp)}
    
    
    
    
    start_capital <- portfolio$data %>% mutate(capital = last_price * quantity) %>%
      transmute(weight = capital / sum(capital)) %>% as.vector() %>% unlist()
    
    
    
    
    if (input$portfolio_only){
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, Portfolio = rowSums(port_dollar))
      port_dollar <- port_dollar[,dim(port_dollar)[2]]
    }
    
    else{
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, Portfolio = rowSums(port_dollar))
    }
    
    
    if (input$snp500){
      snp <- getSymbols('SPY',
                        from = input$slider2,
                        to = Sys.Date(),
                        auto.assign = F) [,6]
      
      port_dollar <- merge.xts(port_dollar, snp)
    }
    
    else 
      port_dollar <- port_dollar 
    
    names(port_dollar) <- sub("\\..*", "", names(port_dollar))
    
    dygraph(port_dollar,
            main = "Portfolio Value") %>% 
      dyAxis("y", label = "Price")
  })
  
  
  #portfolio tab stock returns chart
  output$portfolio_returns_chart <- renderDygraph({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    # merge.xts for all stocks
    ticker_name <- portfolio$data[,1] %>% as.vector()
    
    temp <- list()
    
    
    
    
    if(length(ticker_name)==1){
      
      portfolio_returns <- ROC(na.omit(getSymbols(ticker_name[1],
                                                  #change to slider 
                                                  from = input$slider2,
                                                  to = Sys.Date(),
                                                  auto.assign = F))[,6])
      portfolio_returns[,1][1] <- 0
      portfolio_returns[,1] <- cumsum(portfolio_returns[,1])
      
    }
    
    else{
      
      for (i in 1:length(ticker_name)){
        
        data <- na.omit(getSymbols(ticker_name[i], 
                                   #change to slider 
                                   from = input$slider2,
                                   to = Sys.Date(),
                                   auto.assign = F))[,6] 
        
        data <- ROC(data)
        
        data[,1][1] <- 0
        data[,1] <- cumsum(data[,1])
        
        
        temp[[i]] <- assign(paste0('stock',as.character(i)), data)
      }
      
      portfolio_returns <- do.call('merge.xts',temp)
      
      }
    
    
    
    
    
    start_capital <- portfolio$data %>% mutate(capital = last_price * quantity) %>%
      transmute(weight = capital / sum(capital)) %>% as.vector() %>% unlist()
    
    
    
    if (input$portfolio_only){
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, Portfolio = rowSums(port_dollar))
      port_dollar <- port_dollar[,dim(port_dollar)[2]]
    }
    
    else{
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, Portfolio = rowSums(port_dollar))
    }
    
    
    
    if (input$snp500){
      snp <- ROC(getSymbols('SPY',
                        from = input$slider2,
                        to = Sys.Date(),
                        auto.assign = F) [,6])
      snp[,1][1] <- 0
      snp[,1] <- cumsum(snp[,1])
      
      port_dollar <- merge.xts(port_dollar, snp)
    }
    
    else 
      port_dollar <- port_dollar
    
    names(port_dollar) <- sub("\\..*", "", names(port_dollar))
    
    dygraph(port_dollar,
            main = "Portfolio Returns") %>% 
      dyAxis("y", label = "Returns")
    
    
  })
  
  
  #portfolio tab profit loss table 
  output$Stock_peformance <- renderDataTable({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- portfolio$data 
    
    out <- temp %>% transmute('Symbol' = ticker,
                              'Qantity' = quantity,
                              'Position' = round(quantity * purchase_price,2),
                              'Last price' = round(last_price,2),
                              'Cost' = round(purchase_price,2),
                              'Unrealised gains' = round(last_price - purchase_price,2),
                              'Unrealised P&L' = paste0(as.character(round((last_price - purchase_price)/last_price * 100,2)),'%')
                              ) %>% 
      datatable() %>% 
      formatStyle(c('Unrealised gains','Unrealised P&L'),
                  valueColumns = 'Unrealised gains',
                  color = styleInterval(cuts = 0, values = c("red", "green")))
      
    
    return(out)
    
  })
  
  
  #portfolio tab risk distribution
  port_mean <- reactiveValues(df_data = NULL)
  port_sd <- reactiveValues(df_data = NULL)
  
  
  #portfolio tab risk distribution tab 
  output$risk_dist <- renderPlotly({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    # data 
    tickers <-  portfolio$data[,1] %>% as.vector()
    
    wts <- portfolio$data %>% mutate(capital = last_price * quantity) %>%
      transmute(weight = capital / sum(capital)) %>% as.vector() %>% unlist()
    
    #processing
    price_data <- tq_get(tickers,
                         from = Sys.Date()-720,
                         to = Sys.Date(),
                         get = 'stock.prices')
    
    ret_data <- price_data %>%
      group_by(symbol) %>%
      tq_transmute(select = adjusted,
                   mutate_fun = periodReturn,
                   period = "daily",
                   col_rename = "ret") %>%
      tq_portfolio(assets_col = symbol,
                   returns_col = ret,
                   weights = wts,
                   geometric = FALSE,
                   col_rename = 'port_ret')
    
    port_mean$data <- mean(ret_data$port_ret, na.rm = TRUE)
    port_sd$data <- sd(ret_data$port_ret, na.rm = TRUE)
    
    fig <- plot_ly(ret_data, x =~port_ret, type = 'histogram', marker = list(color = 'grey')) %>% 
      layout(title = 'Daily Portfolio Returns: 2 Years',
             yaxis = list(title = 'Portfolio Returns'),
             xaxis = list(title = 'Frequency')
             )
    
    return(fig)

  })
  
  
  #portfolio tab risk tab text 
  output$portfolio_mean <- renderText({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- port_mean$data %>% as.numeric() 
    temp <- (temp*100) %>% round(., digits = 4) %>%  as.character()  
    
    return(paste0('Portfolio returns: ', temp, '%'))
  })
  
  
  #portfolio tab standard deviation 
  output$portfolio_sd <- renderText({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    temp <- port_sd$data %>% as.numeric() 
    temp <- (temp*100) %>% round(., digits = 2) %>% as.character() 
    
    return(paste0('Portfolio standard deviation: ', temp , '%'))
  })
  
  
  
  #--------------------------------------------------------------------
  #sector allocation 
  #--------------------------------------------------------------------
  
  #sector allocation for pie chart 
  output$sector_pie_chart <- renderPlotly({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    exchange_tickers_sectors <- read_csv("https://colorado.rstudio.com/rsc/sector-labels/data.csv")
    
    stocks <- portfolio$data 
    stocks <- stocks %>% mutate(Total = quantity * last_price)

    stock.sector <- left_join(stocks, exchange_tickers_sectors, by= c("ticker"='ticker'))
    
    stock.sector[is.na(stock.sector$sector),'sector'] <- 'Other/ETF'
    
    print(stock.sector)
    
    stock.sector.num <- stock.sector %>% group_by(sector) %>%
      summarize(Num.diff.stocks = n(),
                Total.asset = round(sum(Total),2))
    
    
    #pie chart
    fig2 <- plot_ly(stock.sector.num, labels = ~sector, values = ~Total.asset, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = 'Black'),
                    hoverinfo = 'text',
                    text = ~paste('</br> Number of different stocks: ', Num.diff.stocks,
                                  '</br> Total amount: ', Total.asset),
                    marker = list(colors = brewer.pal(n = 10, name = "Pastel1")),
                    #The 'pull' attribute can also be used to create space between the sectors
                    showlegend = TRUE)
    
    fig2 <- fig2 %>% layout(title = 'Percentage of Assets per sector',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig2
    
    
  })
  
  
  #sector moving averages comparison 
  #this takes incredibly LONG 
  output$moving_ave <- renderPlot({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    withProgress(message = 'Making Plot', value = 0, {
      
      exchange_tickers_sectors <- read_csv("https://colorado.rstudio.com/rsc/sector-labels/data.csv")
      
      incProgress(0, detail = 'Retreiving data')
      
      vti_total_mkt_holdings <-
        read_csv("https://colorado.rstudio.com/rsc/vti-holdings/data.csv", 
                 col_types = cols(COUNTRY = col_skip(),
                                  `SECURITY DEPOSITORY RECEIPT TYPE` = col_skip(),
                                  SEDOL = col_skip(), X1 = col_skip())) %>%
        janitor::clean_names()
      
      incProgress(.1, detail = 'Creating dataframes')
      
      vti_prices_1_1500 <- vti_total_mkt_holdings %>%
        mutate(ticker = str_replace(ticker, "BRK.B", "BRK-B")) %>%
        slice(1:100) %>%
        pull(ticker) %>%
        tq_get(start_date = start, end_date = end)
      
      incProgress(.6, detail = 'Naming')
      
      names(vti_prices_1_1500)[names(vti_prices_1_1500) == 'symbol'] <- 'ticker'
      
      incProgress(.1, detail = 'Plotting')
      
      #--------------------------------------------------------------------
      
      chart <- vti_prices_1_1500 %>% 
        select(ticker, date, close) %>% 
        left_join(exchange_tickers_sectors %>% select(ticker, sector)) %>%  
        group_by(ticker) %>% 
        mutate(sma_50 = roll_mean(as.matrix(close), 50, complete_obs = T),
               sma_200 = roll_mean(as.matrix(close), 200, complete_obs = T),
               sma_50_greater_than_sma_200 = case_when(sma_50 > sma_200 ~ 1, 
                                                       TRUE ~ 0)) %>%
        na.omit() %>% 
        filter(date == max(date)) %>% 
        group_by(sector) %>% 
        count(sma_50_greater_than_sma_200) %>%  
        mutate(percent = n/sum(n), 
               trend = case_when(sma_50_greater_than_sma_200 == 1 ~ "sma50 above sma200", 
                                 TRUE ~ "sma50 below sma200"),
               percent_label = scales::percent(percent)) %>% 
        group_by(trend) %>% 
        # remove cash and misc sector
        filter(!(str_detect(sector, 'cash|Cash|Miscellaneous'))) %>% 
        filter(sma_50_greater_than_sma_200 == 1 ) %>%
        mutate(ordering = rank(percent, ties.method = "random"),
               percent_label = scales::percent(round(percent, 2))) %>%
        
        ggplot(aes(ordering, group = sector, color = sector,fill = sector)) +
        geom_tile(aes(y = percent/2, 
                      height = percent ,
                      width = .9), alpha = 0.9) +
        # text on top of bars
        geom_text(aes(y = percent, label =  sector ), hjust = -0.1) +
        geom_text(aes(y = percent, label =  percent_label ), color = "white", hjust = 1.2) +
        # text in x-axis (requires clip = "off" in coord_cartesian)
        coord_flip(clip = "off", expand = T)   +
        scale_y_continuous(labels=scales::percent) +
        expand_limits(y = c(.1, 1.2)) +
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
        guides(color=F,fill=F) +
        labs(x = "", y = "",
             caption = "source: tiingo, Vanguard, author calcs") +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) 
      
      incProgress(.2, detail = 'Finishing')
      chart
      
      
      
    })
  })
  
  
  #sector allocation table 
  output$sector_allocation_table <- renderDataTable({
    
    shiny::validate(
      need(dim(portfolio$data)[1] != 0, "")
    )
    
    exchange_tickers_sectors <- read_csv("https://colorado.rstudio.com/rsc/sector-labels/data.csv")
    
    stocks <- portfolio$data 
    stocks <- stocks %>% mutate(Total = quantity * last_price)

    stock.sector <- left_join(stocks, exchange_tickers_sectors, by= c("ticker"='ticker'))
    stock.sector[is.na(stock.sector$sector),'sector'] <- 'Other/ETF'
    
    stock.sector.num <- stock.sector %>% group_by(sector) %>%
      summarize(Num.diff.stocks = n(),
                Total.asset = round(sum(Total),2))
    
    stock.sector.num <- stock.sector.num %>% transmute(Sector = sector, 
                                                      Stocks = Num.diff.stocks,
                                                      Asset_value = Total.asset,
                                                      Weight = paste0(as.character(round(Total.asset/sum(Total.asset) * 100, digits = 2)), '%'))
    
    return(stock.sector.num)
    
  })
  
  
  
  
  #--------------------------------------------------------------------
  #OPTIMISATION TAB
  #--------------------------------------------------------------------
  
  
  #portfolio optimisation tab data
  optimised_port <- reactiveValues(df_data = NULL)
  
  
  #portfolio optimisation min risk button 
  observeEvent(input$min_risk, {
    
    temp <- portfolio$data 
    
    if (length(temp$ticker) <= 1 )
      return(showNotification("You cannot optimise a portfolio of just 1 stock!", type = 'error'))
    
    
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    
    stock_returns <- tq_get(list_of_tickers,
                            from = Sys.Date()-180,
                            to = Sys.Date(),
                            get = 'stock.prices') %>%
      group_by(symbol) %>% tq_transmute(select = adjusted, 
                                        mutate_fun = periodReturn,
                                        period = 'daily',
                                        col_rename = 'ret',
                                        type = 'log') %>%
      spread(symbol, value = ret) %>% tk_xts()
    
    min_var_portfolio <- PortfolioAnalytics::portfolio.spec(assets = list_of_tickers) %>%
      
      PortfolioAnalytics::add.constraint( portfolio = . ,
                                          type = 'full_investment') %>%
      
      PortfolioAnalytics::add.constraint(portfolio = .,
                                         type = "box", min = 0.05, max = 0.6) %>%
      
      PortfolioAnalytics::add.objective(portfolio = .,
                                        type = 'risk', name = 'var') %>% 
      
      PortfolioAnalytics::optimize.portfolio(R = stock_returns,
                                             portfolio = .,
                                             optimize_method = 'quadprog',
                                             trace = T)
    
    optimised_port$data <- min_var_portfolio
    
  })
  
  
  #portfolio optimistaion max return button
  observeEvent(input$max_return, {
    
    temp <- portfolio$data 
    
    if (length(temp$ticker) <= 1 )
      return(showNotification("You cannot optimise a portfolio of just 1 stock!", type = 'error'))
    
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    
    stock_returns <- tq_get(list_of_tickers,
                            from = Sys.Date()-180,
                            to = Sys.Date(),
                            get = 'stock.prices') %>%
      group_by(symbol) %>% tq_transmute(select = adjusted, 
                                        mutate_fun = periodReturn,
                                        period = 'daily',
                                        col_rename = 'ret',
                                        type = 'log') %>%
      spread(symbol, value = ret) %>% tk_xts()
    
    max_return_portfolio <- PortfolioAnalytics::portfolio.spec(assets = list_of_tickers) %>%
      
      PortfolioAnalytics::add.constraint( portfolio = . ,
                                          type = 'full_investment') %>%
      
      PortfolioAnalytics::add.constraint(portfolio = .,
                                         type = "box", min = 0.05, max = 0.6) %>%
      
      PortfolioAnalytics::add.objective(portfolio = .,
                                        type = 'return', name = 'mean') %>%
      
      PortfolioAnalytics::optimize.portfolio(R = stock_returns,
                                             portfolio = .,
                                             optimize_method = 'glpk',
                                             trace = T)
    
    optimised_port$data <- max_return_portfolio
    
  })
  
  
  #portfolio optimisation piechart
  output$piechart1 <- renderPlotly({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    temp <- optimised_port$data
    
    df <- data.frame(ticker = list_of_tickers,
                     allocation = as.vector(temp$weights))
    
    
    pie <- plot_ly(df, labels = ~ticker, values = ~allocation, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = 'Black'),
                    marker = list(colors = brewer.pal(n = 10, name = "Pastel1")),
                    #The 'pull' attribute can also be used to create space between the sectors
                    showlegend = TRUE)
    
    pie <- pie %>% layout(title = 'Optimised portfolio',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    return(pie)
    
  })
  
  
  #portfolio optimisation table
  output$table1 <- renderDataTable({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    temp <- optimised_port$data
    
    temp1 <- data.frame(ticker = list_of_tickers,
                             allocation = as.vector(temp$weights)) %>% 
      mutate(allocation = round(allocation, digits = 4)) %>%
      mutate(allocation = allocation * 100) %>%
      mutate(allocation = as.character(allocation)) %>% 
      mutate(allocation = paste0(allocation, '%'))
    
    # need to calculate how many shares to buy now 
    
    in_data <- portfolio$data 
    
    
    df <- left_join(data.frame(ticker = list_of_tickers,
                               allocation = as.vector(temp$weights)), 
                    in_data, 
                    by= c('ticker'='ticker')) %>%
      
      mutate(current_asset = last_price * quantity,
             new = sum(current_asset)*allocation/last_price,
             change = new - quantity
             )
    

    out <- data.frame(Ticker = temp1$ticker,
                      Position = in_data$quantity,
                      Allocation = temp1$allocation,
                      Optimised_weight = round(df$new, digits = 2),
                      shares_to_buy_or_sell = round(df$change, digits =2)
                      )  %>%  
      
      transmute(Ticker = Ticker,
                Position = Position,
                Allocation = Allocation,
                'Optimised Weight' = Optimised_weight,
                'Shares to Buy or Sell' = shares_to_buy_or_sell)
    
    
    out <- out %>% datatable() %>%
      formatStyle(c('Shares to Buy or Sell'),
                color = styleInterval(cuts = 0, values = c("red", "green")))
    
    return(out)
    
  })
  
  
  #portfolio optimisation efficient frontier
  output$eff_front <- renderPlot({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    annualized.moments <- function(R, scale=12, portfolio=NULL){
      out <- list()
      out$mu <-    matrix(colMeans(R), ncol=1)
      out$sigma <- cov(R)/scale
      return(out)
    }
    
    
    temp <- portfolio$data 
    
    if (length(temp$ticker) == 1 )
      return()
    
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    
    stock_returns <- tq_get(list_of_tickers,
                            from = Sys.Date()-180,
                            to = Sys.Date(),
                            get = 'stock.prices') %>%
      select(symbol, date, adjusted) %>%
      spread(symbol, value = adjusted) %>% tk_xts()
    
    
    
    prt_ef <- PortfolioAnalytics::portfolio.spec(assets = colnames(stock_returns)) %>%
      
      PortfolioAnalytics::add.constraint( portfolio = . ,
                                            type = 'long_only') %>% 
      PortfolioAnalytics::add.constraint(portfolio = .,
                                         type = "leverage") %>%
      PortfolioAnalytics::add.objective(portfolio = .,
                                        type = 'risk', name = 'StdDev') %>%
      PortfolioAnalytics:: create.EfficientFrontier(R=12*stock_returns, 
                                                    portfolio=. ,
                                                    type="mean-StdDev",
                                                    match.col = "StdDev",
                                                    momentFUN=annualized.moments,
                                                    scale=12)
    
    xlim <- range(prt_ef$frontier[,2])*c(1, 1.5)
    ylim <- range(prt_ef$frontier[,1])*c(.80, 1.05)
    
    
    chart.EfficientFrontier(prt_ef, match.col="StdDev", chart.assets = FALSE, 
                            labels.assets = FALSE, xlim=xlim, ylim=ylim )
    
    points(with(annualized.moments(12*stock_returns, scale=12), cbind(sqrt(diag(sigma)), mu)), pch=19 ) 
    
    text(with(annualized.moments(12*stock_returns, scale=12), cbind(sqrt(diag(sigma)), mu)), 
         labels=colnames(stock_returns), cex=.8, pos=4) 
    
    chart.EfficientFrontier(prt_ef, match.col="StdDev")
    
    
    
  })
  
  
  #portfolio optimised risk graph
  opti_port_mean <- reactiveValues(df_data = NULL)
  opti_port_sd <- reactiveValues(df_data = NULL)
  
  
  #portfolio optimistaion risk 
  output$opti_risk <- renderPlotly({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    # data 
    tickers <-  portfolio$data[,1] %>% as.vector()
    
    temp <- optimised_port$data
    wts <- as.vector(temp$weights)
    
    #processing
    price_data <- tq_get(tickers,
                         from = Sys.Date()-720,
                         to = Sys.Date(),
                         get = 'stock.prices')
    
    ret_data <- price_data %>%
      group_by(symbol) %>%
      tq_transmute(select = adjusted,
                   mutate_fun = periodReturn,
                   period = "daily",
                   col_rename = "ret") %>%
      tq_portfolio(assets_col = symbol,
                   returns_col = ret,
                   weights = wts,
                   geometric = FALSE,
                   col_rename = 'port_ret')
    
    opti_port_mean$data <- mean(ret_data$port_ret, na.rm = TRUE)
    opti_port_sd$data <- sd(ret_data$port_ret, na.rm = TRUE)
    
    
    fig <- plot_ly(ret_data, x =~port_ret, type = 'histogram', marker = list(color = 'grey')) %>% 
      layout(title = 'Daily Portfolio Returns: 2 Years',
             yaxis = list(title = 'Proposed Portfolio Returns'),
             xaxis = list(title = 'Frequency')
      )
    
    return(fig)
    
  })
  
  
  #portfolio tab risk tab text 
  output$opti_portfolio_mean <- renderText({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    temp <- opti_port_mean$data %>% as.numeric() 
    temp <- (temp*100) %>% round(., digits = 4) %>%  as.character()  
    
    return(paste0('Portfolio returns: ', temp, '%'))
  })
  
  
  #portfolio optimisation risk 
  output$opti_portfolio_sd <- renderText({
    
    shiny::validate(
      need(length(optimised_port$data) != 0, "")
    )
    
    temp <- opti_port_sd$data %>% as.numeric() 
    temp <- (temp*100) %>% round(., digits = 2) %>% as.character() 
    
    return(paste0('Portfolio standard deviation: ', temp , '%'))
  })
  
  
  
}



shinyApp(ui, server)

































