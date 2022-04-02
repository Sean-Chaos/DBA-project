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
library(gganimate)
library(gifski)
library(png)
library(RColorBrewer)
library(DT)


ui <- navbarPage(title = 'Portfolio manager',
                 theme = bs_theme(version = 4, bootswatch = 'sandstone'),
                 tabPanel(title = 'Stock input',
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              helpText('Please add the stocks that you have in your portfolio currently'),
                              
                              verbatimTextOutput('ticker'),
                              
                              textInput('ticker', 
                                        label = 'Ticker', value = 'NA'),
                              
                              sliderInput('slider1',
                                          label = 'Zoom slider',
                                          min = Sys.Date()- 10*365,
                                          max = Sys.Date(),
                                          value = Sys.Date()-30,
                                          step = 30),
                              
                              dateInput('date1',
                                        label = 'Date purchased',
                                        value = Sys.Date()-1),
                              
                              numericInput('quantity',
                                           label = 'Quantity of stock', min = 0, value = 0 ),
                              
                              actionButton('add_portfolio',
                                           label = 'Add stock to portfolio'),
                              
                              
                              actionButton('remove_portfolio',
                                           label = 'Remove stock from portfolio')
                              
                              
                              
                            ),
                            
                            
                            mainPanel(
                              dygraphOutput('stock_plot'),
                              
                              tableOutput('df_data_out')
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel(title = 'Portfolio',
                          headerPanel(title = 'Portfolio management'),
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              helpText('LOL'),
                              
                              sliderInput('slider2',
                                          label = 'Zoom slider',
                                          min = Sys.Date()- 10*365,
                                          max = Sys.Date(),
                                          value = Sys.Date()-30,
                                          step = 30
                              ),
                              
                              checkboxInput('snp500',
                                            label = 'Add S&P 500 as benchmark'),
                              
                              
                              helpText('Portfolio Value'),
                              textOutput('portfolio_value'),
                              htmlOutput('profit_loss_1'),
                              
                              
                              
                              helpText('This weeks biggest movers'),
                              
                              
                              
                              
                              
                              helpText('Portfolio allocation'),
                              
                              plotOutput('tree_map_portfolio_allocation'),
                              
                            ),
                            
                            
                            mainPanel(
                              tabsetPanel(
                                type = 'tabs',
                                tabPanel('Portfolio Value', dygraphOutput('portfolio_value_chart')),
                                tabPanel('Portfolio Returns',dygraphOutput('portfolio_returns_chart'))
                              ),
                              h3('My portfolio'),
                              DT::dataTableOutput('Stock_peformance')
                              
                              
                            )
                            
                            
                            
                          )
                 ),
                 
                 
                 
                 
                 tabPanel(title = 'World distribution'),
                 
                 
                 
                 
                 tabPanel(title = 'Sector peformace',
                          headerPanel(title = 'Portfolio optimizer'),
                          sidebarLayout(
                            sidebarPanel(
                              helpText('TEMP')
                            ),
                            
                            mainPanel(
                              helpText('TEMP'),
                              plotlyOutput('sector_pie_chart')
                            )
                          ),
                          ),
                 
                 
                 
                 
                 
                 tabPanel(title = 'Portfolio optimizer',
                          headerPanel(title = 'Portfolio optimizer'),
                          
                          sidebarLayout(
                            sidebarPanel(
                              helpText('Please choose how you would like to optimise your portfolio'),
                              actionButton('min_risk',
                                           label = 'Minimum risk'),
                              actionButton('max_return',
                                           label = 'Maximise return'),
                              
                            ),
                            
                            mainPanel(
                              plotOutput('piechart1'),
                              tableOutput('table1'),
                              
                            ),
                            
                            
                          )
                          
                          
                          
                          
                          
                          
                          
                          
                 )
)

















server <- function(input, output, session) {
  
  #INPUT TAB
  
  #input page chart plot
  output$stock_plot <- renderDygraph({
    charting <- getSymbols(input$ticker, 
                           from = input$slider1,
                           to = Sys.Date(),
                           auto.assign = F)[,6]
    dygraph(charting)
    
  })
  
  
  #input page add stock button 
  portfolio <- reactiveValues(df_data = NULL)
  
  
  observeEvent(input$add_portfolio , {
    
    if (tryCatch(getSymbols(input$ticker), error = function(x){F},warning = function(x){F}) == F) 
      {return()}
    
    temp <- portfolio$data
    prices <- getSymbols(input$ticker, auto.assign = F)
    
    
    
    if (length(portfolio$data[,'ticker'])==0){
      print('1')
      
      portfolio$data <- data.frame(ticker = toupper(input$ticker),
                                         quantity = input$quantity,
                                         last_price = as.numeric(prices[dim(prices)[1],6]),
                                         purchase_date = input$date1,
                                         purchase_price = as.numeric(prices[input$date1,6][1])
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
                                         purchase_price = as.numeric(prices[input$date1,6][1])
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
  
  
  #KEEP BUT MAKE IT LOOK BETTER
  output$df_data_out <- renderTable(portfolio$data)
  
  
  
  
  
  #PORTFOLIO TAB
  
  #portfolio tab value and gain
  output$portfolio_value <- renderText({
    temp <- portfolio$data
    
    out <- temp %>% transmute(weight = quantity * last_price) %>% sum(.$weight) %>%
      round(.,digits = 2) %>% as.character()
    return(out)
    
  })
  
  #portfolio tab gain 
  output$profit_loss_1 <- renderText({
    
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
    
    port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
    
    port_dollar <- cbind(portfolio_returns, portfolio = rowSums(port_dollar))
    
    if (input$snp500){
      snp <- getSymbols('SPY',
                        from = input$slider2,
                        to = Sys.Date(),
                        auto.assign = F) [,6]
      
      port_dollar <- merge.xts(port_dollar, snp)
    }
    
    else 
      port_dollar <- port_dollar 
    
    
    dygraph(port_dollar)
    
  })
  
  #portfolio tab stock returns chart
  output$portfolio_returns_chart <- renderDygraph({
    
    # merge.xts for all stocks
    ticker_name <- portfolio$data[,1] %>% as.vector()
    
    temp <- list()
    
    for (i in 1:length(ticker_name)){
      
      data <- na.omit(getSymbols(ticker_name[i], 
                                 #change to slider 
                                 from = input$slider2,
                                 to = Sys.Date(),
                                 auto.assign = F))[,6] 
      
      print(data[,1])
      ## error is here
      data$change <- c(-diff(data[,1])/data[,1][-1] *  100, NA)
      
      print(data)
      
      temp[[i]] <- assign(paste0('stock',as.character(i)), data)
    }
    
    print(temp)
    
    if(length(ticker_name)==1){
      portfolio_returns <- na.omit(getSymbols(ticker_name[1],
                                              #change to slider 
                                              from = input$slider2,
                                              to = Sys.Date(),
                                              auto.assign = F))[,6]}
    
    else{
      portfolio_returns <- do.call('merge.xts',temp)}
    
    
    if (input$snp500){
      snp <- getSymbols('SPY',
                        from = input$slider2,
                        to = Sys.Date(),
                        auto.assign = F) [,6]
      
      port_dollar <- merge.xts(portfolio_returns, snp)
    }
    
    else 
      port_dollar <- portfolio_returns 
    
    print(port_dollar)
    
    
    dygraph(port_dollar)
    
    
  })
  
  #portfolio tab profit loss table 
  output$Stock_peformance <- renderDataTable({
    
    temp <- portfolio$data 
    
    out <- temp %>% transmute('Symbol' = ticker,
                              'Qantity' = quantity,
                              'Position' = round(quantity * purchase_price,2),
                              'Last price' = round(last_price,2),
                              'Cost' = round(purchase_price,2),
                              'Unrealised gains' = round(last_price - purchase_price,2),
                              'Unrealised P&L' = paste0(as.character(round((last_price - purchase_price)/last_price * 100,2)),'%')
                              )
    
    return(out)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  #sector allocation 
  
  output$sector_pie_chart <- renderPlotly({
    
    exchange_tickers_sectors <- read_csv("https://colorado.rstudio.com/rsc/sector-labels/data.csv")
    
    stocks <- portfolio$data 
    stocks <- stocks %>% mutate(Total = quantity * last_price)
    print(stocks)
    
    stock.sector <- inner_join(exchange_tickers_sectors, stocks, by= "ticker")
    
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
  
  
  
  
  
  
  
  
  #OPTIMISATION TAB
  
  #portfolio optimisation tab data
  # CRASHES WHEN ONLY 1 STOCK TO OPTIMISE 
  optimised_port <- reactiveValues(df_data = NULL)
  
  
  observeEvent(input$min_risk, {
    
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
  
  observeEvent(input$max_return, {
    
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
  output$piechart1 <- renderPlot({
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    temp <- optimised_port$data
    
    allocation <- data.frame(ticker = list_of_tickers,
                             allocation = as.vector(temp$weights))
    
    print(allocation)
    
    pie <- ggplot(allocation, aes(x='', y=allocation, fill=ticker)) + 
      geom_bar(stat = 'identity', width = 1) +
      coord_polar('y', start = 0) +
      theme_void()
    
    return(pie)
    
  })
  
  
  #portfolio optimisation table
  output$table1 <- renderTable({
    
    list_of_tickers <- portfolio$data[,1] %>% as.vector()
    temp <- optimised_port$data
    
    allocation <- data.frame(ticker = list_of_tickers,
                             allocation = as.vector(temp$weights)) %>% 
      mutate(allocation = round(allocation, digits = 4)) %>%
      mutate(allocation = allocation * 100) %>%
      mutate(allocation = as.character(allocation)) %>% 
      mutate(allocation = paste0(allocation, '%'))
    
    allocation
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}



shinyApp(ui, server)

































