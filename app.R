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
                              headerPanel(title = 'Stock input'),
                              
                              h6('Please add the stocks that you have in your portfolio currently'),
                              
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
                              
                              div(DT::dataTableOutput("df_data_out"), style = "font-size: 75%; width: 75%")
                              
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 #--------------------------------------------------------------------
                 # PORTFOLIO TAB
                 #--------------------------------------------------------------------
                 tabPanel(title = 'Portfolio',
                          headerPanel(title = 'Portfolio management'),
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              sliderInput('slider2',
                                          label = 'Zoom slider',
                                          min = Sys.Date()- 10*365,
                                          max = Sys.Date(),
                                          value = Sys.Date()-30,
                                          step = 30
                              ),
                              
                              checkboxInput('snp500',
                                            label = 'Add S&P 500 as benchmark'),
                              checkboxInput('portfolio_only',
                                            label = 'Only show portfolio line'),
                              
                              
                              h4('Portfolio Value'),
                              h5(textOutput('portfolio_value')),
                              h6(htmlOutput('profit_loss_1')),
                              
                              h5('Portfolio allocation'),
                              
                              plotOutput('tree_map_portfolio_allocation'),
                              
                            ),
                            
                            #--------------------------------------------------------------------
                            mainPanel(
                              tabsetPanel(
                                type = 'tabs',
                                tabPanel('Portfolio Value', dygraphOutput('portfolio_value_chart')),
                                tabPanel('Portfolio Returns',dygraphOutput('portfolio_returns_chart'))
                              ),
                              h3('My portfolio'),
                              div(DT::dataTableOutput("Stock_peformance"), style = "font-size: 75%; width: 75%")
                              
                              
                            )
                            
                            
                            
                          )
                 ),
                 
                 #--------------------------------------------------------------------
                 # WORLD DISTRIBUTION TAB
                 #--------------------------------------------------------------------
                 tabPanel(title = 'World distribution'),
                 
                 
                 #--------------------------------------------------------------------
                 # SECTOR PEFORMACE TAB
                 #--------------------------------------------------------------------
                 tabPanel(title = 'Sector peformace',
                          headerPanel(title = 'Portfolio optimizer'),
                          sidebarLayout(
                            sidebarPanel(
                              h3('Sector peformance'),
                              plotOutput('moving_ave')
                            ),
                            
                            #--------------------------------------------------------------------
                            mainPanel(
                              helpText('TEMP'),
                              plotlyOutput('sector_pie_chart'),
                              div(DT::dataTableOutput("sector_allocation_table"), style = "font-size: 75%; width: 75%")
                              
                            )
                          ),
                          ),
                 
                 
                 
                 #--------------------------------------------------------------------
                 # pORTFOLIO OPTIMISER TAB
                 #--------------------------------------------------------------------
                 tabPanel(title = 'Portfolio optimizer',
                          headerPanel(title = 'Portfolio optimizer'),
                          
                          sidebarLayout(
                            sidebarPanel(
                              h6('Please choose how you would like to optimise your portfolio'),
                              h6('You must have input at least 2 stocks for the optimiser to work '),
                              
                              actionButton('min_risk',
                                           label = 'Minimum risk'),
                              actionButton('max_return',
                                           label = 'Maximise return'),
                            ),
                            
                            #--------------------------------------------------------------------
                            mainPanel(
                              plotlyOutput('piechart1'),
                              div(DT::dataTableOutput("table1"), style = "font-size: 75%; width: 75%")
                              
                            ),
                            
                            
                          )
                          
                          
                          
                          
                          
                          
                          
                          
                 )
)

















server <- function(input, output, session) {
  
  #--------------------------------------------------------------------
  #INPUT TAB
  #--------------------------------------------------------------------
  
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
  output$df_data_out <- renderDataTable({
    
    temp <- portfolio$data %>% mutate(last_price = round(last_price, digits = 2),
                                      purchase_price = round(purchase_price, digits = 2))
    
    return(temp)
    
    })
  
  
  
  
  #--------------------------------------------------------------------
  #PORTFOLIO TAB
  #--------------------------------------------------------------------
  
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
    
    
    
    
    if (input$portfolio_only){
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, portfolio = rowSums(port_dollar))
      port_dollar <- port_dollar[,dim(port_dollar)[2]]
    }
    
    else{
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, portfolio = rowSums(port_dollar))
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
      
      data <- ROC(data)
      
      data[,1][1] <- 0
      data[,1] <- cumsum(data[,1])
      
      
      temp[[i]] <- assign(paste0('stock',as.character(i)), data)
    }
    
    
    
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
      portfolio_returns <- do.call('merge.xts',temp)}
    
    
    start_capital <- portfolio$data %>% mutate(capital = last_price * quantity) %>%
      transmute(weight = capital / sum(capital)) %>% as.vector() %>% unlist()
    
    
    
    if (input$portfolio_only){
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, portfolio = rowSums(port_dollar))
      port_dollar <- port_dollar[,dim(port_dollar)[2]]
    }
    
    else{
      port_dollar <- portfolio_returns * outer(rep.int(1L, nrow(portfolio_returns)),start_capital)
      port_dollar <- cbind(portfolio_returns, portfolio = rowSums(port_dollar))
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
                              ) %>% 
      datatable() %>% 
      formatStyle(c('Unrealised gains','Unrealised P&L'),
                  valueColumns = 'Unrealised gains',
                  color = styleInterval(cuts = 0, values = c("red", "green")))
      
    
    return(out)
    
  })
  
  
  
  
  
  
  
  
  
  
  #--------------------------------------------------------------------
  #sector allocation 
  #--------------------------------------------------------------------
  
  #sector allocation for pie chart 
  # VALUE IS NULL WHEN STOCK IS UNKNOWN. CHANGE TO UNKNOWN
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
  
  
  #sector moving averages comparison 
  #this takes incredibly LONG LIKE WTF
  output$moving_ave <- renderPlot({
    
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
        labs(x = "", y = "", title = "Percentage tickers sma 50 above sma 200", 
             subtitle = paste("as of", today()),
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
  
  
  output$sector_allocation_table <- renderDataTable({
    
    exchange_tickers_sectors <- read_csv("https://colorado.rstudio.com/rsc/sector-labels/data.csv")
    
    stocks <- portfolio$data 
    stocks <- stocks %>% mutate(Total = quantity * last_price)
    print(stocks)
    
    stock.sector <- inner_join(exchange_tickers_sectors, stocks, by= "ticker")
    
    stock.sector.num <- stock.sector %>% group_by(sector) %>%
      summarize(Num.diff.stocks = n(),
                Total.asset = round(sum(Total),2))
    
    stock.sector.num <- stock.sector.num %>% mutate(Sector = sector, 
                                                    'No. of stocks' = Num.diff.stocks,
                                                    'Asset value' = Total.asset,
                                                    Weight = paste0(as.character(round(Total.asset/sum(Total.asset) * 100, digits = 2)), '%'))
    
    return(stock.sector.num)
    
  })
  
  
  
  
  #--------------------------------------------------------------------
  #OPTIMISATION TAB
  #--------------------------------------------------------------------
  
  #portfolio optimisation tab data
  optimised_port <- reactiveValues(df_data = NULL)
  
  
  observeEvent(input$min_risk, {
    
    temp <- portfolio$data 
    
    if (length(temp$ticker) == 1 )
      return()
    
    
    
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
    
    temp <- portfolio$data 
    
    if (length(temp$ticker) == 1 )
      return()
    
    
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
    
    pie
    
    
    return(pie)
    
  })
  
  
  #portfolio optimisation table
  output$table1 <- renderDataTable({
    
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
    
    print(df)
    
    out <- data.frame(Ticker = temp1$ticker,
                      Position = in_data$quantity,
                      Allocation = temp1$allocation,
                      Optimised_weight = round(df$new, digits = 2),
                      shares_to_buy_or_sell = round(df$change, digits =2)
                      )
    
    
    out <- out %>% datatable() %>%
      formatStyle(c('shares_to_buy_or_sell'),
                color = styleInterval(cuts = 0, values = c("red", "green")))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}



shinyApp(ui, server)

































