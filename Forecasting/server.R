#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  cryptoData <- read.csv('crypto-markets.csv')
  
  library(dplyr)
  library(xts)
  library(forecast)
  
  #data
  bitcoinData <- select(cryptoData[which(cryptoData$name == 'Bitcoin'),],symbol,date,open,close)
  ethereumData <- select(cryptoData[which(cryptoData$name == 'Ethereum'),],symbol,date,open,close)
  xrpData <- select(cryptoData[which(cryptoData$name == 'XRP'),],symbol,date,open,close)
  bitcoinCashData <- select(cryptoData[which(cryptoData$name == 'Bitcoin Cash'),],symbol,date,open,close)
  eosData <- select(cryptoData[which(cryptoData$name == 'EOS'),],symbol,date,open,close)
  litecoinData <- select(cryptoData[which(cryptoData$name == 'Litecoin'),],symbol,date,open,close)
  binanceCoinData <- select(cryptoData[which(cryptoData$name == 'Binance Coin'),],symbol,date,open,close)
  tetherData <- select(cryptoData[which(cryptoData$name == 'Tether'),],symbol,date,open,close)
  stellarData <- select(cryptoData[which(cryptoData$name == 'Stellar'),],symbol,date,open,close)
  cardanoData <- select(cryptoData[which(cryptoData$name == 'Cardano'),],symbol,date,open,close)
  tronData <- select(cryptoData[which(cryptoData$name == 'TRON'),],symbol,date,open,close)
  moneroData <- select(cryptoData[which(cryptoData$name == 'Monero'),],symbol,date,open,close)
  dashData <- select(cryptoData[which(cryptoData$name == 'Dash'),],symbol,date,open,close)
  iotaData <- select(cryptoData[which(cryptoData$name == 'IOTA'),],symbol,date,open,close)
  tezosData <- select(cryptoData[which(cryptoData$name == 'Tezos'),],symbol,date,open,close)
  neoData <- select(cryptoData[which(cryptoData$name == 'NEO'),],symbol,date,open,close)
  ethclassicData <- select(cryptoData[which(cryptoData$name == 'Ethereum Classic'),],symbol,date,open,close)
  makerData <- select(cryptoData[which(cryptoData$name == 'Maker'),],symbol,date,open,close)
  ontoData <- select(cryptoData[which(cryptoData$name == 'Ontology'),],symbol,date,open,close)
  nemData <- select(cryptoData[which(cryptoData$name == 'NEM'),],symbol,date,open,close)
  
  #converting data format
  bitcoinData$date <- as.Date(bitcoinData$date)
  ethereumData$date <- as.Date(ethereumData$date)
  xrpData$date <- as.Date(xrpData$date)
  bitcoinCashData$date <- as.Date(bitcoinCashData$date)
  eosData$date <- as.Date(eosData$date)
  litecoinData$date <- as.Date(litecoinData$date)
  binanceCoinData$date <- as.Date(binanceCoinData$date)
  tetherData$date <- as.Date(tetherData$date)
  stellarData$date <- as.Date(stellarData$date)
  cardanoData$date <- as.Date(cardanoData$date)
  tronData$data <- as.Date(tronData$date)
  moneroData$date <- as.Date(moneroData$date)
  dashData$date <- as.Date(dashData$date)
  iotaData$date <- as.Date(iotaData$date)
  tezosData$date <- as.Date(tezosData$date)
  neoData$date <- as.Date(neoData$date)
  ethclassicData$date <- as.Date(ethclassicData$date)
  makerData$date <- as.Date(makerData$date)
  ontoData$date <- as.Date(ontoData$date)
  nemData$date <- as.Date(nemData$date)
  
  getHorizon <- reactive({ input$horizon })
  
  #Dataset selection for first dropdown
  getDatasetOne <- reactive({
    if (input$datasetOne == "btc") { return(bitcoinData) }
    else if (input$datasetOne == "eth") { return(ethereumData) }
    else if (input$datasetOne == "xrp") { return(xrpData) }
    else if (input$datasetOne == "bcc") { return(bitcoinCashData) }
    else if (input$datasetOne == "eos") { return(eosData) }
    else if (input$datasetOne == "lc") { return(litecoinData) }
    else if (input$datasetOne == "bc") { return(binanceCoinData) }
    else if (input$datasetOne == "te") { return(tetherData) }
    else if (input$datasetOne == "ste") { return(stellarData) }
    else if (input$datasetOne == "tr") { return(tronData) }
    else if (input$datasetOne == "mone") { return(moneroData) }
    else if (input$datasetOne == "dash") { return(dashData) }
    else if (input$datasetOne == "iota") { return(iotaData) }
    else if (input$datasetOne == "tezo") { return(tezosData) }
    else if (input$datasetOne == "neo") { return(neoData) }
    else if (input$datasetOne == "etc") { return(ethclassicData) }
    else if (input$datasetOne == "mak") { return(makerData) }
    else if (input$datasetOne == "onto") { return(ontoData) }
    else if (input$datasetOne == "nem") { return(nemData) }
    else { return(cardanoData) } 
  })
  
  #Dataset selection for second dropdown
  getDatasetTwo <- reactive({
    if (input$datasetTwo == "btc") { return(bitcoinData) }
    else if (input$datasetTwo == "eth") { return(ethereumData) }
    else if (input$datasetTwo == "xrp") { return(xrpData) }
    else if (input$datasetTwo == "bcc") { return(bitcoinCashData) }
    else if (input$datasetTwo == "eos") { return(eosData) }
    else if (input$datasetTwo == "lc") { return(litecoinData) }
    else if (input$datasetTwo == "bc") { return(binanceCoinData) }
    else if (input$datasetTwo == "te") { return(tetherData) }
    else if (input$datasetTwo == "ste") { return(stellarData) }
    else if (input$datasetTwo == "tr") { return(tronData) }
    else if (input$datasetTwo == "mone") { return(moneroData) }
    else if (input$datasetTwo == "dash") { return(dashData) }
    else if (input$datasetTwo == "iota") { return(iotaData) }
    else if (input$datasetTwo == "tezo") { return(tezosData) }
    else if (input$datasetTwo == "neo") { return(neoData) }
    else if (input$datasetTwo == "etc") { return(ethclassicData) }
    else if (input$datasetTwo == "mak") { return(makerData) }
    else if (input$datasetTwo == "onto") { return(ontoData) }
    else if (input$datasetTwo == "nem") { return(nemData) }
    else { return(cardanoData) } 
  })
  
  #computing ets
  open <- reactive({
    Open <- xts(getDatasetOne()$open, order.by=as.Date(getDatasetOne()$date))
    Open <-window(Open, start = '2017-11-30', end = '2018-11-29')
    open_etsFit <- ets(Open, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    return(open_etsFit)
  })
  
  close <- reactive({
    Close <- xts(getDatasetOne()$close, order.by=as.Date(getDatasetOne()$date))
    Close <-window(Close, start = '2017-11-30', end = '2018-11-29')
    close_etsFit <- ets(Close, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    return(close_etsFit)
  })
  
  open_two <- reactive({
    Open <- xts(getDatasetTwo()$open, order.by=as.Date(getDatasetTwo()$date))
    Open <-window(Open, start = '2017-11-30', end = '2018-11-29')
    open_etsFit <- ets(Open, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    return(open_etsFit)
  })
  
  close_two <- reactive({
    Close <- xts(getDatasetTwo()$close, order.by=as.Date(getDatasetTwo()$date))
    Close <-window(Close, start = '2017-11-30', end = '2018-11-29')
    close_etsFit <- ets(Close, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    return(close_etsFit)
  })
  
  # Plot the forecast plot for Open Price
  output$openForecastPlot <- renderPlot({
    tsDataFC <- forecast(open(), h = getHorizon())
    autoplot(tsDataFC, xlab = 'Time', ylab = 'Price')
  })
  
  # Plot the forecast plot for Close Price
  output$closeForecastPlot <- renderPlot({
    tsDataFC <- forecast(close(), h = getHorizon())
    autoplot(tsDataFC, xlab = 'Time', ylab = 'Price')
  })
  
  # Plot the forecast plot for Open Price for comparison
  output$openForecastPlot_cmp <- renderPlot({
    tsDataFC <- forecast(open(), h = getHorizon())
    autoplot(tsDataFC, xlab = 'Time', ylab = 'Price')
  })
  
  # Plot the forecast plot for Close Price for comparison
  output$closeForecastPlot_cmp <- renderPlot({
    tsDataFC <- forecast(close(), h = getHorizon())
    autoplot(tsDataFC, xlab = 'Time', ylab = 'Price')
  })
  
  # Plot the forecast plot for Open Price for comparison
  output$openForecastPlottwo <- renderPlot({
    tsDataFCtwo <- forecast(open_two(), h = getHorizon())
    autoplot(tsDataFCtwo, xlab = 'Time', ylab = 'Price')
  })
  
  # Plot the forecast plot for Close Price for comparison
  output$closeForecastPlottwo <- renderPlot({
    tsDataFCtwo <- forecast(close_two(), h = getHorizon())
    autoplot(tsDataFCtwo, xlab = 'Time', ylab = 'Price')
  })
  
  #parameters
  output$open_parameters <- renderTable({
    open()$par
  }, rownames = TRUE)
  
  output$close_parameters <- renderTable({
    close()$par
  }, rownames = TRUE)
  
  #cross-validation
  output$open_validation <- renderTable({
    Open <- xts(getDatasetOne()$open, order.by=as.Date(getDatasetOne()$date))
    TrainData <- window(Open, start = '2017-11-30', end = '2018-08-26')
    TestData <-  window(Open, start = '2018-08-27', end = '2018-10-25')
    
    open_etsFit <- ets(TrainData, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    open_tsDataFC <- forecast(open_etsFit, h = getHorizon())
    
    open_acc <- accuracy(open_tsDataFC,TestData)
    open_acc
  }, rownames = TRUE)
  
  output$close_validation <- renderTable({
    Close <- xts(getDatasetOne()$close, order.by=as.Date(getDatasetOne()$date))
    TrainData <- window(Close, start = '2017-11-30', end = '2018-08-26')
    TestData <-  window(Close, start = '2018-08-27', end = '2018-10-25')
    
    open_etsFit <- ets(TrainData, model = 'ZZZ',damped = FALSE, na.action = "na.interp")
    open_tsDataFC <- forecast(open_etsFit, h = getHorizon())
    
    close_acc <- accuracy(open_tsDataFC,TestData)
    close_acc
  }, rownames = TRUE)
  
})
