#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Crypto Currency Market Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetOne", "Choose the Dataset",
                  list("BitCoin" = "btc", 
                       "Ethereum" = "eth",
                       "XRP" = "xrp",
                       "BitCoin Cash" = "bcc",
                       "EOS" = "eos",
                       "LiteCoin" = "lc",
                       "Binance Coin" = "bc",
                       "Tether" = "te",
                       "Stellar" = "ste",
                       "Cardano" = "car",
                       "TRON" = "tr",
                       "Monero" = "mone",
                       "Dash" = "dash",
                       "IOTA" = "iota",
                       "Tezos" = "tezo",
                       "NEO" = "neo",
                       "Ethereum Classic" = "etc",
                       "Maker" = "mak",
                       "Ontology" = "onto",
                       "NEM" = "nem")),
      tags$br(),
      numericInput("horizon", "Forecast Horizon (Days)", 60),
      selectInput("datasetTwo", "Choose this Data set only for comparison",
                  list("BitCoin" = "btc", 
                       "Ethereum" = "eth",
                       "XRP" = "xrp",
                       "BitCoin Cash" = "bcc",
                       "EOS" = "eos",
                       "LiteCoin" = "lc",
                       "Binance Coin" = "bc",
                       "Tether" = "te",
                       "Stellar" = "ste",
                       "Cardano" = "car",
                       "TRON" = "tr",
                       "Monero" = "mone",
                       "Dash" = "dash",
                       "IOTA" = "iota",
                       "Tezos" = "tezo",
                       "NEO" = "neo",
                       "Ethereum Classic" = "etc",
                       "Maker" = "mak",
                       "Ontology" = "onto",
                       "NEM" = "nem")),
      submitButton("Update Chart")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel(title = "Opening Price Forecast", value = "O", tags$br(),plotOutput("openForecastPlot"),tags$h3("Parameters"),tableOutput('open_parameters'),tags$h3("Validation Results"),tableOutput('open_validation')), 
        tabPanel(title = "Closing Price Forecast", value = "C", tags$br(),plotOutput("closeForecastPlot"),tags$h3("Parameters"),tableOutput('close_parameters'),tags$h3("Validation Results"),tableOutput("close_validation")),
        tabPanel(title = "Compare Two Prices",tags$h3("Comparison between Opening Price"),splitLayout(plotOutput("openForecastPlot_cmp"),plotOutput("openForecastPlottwo")),tags$h3("Comparison between Closing Price"),splitLayout(plotOutput("closeForecastPlot_cmp"),plotOutput("closeForecastPlottwo")))
      )
    )
  )
))
