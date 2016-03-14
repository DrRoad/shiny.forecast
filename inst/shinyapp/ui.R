#library(shiny)
#library(forecast)
#library(data.table)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

     # Application title
     titlePanel("Seasonality Decomposition!"),

     # Sidebar with a slider input for the number of bins
     sidebarLayout(
          sidebarPanel(

               sliderInput("year_start",
                           "Start Year:",
                           min = 2011,
                           max = 2013,
                           value = 2011,
                           width = "300px")


               ,sliderInput("actual_end",
                           "End Date:",
                           min = as.Date('2013-01-05'),
                           max = as.Date('2016-02-20'),
                           value = as.Date('2015-02-28'),
                           step = 7,
                           width = "300px")

               ,selectInput("freq",
                           "Frequency:",
                           choices = c("POS_WK_NBR","POS_MTH_NBR","POS_QTR"),
                           selected = "POS_WK_NBR",
                           width = "300px")

               ,selectInput("FC_LIST",
                            "Forecast Methods:",
                            choices = c("seasonal.naive","naive.trackto","stlf.ets","tslm.basic","stlf.arima","stlf.arima.xreg"),
                            selected = c("seasonal.naive","naive.trackto","stlf.ets","tslm.basic","stlf.arima","stlf.arima.xreg"),
                            width = "300px",
                            multiple = TRUE)

               ,uiOutput("CUSTList")
               ,uiOutput("BUList")
               ,uiOutput("BLList")
               ,uiOutput("PLList")
               ,uiOutput("ItemList")
               ,uiOutput("GINList")

               ,checkboxInput("csPeriodic",
                              "SeasonWindow Periodic?",
                              value = TRUE)
               ,sliderInput("s.window",
                            "Season_window(peroids back):",
                            min = 1,
                            max = 103,
                            step = 2,
                            value = 13)


               ,sliderInput("t.window",
                           "Trend_window(peroids back):",
                           min = 1,
                           max = 103,
                           step = 2,
                           value = 13)

               ,checkboxInput("cRobust",
                            "Robust outlier detection?",
                            value = TRUE)
          ,width = 3),

          # Show a plot of the generated distribution
          mainPanel(
               tabsetPanel(
               tabPanel("History",
                    #helpText("Active Item:"),
                    textOutput("item",inline=TRUE),
                    actionButton("upd", "Update Charts"),
                    radioButtons("chartVar","Chart:",
                                 choices = c("Units by Customer","Sales & Units Total US"),inline=TRUE),

                    htmlOutput("gv"),
                    htmlOutput("gv_listed")
               ),
               tabPanel("Stat Forecast",
                    tags$head(tags$style(HTML("th {background-color: black; color: white}"))),

                    plotOutput("distPlot",width="100%"),
                    plotOutput("SummaryPlot",width="100%"),
                    #dataTableOutput("SummaryPlot"),
                    dataTableOutput("accuracy_table"),
                    dataTableOutput("weekly_detail")
                    #dataTableOutput("distTable")
               )
               )
          ,width=8)
     )
))
