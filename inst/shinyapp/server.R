#library(shiny)
#library(scales)
#library(ggfortify)
#library(forecast)
#library(data.table)
#library(googleVis)
#source("../forecast_functions.R")
#source("../forecast_loops.R")

# Define server logic required to draw a histogram
#dat <- as.character(unique(TOYS$SEGMENT4))
theme_set(theme_gray(base_size = 18))

summarize_yr <- function(fcobject) {
     y <- ts(c(fcobject$x,fcobject$mean), start=start(fcobject$x), frequency=frequency(fcobject$x))

     ff <- data.table(cbind(year=floor(time(fcombined)),UNITS=fcombined))

     zts <- cbind(rbind(ff[,round(sum(UNITS),0),by=.(year)][,V1]))
     #print(zts)
     colnames(zts)<-rbind(ff[,sum(UNITS),by=.(year)][,year])
     zts
}


PROD_HIER <- unique(TOYS[,.(SEGMENT1,SEGMENT2,SEGMENT3,SEGMENT4,GIN)])
BU <- c('ALL',as.character(unique(PROD_HIER$SEGMENT1)))
BL <- c('ALL',as.character(unique(PROD_HIER$SEGMENT2)))
PL <- c('ALL',as.character(unique(PROD_HIER$SEGMENT3)))
PF <- as.character(unique(PROD_HIER$SEGMENT4))
GIN <- as.character(unique(PROD_HIER$GIN))
CUST <- as.character(unique(TOYS$TT_CUSTOMER))

shinyServer(function(input, output, session) {

     output$CUSTList <- renderUI({
          selectizeInput("vCUST", "Choose Customer(s):",
                         choices  = CUST,
                         #selected = 'ALL'
                         multiple=TRUE,
                         options=list(closeAfterSelect=TRUE))
     })

     output$BUList <- renderUI({
          selectizeInput("BU", "Choose Bus Unit:",
                      choices  = BU,
                      #selected = 'ALL'
                      multiple=TRUE,
                      options=list(closeAfterSelect=TRUE))
     })

     output$BLList <- renderUI({
          selectizeInput("BL", "Choose Bus Line:",
                      choices  = getBLlist(),
                      #selected = 'ALL'
                      multiple=TRUE,
                      options=list(closeAfterSelect=TRUE))
     })

     output$PLList <- renderUI({
          selectizeInput("PL", "Choose Prod Line:",
                      choices  = getPLlist(),
                      #selected = 'ALL'
                      multiple=TRUE,
                      options=list(closeAfterSelect=TRUE))
     })

     output$ItemList <- renderUI({
          selectizeInput("item", "Choose Prod Family:",
                      choices  = getPFlist(),
                      #selected = 'PICNIC BASKET',
                      multiple=TRUE,
                      options=list(closeAfterSelect=TRUE))
     })

     output$GINList <- renderUI({
          selectizeInput("GIN", "Choose GIN:",
                         choices  = getGINlist(),
                         #selected = 'PICNIC BASKET',
                         multiple=TRUE,
                         options=list(closeAfterSelect=TRUE))
     })

     output$item <- renderText({input$GIN})


     getBLlist <- reactive({
          # If missing input, return to avoid error later in function

          if(is.null(input$BU))
               return(BL)

          if(is.null(input$BU)) {
               return(as.character(unique(PROD_HIER$SEGMENT2)))
          }else{as.character(unique(PROD_HIER[SEGMENT1 %in% (input$BU),SEGMENT2]))}
     })

     getPLlist <- reactive({
          if(is.null(input$BL))
               return(PL)

          if(is.null(input$BU)) {
               l <- PROD_HIER
          }else{
               l <- PROD_HIER[SEGMENT1 %in% (input$BU)]}

          if(is.null(input$BL)) {
               l <- l
          }else{
               l <- l[SEGMENT2 %in% (input$BL)]}

          return(as.character(l[,SEGMENT3]))
     })

     getPFlist <- reactive({
          #if(is.null(input$BU)) {
          #     print("item is null")
          #     return(PF)}

          if(is.null(input$BU)) {l <- PROD_HIER
          }else{l <- PROD_HIER[SEGMENT1 %in% (input$BU)]}

          if(is.null(input$BL)) {l <- l
          }else{l <- l[SEGMENT2 %in% (input$BL)]}

          if(is.null(input$PL)) {l <- l
          }else{l <- l[SEGMENT3 %in% (input$PL)]}

          return(as.character(l[,SEGMENT4]))
     })

     getGINlist <- reactive({
          #if(is.null(input$BU)) {
          #     print("item is null")
          #     return(PF)}

          if(is.null(input$BU)) {l <- PROD_HIER
          }else{l <- PROD_HIER[SEGMENT1 %in% (input$BU)]}

          if(is.null(input$BL)) {l <- l
          }else{l <- l[SEGMENT2 %in% (input$BL)]}

          if(is.null(input$PL)) {l <- l
          }else{l <- l[SEGMENT3 %in% (input$PL)]}

          if(is.null(input$item)) {l <- l
          }else{l <- l[SEGMENT4 %in% (input$item)]}

          return(as.character(l[,GIN]))
     })

     rawdata <- reactive({

          ###FILTER DATA SET BY SELECTED CUSTOMER
     tt <- copy(filterByProd(TOYS, input$BU, input$BL, input$PL, input$item, input$GIN, input$vCUST))

       ###REBUILD TIME FREQUENCY
     if(input$freq=="POS_WK_NBR")
     {
          f <- 52
          tt[,PERIOD:=POS_WK_NBR]
     } else if(input$freq=="POS_MTH_NBR") {
          f <- 12
          tt[,PERIOD:=POS_MTH_NBR]
     } else {
          f <- 4
          tt[,PERIOD:=ceiling(POS_MTH_NBR/4)]
     }

     })

     raw_ts <- reactive({
          tt <- rawdata()[,.(UNITS=sum(UNITS)),by=.(POS_YEAR,PERIOD)][order(POS_YEAR,PERIOD)]
          tstart <- tt[1,c(POS_YEAR,PERIOD)]

          f <- switch(input$freq,
               "POS_WK_NBR" = 52,
               "POS_MTH_NBR" = 12,
               "POS_QTR" = 4)
          t_s <- ts(tt[,UNITS],start = tstart,frequency=f)
          if(tstart[1] < input$year_start) {t_s <- window(t_s,start=c(input$year_start,1))}
          return(t_s)
     })

     #stlobject <- reactive({
    # fcobject <- reactive({

     #     sw <- ifelse(input$csPeriodic,"per",input$s.window)

      #    s <- naive.trackto(raw_ts() #t[,"UNITS"]
      #              ,h = 52
      #              ,lookback = 13
      #              ,end_date = as.Date('2015-12-26'))
          #print(str(s))


          #s<- stl(raw_ts() #t[,"UNITS"]
          #          ,s.window=sw
          #          ,t.window=input$t.window
          #          ,robust=input$cRobust)

     #     return(s)
     #})

     fcobject <- reactive({
          prepped_data <- data.table(complete_dates(rawdata()[POS_YEAR >= input$year_start,.(UNITS=sum(UNITS),ASP=sum(SALES)/sum(UNITS),INSTOCK=mean(INSTOCK), HOLIDAY_LEN=mean(HOLIDAY_LEN)),by=.(WK_END_DT, POS_YEAR,PERIOD, HOLIDAY)][order(POS_YEAR,PERIOD)],'WK_END_DT'))
          prepped_data[is.na(prepped_data)] <- 0
          #saveRDS(prepped_data,"../data/prepped_data.rds")
          end_date <- input$actual_end #max(prepped_data$WK_END_DT)
          actuals <- prepped_data[WK_END_DT > end_date & WK_END_DT < (end_date + 365),.(WK_END_DT=as.character(WK_END_DT),ACTUALS=UNITS)]

          xreg <- prepped_data[WK_END_DT <= end_date,.(ASP,INSTOCK,HOLIDAY)]
          newxreg <- prepped_data[WK_END_DT > end_date & WK_END_DT < (end_date + 365),.(ASP,INSTOCK, HOLIDAY)]
          avg_instock <- xreg[!INSTOCK == 0,mean(INSTOCK)]
          xreg[INSTOCK == 0,INSTOCK:=avg_instock]

          ###removed for cv###
          prepped_data <- prepped_data[WK_END_DT <= end_date,UNITS]
          prepped_data <- data.frame(UNITS=ts(prepped_data, frequency = 52))

          cat("starting forecast...")
          #f <- item.forecasts.cv(prepped_data[,.(UNITS, ASP, INSTOCK, HOLIDAY),by=.(WK_END_DT)], as.Date('2016-01-02'), input$FC_LIST, lookback=4, end_date = end_date)
          f <- item.forecasts(prepped_data, as.Date('2016-01-02'), input$FC_LIST, lookback=4, end_date = end_date, xreg=xreg, newxreg=newxreg)
          #f$mean <- apply(f[,3:ncol(f)], 1, mean, na.rm = TRUE)
          #str(f)
          #str(actuals)

          base::merge(f$consolidated,actuals, by='WK_END_DT', all.x = T)


     })

     #fcobject <- reactive({
     #     #return(forecast.stl(stlobject()))
     #     return(stlobject)
     #})

     #summarize_yr(sfc)
     output$accuracy_table <- renderDataTable({

          fc <- fcobject()
          cs <- c('TOTAL',colSums(fc[,2:ncol(fc)]))
          fc_actual <- fc[,'ACTUALS']

          #Calculate Accuracy for validation
          a <- matrix(NA,7,(ncol(fc)-1))

          a_ttl <- a
          for(i in 2:ncol(fc)) {
               #a[,i] <- (cbind(names(fc)[i],round(accuracy(fc[,i],fc_actual),1)))
               x <- c(sum(fc[,i]),ttl_accuracy(fc[,i],fc_actual))

               a[,(i-1)] <- x
               #a_ttl[,i] <- (cbind(names(fc)[i],round(accuracy(sum(fc[,i]),sum(fc_actual)),1)))
          }

          #add metric names
          a <- data.table(a)
          names(a) <-  toupper(names(fc)[2:ncol(fc)])
          metricnames <- c('SUM','ME','RMSE','MAE','MPE','MAPE','TTL_ACC')
          cbind(METRIC=metricnames,a)
     }, options = list(paging = FALSE, ordering = FALSE,info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(1), type = "num-fmt")), language.thousands=","))

     output$weekly_detail <- renderDataTable({
          fc <- fcobject()
          cs <- c('TOTAL',colSums(fc[,2:ncol(fc)]))
          fc_actual <- fc[,'ACTUALS']
          weekly_detail <- (rbind(fc,cs))
     }, options = list(paging = FALSE, ordering = FALSE,info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(1), type = "num-fmt")), language.thousands=","))


          gvu <- eventReactive(input$upd,{tt <- rawdata()})

          listing <- eventReactive(input$upd,{
               l <- filterByProd(listings,input$BU,input$BL,input$PL,input$item,input$GIN)
               if(length(unique(l[,GIN]))>5) {
                    l <- l[,.(STORECT=as.character(sum(STORECT)),LISTED=as.character(round(sum(LISTED)),0)),by=.(TT_CUSTOMER,start_dt,end_dt)]
                    return(l[,barlabel:=paste0("ct:",LISTED)])
               } else {
                    return(l[,barlabel:=paste0("gin:",GIN)])
               }
          })


          output$gv <- renderGvis({
               tt <- gvu()
          if(input$chartVar=="Units by Customer") {
          #Units by Account
               tt <- tt[,.(UNITS=sum(UNITS),AD=combineAds(.SD$AD)),by=.(WK_END_DT,TT_CUSTOMER),.SDcols=c("AD","UNITS")]
               gac <- gvisAnnotationChart(tt,datevar = "WK_END_DT", numvar = "UNITS",idvar="TT_CUSTOMER",annotationvar = "AD",options=list(width="100%", height="400px"))
          }else{
          #Sales and Units Total US
               tt <- tt[,.(UNITS=sum(UNITS,na.rm=TRUE),sales=sum(sales,na.rm=TRUE),asp=(sum(sales)/sum(UNITS)),AD=combineAds(.SD$AD)),by=.(WK_END_DT),.SDcols=c("AD","UNITS","sales")]
               tt <- melt(tt,c("WK_END_DT","AD"),c("UNITS","sales","asp"))
               #clear duplicate ad annotations
               tt<- tt[variable!="UNITS",AD:=""]
               #build chart
               gac <- gvisAnnotationChart(tt,datevar = "WK_END_DT", numvar = "value",idvar="variable",annotationvar = "AD",options=list(width="100%", height="400px",scaleColumns="[0,1,2]",thickness=2,fill=0,displayAnnotationsFilter="TRUE",dateFormat="M/d/yy"))
          }

               gtl <- gvisTimeline(listing(),rowlabel="TT_CUSTOMER",barlabel="barlabel",start = "start_dt",end = "end_dt",options=list(width="100%", height="300px"))
               gvisMerge(gac,gtl,horizontal = FALSE, tableOptions = "width=\"100%\"")
               })

          output$distPlot <- renderPlot({
               fc_melt <- melt(fcobject(),id.vars='WK_END_DT',variable.name='method')
               #saveRDS(fc_melt, "../output/fc_melt.RDS")
               ggplot(fc_melt) + geom_line(aes(x=as.Date(WK_END_DT),y=value, colour = method)) +
                    #scale_y_continuous(labels=comma)
                    scale_y_log10(labels=comma)
               })

          output$SummaryPlot <- renderPlot({

               fc_melt <- melt(fcobject(),id.vars='WK_END_DT',variable.name='method')

               fc <- with(fc_melt, tapply(value, method, sum))

               fc <- data.frame(method=names(fc),value=fc/1000)
               g <- ggplot(fc) + scale_y_continuous(labels=comma)
                    g <- g + geom_bar(aes(y=value, x = method, fill = method), stat = 'identity')
                    g <- g + geom_text(aes(x = method, y = value, label = round(value), vjust = 2))
               g
          })
          #output$stlPlot <- renderPlot({
               #y <- ts(c(fcobject()$x,fcobject()$mean), start=start(fcobject()$x), frequency=frequency(fcobject()$x))
               #autoplot(stl(y,s.window="per"))
          #     autoplot(stlobject())
          #     })

})
