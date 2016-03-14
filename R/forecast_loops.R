#require(data.table)
#source('forecast_functions.R')

append.dt <- function(BI=NULL) {
     files <- list.files(path="output/",pattern="fc*")
     x <- NULL
     for(i in 1:length(files)){
          x <- rbind(x,fread(paste0("output/",files[i]),colClasses = c('Date','integer','character','character','character')))
     }
     #Ensure date type for join with listing
     x[,WK_END_DT:=as.Date(x$WK_END_DT,'%Y-%m-%d')]
     x[value==0,value:=NA]

     AvgCols <- unique(x$method)

     x <- rbind(x,validate)

     x <- dcast.data.table(x,WK_END_DT+TT_CUSTOMER+GIN ~ method,fun.aggregate = sum)

     #Straight Average All Forecast methods
     x[,mean:=round(rowMeans(.SD,na.rm=T),0),keyby=.(WK_END_DT,TT_CUSTOMER,GIN),.SDcols=AvgCols]

     x <- x[order(TT_CUSTOMER,GIN,WK_END_DT)]
     setkey(x,TT_CUSTOMER,GIN,WK_END_DT)
     setkey(tblListing,TT_CUSTOMER,GIN,WK_END_DT)
     x <- tblListing[x]

     x[!is.na(LISTED) & !is.na(mean),mean_listed:=mean*LISTED]

     ##IF provided with BI data.table, then calc Inventory Rundown
     if(!is.null(BI)){
          setkey(x,TT_CUSTOMER,GIN,WK_END_DT)
          setkey(BI,TT_CUSTOMER,GIN,WK_END_DT)
          x <- BI[x]
     }

     x[,YEAR:=year(WK_END_DT)]
     return(x)
}

allocation.forecast  <- function(train,horizon,fname,end_date){

}

group.forecast <- function(train, horizon, fname, end_date, lookback, ...){
     #Prep Forecast Output Matrix
     test <- matrix(NA,nrow=horizon,ncol=ncol(train))

     #Forecast Method Options
     FNAMES <- c('seasonal.naive',
                 'snaive.drift',
                 'naive.trackto',
                 'naive.profile',
                 'simple.ets',
                 'stlf.ets',
                 'stlf.arima',
                 'stlf.arima.xreg',
                 'fourier.arima',
                 'stlf.nn',
                 'seasonal.arima.svd',
                 'tslm.basic')

     #Validate selected fc_method
     if(fname %in% FNAMES){
          f <- get(fname)
     }else{
          stop(fname,' not legal forecast option')
     }

     #Loop through columns and run actual forecast, store in test matrix
     for(i in 1:ncol(train)) {
          #message(paste('column:',i,'of',ncol(train)))
          if(sum(tail(train[,i],4)) > 20){
               cust <- strsplit(colnames(train)[1],'_', fixed = TRUE)[[1]][2]

               test[,i] <- round(f(train[,i],h=horizon,lookback=(lookback), end_date=end_date, CUST=cust)$mean,0)
          } else {

               test[,i] <- rep(0,horizon)
          }
          #test[,i] <- round(f(train[,i],h=horizon)$mean,0)
          #print(paste(fname,colnames(train)[i]))
          #print(accuracy(test[,i],validate[,i]))
     }

     #Wipe out negatives
     test[test<0] <- 0

     test <- data.table(test)

     test_periods <- seq.Date(end_date+7,by=7,length.out = horizon)

     test <- (cbind(as.character.Date(test_periods),(test)))
     colnames(test) <- c("WK_END_DT",colnames(train))

     test <- melt.data.table(test,id.vars='WK_END_DT',variable.name='GIN_ACCT')
     test[, c("GIN", "TT_CUSTOMER") := tstrsplit((GIN_ACCT), "_", fixed=TRUE)]
     test[,GIN_ACCT:=NULL]

     test[,method:=(fname)][,END_DATE:=end_date]


     #test_periods <- train_test_periods[(nrow(train)+1):length(train_test_periods)]
     #test <- cbind(as.character.Date(test_periods),test)
     #colnames(test) <- c("WK_END_DT",colnames(train))

     #train_test <- cbind(as.character.Date(train_test_periods),year(train_test_periods),train_test)

     #write.csv(test,paste0("output/fc_",fname,".csv"),row.names = F)
     as.data.frame(test)
}

item.forecasts.cv <- function(train_test, horizon, fname, end_date, lookback, ...){
     #ar <- list(...)
     print(str(train_test))
     avg_instock <- train_test[!INSTOCK == 0,mean(INSTOCK)]
     train_test[INSTOCK == 0,INSTOCK:=avg_instock]

     cat("subsetting train set by date")
     train <- train_test[WK_END_DT <= end_date, UNITS]
     cat("converting to TS")
     train <- data.frame(UNITS=ts(train, frequency = 52))

     print("counting rows")
     n_train <- nrow(train)
     n_test <- nrow(train_test) - n_train
     cat("n_train:",n_train,"n_test",n_test,"n_train_test",nrow(train_test))

     print("Convert xreg to model.matrix")
     train_test[,WK_END_DT := NULL]
     #print(train_test)
     xreg <- model.matrix(UNITS ~ ., train_test)
     print("xreg = ")
     print(str(xreg))
     #print(xreg)
     print("Splitting out newxreg")
     newxreg <- xreg[(n_train+1):nrow(train_test),]
     print("Splitting out xreg")
     xreg <- xreg[1:n_train,]

     print("sending to item.forecasts")
     item.forecasts(train, horizon, fname, end_date, lookback=4, xreg = xreg, newxreg = newxreg, ...)
}

item.forecasts <- function(train, horizon, fname, end_date, lookback, xreg, newxreg, ...){
     #Prep Forecast Output Matrix
     if(is.data.frame(train))
          train <- train[,1]

     if('Date' %in% class(horizon))
          horizon <- as.integer(((horizon - end_date)/7))
     if(horizon < 1) {
          message("zero or negative horizon, defaulting to 52 (weeks)")
          horizon = 52
     }

     if(!is.null(newxreg)) {
          if(nrow(newxreg) > horizon)
               newxreg <- newxreg[1:horizon,]
          print(horizon)
          cat('newxreg Dims:',dim(newxreg))
          cat('xreg Dims:',dim(xreg))
     }

     if(is.numeric(horizon))
        horizon = as.integer(horizon)

     if(!('integer' %in% class(horizon)))
          stop('horizon must either be type Date or integer')

     message("horizon set to:",horizon)

     test <- matrix(NA,nrow=horizon,ncol=length(fname))

     #Forecast Method Options
     FNAMES <- c('seasonal.naive',
                 'snaive.drift',
                 'naive.trackto',
                 'naive.profile',
                 'simple.ets',
                 'stlf.ets',
                 'stlf.arima',
                 'stlf.arima.xreg',
                 'fourier.arima',
                 'stlf.nn',
                 'seasonal.arima.svd',
                 'tslm.basic')

     #Validate selected fc_method
     fname <- fname[fname %in% FNAMES]


     if(length(fname) == 0)
          stop(fname,' not legal forecast option(s)')


     #convert to list of actual functions for lapply
     ffname <- lapply(fname,get)

     Model_Store <- lapply(ffname, function(f) f(train,
                                                 h=horizon,
                                                 lookback=(lookback),
                                                 end_date=end_date,
                                                 CUST=cust,
                                                 newxreg=newxreg,
                                                 xreg=xreg, ...))
     names(Model_Store) <- fname

     ###create summary table
     test_periods <- seq.Date(end_date+7,by=7,length.out = horizon)
     consolidated <- round(sapply(Model_Store, "[[","mean"),0)
     names(Model_Store) <- fname
     consolidated <- data.frame(WK_END_DT = as.character(test_periods), consolidated)
     Model_Store[["consolidated"]] <- consolidated

     return(Model_Store)
}
