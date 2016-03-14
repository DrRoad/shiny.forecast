#source('imports.R')
#source('wos.R')
#require(forecast)

seasonal.naive <- function(x,horizon,prep=NULL, ...){

     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }
     snaive(x,h=horizon)
}

snaive.drift <- function(x,horizon,prep=NULL, ...){
     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }
     rwf(x,h=horizon,drift=TRUE)
}

simple.ets <- function(x,horizon,prep=NULL, ...){
     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }
    forecast(x,h=horizon)
}


tslm.basic <- function(train, horizon,prep=NULL, ...){
     # Computes a forecast using linear regression and seasonal dummy variables
     #
     # args:
     # train - timeseries vector of sales to forecast from
     # horizon - number of weeks to forecast forward
     #
     # returns:
     #  the test(forecast) vector

     #     train[is.na(train)] <- 0
     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }

     model <- tslm(train ~ trend + season)
     forecast(model, h=horizon)
}

stlf.ets <- function(train, horizon,prep=NULL, ...){
     #Pre Process if Requested
     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }

#Forecast
     fc <- stlf(train,
                h=horizon,
                s.window='per',
                method='ets',
                ic='bic',
                opt.crit='mae')
}
stlf.arima <- function(train, horizon,prep=NULL, ...){

     #Pre Process if Requested
     if('svd' %in% prep){
          x <- preprocess.svd(x,10)
     }

     #Forecast
     fc <- stlf(train,
                h=horizon,
                s.window=3,
                method='arima',
                ic='bic')
}

stlf.arima.xreg <- function(train, horizon,prep=NULL, xreg, newxreg, ...){

     #arguments <- list(...)
     #Pre Process if Requested
     if('svd' %in% prep){
     x <- preprocess.svd(x,10)
     }

     cat("Running Arima_Xreg...")
     #print(arguments$xreg)
     #print(arguments$newxreg)
     #Forecast
     fc <- tryCatch(stlf(train,
                h=horizon,
                s.window=3,
                method='arima',
                ic='bic',
                #xreg=xreg,
                #newxreg=newxreg)
                xreg=model.matrix(~.,xreg),
                newxreg=model.matrix(~.,newxreg)),
     error = function(e) getDummyFC("stlf.arima.xreg", train, horizon)
     )
     #message(summary(fc))

}

product <- function(train, horizon, prep=NULL, ...){
     # Computes forecasts with the product model. This model predicts the mean
     # value by store times the mean value by week divided by the mean value
     # over the department.
     #
     # args:
     # train - A matrix of Weekly_Sales values from the training set of dimension
     #         (number of weeeks in training data) x (number of stores)
     #
     # returns:
     # matrix matching Train Columns by Horizon Rows
     tr <- train[nrow(train) - (52:1) + 1,]
     tr[is.na(tr)] <- 0
     levels <- colMeans(tr,na.rm = TRUE)
     profile <- rowMeans(tr, na.rm = TRUE) / mean(levels, na.rm = TRUE)
     pred <- matrix(profile, ncol=1) %*% matrix(levels, nrow=1)
     #pred <- round(pred / overall,0)
     return(round(pred,0))
}

preprocess.svd <- function(train, n.comp){
     # Replaces the training data with a rank-reduced approximation of itself.
     # This is for noise reduction. The intuition is that characteristics
     # that are common across stores (within the same department) are probably
     # signal, while those that are unique to one store may be noise.
     #
     # args:
     # train - A matrix of Weekly_Sales values from the training set of dimension
     #         (number of weeeks in training data) x (number of stores)
     # n.comp - the number of components to keep in the singular value
     #         decomposition
     #
     # returns:
     #  the rank-reduced approximation of the training data
     train[is.na(train)] <- 0
     if(n.comp > 0) {
          z <- svd(train[, 1:ncol(train)], nu=n.comp, nv=n.comp)
          s <- diag(z$d[1:n.comp])
          #write.csv(train,"train_svd.csv")
          train[, 1:ncol(train)] <- z$u %*% s %*% t(z$v)
     }
     train
}

naive.trackto <- function(train, h, lookback=4, ...) {
     if(length(train) < 52 + lookback) {
          fc <- rep(NA,length.out = horizon)
     } else {
     ty <- train[length(train) - (lookback:1) + 1]
     ly <- train[length(train) - (lookback:1) - 52 + 1]
     trend <- sum(ty)/sum(ly)
     fc <- (train[length(train) - 52 + (1:h)])*trend
     }
     fc <- data.frame(mean=fc)
     return(fc)
}

naive.profile <- function(train, h, lookback, end_date, CUST, YEAR=2014, profileName='LRSW:ALL', ...) {

     w <- week(end_date)
     LookBackRange <- c((w-lookback+1), w)

     prof <- tblProfiles[FC_GROUP==(profileName) & POS_YEAR==(YEAR) & TT_CUSTOMER==(CUST),.(WEEK,PROFILE)][order(WEEK)]

     #Calc lookback period in Actuals from train, and matching Profile
          ty <- sum(train[length(train) - (lookback:1) + 1])
          baseline_profile <- prof[WEEK %between% LookBackRange,sum(PROFILE)]
          annual <- ty/baseline_profile

          #Loop Profile in case we wrap a calendar year
          future_profile <- rep_len(prof$PROFILE,(LookBackRange[2]+h))
          future_profile <- future_profile[(LookBackRange[2]+1):length(future_profile)]

          fc <- future_profile*annual
     #}

     #message(annual)
     fc <- data.frame(mean=fc)
     return(fc)
}


##Takes a vector and replaces leading or trailing zeros
zeroToNA <- function(x,Lead=TRUE,Tail=FALSE){
     r <- rle(x)
     if(Lead) {
          if(r$values[1] == 0 ) r$values[1] <- NA
     }
     if(Tail) {
          n <- length(r$values)
          if(r$values[n] == 0 ) r$values[n] <- NA
     }
     inverse.rle(r)
}

complete_dates <- function(x,col=NULL) {
     x <- data.frame(x)

     #Parse out date column
     if(is.null(ncol(x))) {
          dt_vector <- x
     } else {
         dt_vector <- x[,col]
         if(is.na(col)) col <- 1
         ifelse(is.character(col), dt_name <- col, dt_name <- names(x)[col])
         names(dt_vector) <- dt_name
     }

     #Error out if dates not supplied
     if(!(class(dt_vector) %in% c('Date','POSIXct','POSIXct')))
          stop('supplied vector or column is not a Date type')

     #find the existing time sequence
     seq_by <- median(as.numeric(diff(dt_vector)))

     #Create new date vector with missing dates
     new_dt_vector <- seq.Date(min(dt_vector), max(dt_vector), by = seq_by)

     if(!is.null(ncol(x))) {
          #For Data.Frame input, merge back with original data
          dt_df <- data.frame(new_dt_vector)
          names(dt_df) <- dt_name
          base::merge(dt_df, x, by = (dt_name), all = TRUE)
     } else {
          new_dt_vector
     }
}


#' Harmonic mean
#'
#' Calculate the harmonic mean of a numeric vector
#' (will return NA if there are any negative numbers in the vector)
#' @param x numeric vector
#' @param na.rm logical remove NAs prior or calculation
#' @export
#' @export
#' @return harmonic mean of vector
#' @examples
#'
#' data(nancycats)
#' pop.sizes <- table(pop(nancycats))
#' harmonic_mean(pop.sizes)
harmonic_mean <- function(x, na.rm=TRUE, bumpzero=TRUE){
     if(na.rm){
          x <- x[!is.na(x)]
     } else {
          if(any(is.na(x))) return(NA)
     }

     if(bumpzero)
          x[x<1] <- 1

     if(any(x < 0)){
          return(NA)
     }
     1/mean(1/x)
}

ttl_accuracy <- function(f, x, ...) {
     NET_ACC <- percent((1 - (abs(sum(f)-sum(x)) / sum(x))))
     ACC <- accuracy(f, x, ...)
     c(comma(round(ACC[1:3],0)),
          percent(ACC[4:5]/100),
          NET_ACC)
}

getDummyFC <- function(method, x, horizon, fill=NA) {
     fco <- list(method = method,
                 mean = rep(fill, horizon),
                 x = x)
     class(fco) <- "forecast"
     return(fco)
}
