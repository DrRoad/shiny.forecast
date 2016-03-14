#library(xts)
#library(data.table)
#source('../forecast_functions.R')
#source('../forecast_loops.R')

#rm(TOYS)
#rm(listings)
#rm(promo)
tblHOL <- fread("data/holidays.csv")
     tblHOL[,WK_END_DT:=as.Date(WK_END_DT)]
     tblHOL[,HOLIDAY:=as.factor(HOLIDAY)]
     tblHOL[,HOLIDAY:=as.factor(EASTER)]
     setkey(tblHOL,WK_END_DT)

TOYS <- readRDS("data/toys.RDS")
setkey(TOYS,WK_END_DT)
TOYS <- tblHOL[TOYS]
TOYS[is.na(HOLIDAY_LEN),HOLIDAY_LEN:=0]
TOYS[is.na(HOLIDAY),HOLIDAY:='NONE']


#TOYS[,WK_END_DT := as.Date(POS_WK_END_DT)]

#PREP LISTING DATA
listings <- copy(TOYS[!is.na(LISTED) & LISTED!=0,.(LISTED=mean(LISTED),STORECT=as.integer(round(mean(STORECT),0))),by=.(POS_YEAR,SEASON,TT_CUSTOMER,SEGMENT1,SEGMENT2,SEGMENT3,SEGMENT4,GIN)])
     listings[SEASON=="SPRING",SSN_MTH:="01"]
     listings[SEASON=="FALL",SSN_MTH:="08"]
     listings[,start_dt:=as.Date(paste0(POS_YEAR,"-",SSN_MTH,"-",1))]
     listings[SEASON=="FALL",end_dt:=start_dt+152]
     listings[SEASON=="SPRING",end_dt:=start_dt+211]

#PREP PROMO AND RAW DATA
     promo <- readRDS("data/promo.rds")
     promo[,AD:=as.factor(paste0(TT_CUSTOMER,":",PROGRAM,"-",AD_COMMENT))]
     promo[,WK_END_DT := as.Date(POS_WK_END_DT)]
     setkey(promo,TT_CUSTOMER,WK_END_DT,SEGMENT4)
     setkey(TOYS,TT_CUSTOMER,WK_END_DT,SEGMENT4)

     TOYS <- promo[TOYS]
     TOYS[,SEGMENT1:=NULL][,SEGMENT2:=NULL][,SEGMENT3:=NULL]

     setnames(TOYS,"i.SEGMENT1","SEGMENT1")
     setnames(TOYS,"i.SEGMENT2","SEGMENT2")
     setnames(TOYS,"i.SEGMENT3","SEGMENT3")

#PREP PROMO DATA
import_data <- function() {
     TOYS <- qryResults(sql)
     saveRDS(TOYS,"toys.rds")
     promo <- qryResults(sql_promo)
     saveRDS(promo,"promo.rds")
}


combineAds <- function(x) {
     y <- NULL
     for(i in x)
          if(!is.na(i))
               y <- c(y,paste0(i,";"))
     return(paste(unique(y),collapse=''))
}

filterByProd <- function(DT,S1,S2,S3,S4,SKU,vCUST=NULL){
     #make sure I have a data table
     if(grep("data.table",class(listings))==0)
          return("not data table")


     class(vCUST)
     x <- DT
          if(!is.null(vCUST))
               x <- x[TT_CUSTOMER %in% (vCUST)]

          if(!is.null(S1))
               x <- x[SEGMENT1 %in% (S1)]
          if(!is.null(S2))
               x <- x[SEGMENT2 %in% (S2)]
          if(!is.null(S3))
               x <- x[SEGMENT3 %in% (S3)]
          if(!is.null(S4))
               x <- x[SEGMENT4 %in% (S4)]
          if(!is.null(SKU))
               x <- x[GIN %in% (SKU)]
     print(str(x))
     print(SKU)
     return(x)
}


Delt <- function (x1, x2 = NULL, k = 0, type = c("arithmetic", "log"))
{
     x1 <- try.xts(x1, error = FALSE)
     type <- match.arg(type[1], c("log", "arithmetic"))
     if (length(x2) != length(x1) && !is.null(x2))
          stop("x1 and x2 must be of same length")
     if (is.null(x2)) {
          x2 <- x1
          if (length(k) < 2) {
               k <- max(1, k)
          }
     }
     dim(x2) <- NULL
     if (type == "log") {
          xx <- lapply(k, function(K.) {
               log(unclass(x2)/shift(x1, K.))
          })
     }
     else {
          xx <- lapply(k, function(K.) {
               unclass(x2)/shift(x1, K.) - 1
          })
     }
     xx <- do.call("cbind", xx)
     colnames(xx) <- paste("Delt", k, type, sep = ".")
     reclass(xx, x1)
}

mergesumts <- function(x,y) {
     s = start(x)
     f = frequency(x)
     z <- merge(zoo(x),zoo(y),fill=0)
     ts(z[,1]+z[,2],start=s,frequency = f)
}

tsannualize <-function(t,offset=0){

     ff <- data.table(cbind(year=floor(time(t)+offset),units=t))
     #zts <- cbind(rbind(ff[,.(units=round(sum(units),0)),by=.(year)][,units]))
     #zts <- rbind(as.character(zts),paste0(round(t(Delt(zts)*100),0),rep('%',length(zts))))
     #colnames(zts)<-rbind(ff[,sum(units),by=.(year)][,year])
     #rownames(zts) <- c("units","Y/Y%")
     ff[,sum(units),by=year]
}
