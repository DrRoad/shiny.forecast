#########################################
##Data Preparation and import functions##
#########################################


qryResults <- function(sql) {
     conn <- odbcDriverConnect('driver={SQL Server};server=NAEMSQL02\\SPREPORTING;database=SP_REPORTING;trusted_connection=true')
     sql <- strwrap(sql, width=10000, simplify=TRUE)
     dtbl <- data.table(sqlQuery(conn, sql,stringsAsFactors=FALSE))
     odbcClose(conn)
     return(dtbl)
}

updateToysRDS <- function(){
     x <- qryResults(sql_pos)

     tblGIN_SUBS <- qryResults(sql_gin_subs)
     x[,WK_END_DT:=as.Date(WK_END_DT)]
     tblGIN_SUBS[,GIN:=as.character(GIN)]
     tblGIN_SUBS[,GIN_TO:=as.character(GIN_TO)]
     setkey(tblGIN_SUBS,GIN)
     setkey(x,GIN)
     x <- tblGIN_SUBS[x]
     x[!is.na(GIN_TO),GIN:=GIN_TO]
     x[,GIN_TO:=NULL]

     if(nrow(x) > 52) {
          saveRDS(x,"data/toys.rds")
     }else{
          print("query problem, data not returned:")
          print(x)
     }
}

updateListingRDS <- function(){
     x <- qryResults(sql_listings)
               if(nrow(x) > 52) {
          x[,GIN:=as.character(GIN)]
          saveRDS(x,"data/listing.rds")
     }else{
          print("query problem, data not returned:")
          print(x)
     }
}



##



#Shortcut Funtions to use filterByProd to get first level Data
getLeapReaderSW <- function(DT,listedDate=NULL){
          DT <- filterByProd(DT,S2="CONTENT",S4="LEAPREADER")
          if(!is.null(listedDate)) {
               DT <- DT[GIN %in% getListedItems('2015-12-12')]
          }
          return(DT)
     }

getLeapReaderHW <- function(DT){
          DT <- filterByProd(DT,S2="PLATFORM",S4="LEAPREADER")
          return(DT)
     }

#Shortcut Funtions to use filterByProd to get first level Data
getExplorerSW <- function(DT,listedDate=NULL){
     DT <- filterByProd(DT,S2="CONTENT",S4="LEARNING LIBRARY")
     if(!is.null(listedDate)) {
          DT <- DT[GIN %in% getListedItems(listedDate)]
     }
     return(DT)
}

getExplorerHW <- function(DT){
     DT <- filterByProd(DT,S2="PLATFORM",S3=c("ELA TABLET","HANDHELD"))
     return(DT)
}

getToys <- function(DT){
     DT <- filterByProd(DT,S1="TOYS")
     return(DT)
}


getListedItems <- function(Set.Date.YMD,CUST=NULL) {
     if(is.null(CUST)) {
          (unique(tblTotal[WK_END_DT==(Set.Date.YMD) & LISTED>0,GIN]))
     }
     else {
          (unique(tblTotal[WK_END_DT==(Set.Date.YMD) & TT_CUSTOMER==CUST & LISTED>0,GIN]))
     }
}

## Filters total product data.table by product hierarchy and Customer
filterByProd <- function(DT,S1=NULL,S2=NULL,S3=NULL,S4=NULL,vCUST=NULL){
     #make sure I have a data table
     if(grep("data.table",class(DT))==0)
          return("not data table")

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

     return(copy(x))
}

##Uses Previous functions to simply generate matrix of item / cust sales values.
#includeXREG not yet implemented for ARIMA and LM
convertToMatrix <- function(DT=getLeapReaderSW(tblTotal),EndDateYMD=NULL,HistoryWks=NULL,OnlyListed=FALSE,includeXREG=FALSE, ValueCol='UNITS', TimeCol='WK_END_DT', ItemCol='GIN',CustCol='TT_CUSTOMER'){
freq = 52

     #Trim Time Length based on input.  Default to 105 weeks if no HistoryWks is given.  If no EndDate is given, use all data.
     if(!is.null(EndDateYMD)) {
          StartDateYMD <- as.Date(EndDateYMD) -
               ifelse(!is.null(HistoryWks),HistoryWks,104) * 7
                    DT <- DT[WK_END_DT >= StartDateYMD & WK_END_DT <= as.Date(EndDateYMD)]
     }

     if(OnlyListed) DT <- DT[LISTED==1]
     #Spread melted data into a matrix with a column for each Gin/Customer combination
     dcastFormula <- paste(TimeCol,"~",ItemCol,"+",CustCol)
          #Allow ignoring Gin or Customer
     if(is.null(CustCol)) dcastFormula <- paste(TimeCol,"~",ItemCol)
     if(is.null(ItemCol)) dcastFormula <- paste(TimeCol,"~",CustCol)

     #Calc Week Numbers for TimeSeries
     ts_start <- min(DT$WK_END_DT)
     ts_start <- year(ts_start) + ((week(ts_start)-1) / freq)
     ts_end <- max(DT$WK_END_DT)
     ts_end <- year(ts_end) + ((week(ts_end)-1) / freq)

          d <- dcast.data.table(DT[,.(WK_END_DT=WK_END_DT,GIN,TT_CUSTOMER,UNITS,SALES,LISTED,ASP=SALES/UNITS)],
          as.formula(dcastFormula),value.var=ValueCol,fun=sum)

          #replace NA with zero; this was for LISTED, may need to reconsider for units/sales
          d[is.na(d)] <- 0

     ts(d[,2:ncol(d),with=F],start = ts_start, end=ts_end,frequency = freq)
     }

convertToDT <- function(DT=getLeapReaderSW(),EndDateYMD=NULL,HistoryWks=NULL,OnlyListed=FALSE,includeXREG=FALSE, ValueCol='UNITS', TimeCol='WK_END_DT', ItemCol='GIN',CustCol='TT_CUSTOMER'){
     freq = 52

     #Trim Time Length based on input.  Default to 105 weeks if no HistoryWks is given.  If no EndDate is given, use all data.
     if(!is.null(EndDateYMD)) {
          StartDateYMD <- as.Date(EndDateYMD) -
               ifelse(!is.null(HistoryWks),HistoryWks,104) * 7
          DT <- DT[WK_END_DT >= StartDateYMD & WK_END_DT <= as.Date(EndDateYMD)]
     }

     if(OnlyListed) DT <- DT[LISTED==1]
     return(DT)
}
