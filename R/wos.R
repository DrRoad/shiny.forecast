#Calculate point in time forward looking weeks of supply
fwos <- function(BI,demand) {
     supply <- 1000
     demand <- c(100,200,150,250,150,200,100,100,125,35,350)
          if(length(supply)>1) stop("Supply should be a single numeric value")
          if(length(demand)<2) stop("demand should be a multi-value (weekly) vector")
     
     for(i in 1:length(demand)){
     
          if( sum(demand[1:i]) > supply ){
               b <- BI
               d <- sum(demand[1:(i-1)])
               #print(paste(b,"-",d,"=",b-d,"remaining after",i-1,"weeks with",demand[i],"remaining demand in week",i))
               #print(paste(b-d,"/",demand[i],"=",(b-d)/demand[i]))
                         return((i-1) + (b-d)/demand[i])
          }
     }
}

##Input constrained demand as well as lost sale / supply need.
##requires a data frame with a minimum of 2 columns (BI, DEMAND) and a Maximum of 4 (INDEX,BI,DEMAND,SUPPLY) in that order.
##for 3 column DF, function will try to identify if Col 1 is an index or time reference by column name,
##other assume it is BI and assign Col 3 to SUPPLY
constrainDemand <- function(df,BIoffset=FALSE,IndexName=NULL,ReturnAll=FALSE){
     
          if(nrow(df)<2) { 
               ifelse(is.null(IndexName),iname <- 'INDEX',iname<-IndexName)
               cn <- c(iname,"BI","DEMAND","DEMAND_FULLFILLED","DEMAND_LOST")
               y <- matrix(0,nrow(df),5)
               colnames(y)<-cn
               return(data.table(y))
               }
     
     #str(df)
     df <- data.table(df)
     #print(colnames(df))
     #Line up Column Names
     if (all(colnames(df) %in% c('INDEX','BI','DEMAND','SUPPLY'))){
          setcolorder(df,'INDEX','BI','DEMAND','SUPPLY')
     }
     
     if(ncol(df)==2){colnames(df) <- c('BI','DEMAND')}
     
     TimeIndexNames <- c('WK_END_DT','POS_WK_END_DATE','WEEK','WK','DATE','INDEX','YEAR','MONTH','DAY','TIME','PERIOD')
     if(ncol(df)==3){
          
               if(toupper(colnames(df)[1]) %in% TimeIndexNames){
                    
                    colnames(df) <- c('INDEX','BI','DEMAND')
               } else {
                    
                    colnames(df) <- c('BI','DEMAND','SUPPLY')
               }
     }
     if(ncol(df)==2){colnames(df) <- c('BI','DEMAND')}
     #print(colnames(df))
     ##Lag BI if requested (ie if BI is actually EI)

     ##Force NA to zero
     df[is.na(BI),BI:=0]
     df[is.na(DEMAND),DEMAND:=0]
     
     #Implement Lag for BIoffset
     if(BIoffset==TRUE) {
          df[,BI:=shift(BI,1L,type="lag")]
          df[is.na(BI),BI:=0]
     }
          #print(df)
     
     #STILL NEED TO Add code for supply
     #Temporarily store columns with Lagged values (so they are available even if you filter rows)
     df[,BILAG:=shift(BI,1L,type="lag")]     
     df[,DEMANDLAG:=shift(DEMAND,1L,type="lag")]
     
     df[,BI2:=0]
     
     #Calc first round of Missing BI       
     for(i in 2:nrow(df)){
          iRow <- (df[i])
          lastRow <- (df[i-1])
          #print(iRow)
          #print(iRow[,BI])
          if(iRow[,BI]==0) {
               #print(paste("match 0",lastRow[,BI2],lastRow[,DEMAND]))
               set(df,i,'BI2',lastRow[,BI2]-lastRow[,DEMAND])
          } else {
          #     print(paste("not 0",iRow[,BI]))
               #df[i,BI2:=iRow[,BI]]
               set(df,i,'BI2',iRow[,BI])
          }
     }
     
     
     
     #Make the Lagged Calculated BI available for second round (more than 1 hold in BI in a row)
     #df[,BI2LAG:=shift(BI2,1L,type="lag")]     
     #df[BILAG==0,BI2:=BI2LAG-DEMANDLAG]
     
     #Always use reported if available, otherwise calc
     df[BI>0,BI_CALC:=BI]
     df[BI<=0,BI_CALC:=BI2]
     
     #Move Negative BI to Supply Need, zero minimum for BI_CALC     
     #df[BI_CALC<0,SUPPLY_NEED:=-(BI_CALC)]
     df[BI_CALC<0,BI_CALC:=0]
     
     #Constrain Demand to BI
     df[,DEMAND_FULLFILLED:=DEMAND]
     df[DEMAND > BI_CALC,DEMAND_FULLFILLED:=BI_CALC]
     df[,DEMAND_LOST:=DEMAND - DEMAND_FULLFILLED]
     
     #CLEAN UP DF for export
     if(ReturnAll==FALSE) {
          #df[,BI:=BI2]
          df[,BI:=BI_CALC]
          df[,BI2:=NULL]
          df[,BI_CALC:=NULL]
          df[,BILAG:=NULL]
          df[,DEMANDLAG:=NULL]
          #setcolorder(df,'BI','DEMAND','DEMAND_FULFILLED','DEMAND_LOST')
     }
     #Rename Index if Requested
     if(!is.null(IndexName)){setnames(df,'INDEX',(IndexName))}
     #print(colnames(df))
     return(df)
}

