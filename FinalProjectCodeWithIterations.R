library(rvest)
library(plotrix)
library(dplyr)

Sys.Date()
endDate=gsub("-","",Sys.Date())
endDate

#Bitcoin Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "BitcoinData"
head(BitcoinData)

#Litecoin Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/litecoin/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "LitecoinData"
head(LitecoinData)

#Ethereum Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/ripple/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "EthereumData"
head(EthereumData)

#considering only fisrt 5 columns in the data
BData<- BitcoinData[,1:5]
LData<- LitecoinData[,1:5]
EData<-EthereumData[1:5]


BData$Change<- (BData$Close-BData$Open)/BData$Open
LData$Change<- (LData$Close-LData$Open)/LData$Open
EData$Change<- (EData$Close-EData$Open)/EData$Open
#Combining all the Change values into one new data frame
DailyReturns <- data.frame(BDate=BData$Date, BCoin=BData$Change,
                           LDate=LData$Date, LCoin=LData$Change, EDate=EData$Date,
                           ECoin=EData$Close)

#Declaring three new functions to pick the change values
getBCoinReturn = function(PickRow) {
  DailyReturns$BCoin[PickRow]
}
getLCoinReturn = function(PickRow) {
  DailyReturns$LCoin[PickRow]
}
getECoinReturn = function(PickRow) {
  DailyReturns$ECoin[PickRow]
}


#eAmt1 for BitCoin Data
#eAmt2 for LiteCoin Data
#eAmt3 for Ethereum Data


#Bootstrapping for 1 years data
bAmt = 1000

investment=data.frame(day=0, PickRow = 0,
                      rBitCoin = 0, PercentBitCoin=0,  rLiteCoin = 0,
                      PercentLiteCoin=0, rECoin = 0, PercentRippleCoin=0, eAmt1 = 0, eAmt2 = 0, eAmt3 = 0,
                      eAmtFinal= 0)
investment=investment[0,]

eAmt = data.frame(eAmt=0, i=0, j=0, k=0, rBCoin=0, rLCoin=0, rECoin=0, eAmt1=0, eAmt2=0, eAmt3=0, eAmtFinal=0)

for(i in seq(0,100,5)){
  #rBCoin<-rBCoin*(i/100)
  for(j in seq(0,(100-i),5)){
    #rLCoin<-rLCoin*(j/100)
    #rECoin<- rECoin*(100-j-i)/100
    k=100-j-i
   
    eAmt = eAmt[0,]
    
    for(itrn in c(1:100)){
      
      PickRow = sample(1:nrow(DailyReturns),1)
      rBCoin = getBCoinReturn(PickRow)
      rLCoin = getLCoinReturn(PickRow)
      rECoin = getECoinReturn(PickRow)
      
      eAmt1 = (bAmt*i/100) * (1 + rBCoin)
      eAmt2 = (bAmt*j/100) * (1 + rLCoin)
      eAmt3 = (bAmt*k/100) * (1 + rECoin)
      eAmtFinal= eAmt1+eAmt2+eAmt3
      
      eAmt = rbind(eAmt,data.frame(bAmt=bAmt, i=i, j=j, k=k, rBCoin=rBCoin, rLCoin=rLCoin, rECoin=rECoin, eAmt1=eAmt1, eAmt2=eAmt2, eAmt3=eAmt3, eAmtFinal=eAmtFinal))
      
      
    }
    meaneAmtFinal = mean(eAmt$eAmtFinal)  
    mean_eAmt1=mean(eAmt$eAmt1)
    mean_eAmt2=mean(eAmt$eAmt2)
    mean_eAmt3=mean(eAmt$eAmt3)
    
    investment=rbind(investment,data.frame(day=1, PickRow = PickRow,
                                           rBitCoin = rBCoin,PercentBitCoin=i/100, rLiteCoin = rLCoin,
                                           PercentLiteCoin=j/100, rECoin = rECoin,
                                           PercentRippleCoin=(100-j-i)/100, eAmt1 = mean_eAmt1, eAmt2 = mean_eAmt2,
                                           eAmt3 = mean_eAmt3, eAmtFinal= meaneAmtFinal) )
    
  }
}

combination=1
for(d in c(2:3)){
  
  # bAmt should be the eAmt corresponding to each combination from the previous day
  
  for(i in seq(0,100,5)){
    for(j in seq(0,(100-i),5)){
      
      bAmt=investment[combination,12]
      combination=combination+1
      k=100-j-i
      eAmt = eAmt[0,]
      
      for(itrn in c(1:100)){
        
        PickRow = sample(1:nrow(DailyReturns),1)
        rBCoin = getBCoinReturn(PickRow)
        rLCoin = getLCoinReturn(PickRow)
        rECoin = getECoinReturn(PickRow)
        
        eAmt1 = (bAmt*i/100) * (1 + rBCoin)
        eAmt2 = (bAmt*j/100) * (1 + rLCoin)
        eAmt3 = (bAmt*k/100) * (1 + rECoin)
        eAmtFinal= eAmt1+eAmt2+eAmt3
        
        eAmt = rbind(eAmt,data.frame(bAmt=bAmt, i=i, j=j, k=k, rBCoin=rBCoin, rLCoin=rLCoin, rECoin=rECoin, eAmt1=eAmt1, eAmt2=eAmt2, eAmt3=eAmt3, eAmtFinal=eAmtFinal))
          
        
      }
      meaneAmtFinal = mean(eAmt$eAmtFinal).
      mean_eAmt1=mean(eAmt$eAmt1).
      mean_eAmt2=mean(eAmt$eAmt2).
      mean_eAmt3=mean(eAmt$eAmt3).
      
      lastDay=d
      investment=rbind(investment,data.frame(day=d, PickRow = PickRow,
                                             rBitCoin = rBCoin,PercentBitCoin=i/100, rLiteCoin = rLCoin,
                                             PercentLiteCoin=j/100, rECoin = rECoin,
                                             PercentRippleCoin=(100-j-i)/100, eAmt1 = mean_eAmt1, eAmt2 = mean_eAmt2,
                                             eAmt3 = mean_eAmt3, eAmtFinal= meaneAmtFinal) )
    }
  }
}
tail(investment)

last_day_investment=subset(investment,investment$day==lastDay)
which.max(last_day_investment$eAmtFinal)
last_day_investment[which.max(last_day_investment$eAmtFinal),]
