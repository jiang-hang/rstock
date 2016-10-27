library(quantmod)
library(fBasics)
library(TTR)
library(fractal)
library(RCurl)
library(e1071)
library(R2HTML) #生成html report
library(chron)  #时间
library(sampling)  #strata , 采样函数
library(ggplot2)

#function to load the hist data


#setwd("c:\\myR")
setwd("/home/xuyang/myR/stockhistory/myR")

#default options
options(stringsAsFactors=F)
source("globals.R")

getHgFile<-function(zhibiao){
    if (g_hongguangFilesLoaded==0){
	g_hongguanFiles<<-read.table("hongguanFiles.txt",head=T,sep=",")
	g_hongguanFilesLoaded <<- 1
    }
    tt=g_hongguanFiles[which(g_hongguanFiles$zhibiao==zhibiao),2]
    return(as.character(tt))
}

getMarketCode<-function(code,lowerCase=F){
	if(nchar(code) > 6){
		return(code)
	}
        if(as.numeric(code) < 600000) {
		if(lowerCase) {
			prefix="sz"
		}else{
			prefix="SZ"
		}
	}
        else {
		if(lowerCase){
			prefix="sh"
		}else{
			prefix="SH"
		}
	}
	#print(lowerCase)
        cc=sprintf("%s%06d",prefix, as.numeric(code))
	return(cc)
}

getHistFile<-function(code,type="day"){
    #datadir="C:\\zd_zszq\\T0002\\export\\"
    #datadir="/home/xuyang/myR/stockhistory/data/"
    datadir="/mnt/zd_zszq/T0002/export/"
    print("in get hist file, code:")
    print(code)
    if (nchar(code) <= 6){
	mkcode=getMarketCode(code,F)
        if(type=="5m") {
            url=paste(datadir,mkcode,"_5.txt",sep="")
	}else{
            url=paste(datadir,mkcode,".txt",sep="")
	}
    }else{
        #the code with the txt already
	if(type != "day"){
	   print("not supported code for getHistFile in not day type")
           return(NA)
	}
        url<- paste(datadir,code,sep="")
        if( ! regexpr("txt",url) > 0 ) {
		url<-paste(url,".txt",sep="")
	}
    }
    #print(url)
    return(url)
}

#codes is a vec c("600309","000001","600005")
getPrices<-function(codes){
#	mkcode=getMarketCode(codes,lowerCase=T)
	print(length(codes))
	if(length(codes) == 1){
		mkcode=getMarketCode(codes,lowerCase=T)
		url=paste("http://hq.sinajs.cn/?list=",mkcode,sep="")
	}else{
		mkcode=NULL
		#print(codes)
		for(code in codes){
			mkcode=c(mkcode,getMarketCode(code,lowerCase=T))
		}
		url=paste("http://hq.sinajs.cn/?list=",mkcode[1],sep="")
	}
	print(url)
	if(length(mkcode) <= 50){
		for (code in mkcode[-1]){
			url=paste(url,code,sep=",")
		}	
		dd=read.table(url,sep=",",head=F)
		lprice=NULL
		for(i in seq(1,length(codes))){
			#date O H L C V RMB  #row.name=code
			cur=as.character(dd[i,])
			#print(cur)
			bb=strsplit(cur,split=",")
			#print(bb)
			bb=bb[[1]]
			bb=as.double(bb)
			#print(bb)
			lprice=rbind(lprice,c(bb[2],bb[5],bb[6],bb[4],bb[9],bb[10]))
		}
		lprice=as.data.frame(lprice)
		#print("lprice")
		#print(lprice)
		rownames(lprice)=mkcode
		colnames(lprice)=c("Open","High","Low","Close","Volume","RMB")
	}else{
		lprice=NA
	}
	return(as.data.frame(lprice))
}

updateHist<-function(x,val) {
    ee=last(x)
    myappend=T  #T append
    if(index(ee) < Sys.Date() ){
    	index(ee)=index(ee) + 1
    }else{
	myappend=F  #do not append , update instead
    }
    ee$Close=as.double(val$Close)
    ee$Open=as.double(val$Open)
    ee$High=as.double(val$High)
    ee$Low=as.double(val$Low)
    ee$Volume=as.double(val$Volume)
    ee$RMB=as.double(val$RMB)
    if (myappend){
	print("append")
        bb=rbind(x,ee)
        return(bb)
    }else{
	print("update")
    	x[index(ee),]=ee
        return(x)
    }
}
#usage:
#loadhist("600309")
#loadhist("SH000032")
#loadhist("SH000032.txt")
loadhist <- function(code,n=90,atleast=61,fetchLatestPrice=F,period="daily",...) {  #by default 90 days , atleast  12 obs, or return NA
    margs=list(...)
    if(period=="5m") {
	    url=getHistFile(code,type="5m")
	    print(url)
	    if( ! file.exists(url) ) return (NA)
	    hist=read.zoo(url,sep=",",index=1:2,colClass=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric"),FUN=function(d,m) chron(dates=d,times=paste(m,"00",sep=""),format=c(dates="ymd",times="hms")))
    }else{
	    cname=getMarketCode(code)
	    if(is.null(g_hist[[cname]])){
                url=getHistFile(code)
    	        if( ! file.exists(url) ) return (NA)
                hist<-read.zoo(url,sep=" ",format="%Y-%m-%d")
		g_hist[[cname]]<<-hist
	    }else{
                hist<-g_hist[[cname]]
	    }
    }
    if( !is.null(margs$from) & !is.null(margs$to)){
	    beg=as.Date(margs$from)
	    #print("nrow hist")
	    #print(nrow(hist))
	    if(nrow(hist) < atleast) return(NA)
	    yHist<-window(hist,start=as.Date(margs$from),end=as.Date(margs$to))
	    #print("nrow yHist")
	    #print(nrow(yHist))
	    if(nrow(yHist) < atleast) return(NA)
    }else{
	    print("warning, no from to are given for loading hist")
            today=Sys.Date()
	    beg=today - n
	    if(end(hist) < beg) return(NA)
	    yHist<-window(hist,start=beg)
    }
    #names(yHist) <- c("Open","High","Low","Close","Volume","RMB")
    names(yHist) <- c("Open","High","Low","Close","Volume","Close")
    if (length(yHist$Close) <atleast) return(NA)

    if(fetchLatestPrice){
	    if(code=='999999'){
		    code='sh000001'
	    }
	    lprices=getPrices(code)
	    mkcode=getMarketCode(code,lowerCase=T)
	    print(lprices)
	    if( any(as.double(lprices[mkcode,]) > 0) ) { 
	    	yHist=updateHist(yHist,lprices[mkcode,])
	    }
    }
    
    if(period == "weekly"){
	    print("weekly")
	    yHist=to.weekly(yHist)
	    names(yHist) <- c("Open","High","Low","Close","Volume")
	    #in weekly mode, atleast is also based on weekly
            if (length(yHist$Close) < atleast) return(NA)
    }
    if(period == "monthly"){
	    yHist=to.monthly(yHist)
	    names(yHist) <- c("Open","High","Low","Close","Volume")
            if (length(yHist$Close) < atleast) return(NA)
    }
    return(yHist) 
}


tradingDays<-function(from,to)
{
	dp=loadhist("999999",from=from,to=to,atleast=1)
	if( class(dp) == "zoo" ){
		wh=window(dp,start=from,end=to)
		return(nrow(wh))
	}
	return(NA)
}

lastTradeDay<-function()
{
	dp=loadhist("SH999999")
	lastDay=index(last(dp))
	return(lastDay)
}

#here the codes is a list of code
#sample usage:
#cc=c("600000","600309","SH000032")
#dd=loadhists(cc)
#or
#cc=c("SH000032.txt","SH000033.txt")
loadhists<-function(codes,n=90,atleast=61)
{
    aa=NULL
    validcodes=NULL
    for (cc in codes){
	    tmpaa=loadhist(cc,n,atleast)
	    if( class(tmpaa) != "zoo" ) {
		    next
	    }
	    validcodes=c(validcodes,cc)
	    pp=tmpaa[,"Close"] #only care about the Close here
	    if(is.null(aa)) {
		    aa=pp
	    }else{
		    aa=merge(aa,pp)
	    }
    }
    mname=substr(validcodes,1,8)
    names(aa)<-mname
    return(aa)
}

appendZhangting<-function(x,appendzt=1) {
    ee=last(x)
    index(ee)=index(ee) + 1
    if (appendzt == 1){   #zhangting
	    ee$Open=ee$Close
	    ee$Low=ee$Open
	    ee$High=ee$Open * ( 1.095 )
	    ee$Close=ee$High
	    ee$Volume=ee$Volume * 2
	    ee$RMB=ee$RMB * 2
    }else if(appendzt == 2){  #dieting
	    ee$Open=ee$Close
	    ee$High=ee$Close
	    ee$Low=ee$Open * ( 0.90 )
	    ee$Close=ee$Low
	    ee$Volume=ee$Volume * 2
	    ee$RMB=ee$RMB * 2
    }else if(appendzt==3){  #append yesterday's close
	    ee$Open=ee$Close
	    ee$High=ee$Close
	    ee$Low=ee$Close
	    ee$Close=ee$Close
	    ee$Volume=ee$Volume 
	    ee$RMB=ee$RMB 
    }else if( appendzt <= 0.1 & appendzt >= -0.1){
	    if(appendzt > 0){  #上涨
            ee$Open=ee$Close
	    ee$High=ee$Close * (1 + appendzt)
	    ee$Low=ee$Open
	    ee$Close=ee$High
	    }else{   #下跌
            ee$Open=ee$Close
	    ee$Low=ee$Close * (1 + appendzt)
	    ee$High=ee$Open
	    ee$Close=ee$Low
	    }
	    ee$Volume=ee$Volume 
	    ee$RMB=ee$RMB 
    }
    bb=rbind(x,ee)
    return(bb)
}


#the return rate for the series
ret<-function(x) {
    returnRate<-expm1(diff(log(x)))
    return(returnRate)
}

#对数收益率
logret<-function(x){
    returnRate<-diff(log(x))
    return(returnRate)
}

#the return rate of the next day
#days=5 表示5日收盘价
nextret<-function(x,days=1){
    nextretRate<-expm1(diff(log(x),-days))
    return(nextretRate)
}

#the max risk in the following n days
#the x is the OHLC series
nextrisk<-function(x,days=1){
    x$nLowest=lag(runMin(x$Low,days),days)
    x$lowret=expm1(log(as.numeric(x$nLowest)) - log(as.numeric(x$Close)))
    return(x$lowret)
}

nexthighret<-function(x,days=1){
    x$nHighest=lag(runMax(x$High,days),days)
    x$highret=expm1(log(as.numeric(x$nHighest)) - log(as.numeric(x$Close)))
    return(x$highret)
}

tpret<-function(x){
    x$tp=(x$High + x$Low + 2* x$Close) / 4
    x$nexttp=lag(x$tp,1)
    x$tpret=expm1(log(x$nexttp) - log(x$Close))
    return(x$tpret)
}

#for the give vector , calc the highest index offset based on the last item
#-1 means the -2 item is the highest
#example: ct=(1,23,4,5,6,23,7)
#return : -2
highestIndexAtom<-function(x){
    aa=max(x)
    bb=which(x==aa)-length(x)
    return(bb[length(bb)])
}

#n highest price offset based on today
#x is the OHLC object
highestOffset<-function(x,days=5){
    highoffset=rollapply(x$High,days,highestIndexAtom,align="right")
    return(highoffset)
}

#n highest price offset based on today
#x is the OHLC object
lowestIndexAtom<-function(x){
    aa=min(x)
    bb=which(x==aa)-length(x)
    return(bb[length(bb)])
}
lowestOffset<-function(x,days=5){
    highoffset=rollapply(x$Low,days,lowestIndexAtom,align="right")
    return(highoffset)
}

codeInZhishu<-function(code,zsSet)
{
	print(code)
	print(zsSet)
	if(as.double(code) >= 600000) {
		return("999999" %in% zsSet)  #上证指数
	}else if (as.double(code) >= 300000){
		return("399006" %in% zsSet)
	}else if(as.double(code) >= 2000){
		return("399005" %in% zsSet)
		#return("中小板指" %in% zsSet)
	}else{
		return("399001" %in% zsSet)
		#return("深圳成指" %in% zsSet)
	}
}
#流通股

source("tdxBankuai.R")
source("stockfunc.R")
source("s.nmm.R")
#source("sfeatures.R")



