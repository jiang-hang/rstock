#x is a OHLC object
ene<- function(x,...) {
    ma11<- SMA(x$Close,n=11)
    lower<-(1-0.09) * ma11 
    res=x$Close<lower 
    #return the last data
    return(res)
}

mtrue<-function(x,...){
	ene=x$Close > 0
	return(ene)
}

#涨幅超8%突破
z8Tupo<-function(x){
    highoffset=highestOffset(x,days=60)
    highoffsetlag=lag(highoffset,k=-1)
    ret0=ret(x$Close)
    preret1=lag(ret0,k=-1)
    highest=runMax(x$High,n=60)
    
    bb=merge(highoffset,highoffsetlag,ret0,highest,preret1,x)
    #bb=as.data.frame(bb)
    bb$highoffset<- as.numeric(bb$highoffset)
    bb$highoffsetlag<- as.numeric(bb$highoffsetlag)
    bb$ret0<- as.numeric(bb$ret0)
    bb$preret1<- as.numeric(bb$preret1)
    bb$highest<- as.numeric(bb$highest)
    
    bb$ene=(bb$highoffset == 0) & (bb$highoffsetlag <= -14) & (bb$ret0 >= 0.08)  
    
    return(bb$ene)
}

#碰到布林下轨
bollXiaG<-function(x)
{
	bb=BBands(x[,c("High","Low","Close")])
	ee=bb[,"pctB"]
	ene=ee < 0.20
	return(ene)
}
bollShangG<-function(x)
{
	bb=BBands(x[,c("High","Low","Close")])
	ee=bb[,"pctB"]
	ene=ee > 0.80
	return(ene)
}

ATRP<-function(x)
{
	atr=ATR(x[,c("High","Low","Close")],n=14)
        ma=runMean(x$Close,n=14)
	atrp=atr$atr/ma
	return(atrp)
}

maUp<-function(x,...)
{
        ma=runMean(x$Close,n=60)
	malag=lag(ma,k=-1)
	ene=(ma > malag)
	return(ene)
}

#放量不跌
flbd<-function(x,...)
{
	mv3=runMean(x$Volum,3)
	mv5=runMean(x$Volum,5)
	ret=ROC(x$Close)
	ene=(mv3>mv5) & (ret > 0.0)
}

bollTupo<-function(x,...)
{
	margs=list(...)
	sd=2.33
	if( ! is.null(margs$sd) ) {
		sd=margs$sd
	}
	bb=BBands(x$Close,sd=sd)
	ret=ROC(x$Close)
	threshold=0.00
	if( ! is.null(margs$threshold) ){
		threshold=margs$threshold
	}

	hdiff=x$Close - bb$up
	belowBHDays=20
	if( ! is.null(margs$belowBHDays) ){
	        belowBHDays=margs$belowBHDays
	}
	hdiffMax=runMax(hdiff,belowBHDays)
	hdiffMaxLag=lag(hdiffMax,k=-1)

	if( ! is.null(margs$atr) ){
	    atr=ATR(x[,c("High","Low","Close")],n=20)
	    atr=atr$atr / bb$mavg
	    #print(atr)
	    ene=(ret > threshold) & (x$Close > bb$up) & (hdiffMaxLag < 0) & (atr < margs$atr)
	}else{
	    #print(ret)
	    #print(hdiffMaxLag)
	    ene=(ret > threshold) & (x$Close > bb$up) & (hdiffMaxLag < 0) 
	}


	#sell=x$Close < bb$up
	sell=x$Close < bb$mavg

	tmpt=merge(ene,sell) #align the timing

	rene= ifelse(tmpt$ene,1 ,ifelse(tmpt$sell,-1,0))
	return(rene)
}

#根据rsi的分布来决定阈值
drsi<-function(x,...){
	rsi=RSI(x$Close)
	mm=runMean(rsi,n=250)
	sdrsi=runSD(rsi,n=250)

	arsi=aroon(rsi)

	rsilag=lag(rsi,k=-1)

	#ene=(mm <=60 & mm >=40 ) & (rsilag < (mm-2*sdrsi)) & (arsi$aroonDn == 95)
	ene=(mm >=50 ) & (rsilag < (mm-2*sdrsi)) & (arsi$aroonDn == 95)
	return(ene)
}

rsiDi<-function(x,...){
	rsi=RSI(x$Close)
	#rsi < 20 并且不再创新低
	arsi=aroon(rsi)

	rsilag=lag(rsi,k=-1)
	#ene=(rsi < 20) & (arsi$aroonDn < 100)
	ene=(rsilag < 20) & (arsi$aroonDn == 95)
        return(ene)	
}

kdjDi<-function(x,...){
	kdj=stoch(x[,c("High","Low","Close")])
	akdj=aroon(kdj$fastK)

	fastKlag=lag(kdj$fastK,k=-1)
	ene=(fastKlag < 0.20) & (akdj$aroonDn == 95 )  #95倒数第二低
	return(ene)
}


macdDi<-function(x,...){
	macd=MACD(x$Close,percent=F)
	amacd=aroon(macd$macd)
	ene= amacd$aroonDn==95
	
	sell=amacd$aroonDn==100 | amacd$aroonDn == 85
	tmpt=merge(ene,sell) #align the timing

	rene= ifelse(tmpt$ene,1 ,ifelse(tmpt$sell,-1,0))
	return(rene)
}
postN<-function(x,...){
	margs=list(...)
	if( !is.null(margs$postFunc)  & !is.null(margs$postn) ) {
		ene=margs$postFunc(x,...)
		#do not have the withSellSig
		#remove the -1 in the ene
		anyone<-function(x) return(any(x==1))
                pene=lag(ene,k=-margs$postn:0)
		#print(pene)
                nene=apply(pene,1,anyone)
		bb=zoo(nene,as.Date(names(nene)))
		#print(bb)
		return(bb)
	}else{
		print("error ,please give the postFunc and postn for the func postN")
		return(NA)
	}
        
}

#kdj + macd + rsi Di
kmrDi<-function(x)
{
	kdj=kdjDi(x)
	macd=macdDi(x)
	rsi=rsiDi(x)
	ene=(kdj + macd + rsi) >= 1
	return(ene)
}

flssht<-function(x)
{
	mav10=runMean(x$Volume,10)
	mav20=runMean(x$Volume,20)
	mac20=runMean(x$Close,20)
	mac10=runMean(x$Close,10)
	ac=aroon(x$Close)
	
	#10日均线位于20日均线以上
	#mav10 - mav20 > 30%
	#收盘价位于20日均线以下
	ene= ( mac10 > mac20 ) & ((mav10 - mav20)/mav20 > 0.3) & (x$Close < mac20) & (ac$aroonUp > 70)
	return(ene)
}

#zhangtingTupo
zhangtingTupo<-function(x){
    highoffset=highestOffset(x,days=60)
    highoffsetlag=lag(highoffset,k=-1)
    ret0=ret(x$Close)
    preret1=lag(ret0,k=-1)
    highest=runMax(x$High,n=60)
    
    bb=merge(highoffset,highoffsetlag,ret0,highest,preret1,x)
    #bb=as.data.frame(bb)
    bb$highoffset<- as.numeric(bb$highoffset)
    bb$highoffsetlag<- as.numeric(bb$highoffsetlag)
    bb$ret0<- as.numeric(bb$ret0)
    bb$preret1<- as.numeric(bb$preret1)
    bb$highest<- as.numeric(bb$highest)
    #case1  涨停没有突破，但是已经逼近前期新高
    bb$case1=(bb$highoffset <= -15) & (bb$ret0>= 0.092) &  ((bb$highest - bb$Close) / bb$highest < 0.02) #& (preret1 < 0.05)
    #涨停突破前提高点
    
    bb$case2=(bb$highoffset == 0) & (bb$highoffsetlag <= -14) & (bb$ret0 >= 0.092)  & (bb$Close >= bb$highest) #& (preret1 < 0.05)
    bb$ene=bb$case1 | bb$case2
    
    return(bb$ene)
}
dietingTupo<-function(x){
    lowoffset=lowestOffset(x,days=60)
    lowoffsetlag=lag(lowoffset,k=-1)
    ret0=ret(x$Close)
    preret1=lag(ret0,k=-1)
    lowest=runMin(x$Low,n=60)
    
    bb=merge(lowoffset,lowoffsetlag,ret0,lowest,preret1,x)
    #bb=as.data.frame(bb)
    bb$lowoffset<- as.numeric(bb$lowoffset)
    bb$lowoffsetlag<- as.numeric(bb$lowoffsetlag)
    bb$ret0<- as.numeric(bb$ret0)
    bb$preret1<- as.numeric(bb$preret1)
    bb$lowest<- as.numeric(bb$lowest)
    #case1  涨停没有突破，但是已经逼近前期新高
    bb$case1=(bb$lowoffset <= -15) & (bb$ret0 <= -0.095) &  (abs(bb$lowest - bb$Close) / bb$lowest < 0.02) 
    #涨停突破前提高点
    
    bb$case2=(bb$lowoffset == 0) & (bb$lowoffsetlag <= -14) & (bb$ret0 <= -0.095)  & (bb$Close <= bb$lowest) 
    bb$ene=bb$case1 | bb$case2
    
    return(bb$ene)
}
#MACD底背离
macdDiBL<-function(x){
    mperiod=60  #60天高点和低点
    nDays = 30  #30天macd未创新低
    npDays = 10  #价格的新低在10天内达到  ， 以此可得到一个macd底背离，然后此时的macd金叉可视为进场信号
    nsdDays = 5  #最近5天内波动率达到最低
    macd=MACD(x$Close,percent=F)
    macds=macd$macd-macd$signal
    macdslag=lag(macds,k=-1)
    macdgold=(macds >0) & (macdslag < 0)

    #低波动率
    sd=runSD(x$Close,n=60)
    sda=aroon(sd,n=60)

    maa=aroon(macd$macd,mperiod)
    pa=aroon(x[,c("High","Low")],mperiod)

    mpa=merge(maa,pa, macdgold,sda)
    #macd的低点至少在30天以前
    #30天未创新低，aroonDn = 100 - 30 *　(100/60)
    maaDn = 100 - nDays * 100 / mperiod
    #价格的新低在10天以内,此时出现macd金叉可视为进场信号
    paDn = 100 - npDays * 100 / mperiod

    #sd的新低数字为近期所达到，表示最近的波动率较小，也就是不是刚刚经过一轮大幅下跌
    sdDn = 100  - nsdDays * 100 / mperiod #
    mpa_bottom=mpa[which(mpa$aroonDn.maa < maaDn & mpa$aroonDn.pa > paDn & mpa$macdgold & mpa$aroonDn.sda > sdDn),]

    if(length(mpa_bottom) > 0) {
            mpa_bottom$ene=1
	    return (mpa_bottom$ene)
    }else{
	    return(NA)
    }
}
macdDingBL<-function(x){
    macd=MACD(x$Close,percent=F)
    maa=aroon(macd$macd,60)
    pa=aroon(x[,c("High","Low")],60)
    mpa=merge(maa,pa)
    mpa_top=mpa[which(mpa$oscillator.maa < 0 & mpa$oscillator.pa > 0 & mpa$aroonUp.pa == 100),]

    if(length(mpa_top) > 0) {
            mpa_top$ene=1
	    return (mpa_top$ene)
    }else{
	    return(NA)
    }
}
#x is a OHLC object
features<-function(x){
	#布林带
	#boll = (c - low) / (high - low)
	#     >1 上轨上面
	#     < 0 下轨下面
	bollr=BBands(x[,c("High","Low","Close")])
	boll=bollr[,c("pctB")]
	
	#相对强弱指标 , 过去10天上涨幅度与下跌幅度之比
	rsir = RSI(x[,c("Close")])
	rsi=rsir/100
	
	#clv close location Value , 收盘价在当天最高和最低之间的比例
	#+1 = High， -1 = Low， 0 = high low 中间
	#用来反映当天K线的形态
	clv = CLV(x[,c("High","Low","Close")])

	#顺势通道指标 , 波动范围为-100到+100 ， 页可以超过100，-100
	ccir = CCI(x[,c("High","Low","Close")])
	cci = ccir / 100 

	#roc rate of change
	roc= ROC(x[,"Close"])

	#adxr
	adxr = ADX(x[,c("High","Low","Close")])
	adx=adxr[,"ADX"]

	#以上为价格指标
	#--------------------------------------------
	#以下为量能指标

	#累计量能指标，通常用来和其他指标联合判断资金进出
	obvr = OBV(x[,"Close"],x[,"Volume"])
	obv = obvr / abs(max(obvr))

	#量能RSI
	vrsir= RSI(x[,"Volume"])
	vrsi= vrsir/100

	bb=merge(x,boll,rsi,clv,cci,roc,adx,obv,vrsi)
	return(bb)
}

featuresForLastDay<-function(x)
{
	kdj=stoch(x[,c("High","Low","Close")])
	lkdj=tail(kdj,1)
	
	rsi=RSI(x$Close)
	lrsi=tail(rsi,1)

	macd=MACD(x$Close,percent=F)
	lmacd=tail(macd,1)

	boll=BBands(x$Close)
	lboll=tail(boll,1)

	atr=ATR(x[,c("High","Low","Close")],n=10)
	latr=tail(atr,1)
	
	nmm=s.nmm(x)
	lnmm=tail(nmm,1)

	ret=c(as.double(lkdj$fastK) * 100,as.double(lrsi),as.double(lmacd$macd),as.double(lboll$pctB),as.double(lboll$up),as.double(latr$atr),as.double(lnmm))

	ret=sprintf("%.2f",ret)
	names(ret)=c("kdj","rsi","macd","bpct","bollup","atr","nmm")

	return(ret)
}

cupBase<-function(x){
#直接计算盘整天数 5
#盘整位置接近60日高点
#60日高点日期远离当前盘整日期

    logr=logret(x$Close)
    bd=bodong(x)
    logr5=rollapply(logr,5,mean,align="right")
    #bd5=rollapply(bd,5,mean,align="right")
    bd5=runMax(bd,5)
    
    close5=rollapply(x$Close,5,mean,align="right")
    close5lag=lag(close5,k=-1)
    
    bd5lag=lag(bd5,k=-1)
    logr5lag=lag(logr5,k=-1)
    
    highoffset=highestOffset(x,days=60)
    highoffsetlag=lag(highoffset,k=-1)
    
    highest=runMax(x$High,n=60)
    highestlag=lag(highest,k=-1)

    vol5=runMean(x$Volume,n=5)
    vol5lag=lag(vol5,k=-1)

    ret10=nextret(x$Close,10)
    #bd5lag < 0.03 
    #logr5lag < 0.05
    #highoffsetlag < -10
    #highoffset = 0 
    #Close > highestlag
    bb=merge(x,highest,highestlag,highoffset,highoffsetlag,logr5lag,bd5lag,close5lag,logr5,bd5,close5,vol5,vol5lag,ret10,logr)
    return(bb)
}

cupTupo<-function(x)
{
    bb=cupBase(x)
    mene=(bb$bd5lag < 0.02) & (abs(bb$logr5lag) < 0.01) & (bb$highoffsetlag < -20) & (bb$highoffset == 0) & (bb$Close > bb$highestlag) & ((bb$highestlag - bb$close5lag)/bb$highestlag < 0.04) & (bb$Volume > (3 * bb$vol5lag) )
    
    return(mene)
}

cup<-function(x)
{
    bb=cupBase(x)
    mene=(bb$bd5 < 0.02) & (abs(bb$logr5) < 0.01) & (bb$highoffset < -20) & ((bb$highest - bb$close5)/bb$highest < 0.04) 
    
    return(mene)
}

dazhang<-function(x){
	bb=cupBase(x)
	mene=(bb$ret10 > 0.3) & (bb$logr > 0.09) & (bb$bd5lag < 0.06) & (bb$Volume > (2 * bb$vol5lag))
	return(mene)
}

dazhangFail<-function(x){
	bb=cupBase(x)
	mene=(bb$ret10 < 0.2) & (bb$logr > 0.09) & (bb$bd5lag < 0.06) & (bb$Volume > (2 * bb$vol5lag))
	return(mene)
}


betaFunc<-function(code)
{
	today = Sys.Date()
	lastYear = today - 500
	ahist=loadhist(code,from=lastYear,to=today +1, atleast=120)
	if( class(ahist) != "zoo") {
		return(c(NA,NA))
	}
	dphist=loadhist("999999",from=lastYear,to=today+1)
	reti=ROC(ahist$Close)
	retm=ROC(dphist$Close)
	df=merge(reti,retm)
	df=df[complete.cases(df),]
	yy=lm(reti~retm,df)
	return(c(yy$coeff[1],yy$coeff[2]))
}

#资金流
#from and to margs are necessary
#and code is needed

zjl<-function(x,...)
{
	#对5分钟线而言，每天48根线，20天就是960根线
	#convert it to xts
	#count the days
	dailyH=x
	margs=list(...)
	countDays=2
	if (!is.null(margs$countDays)){
		countDays=margs$countDays
	}
	#print(dailyH)
	print("loading 5m hist")
	#print(as.Date(as.numeric(g_state[['from']])))
	x=loadhist(g_state[['code']],from=as.Date(as.numeric(g_state[['from']])),to=as.Date(as.numeric(g_state[['to']])),period="5m",atleast=as.numeric(g_state[['atleast']])*48)
	print("5m hist is loaded")
	xx=xts(x)
	index(xx)=index(x)  #now xx is valid xts obj
	xx$roc=ROC(xx$Close)
	#split it to days
	days=split(xx,f="days")
	#delete the days that has zero Volume
	for( i in seq(length(days),1) ){
		if(all(days[[i]]$Volume == 0)){
			days[[i]] = NULL
			print("removing item ")
			print(i)
		}
	}
	#print("split is loaded")
	if(length(days) <=countDays){
		return(NA)
	}
	##calc the ret for each min
	mf=rep(NA,length(days))
	print(length(days))
	for( i in seq(2,length(days)) ){
		days[[i]]$cumroc=cumsum(days[[i]]$roc)
		days[[i]]$cumroc=expm1(days[[i]]$cumroc)
		days[[i]]$zdt=ifelse(days[[i]]$cumroc >=0.0985,1,ifelse(days[[i]]$cumroc <= -0.0985,-1,0))
		days[[i]]$mfflag=ifelse(days[[i]]$roc <0 , -1 , ifelse(days[[i]]$roc >0, 1, days[[i]]$zdt))
		days[[i]]$mf=sum(days[[i]]$mfflag * days[[i]]$Volume)
	}
	days[[1]]$roc=NA
	days[[1]]$zdt=NA
	days[[1]]$mf=NA
	#print(mf)
	for( i in seq(2,length(days)) ){
		mf[i]=tail(days[[i]],1)$mf
	}
	#print(mf)

	res=zoo(mf,index(dailyH))
	#3日流入
	#print(res)
	ret=runSum(res,countDays)

        ltg=g_liutonggu[which(g_liutonggu$code==g_state[['code']]),]$ltg
	ret=ret/ltg
	
	threshold=0.01
	if(!is.null(margs$threshold)){
	     threshold=margs$threshold
	}
	ene=(ret > threshold)

	return(ene)
}

#ene =1  buy 
#ene = -1 sell
#cal the return between two buy/sell
calReturn<-function(x,ene){
	dd=merge(x$Close,ene)
	colnames(dd)=c("Close","ene")
	buy=index(subset(dd,dd$ene==1))

	if(length(buy) > 0) {
		sell=NULL
		for (bb in buy){
			tset=subset(dd,dd$ene==-1 & index(dd) > bb)
			if(nrow(tset) > 0){
				ss=min(index(subset(dd,dd$ene==-1 & index(dd) > bb)))
				sell=c(sell,ss)
			}
		}
		#print("sell")
		#print(as.Date(sell))
		#print("buy")
		#print(as.Date(buy))
		#print(dd)

		days=NULL
		ret=NULL
		for (i in seq(1,length(buy))){
		        if(i <= length(sell)){
				days=c(days,as.Date(sell[i]) - as.Date(buy[i]))
				ret=c(ret,(as.double(dd[as.Date(sell[i])]$Close) - as.double(dd[as.Date(buy[i])]$Close)) /  as.double(dd[as.Date(buy[i])]$Close))
			}else{
				days=c(days,end(dd) - as.Date(buy[i]))
				#ret=c(ret,(as.double(last(dd)$Close) - as.double(dd[as.Date(buy[i])]$Close)) /  as.double(dd[as.Date(buy[i])]$Close))
				ret=c(ret,NA)
			}
		}
		print(days)
		print(ret)
		ret=sprintf("%.3f",ret)
		ndata=zoo(cbind(days,ret),as.Date(buy))
		dd=merge(dd,ndata)
		dd$Close=NULL
		dd$ene=NULL
		return(dd)
	}else{
		return(NA)
	}
}

#statistics the return for ene
eneStat<-function(code,n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",...){
    mdd=loadhist(code,n,atleast,fetchLatestPrice,period,...)
    if(class(mdd) != "zoo") return(NA)
    if(myappend){
	    mdd=appendZhangting(mdd,appendzt)
    }
   
    margs=list(...)
    mene=myfunc(mdd,...)
    #print(mene)
    if( class(mene) != "zoo" ) {
	    return(NA)
    }
    if(calcfeatures){
        ff=features(mdd)
    }

    if( is.null(margs$retn) ){
    	highret=nexthighret(mdd,1)
        ret=nextret(mdd$Close,1)
        risk=nextrisk(mdd,1)
    }else{
	highret=nexthighret(mdd,margs$retn)
        ret=nextret(mdd$Close,margs$retn)
	risk=nextrisk(mdd,margs$retn)
    }

    
    if(calcfeatures){
       mdf=merge(mene,highret,ret,risk,ff)
    }else{
       mdf=merge(mene,highret,ret,risk)
    }
    #print(highret)
    #print(mdf)
    if(withSellSig){
   	 simret=calReturn(mdd,mene)  #mene = 1 : buy , mene=-1 sell #return the days and ret
    	 if( class(simret) == "zoo" ){
	    mdf=merge(mdf,simret)
            mdf$simret=sprintf("%.3f",as.double(as.vector(mdf$simret)))
         }
    }
    
    mdf<-as.data.frame(mdf)
    mdf$ret=sprintf("%.3f",as.double(as.vector(mdf$ret)))
    mdf$highret=sprintf("%.3f",as.double(as.vector(mdf$highret)))
    mdf$risk=sprintf("%.3f",as.double(as.vector(mdf$risk)))
    
    #by default , myene is a boolFeature
    # in this case , just select the true result out
    if( is.null(margs$boolFeature) ){
        mdfs=subset(mdf,(mene==1))
    }else{
	if(margs$boolFeature){
            mdfs=subset(mdf,(mene==1))
	}else{
		mdfs=mdf
	}
    }
    if (length(mdfs$mene) > 0) {
        mmdfs=cbind(mdfs,date=rownames(mdfs))
        mmdfs$date<-as.Date(mmdfs$date)
        #mmdfs=mdfs
        rownames(mmdfs)<-NULL
        mmdfs=cbind(mmdfs,code=rep(as.integer(code),length(mmdfs$mene)))
        return(mmdfs)
    }else{
        return(NA)
    }
    
    #return(mdf)
}

#分析一个代码集的ene分布情况
eneStatBat<-function(codeset="bankuai\\codes1.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",...){
    codes=loadcodeset(codeset)
    loadLiutonggu()
    margs=list(...)
    from=Sys.Date() - n
    to=Sys.Date()
    if(!is.null(margs$from)){
	    from=as.Date(margs$from)
    }
    if(!is.null(margs$to)){
	    to=as.Date(margs$to)
    }
    g_state[['tradingDays']]<<-tradingDays(from,to)
    g_state[['from']]<<-as.Date(from)
    g_state[['to']]<<-as.Date(to)
    g_state[['atleast']]<<-atleast
    res=NULL
    for(code in codes) {
        print(code)
        g_state[['code']]<<-code
        aa=eneStat(code,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)
        if (is.data.frame(aa)) {
            res=rbind(res,aa)
        }
    }
    #sort the res by date
    if(length(res$code) > 0){
        sres=res[order(res$date),]
        return(sres)
    }else{
        return(NA)
    }
}

genReportForEneResult<-function(sres,codeset="bankuai\\codes.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...)
{
    retavg=mean(as.double(sres$ret),na.rm=T)
    highavg=mean(as.double(sres$highret),na.rm=T)
    lowavg=mean(as.double(sres$risk),na.rm=T)
    b=highavg/abs(lowavg)
    kl=(b*0.5-0.5)/b

    #期望可算得 highavg * 0.5 + lowavg * 0.5 
    #标准差可算得 
    retsd=sd(as.double(sres$ret),na.rm=T)
    sharp=(retavg*252-0.04)/(sqrt(252)*retsd)
    kl=retavg/(retsd*retsd)

    results=c(highavg,lowavg,kl,retavg,retsd,sharp)
    mnames=c("highavg","lowavg","kl","retavg","retsd","sharp")
    if(withSellSig){
	    retavg2=mean(as.double(as.vector(sres$simret)),na.rm=T)
	    retsd2=sd(as.double(as.vector(sres$simret)),na.rm=T)
	    results=c(results,retavg2,retsd2)
	    mnames=c(mnames,"retavg2","retsd2")
    }

    results=sprintf("%.3f",results)
    names(results)=mnames
    paras=c(codeset,n,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,report)
    names(paras)=c("codeset","n","atleast","myappend","appendzt","calcfeatures","fetchLatestPrice","withSellSig","period","report")
    moreparas=list(...)

    if(report != "report") {
    	target <- HTMLInitFile(".",filename=report, BackGroundColor="#BBBBEE")
	HTML(as.title(paste("eneStatBatWithBk report for function",report," - ", Sys.Date())),target)
	HTML(as.data.frame(paras),target)
        if(length(moreparas) > 0){
           HTML(as.data.frame(moreparas),target)
        }
        HTML(as.data.frame(results),target)
        HTML(myfunc,target)
        HTML(as.data.frame(sres),target)
        HTMLEndFile()
    }

    return(list(res=sres,myfunc=myfunc,paras=paras,moreparas=moreparas,results=results))

}

#基于板块来分析ene
eneStatBatWithBk<-function(codeset="bankuai\\codes.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
    margs=list(...)
    if( ! is.null(margs$tdxBk) ){
	bkMap=read.table("bankuai\\blockMap.txt",sep=",",head=F)
	colnames(bkMap)=c("name","code","num")

	tbkMap=bkMap[which(bkMap$num > 0),]
	savecodeset(tbkMap$code,"bankuai\\tdxBankuaiSet.txt")
	codeset="bankuai\\tdxBankuaiSet.txt"
    }
    res=eneStatBat(codeset,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)
    if( ! is.data.frame(res) ) {
        return(NA)
    }

    if( ! is.null(margs$tdxBk) ){
	rres=merge(tbkMap,res)
    }else{
    	bk=loadbk()
	#bk1=bk[,c("code","name","hy3n")]
	bk1=bk[,c("code","name")]
    
	bk2=loadshizhiBk()
        
	#linfo=loadlinfo()
        #linfo1=linfo[,c("code","zguben","ltguben","eps")]
        rres=merge(res,bk1)
        rres=merge(rres,bk2)
    }

    sres=rres[order(rres$date),]

    res=genReportForEneResult(sres,codeset=codeset,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)

    return(res)
}


foxHen<-function(foxfunc=macdDi,henfunc=macdDi,n=120,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
	#layer 1 tdxBankuai
	margs=list(...)
	fox=newfox(myfunc=foxfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,...)
	#layer1 with the col name (code,name, highret1,...,date)
	hen=newhen(myfunc=henfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,...)
	#layer2 is for all the codes , 
	#currently , the two layers are independently
	#now merge the two layers, that is filter the layer2 with layer1

	#interested data
	#layer1$res	
	#layer1$result$ret5avg
	foxres=fox$res[,c("code","name","date","highret","risk","ret")]
	henres=hen$res[,c("code","name","date","highret","risk","ret")]
	res=rbind(foxres,henres)
	sres=res[order(res$date),]
	#get the uniq date
	
	interestedDate=sres[!duplicated(sres$date),]$date

	bk=genCodeWithTdxBk()
	#in bk , bk[['000001']] = c(板块名字)

	for ( dd in interestedDate ){
		subset=sres[which(sres$date == dd),]
		bkset=subset[which(subset$code > 880000),]$name
		if(length(bkset) == 0){
			#no code should be selected
			#remove all the items for that day
			sres=sres[which(sres$date != dd),]		
			print("remove code for date")
			print(as.Date(dd))
		}else{ #load the codeset
			#bkcodes=
			codeSubset=subset[which(subset$code < 800000),]$code
			for( cod in codeSubset){
			      scod=sprintf("%06d",cod)
			      if (length(intersect(bkset,bk[[scod]])) == 0){
				      #remove it if no bk is select
				      print("remove code for")
				      print(scod)
				      print(bkset)
				      print(bk[[scod]])
				      sres=sres[-which(sres$date == dd & sres$code==cod),]
			      }	      
			}
		}
	}
	foxedHen=list(foxfunc=foxfunc,henfunc=henfunc,atleast=atleast,n=n,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,margs=margs)
	class(foxedHen)="foxedHen"
	foxedHen[['res']] = sres[which(sres$code<800000),]
	foxedHen[['results']] = stat.hen(foxedHen)

	#fox[['results']]=stat.hen(fox)
	#hen[['results']]=stat.hen(hen)

	return(foxedHen)
}

#report the ene result
#where the ene report is from the eneStatBatWithBk
drawene<-function(x){
    boxplot(ret10 ~ date, data=x, col="red")
    boxplot(risk10 ~ date, data=x, col="green", add=T)
    cnt=aggregate(ret10 ~ date , data=x,length)
    mmax=max(cnt$ret10)
    cnt$ret1=cnt$ret10/(2*mmax)
    lines(cnt$ret10,col="blue")
    abline(c(0,0),col="black")
    title("ene")
    text(5,0.45,paste("max count is",mmax))
}

#dump the ene within the latest 5 days
dumpene<-function(x,myfile="ene.txt"){
    #bb=x[which(x$date > (Sys.Date() - 5)),c("code","name","date","ret1","risk1","ret5","risk5","hy2n")]
    #x=x$res
    if( class(x) != "list" ){
       cat("",file=myfile)
       return (NA)
    }
    maxDate=max(x$res$date)
    nextDay=lastTradeDay() + 1
    
    if(maxDate == nextDay){
       cc=x$res[which(x$res$date == maxDate),c("code")]
       dd=sprintf("%06d",cc)
       write.table(as.data.frame(dd),file=myfile,col.names=F,row.names=F,quote=F)
    }else{
       cat("",file=myfile)
    }
}


kmrDaily<-function()
{
	genName<-function(zb,n){
		aa=n
		if(aa >= 0){
		     ofile=sprintf("%s%d.txt",zb,aa)
		}else{
    		     ofile=sprintf("%sm%d.txt",zb,abs(aa))
		}
		return(ofile)
	}
	#rate=c(-9,-7,-5,-3,-1,0,1,3,5,7,10)
	#rate=c(0,1,3,5,7,10)
	rate=c(0,1,3)
	for(aa in rate){
		appendrate=aa*0.01
		kdj0=eneStatBatWithBk(myfunc=kdjDi,myappend=T,appendzt=appendrate)
		ofile=genName("kdj",aa)
		dumpene(kdj0,ofile)

		rsi0=eneStatBatWithBk(myfunc=drsi,myappend=T,appendzt=appendrate,n=1200,atleast=600)
		ofile=genName("rsi",aa)
		dumpene(rsi0,ofile)

		macd0=eneStatBatWithBk(myfunc=macdDi,myappend=T,appendzt=appendrate)
		ofile=genName("macd",aa)
		dumpene(macd0,ofile)

		kdj0=eneStatBatWithBk(myfunc=kdjDi,myappend=T,appendzt=appendrate,tdxBk=1)
		ofile=genName("bkkdj",aa)
		dumpene(kdj0,ofile)

		rsi0=eneStatBatWithBk(myfunc=drsi,myappend=T,appendzt=appendrate,tdxBk=1,n=1200,atleast=600)
		ofile=genName("bkrsi",aa)
		dumpene(rsi0,ofile)

		macd0=eneStatBatWithBk(myfunc=macdDi,myappend=T,appendzt=appendrate,tdxBk=1)
		ofile=genName("bkmacd",aa)
		dumpene(macd0,ofile)

		boll0=eneStatBatWithBk(myfunc=bollTupo,myappend=T,appendzt=appendrate)
	        ofile=genName("boll",aa)
		dumpene(boll0,ofile)
	}

	#save the codeset
	zbs=c("kdj","rsi","macd","boll","bkkdj","bkmacd","bkrsi")
	for(i in seq(length(rate),2)){   # i = 6 5 4 3 2 1
		for (zb in zbs){
		  i1file=genName(zb,rate[i])
		  i0file=genName(zb,rate[i-1])
		  codeset0=scan(i0file)
		  codeset1=scan(i1file)
		  c1=setdiff(codeset1,codeset0)
		  savecodeset(c1,i1file)
		}
	}
	genReportDaily()

}

genReportDaily<-function()
{
	#calc the return when no append
	fhkdj=foxHen(foxfunc=kdjDi,henfunc=kdjDi)
	write.table(as.data.frame(fhkdj$results$results),file="fhkdjReport.txt",sep=",",quote=F,row.names=F)
	dumphen(fhkdj,myfile="fhkdjDayStat.txt",info="dayStat")

        hkdj=newhen(myfunc=kdjDi)
	write.table(as.data.frame(hkdj$results$results),file="hkdjReport.txt",sep=",",quote=F,row.names=F)
	dumphen(hkdj,myfile="hkdjDayStat.txt",info="dayStat")
        hmacd=newhen(myfunc=macdDi)
	write.table(as.data.frame(hmacd$results$results),file="hmacdReport.txt",sep=",",quote=F,row.names=F)
	dumphen(hmacd,myfile="hmacdDayStat.txt",info="dayStat")
        hdrsi=newhen(myfunc=drsi,n=1200,atleast=600)
	write.table(as.data.frame(hdrsi$results$results),file="hdrsiReport.txt",sep=",",quote=F,row.names=F)
	dumphen(hdrsi,myfile="hdrsiDayStat.txt",info="dayStat")

        hkdj=newhen(myfunc=bollTupo)
	write.table(as.data.frame(hkdj$results$results),file="hbollReport.txt",sep=",",quote=F,row.names=F)
	dumphen(hkdj,myfile="hbollStat.txt",info="dayStat")
}

foxHenDaily<-function()
{
	genName<-function(zb,n){
		aa=n
		if(aa >= 0){
		     ofile=sprintf("%s%d.txt",zb,aa)
		}else{
    		     ofile=sprintf("%sm%d.txt",zb,abs(aa))
		}
		return(ofile)
	}
	#rate=c(-9,-7,-5,-3,-1,0,1,3,5,7,10)
	rate=c(0,1,3,5,7,10)
	for(aa in rate){
		appendrate=aa*0.01
		kdj0=foxHen(foxfunc=kdjDi,henfunc=kdjDi,myappend=T,appendzt=appendrate)
		ofile=genName("fhkdj",aa)
		dumpene(kdj0,ofile)

		kdj0=foxHen(foxfunc=macdDi,henfunc=macdDi,myappend=T,appendzt=appendrate)
		ofile=genName("fhmacd",aa)
		dumpene(kdj0,ofile)

		kdj0=foxHen(foxfunc=rsiDi,henfunc=rsiDi,myappend=T,appendzt=appendrate)
		ofile=genName("fhrsi",aa)
		dumpene(kdj0,ofile)

	}

	#save the codeset
	zbs=c("fhkdj","fhmacd","fhrsi")
	for(i in seq(length(rate),2)){   # i = 6 5 4 3 2 1
		for (zb in zbs){
		  i1file=genName(zb,rate[i])
		  i0file=genName(zb,rate[i-1])
		  codeset0=scan(i0file)
		  codeset1=scan(i1file)
		  c1=setdiff(codeset1,codeset0)
		  savecodeset(c1,i1file)
		}
	}
}

#x is the output of eneStatBat
calcFreq<-function(x){
	#weekly and monthly
	#convert it to a xts and use to.weekly and to.monthly
	#count per day
	dayFreq=aggregate(mene~date , x ,  sum)
	#convert it to OHLCV format
	#Open,High,Low,Close are fake, Volume is the mene 
	#we need the function to sum up the mene per week/month to calc the freq
	dayFreq$Open=1
	dayFreq$High=2
	dayFreq$Low=0
	dayFreq$Close=2
	dayFreq$Volume=dayFreq$mene
	dayFreq$mene=NULL

	#without the write read , to.weekly/monthly can not work correctly
	#not sure where is the issue yet
	write.table(dayFreq,file="__mytmp.txt",sep=",",quote=F,row.names=F)
	mdayFreq=read.zoo("__mytmp.txt",format="%Y-%m-%d",sep=",",head=T)

	weekFreq=to.weekly(mdayFreq)
	monthFreq=to.monthly(mdayFreq)

	names(weekFreq)=c("Open","High","Low","Close","Volume")
	names(monthFreq)=c("Open","High","Low","Close","Volume")

	weekFreq$Open=NULL
	weekFreq$High=NULL
	weekFreq$Low=NULL
	weekFreq$Close=NULL

	monthFreq$Open=NULL
	monthFreq$High=NULL
	monthFreq$Low=NULL
	monthFreq$Close=NULL

	weekFreq$freq=weekFreq$Volume
	monthFreq$freq=monthFreq$Volume

	weekFreq$Volume=NULL
	monthFreq$Volume=NULL

	return(list(week=weekFreq,month=monthFreq))

}

betaBat<-function(codeset,out="beta.txt")
{
    codes=loadcodeset(codeset)
    #calc beta
    beta=data.frame(code=NULL,alpha=NULL,beta=NULL)
    for(code in codes){
	    r=betaFunc(code)
	    beta=rbind(beta,c(code,r[1],r[2]))

    }
    colnames(beta)=c("code","alpha","beta")
    beta=beta[order(beta$beta),]
    beta$beta=sprintf("%.3f",as.double(beta$beta))
    beta$alpha=sprintf("%.3f",as.double(beta$alpha) * 1000)
    write.table(beta,file=out,row.names=F,quote=F)
    return(beta)
}
dailyFeatures<-function(codeset="bankuai\\codes.txt",out="dailyFeatures.txt",beta_out="beta.txt")
{
    codes=loadcodeset(codeset)
    res=NULL
    for(code in codes) {
        print(code)
        x=loadhist(code,n=220,atleast=61)
	if( class(x) == "zoo" ){
    		ret=featuresForLastDay(x)
		if( is.null(res) ){
			print(res)
			res=c(code,ret)
			print(res)
		}else{
			res=rbind(res,c(code,ret))
		}
	}
    }

    rownames(res)=NULL
    colnames(res)=c("code","kdj","rsi","macd","bpctb","bollup","atr","nmm")

    res=as.data.frame(res)
    write.table(res,out,row.names=F,quote=F,sep=",")

    betaBat(codeset,out=beta_out)

    return(res)
}

#cow fox hen
cowFoxHen<-function(cowfunc=kdjDi,foxfunc=macdDi,henfunc=macdDi,n=120,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
	margs=list(...)
	cow=newcow(myfunc=cowfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,fetchLatestPrice=fetchLatestPrice,...)
	#if(foxfunc==mtrue){
	#    foxedHen=newhen(myfunc=henfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,...)
	#}else{
	    foxedHen=foxHen(foxfunc=foxfunc,henfunc=henfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,...)
	#}

	cowres=cow$res[,c("code","name","date","highret","risk","ret")]
	fhres=foxedHen$res[,c("code","name","date","highret","risk","ret")]
	res=rbind(cowres,fhres)
	sres=res[order(res$date),]
	print(fhres)

	interestedDate=sres[!duplicated(sres$date),]$date
	for ( dd in interestedDate ){
		subset=sres[which(sres$date == dd),]
		#bkset=subset[which(subset$code %in% cow[['codeset']]),]$name
		bkset=subset[which(subset$code %in% cow[['codeset']]),]$code
		print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
		print(bkset)
		#print(subset)
		if(length(bkset) == 0){
			#no code should be selected
			#remove all the items for that day
			sres=sres[which(sres$date != dd),]		
			print("remove code for date")
			print(as.Date(dd))
		}else{ #load the codeset
			#bkcodes=
			codeSubset=subset[-which(subset$code %in% cow[['codeset']]),]$code
			print(codeSubset)
			for( cod in codeSubset){
			      scod=sprintf("%06d",cod)
			      if ( ! codeInZhishu(cod,bkset) ){
				      #remove it if no bk is select
				      print("remove code for")
				      print(scod)
				      sres=sres[-which(sres$date == dd & sres$code==cod),]
			      }	      
			}
		}
	}

        cfh<-list(cowfunc=cowfunc,foxfunc=foxfunc,henfunc=henfunc,n=n,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,morepara=margs)
	class(cfh)="cfh"
	cfh[['res']]=sres[-which(sres$code %in% cow[['codeset']]),]
	cfh[['results']] = stat.hen(cfh)

	return(cfh)


}

newcow<-function(bkfile="bankuai\\dapanMap.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
	margs=list(...)
	bkMap=read.table(bkfile,sep=",",head=F)
	colnames(bkMap)=c("name","code","num")

	tbkMap=bkMap[which(bkMap$num > 0),]
	savecodeset(tbkMap$code,"bankuai\\dapanSet.txt")
	codeset="bankuai\\dapanSet.txt"
        
	res=eneStatBat(codeset,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)
    	if( ! is.data.frame(res) ) {
	        return(NA)
        }
	rres=merge(tbkMap,res)
        sres=rres[order(rres$date),]

        cow<-list(bkfile=bkfile,n=n,myfunc=myfunc,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,morepara=margs)

	cow[['codeset']] = tbkMap$code
        cow[['res']] = sres
	class(cow)="cow"

	cow[['results']] = stat.hen(cow)

	return(cow)

}

#for the tdxbk
newfox<-function(bkfile="bankuai\\blockMap.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
	margs=list(...)
	bkMap=read.table(bkfile,sep=",",head=F)
	colnames(bkMap)=c("name","code","num")

	tbkMap=bkMap[which(bkMap$num > 0),]
	savecodeset(tbkMap$code,"bankuai\\tdxBankuaiSet.txt")
	codeset="bankuai\\tdxBankuaiSet.txt"
        
	res=eneStatBat(codeset,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)
    	if( ! is.data.frame(res) ) {
	        return(NA)
        }

	rres=merge(tbkMap,res)
        sres=rres[order(rres$date),]

        fox<-list(bkfile=bkfile,n=n,myfunc=myfunc,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,morepara=margs)

        fox[['res']] = sres
	class(fox)="fox"

	fox[['results']] = stat.hen(fox)

	return(fox)
}

newhen<-function(codeset="bankuai\\codes.txt",n=120,myfunc=ene,atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",report="report",...){
    margs=list(...)
    hen<-list(codeset=codeset,n=n,myfunc=myfunc,atleast=atleast,myappend=myappend,appendzt=appendzt,calcfeatures=calcfeatures,fetchLatestPrice=fetchLatestPrice,withSellSig=withSellSig,period=period,report=report,morepara=margs)
    class(hen)="hen"
    if( ! is.null(margs$tdxBk) ){
	    print("please use the newfox instead for the tdxBk")
	    return(NA)
    }
    res=eneStatBat(codeset,n,myfunc,atleast,myappend,appendzt,calcfeatures,fetchLatestPrice,withSellSig,period,...)
    if( ! is.data.frame(res) ) {
        return(NA)
    }

    bk=loadbk()
    #bk1=bk[,c("code","name","hy3n")]
    bk1=bk[,c("code","name")]
   
    bk2=loadshizhiBk()
        
    #linfo=loadlinfo()
    #linfo1=linfo[,c("code","zguben","ltguben","eps")]
    rres=merge(res,bk1)
    rres=merge(rres,bk2)

    sres=rres[order(rres$date),]
    hen[['res']] = sres

    hen[['results']]=stat.hen(hen)

    return(hen)
}

stat.hen<-function(data){
    print("in stat.hen")
    if(class(data) != "hen" & class(data) !="fox" & class(data) != "foxedHen" & class(data) !="cow" & class(data) !="cfh") {
	    return(NA)
    }
    sres=data[['res']]
    retavg2=aggregate(as.double(sres$ret),list(sres$date),mean)
    names(retavg2)=c("date","ret")
    highretavg2=aggregate(as.double(sres$highret),list(sres$date),mean)
    names(highretavg2)=c("date","highret")
    lowavg2=aggregate(as.double(sres$risk),list(sres$date),mean)
    names(lowavg2)=c("date","risk")
    len=aggregate(as.double(sres$risk),list(sres$date),length)
    names(len)=c("date","num")

    res2=merge(retavg2,highretavg2,by="date")
    res2=merge(res2,lowavg2,by="date")
    res2=merge(res2,len,by="date")
    retavg2=mean(res2$ret,na.rm=T)
    highavg2=mean(res2$highret,na.rm=T)
    lowavg2=mean(res2$risk,na.rm=T)
    b=highavg2/abs(lowavg2)
    daykl=(b*0.5-0.5)/b

    #select two item for each day randomly
    #if(nrow(sres[which(sres$code<700000),])){
    #	    sres=sres[which(sres$code<700000),]
    #}
    retavg=mean(as.double(sres$ret),na.rm=T)
    highavg=mean(as.double(sres$highret),na.rm=T)
    lowavg=mean(as.double(sres$risk),na.rm=T)
    b=highavg/abs(lowavg)
    kl1=(b*0.5-0.5)/b

    #期望可算得 highavg * 0.5 + lowavg * 0.5 
    #标准差可算得 
    retsd=sd(as.double(sres$ret),na.rm=T)
    sharp=(retavg*252-0.04)/(sqrt(252)*retsd)
    retbysd=retavg/retsd

    results=c(highavg,lowavg,kl1,retavg,retsd,sharp,retbysd,retavg2,highavg2,lowavg2,daykl)
    mnames=c("highavg","lowavg","kl1","retavg","retsd","sharp","retbysd","dayretavg","dayhighavg","daylowavg","daykl")
    if(data[['withSellSig']]){
	    retavg2=mean(as.double(as.vector(sres$simret)),na.rm=T)
	    retsd2=sd(as.double(as.vector(sres$simret)),na.rm=T)
	    results=c(results,retavg2,retsd2)
	    mnames=c(mnames,"retavg2","retsd2")
    }

    results=sprintf("%.3f",results)
    names(results)=mnames

    rr=list(results=results,dayStat=res2)

    return(rr)
}

dumphen<-function(x,myfile="hen.txt",info="nextDay"){
    #if( class(x) != "list" ){
    #   cat("",file=myfile)
    #   return (NA)
    #}
    if(info == "nextDay"){ 
        maxDate=max(x$res$date)
        nextDay=lastTradeDay() + 1
        if(maxDate == nextDay){
           cc=x$res[which(x$res$date == maxDate),c("code")]
           dd=sprintf("%06d",cc)
           write.table(as.data.frame(dd),file=myfile,col.names=F,row.names=F,quote=F)
        }else{
           cat("",file=myfile)
        }
    }

    if(info == "dayStat"){
	    dayStat=x$results$dayStat
	    write.table(as.data.frame(dayStat),myfile,quote=F,sep=",")
    }

}

getFoxName<-function(foxcode)
{
	if(is.null(g_fox)){
           loadFoxNames()
	}
	foxname=g_fox[which(g_fox$code==foxcode),]$name
	return(foxname)
}
getHenCodesInFox<-function(foxcode)
{
	if(is.null(g_fox)){
           loadFoxNames()
	}
	foxname=g_fox[which(g_fox$code==foxcode),]$name
	file=paste("bankuai\\",foxname,".txt",sep="")
        codes=loadcodeset(file)
	return(codes)
}
filterHenByFox<-function(res,foxcode)
{
    codes=getHenCodesInFox(foxcode)
    fres=res[which(res$code %in% codes),]
    return(fres)
}

rankByFox<-function(res)
{
	#res is the final result , to get the codeset with the best score
	if(is.null(g_fox)){
           loadFoxNames()
	}

	foxCodes=g_fox$code
	foxres=data.frame()
	for(code in foxCodes){
		fres=filterHenByFox(res,code)
		if(nrow(fres) > 0){
			ahen=list()
			fname=getFoxName(code)
			print(fname)
			class(ahen)="hen"
			ahen[['res']]=fres
			ahen[['withSellSig']]=F
			ahen[['results']]=stat.hen(ahen)
			foxres=rbind(foxres,c(code,fname,nrow(fres),ahen$results$results))
		}
		#save the results
	}
	colnames(foxres)=c("bk","bkname","samples",names(ahen$results$results))
        print(names(ahen$results$results))
	print(colnames(foxres))
	foxres=foxres[order(foxres$dayretavg),]
	return(foxres)
}

loadltg<-function()
{
}
#from and to margs are necessary
#retn is necessary too
zjlBat<-function(codeset="bankuai\\codes1.txt",myfunc=zjl,period="5m",...){
    codes=loadcodeset(codeset)
    res=NULL
    loadLiutonggu()
    margs=list(...)
    from=Sys.Date() - 32
    to=Sys.Date()
    if(!is.null(margs$from)){
	    from=as.Date(margs$from)
    }
    if(!is.null(margs$to)){
	    to=as.Date(margs$to)
    }
    atleast=961
    if(!is.null(margs$atleast)){
	    atleast=margs$atleast
    }
    g_state[['from']]<<-from
    g_state[['to']]<<-to
    g_state[['atleast']]<<-atleast
    print(from)
    print(to)
    print(atleast)
    for(code in codes) {
	print(code)
        g_state[['code']]<<-code
        x=loadhist(code,from=from,to=to,atleast=atleast)
        if(class(x) == "zoo"){
            zj=myfunc(x)
	    rr=tail(zj,1)
	    mf=c(code,as.Date(to),rr)
            res=rbind(res,mf)
	}
    }
    print("calc done")
    print(res)
    if(nrow(res) > 0){
	res=as.data.frame(res)
	colnames(res)=c("code","date","zjl")
        rownames(res)=NULL
	res$zjl=as.double(res$zjl)
	res$date=as.Date(as.numeric(res$date))
        #print(res)
        sres=res[order(res$zjl),]
    }else{
        return(NA)
    }

    #now zjl res is ready, we calc the retn a
    print(sres)
    if(is.null(margs$retDate)){
	    print("retDate is needed")
	    return(sres)
    }else{
	    #get trading days by loading 999999
	    aa=loadhist("999999",from=as.Date(margs$to)-1 ,to=margs$retDate,atleast=1)
	    tradingDays=nrow(aa)
	    rr=function(x,...) expm1(ROC(x$Close,tradingDays-1))
            ret=featureBat(codeset=codeset,myfunc=rr,fname="retn",atleast=tradingDays,from=as.Date(margs$to)-1,to=margs$retDate)
	    rep=merge(sres,ret,by="code")
    }
    return(rep)
}

#generate the result like( code , feature-name )
#and only the last value is what we are interested
featureBat<-function(codeset="bankuai\\codes1.txt",n=120,myfunc=ene,fname="fname",atleast=61,myappend=F,appendzt=1,calcfeatures=F,fetchLatestPrice=F,withSellSig=F,period="daily",...){
    codes=loadcodeset(codeset)
    res=NULL
    margs=list(...)
    to=Sys.Date()
    if(!is.null(margs$to)){
	    to=margs$to
    }
    for(code in codes) {
       x=loadhist(code,n,atleast,fetchLatestPrice,period,...)
       if(class(x) == "zoo"){
        	ff=myfunc(x,...)  #it is a numeric result
                ff=tail(ff,1)
                mf=c(code,as.Date(to),ff)
                res=rbind(res,mf)
       }
    }
    print("calc done")
    if(!is.null(res)){
	if( nrow(res) > 0){
	res=as.data.frame(res)
	colnames(res)=c("code","date",fname)
	res$date=as.Date(as.numeric(res$date))
        rownames(res)=NULL
	}
    }
    return(res)
}

