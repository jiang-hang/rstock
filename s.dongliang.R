#loading the hist data
#loadghist()
#动量
dongLiangSelect<-function(codeset="bankuai\\上证50.txt",p=3,q=1,sampleN=3,from="2010-01-01",to="2015-10-16",realtime=F)
{
	codes=loadcodeset(codeset)
	rocSet=data.frame()
	for(code in codes){
		if(realtime){
		    mm=loadhist(code,period="monthly",from=from,to=to,atleast=(p+q),fetchLatestPrice=T)
		    
		}else{
                    print(paste("dongliangSelect load hist",code))
		    mm=loadhist(code,period="monthly",from=from,to=to,atleast=(p+q))
		}
		if(class(mm) == "zoo"){
		    rocp=ROC(mm$Close,p)
   		    rocq=ROC(mm$Close,q)
   		    rocq=lag(ROC(mm$Close,q),k=q)
		    rocq=as.vector(rocq)
   		    rocq=c(rocq,rep(NA,q))
		    rocq=rocq/q
		    #print(rocq)
		    res=data.frame(code=code,rocp=rocp,rocq=rocq,date=index(rocp))
		    rocSet=rbind(rocSet,res)
		}
	}
	return(rocSet)

}
dongLiang<-function(codeset="bankuai\\上证50.txt",p=3,q=1,sampleN=3,from="2010-01-01",to="2015-05-30")
{
	codes=loadcodeset(codeset)
	rocSet=data.frame()
	#basePeriod="weekly"
	basePeriod="monthly"
	for(code in codes){
		mm=loadhist(code,period=basePeriod,from=from,to=to,atleast=(p+q))
		if(class(mm) == "zoo"){
		    rocp=ROC(mm$Close,p)
   		    rocq=ROC(mm$Close,q)
		    #print(rocq)
   		    rocq=lag(ROC(mm$Close,q),k=q)
		    #print(rocq)
		    rocq=as.vector(rocq)
   		    rocq=c(rocq,rep(NA,q))
		    rocq=rocq/q
		    #print(rocq)
		    res=data.frame(code=code,rocp=rocp,rocq=rocq,date=index(rocp))
		    #print(res)
		    rocSet=rbind(rocSet,res)
		}
	}

	#dp for ref
	dp=loadhist("999999",period=basePeriod,from=from,to=to,atleast=(p+q))
	dprocq=c(as.vector(lag(ROC(dp$Close,q),q)),rep(NA,q))
	dprocq=dprocq/q
	dprocq=data.frame(rocq=dprocq,date=index(dp))

	#remove the lines with NA
        rocSet=rocSet[complete.cases(rocSet),]
	res=data.frame()
	interestedDate=rocSet[!duplicated(rocSet$date),]$date
	for (dat in interestedDate){
		sub=rocSet[which(rocSet$date == dat),]
		#get top/bot N code
		sub=sub[order(sub$rocp),]
		bottom=head(sub,sampleN)
		top=tail(sub,sampleN)
		print("top")
		print(top)
		print("bottom")
		print(bottom)
		topret=mean(top$rocq)
		bottomret=mean(bottom$rocq)
		dproc=dprocq[which(dprocq$date ==dat),]$rocq
		topSharp=topret-dproc
		botSharp=bottomret - dproc
		if( top$code == "300001"){
			print(dat)
			print(topret)
			print(bottomret)
			print(dproc)
			print(topSharp)
			print(botSharp)
			print(top$code)
			print(bottom$code)
		}
		bb=data.frame(date=dat,topret=topret,bottomret=bottomret,dproc=dproc,topSharp=topSharp,botSharp=botSharp,topCode=top$code,bottomCode=bottom$code)
		res=rbind(res,bb)
	}

	topSharpAvg=mean(res$topSharp)
	botSharpAvg=mean(res$botSharp)
	topWinRate=nrow(res[which(res$topSharp > 0),]) / nrow(res)
	botWinRate=nrow(res[which(res$botSharp > 0),]) / nrow(res)

	res=list(res=res,topSharp=topSharpAvg,botSharp=botSharpAvg,topWinRate=topWinRate,botWinRate=botWinRate)

	return(res)
}

dongLiangBat<-function(codeset)
{
	ps=seq(1,24)
	qs=seq(1,12)
	res=data.frame()
	for(p in ps){
		for (q in qs){
                    aa=dongLiang(codeset=codeset,p=p,q=q,sampleN=3,from="2010-01-01",to="2014-05-30")
		    bb=data.frame(p=p,q=q,topSharp=aa$topSharp,botSharp=aa$botSharp,topWinRate=aa$topWinRate,botWinRate=aa$botWinRate)
		    res=rbind(res,bb)
		}
	}
	return(res)
}

