#nmm 自然市场镜像
#动量
s.nmm<-function(x)
{
	fab=c(1,2,3,5,8,13,21,34,55)
	retn=NULL
	#o1=ROC(x$Close,1)/sqrt(1)
	#o2=ROC(x$Close,2)/sqrt(2)
	#o3=ROC(x$Close,3)/sqrt(3)
	#o5=ROC(x$Close,5)/sqrt(5)
	#o8=ROC(x$Close,8)/sqrt(8)
	#o13=ROC(x$Close,13)/sqrt(13)
	#o21=ROC(x$Close,21)/sqrt(21)
	#o34=ROC(x$Close,34)/sqrt(34)
	#o55=ROC(x$Close,55)/sqrt(55)
	#nmm = (o1 + o2 + o3 + o5 + o8 + o13 + o21 + o34 + o55)/9
	for( k  in 1:40 ) {
		if(is.null(retn)){
		    retn = ROC(x$Close,k)/sqrt(k)
		}else{
		    retn = retn + ROC(x$Close,k)/sqrt(k)
		}
	}
	retn = 100 * retn / 40 
	return(retn)
}
s.nmr<-function(x)
{
	fab=c(1,2,3,5,8,13,21,34,55)
	retn=NULL
	#o1=ROC(x$Close,1)/sqrt(1)
	#o2=ROC(x$Close,2)/sqrt(2)
	#o3=ROC(x$Close,3)/sqrt(3)
	#o5=ROC(x$Close,5)/sqrt(5)
	#o8=ROC(x$Close,8)/sqrt(8)
	#o13=ROC(x$Close,13)/sqrt(13)
	#o21=ROC(x$Close,21)/sqrt(21)
	#o34=ROC(x$Close,34)/sqrt(34)
	#o55=ROC(x$Close,55)/sqrt(55)
	#nmm = (o1 + o2 + o3 + o5 + o8 + o13 + o21 + o34 + o55)/9
	for( k  in 1:40 ) {
		if(is.null(retn)){
		    retn = ROC(x$Close,k)/sqrt(k)
		}else{
		    retn = retn + ROC(x$Close,k)*(sqrt(k)-sqrt(k-1))
		}
	}
	return(retn)
}
s.nmm.plot<-function(code,nmm=T){
	x=loadhist(code,from="2012-01-01",to="2015-11-16",fetchLatestPrice=T)
	if(nmm){
	    retn=s.nmm(x)
	}else{
	    retn=s.nmr(x)
	}
	ma40=runMean(x$Close,40)
	y=merge(x,retn)
	y=merge(y,ma40)
	dfdd=as.data.frame(y)
	dfdd$color=ifelse(dfdd$retn>0,"green","red")
	dfdd$shape=ifelse(dfdd$ma40 > dfdd$Close,1,2)
	dfdd=tail(dfdd,200)
	dfdd$index=1:nrow(dfdd)
	#print(head(dfdd))
	
	lastretn=tail(dfdd,1)$retn
	#if(lastretn > 0){
	    mfile=paste("img\\nmm",code,".png",sep="")
            #pdf(file=mfile)
	    p<-ggplot(data=dfdd,aes(index,Close))
	    p + geom_line() + geom_point(aes(colour=color,size=abs(retn))) 
	    #p + geom_line() + geom_point(aes(colour=color,size=1,shape=factor(shape))) 
	    #ggsave(file=mfile)
	    #dev.off()
	#}
	#qplot(1:200,Close,data=dfdd,geom=c("point"),color=color,size=abs(retn)) 
	#qplot(index(dfdd),dfdd$Close,geom=c("point"),color=sign(dfdd$retn),size=abs(dfdd$retn))
}

s.nmm.plot.bat<-function(codeset)
{
    codes=loadcodeset(codeset)
    for(code in codes){
	    print(code)
	    s.nmm.plot(code)
    }
}
