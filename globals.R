
#CPI -> 382_CPI.txt
g_hongguangFilesLoaded=0
g_hongguangFiles=NULL
#define the globals
#流通股
g_liutonggu=NULL
g_shizhi=NULL
g_names=NULL
#板块
g_fox=NULL
#大盘
g_cow=NULL


#历史数据
g_hist=NULL

#运行时状态
g_state=NULL

loadLiutonggu<-function()
{
	if(is.null(g_liutonggu)){
	    print("loading ltg")
	    bk=read.table("liutonggu.txt",header=T,sep=",")
	    colnames(bk)=c("code","ltg")
	    bk$ltg=bk$ltg*100000000
	    g_liutonggu<<-bk
	}
}

#load bankaui in bankuai0526.csv
loadbk<-function()
{
    bk<-read.table("bankuai0526.csv",header=T,sep=",")
    return(bk)
}

loadFoxNames<-function()
{
	if(is.null(g_fox)){
	    bkfile="bankuai\\blockMap.txt"
            bkMap=read.table(bkfile,sep=",",head=F)
	    colnames(bkMap)=c("name","code","num")
	    bkMap=bkMap[which(bkMap$num>0),]
	    g_fox<<-bkMap
	}
}

loadHenNames<-function()
{
	if(is.null(g_names)){
	}
}

loadshizhiBk<-function(){
    bk=read.table("shizhiBk.txt",header=T,sep=",")
    bk=bk[,c("code","shizhiBk")]
    return(bk)
}

loadlinfo<-function()
{
    linfo=read.table("C:\\genData\\latest_cninfo\\linfoAll.txt",head=T,sep=",")
    names(linfo)<-c("code","zguben","ltguben","gjguben","frguben","fqrguben","zpguben","bguben","hguben","eps","zbgongji","wfplirun","jzcsyl","jzc")
    return(linfo)
}
loadcodeset<-function(codeset="C:\\myR\\bankuai\\codes.txt"){
    codes<-scan(codeset,what="")
    return(codes)
}

loadghist<-function(...)
{
	if(is.null(g_hist)){
		margs=list(...)
		from="2010-01-01"
		to=Sys.Date() + 1
		if(!is.null(margs$to)){
			to=margs$to
		}
		codes=loadcodeset()
		g_hist=list()
		for (code in codes){
		      cname=getMarketCode(code)
		      aa=loadhist(code,from=from,to=to)
		      g_hist[[cname]]<<- aa	
		}

	}
}

savecodeset<-function(x,myfile="out.txt")
{
	dd=sprintf("%06d",x)
        write.table(as.data.frame(dd),file=myfile,col.names=F,row.names=F,quote=F)
}
