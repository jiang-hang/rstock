#source("c:\\myR\\stockutil_win.R")

source("globals.R")  #it is required by stockUtil_win.R
source("stockUtil_win.R")
source("tdxBankuai.R")
source("stockfunc.R")
source("s.dongliang.R")
source("s.nmm.R")

aa=dongLiangSelect("bankuai2/chuangye300.txt",p=23,q=9,from="2010-01-01",to=(Sys.Date()+1))
bb=aa[which(aa$date == tail(aa[order(aa$date),],1)$date) , ]  #the data for the last month
bb=bb[order(bb$rocp),]
top=as.numeric(tail(bb,10)$code)
savecodeset(top,"chuangyeDongLiangTop.txt")

#aa=dongLiangSelect("bankuai2/hushen300.txt",p=23,q=2,from="2010-01-01",to=(Sys.Date()+1))
#bb=aa[which(aa$date == tail(aa[order(aa$date),],1)$date) , ]  #the data for the last month
#bb=bb[order(bb$rocp),]
#top=as.numeric(tail(bb,10)$code)
#savecodeset(top,"hushenDongLiangTop.txt")
#
##aa=kmrDaily()
#
#aa=dailyFeatures()
#aa=dailyFeatures(codeset="bankuai\\zhishu.txt",out="zhishuDailyFeatures.txt",beta_out="zhishuBeta.txt")
#
#
##topBeta and TopAlpha
#aa=read.table("beta.txt",head=T)
#aa=aa[order(aa$beta),]
#bb=aa[complete.cases(aa),]
#aa=tail(bb,20)$code
#savecodeset(aa,"topBeta.txt")
#aa=head(bb,20)$code
#savecodeset(aa,"botBeta.txt")
#
#bb=bb[order(bb$alpha),]
#aa=tail(bb,20)$code
#savecodeset(aa,"topAlpha.txt")
#aa=head(bb,20)$code
#savecodeset(aa,"botAlpha.txt")
#
#
#source("rmb.R")
#aa=s.rmb()
#write.table(aa,"rmb.txt",row.names=F,quote=F,sep=",")
#
#source("xdb.R")
##hyjzd
#aa=shhyjzd()
#shJzd=as.numeric(tail(aa,1))
#bb=as.data.frame(tail(aa,10))
#write.table(bb,file="shhyjzd.txt",quote=F)
#aa=szhyjzd()
#szJzd=as.numeric(tail(aa,1))
#bb=as.data.frame(tail(aa,10))
#write.table(bb,file="szhyjzd.txt",quote=F)
#
#aa=shxdb(sh=T)
#shQutong=as.numeric(tail(aa,1))
#bb=as.data.frame(tail(aa,10))
#write.table(bb,file="shqutong.txt",quote=F)
#aa=shxdb(sh=F)
#szQutong=as.numeric(tail(aa,1))
#bb=as.data.frame(tail(aa,10))
#write.table(bb,file="szqutong.txt",quote=F)
#aa=data.frame(shJzd=sprintf("%.2f",shJzd),szJzd=sprintf("%.2f",szJzd),shQutong=sprintf("%.2f",shQutong),szQutong=sprintf("%.2f",szQutong))
#write.table(aa,"xdb.txt",quote=F,row.names=F)
#
#
##tdxBankuai
##aa=foxHenDaily()
