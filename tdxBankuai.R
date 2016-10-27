#打印每个代码属于哪些tdx板块


#生成 每个代码所属的tdx板块
genCodeWithTdxBk<-function()
{
	bkMap=read.table("bankuai\\blockMap.txt",sep=",",head=F)
	colnames(bkMap)=c("name","code","num")

	tbkMap=bkMap[which(bkMap$num > 0),]

	bk=list()
	codes=loadcodeset()
	for(code in codes){
		bk[[code]]=NULL
	}

	for (idx in seq(1,nrow(tbkMap))){
		cc=scan(paste("bankuai\\",as.character(tbkMap[idx,1]),".txt",sep=""),what="")
		#print(cc)
		print(as.character(tbkMap[idx,1]))
		for( code in cc) {
			if (! as.character(tbkMap[idx,1]) %in% bk[[code]] ) {
			    bk[[code]] = c(bk[[code]],as.character(tbkMap[idx,1]))
			}
		}
	}

	#print(bk)
	myfile="codeWithTdxBk.txt"
	cat("",file=myfile)
	for(idx in seq(1,length(bk))){
		cat(paste(names(bk)[idx],",",sep=""),file=myfile,append=T)
		cat(bk[[idx]],file=myfile,append=T,sep=",",fill=400)
	}

	return(bk)
}


