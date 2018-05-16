source("makepaleoTS.R")
require(paleoTS)
require(xlsx)
options(stringsAsFactors = F)

caracara.TS<-make.paleoTS("caracara.csv")
caracara.test<-test3.paleoTS(caracara.TS)

equusAstrag.TS<-make.paleoTS("equusAstragali.csv")

aquila.TS<-make.paleoTS("aquilaTMT.csv")
aquila.test<-test3.paleoTS(aquila.TS)
x<-compareModels(opt.URW(aquila.TS[[1]]),opt.Stasis(aquila.TS[[1]]))
aquila.test[1,2:3]<-x$Akaike.wt
write.csv(aquila.test,file="modelTests/aquilaTest.csv")

setwd("C:/Users/vjsyvers/Documents/R/RLB/csvs")
fileList<-dir()
fileList<-fileList[-c(14,19,29:32)]

for (i in 1:length(fileList)){
	print(fileList[i])
	TS<-make.paleoTS(fileList[i])
	test<-test3.paleoTS(TS)
	write.csv(test,file=paste("modelTests/",substr(fileList[i],1,nchar(fileList[i])-4),"Test.csv",sep=""))
	}

count<-array(dim=3)
for (i in 1:length(fileList)){
	print(fileList[i])
	TS<-make.paleoTS(fileList[i])
	foo<-array(dim=c(length(TS),3))
	foo[,1]<-rep(fileList[i],length(TS))
	foo[,2]<-names(TS)
	for (j in 1:length(TS)){foo[j,3]<-sum(TS[[j]]$nn)}
	count<-rbind(count,foo)
	}



test<-make.paleoTS("gymnogypsCMC.csv")
testTS<-test3.paleoTS(test)
length(test)
par(mfrow=c(1,1))
for (i in 2:2) plot(test[[i]])

opt.GRW(test[[1]],cl=list(fnscale=-1, ndeps=c(1e-9, 1e-9)))
fit3models.hacked(test[[1]])
fit3models(test[[1]])

fileList2<-fileList[c(3,9,10,13,15,16,17,19,21,22,24)]
for (i in 1:length(fileList2)){
	print(fileList2[i])
	TS<-make.paleoTS(fileList2[i])
	test<-test3.paleoTS(TS)
#	write.csv(test,file=paste("modelTests/",substr(fileList2[i],1,nchar(fileList2[i])-4),"Test.csv",sep=""))
	}

size<-array(dim=length(fileList))
for (i in 1:length(fileList)){
	temp<-read.csv(fileList[i])
	ages<-array(dim=dim(temp)[1])
	for (j in 1:dim(temp)[1]){
		pits<-pit.ages[which(pit.ages[,1]==temp[j,1]),2]
		if (length(pits)>0){ages[j]<-pits}
	}
	size[i]<-length(which(!is.na(ages)))
}

setwd("C:/Users/vjsyvers/Documents/R/RLB/csvs/modelTests")
testList<-dir()[-1]
allTests<-array()
testNames<-array()
for (i in 1:length(testList)){
	read<-read.csv(testList[i])
	allTests<-rbind(allTests,read)
	testNames<-c(testNames,rep(substr(testList[i],1,nchar(testList[i])-8),nrow(read)))
	}
allTests<-cbind(testNames[2:length(testNames)],allTests[2:nrow(allTests),])
colnames(allTests)[1:2]<-c("sp","measurement")
write.csv(allTests,file="allTests.csv")

look<-which(allTests[,3]>allTests[,4]|allTests[,3]>allTests[,5])
allTests[look,] #there are a lot! none for & instead of | though

random<-which(allTests[,4]>allTests[,5]&allTests[,4]>allTests[,3])
allTests[random,]

static<-which(allTests[,5]>allTests[,4]&allTests[,5]>allTests[,3])
allTests[static,]

whichModel<-allTests[,3:5]
for (i in 1:dim(allTests)[1]){for (j in 1:3){whichModel[i,j]<-whichModel[i,j]==max(whichModel[i,])}}
apply(allTests[,3:5],2,sum,na.rm=T)
apply(whichModel,2,sum,na.rm=T)
apply(whichModel,2,sum,na.rm=T)/sum(apply(whichModel,2,sum,na.rm=T))

#   GRW    URW Stasis 
# 4.943 97.322 52.742 

#   GRW    URW Stasis 
#     0     99     56

allTests
whichModel<-cbind(allTests[,1:2],whichModel)
whichModel

mod<-opt.Stasis(make.paleoTS("aquilaTMT.csv"))
?sim.GRW

errors<-read.table(file="errors.txt",sep="!")
warnings<-apply(errors,2,grepl,pattern="Warning")

failed<-list(
	list("bisonMetacarpal.csv",4),
	list("camelopsPatellae.csv",c(1:3)),
	list("caracara.csv",c(1,3,5:10)),
	list("equusAstragali.csv",3),
	list("equusBackLimbs.csv",c(1:2)),
	list("equusFrontLimbs.csv",c(1:7)),
	list("felisAstragali.csv",4),
	list("gymnogypsFemora.csv",1),
	list("smilodonHumeri.csv",2))

data<-list()
for (i in 1:length(failed)){
	data[[i]]<-make.paleoTS(failed[[i]][[1]])
	}

fails.under.fit<-list()
for (i in 1:length(data)){
	fails.under.fit[[i]]<-list()
	for (j in 1:length(failed[[i]][[2]])){
		fails.under.fit[[i]][[j]]<-data[[i]][failed[[i]][[2]]][j]
		}
	names(fails.under.fit[[i]])<-rep(failed[[i]][[1]][1],length(failed[[i]][[2]]))
}

failed2<-list(	
	list("caracara.csv",7),
	list("equusAstragali.csv",3))

data2<-list()
for (i in 1:length(failed2)){
	data2[[i]]<-make.paleoTS(failed2[[i]][[1]])
	}

fails.under.tweaked.fit<-list()
for (i in 1:length(data2)){
	fails.under.tweaked.fit[[i]]<-list()
	for (j in 1:length(failed2[[i]][[2]])){
		fails.under.tweaked.fit[[i]][[j]]<-data2[[i]][failed2[[i]][[2]]][j]
		}
	names(fails.under.tweaked.fit[[i]])<-rep(failed2[[i]][[1]][1],length(failed2[[i]][[2]]))
}

#========================
library(paleoTS)
source("makepaleoTS.R")
source("../r-utilities/utilities.R")
temp<-grep("TMT",dir("csvs"),value=T)
all.TMT.names<-paste("csvs/",temp,sep="")
# pit.ages<-read.csv("csvs/pitages.csv")

tyto<-read.csv("TytoTMT.csv")
coragyps<-read.csv("CoragypsTMT.csv")
teratornis<-read.csv("csvs/teratornisTMT.csv")
tytoTS<-make.paleoTS("tytoTMT.csv",agelookup=F)
coragypsTS<-make.paleoTS("coragypsTMT.csv",agelookup=F)
teratornisTS<-make.paleoTS("csvs/teratornisTMT.csv",agelookup=F)
par(mfrow=c(2,3),mar=c(2,2,1,1))
lapply(tytoTS,plot.paleoTS)
lapply(coragypsTS,plot.paleoTS)
lapply(teratornisTS,plot.paleoTS)
dev.copy(png,'teratornis.png')
dev.off()

#images
cairo_pdf(width=8,height=5,filename="coragyps.pdf")
par(mfrow=c(2,3),mar=c(2,2,2,1))
for(i in 1:5) {
	plot(coragyps[,1],y=coragyps[,i+1],col="darkgrey",pch=1,xlim=rev(range(coragyps[,1])),
		xlab="Pit age (ka)", main=colnames(coragyps)[i+1],ylab="")
	plot.paleoTS(coragypsTS[[i]],add=T)
}
dev.off()

# basic stats 
fossil<-coragyps[,1]!=0
test<-apply(coragyps[fossil,2:6],2,kruskal.test,as.factor(coragyps$AGE[fossil]))
kw.result<-data.frame(sapply(test,"[[","statistic"),sapply(test,"[[","parameter"),sapply(test,"[[","p.value"))
write.csv(kw.result,file="kw-result.csv")

coragyps.s<-split(coragyps[,2:6],factor(coragyps[,1]))
MW.boot<-array(dim=c(5,2*length(coragyps.s)))
for(i in 1:length(coragyps.s)){
	other<-coragyps[,1]!=as.numeric(names(coragyps.s[i]))
	for(j in 2:6) {
		MW.boot[j-1,(2*i)-1]<-mean(coragyps[,j][!other])-mean(coragyps[,j][other])
		MW.boot[j-1,2*i]<-wilcox.test(coragyps[,j]~!other)$p.value
	}
}
write.csv(MW.boot,file="MW-boot.csv")

fit4models.punc(coragypsTS[[1]],pool=F,method="AD")
punc<-lapply(coragypsTS,fitGpunc,pool=F,minb=2)
sapply(punc,"[[","parameters")

test<-lapply(coragypsTS,fit4models.punc,minb=2)
test.array<-array(dim=c(5,8))
test.array[,c(1,3,5,7)]<-t(sapply(test,"[[","Akaike.wt"))
test.array[,c(2,4,6,8)]<-t(sapply(test,"[[","AICc"))
write.csv(test.array,file="Akaike.csv")

#general form of data processing
taxon<-"asio"
data<-read.csv(paste0("csvs/",taxon,"TMT.csv"))
data$ROBUSTNESS<-(data$WIDTH*data$DEPTH)/data$LENGTH
dataTS<-make.paleoTS(data,agelookup=F)
age.fac<-factor(data$AGE,levels=rev(levels(factor(data$AGE))))

par(mfrow=c(2,2)); for (i in 1:4) hist(data[,i+1],main=names(data)[i+1])
par(mfrow=c(1,1)); plot(data$LENGTH,data$WIDTH/data$DEPTH,col=age.fac,pch=19)
par(mfrow=c(2,2),mar=c(2,3,3,1),oma=c(1,0,0,0)); for (i in 1:4) {
  plot.paleoTS(dataTS[[i]],main=tolower(names(dataTS)[i]),ylim=range(data[,i+1]))
  points(data[,1],data[,i+1])
}
for (i in 1:4) print(kruskal.test(data[,i+1],age.fac))
correction<-0.05/length(levels(age.fac))
MWboot<-array(dim=c(length(levels(age.fac)),4));colnames(MWboot)<-names(data)[-1];rownames(MWboot)<-levels(age.fac)
for (j in 1:4){
  groups<-split(data[,j+1],age.fac)
  for (i in 1:length(levels(age.fac))) {
    x1<-unlist(groups[i]); x2<-unlist(groups[-i])
    MWboot[i,j]<-round(wilcox.test(x1,x2)$p.value,4)
  }
}
for(i in 1:length(MWboot)) if(MWboot[i]<=correction) MWboot[i]<-paste0(as.character(MWboot[i]),"*")
write.csv(MWboot,paste0(taxon,"MWboot.csv"))
lapply(dataTS,fit4models.punc,pool=F)

#Katherine's thesis
raw<-list(length=data.frame(),width=data.frame(),depth=data.frame(),area=data.frame(),robustness=data.frame())
for (i in 1:5) {
  k<-read.xlsx("Katherine/Black Vulture Data Analysis.xlsm",sheetIndex = 2*i-1)
  raw[[i]]<-data.frame(k)
}
raw$length<-raw$length[,2:6]
raw$width<-raw$width[,1:5]
raw$depth<-raw$depth[,1:5]
for (i in 1:5) {
  raw[[i]]<-raw[[i]][-1,]
  colnames(raw[[i]])<-c(0,14,18,21,35)
}
C.occ<-list(length=list(),width=list(),depth=list(),area=list(),robustness=list())
for (i in 1:5) {
  C.occ[[i]]<-as.list(as.data.frame(raw[[i]]))
}

ecdfPlot<-function(x){
  #x is a list of vectors
  test<-unlist(lapply(x,ecdf))
  plot(test$`0`)
  plot(test$`14`,col="red",add=T)
  plot(test$`18`,col="blue",add=T)
  plot(test$`21`,col="green",add=T)
  plot(test$`35`,col="yellow",add=T)
}

test<-lapply(C.occ,kruskal.test)
