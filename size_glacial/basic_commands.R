require(openxlsx);require(paleoTS)
#general form of data processing
#taxon<-"athene"
#data<-read.csv(paste0("csvs/",taxon,"TMT.csv"))
taxon<-"Neophrontops"
path<-"vultures/"
data<-read.xlsx(paste0(path,taxon,".xlsx"),1)[,c("AGE","Length","Area","Midshaft")]
names(data)<-c("AGE","LENGTH","DEPTH","WIDTH")
data$DEPTH<-data$WIDTH
data$ROBUSTNESS<-(pi*(data$WIDTH/2)^2)/data$LENGTH
data<-data[-which(is.na(data$WIDTH)),]
#data$ROBUSTNESS<-(data$WIDTH*data$DEPTH)/data$LENGTH
#data<-read.xlsx(paste0(taxon,".xlsx"),1)[,c("PIT","AGE","LENGTH","WIDTH","DEPTH")]
#data<-subset(data,data$PIT!="AMNH")[,-1]
#data<-data[,-1]
age.fac<-factor(data$AGE,levels=rev(levels(factor(data$AGE))))
table(data$AGE)
#basic stats
filename<-paste0(taxon,"_basic.csv")
write.csv(taxon,file=filename)
for(i in 1:3){
  suppressWarnings(write(names(data)[i+2],file=filename,append=T))
  statblock<-data.frame(Age.ka=levels(age.fac),
                        N=as.numeric(table(age.fac)),
                        Mean=as.numeric(by(data[,i+2],age.fac,mean)),
                        SD=as.numeric(by(data[,i+2],age.fac,sd)))
  statblock$CV=statblock$Mean/statblock$SD
  suppressWarnings(write.table(statblock,file=filename,append=T,row.names=F))
}

#TS models
pit.ages<-read.csv("pitages_Fuller2015.csv",stringsAsFactors = F)
dataTS<-make.paleoTS(data,agelookup=F)
#dataTS<-make.paleoTS(data,agelookup=F)
TSfit4<-lapply(dataTS,fit4models,pool=F)
TSfit4.wt<-rbind(TSfit4$LENGTH$Akaike.wt,TSfit4$WIDTH$Akaike.wt,TSfit4$DEPTH$Akaike.wt,TSfit4$ROBUSTNESS$Akaike.wt);rownames(TSfit4.wt)<-names(TSfit4);colnames(TSfit4.wt)<-rownames(TSfit4$LENGTH)
write.table(TSfit4.wt,paste0(taxon,"TS.csv"),row.names = F)
#TS plots
source("read.paleoTS.R") #for some reason the function is not loading with the package; this is from github
par(mfrow=c(2,2),mar=c(2,3,3,1),oma=c(1,0,2,0)); for (i in 1:4) {
  plot.paleoTS(dataTS[[i]],main=tolower(names(dataTS)[i]),ylim=range(data[,i+1]))
  points(data[,1],data[,i+1])
}; mtext(paste0(taxon," (with AMNH specimens)"),outer=TRUE,cex=1.5)
#significance tests
correction<-0.05/length(levels(age.fac))
KWtest<-array(dim=c(4,4));rownames(KWtest)<-names(data[-1]);colnames(KWtest)<-c("K-W chi-squared","df","p-value",paste0("significant (p<",correction,")"))
for (i in 1:4) {
  test<-kruskal.test(data[,i+1],age.fac)
  KWtest[i,1:4]<-c(test$statistic,test$parameter,test$p.value,test$p.value<correction)
}
filename<-paste0(taxon,"KWtest.csv")
write.csv(KWtest,file=filename)
MWboot<-array(dim=c(length(levels(age.fac)),4));colnames(MWboot)<-names(data)[-1];rownames(MWboot)<-levels(age.fac)
for (j in 1:4){
  groups<-split(data[,j+1],age.fac)
  for (i in 1:length(levels(age.fac))) {
    x1<-unlist(groups[i]); x2<-unlist(groups[-i])
    MWboot[i,j]<-round(wilcox.test(x1,x2)$p.value,4)
  }
}
for(i in 1:length(MWboot)) if(MWboot[i]<correction) MWboot[i]<-paste0(MWboot[i],"*")
MWx<-MWboot
for (j in 1:4){
  groups<-split(data[,j+1],age.fac)
  for (i in 1:length(levels(age.fac))) {
    x1<-unlist(groups[i]); x2<-unlist(groups[-i])
    MWx[i,j]<-round(wilcox.test(x1,x2)$statistic,4)
  }
}
MWresults<-cbind(MWx[,"LENGTH"],MWboot[,"LENGTH"],MWx[,"WIDTH"],MWboot[,"WIDTH"],MWx[,"DEPTH"],MWboot[,"DEPTH"],MWx[,"ROBUSTNESS"],MWboot[,"ROBUSTNESS"]);colnames(MWresults)<-c("Length.U","Length.p","Width.U","Width.p","Depth.U","Depth.p","Robustness.U","Robustness.p")
write.csv(MWresults,paste0(taxon,"MW.csv"))

#--------------------
#new dates (Fuller et al. 2015)
require(Bchron)
pitAgesNew<-read.csv("pitages_Fuller2015.csv",stringsAsFactors=F)[-c(12,13,14),]
ages1<-Bchron::BchronCalibrate(ages=pitAgesNew$MeanAge, ageSds = pitAgesNew$SD,calCurves = rep('intcal13',11))
plot(ages1$Date1)
plot(ages1)
age_samples<-Bchron::sampleAges(ages1)
ageRanges<-t(rbind(apply(age_samples, 2, quantile, prob=c(0.05,0.5,0.95))))
rownames(ageRanges)<-pitAgesNew$PitNo
plot(ageRanges[,2],1:11,xlim=rev(range(ageRanges)),col="white",xlab="cal BP",main="Age ranges of pits (95% CI)",yaxt="n",ylab="")
segments(y0=1:11,x0=ageRanges[,1],x1=ageRanges[,3])
points(ageRanges[,2],1:11,col="white",pch=20,cex=2)
text(y=1:11,x=ageRanges[,2],labels=rownames(ageRanges),cex=0.8)
write.csv(ageRanges,"ageRanges_Fuller2015_cal.csv")
