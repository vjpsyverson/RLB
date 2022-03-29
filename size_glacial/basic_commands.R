require(openxlsx);require(paleoTS)
#general form of data processing
#taxon<-"athene"
#data<-read.csv(paste0("csvs/",taxon,"TMT.csv"))
taxon<-"B.swainsoni"
path<-"largebirds/Buteo/"
data<-read.xlsx(paste0(path,taxon,".xlsx"),1)[,c("Age","Length","Width")]
names(data)<-c("AGE","LENGTH","WIDTH")
#data$DEPTH<-data$WIDTH
data$ROBUSTNESS<-(pi*(data$WIDTH/2)^2)/data$LENGTH
#data<-data[-which(is.na(data$WIDTH)),]
#data$ROBUSTNESS<-(data$WIDTH*data$DEPTH)/data$LENGTH
#data<-read.xlsx(paste0(taxon,".xlsx"),1)[,c("PIT","AGE","LENGTH","WIDTH","DEPTH")]
#data<-subset(data,data$PIT!="AMNH")[,-1]
data<-data[-c(107),]

age.fac<-factor(data$AGE,levels=rev(levels(factor(data$AGE))))
table(data$AGE)
minspecimens<-sum(unlist(lapply(table(data$AGE),function(x) ceiling(x/2))))
#basic stats
nvar=3
nignore=2
statblock<-vector("list",nvar)
for(i in 0:(nvar-1)){
  statblock[[i+1]]<-data.frame(Character=names(data)[i+nignore],
                        Age.ka=levels(age.fac),
                        N=round(as.numeric(table(age.fac)),1),
                        Mean=round(as.numeric(by(data[,i+nignore],age.fac,mean)),1),
                        SD=round(as.numeric(by(data[,i+nignore],age.fac,sd)),5))
  statblock[[i+1]]$CV=round(statblock[[i+1]]$Mean/statblock[[i+1]]$SD,5)
}
write.csv(do.call(rbind.data.frame,statblock),file=paste0(path,taxon,"_basic.csv"),row.names=F)
for (i in 1:nvar) {
  normtest<-by(data[,i+1],age.fac,shapiro.test)
  print(c(names(data)[i+1],normtest))
}
#TS models
pit.ages<-read.csv("pitages_Fuller2015.csv",stringsAsFactors = F)
dataTS<-make.paleoTS(data,agelookup=F)
#dataTS<-make.paleoTS(data,agelookup=F)
TSfit4<-lapply(dataTS,fit4models,pool=F)
#TSfit4.wt<-rbind(TSfit4$LENGTH$Akaike.wt,TSfit4$WIDTH$Akaike.wt,TSfit4$DEPTH$Akaike.wt,TSfit4$ROBUSTNESS$Akaike.wt);rownames(TSfit4.wt)<-names(TSfit4);colnames(TSfit4.wt)<-rownames(TSfit4$LENGTH)
TSfit4.wt<-rbind(TSfit4$LENGTH$Akaike.wt,TSfit4$WIDTH$Akaike.wt,TSfit4$ROBUSTNESS$Akaike.wt);rownames(TSfit4.wt)<-names(TSfit4);colnames(TSfit4.wt)<-rownames(TSfit4$LENGTH)
write.csv(TSfit4.wt,paste0(path,taxon,"TS.csv"),row.names = T)
#TS plots
source("read.paleoTS.R") #for some reason the function is not loading with the package; this is from github
par(mfrow=c(1,3),mar=c(2,3,3,1),oma=c(1,0,2,0)); for (i in 1:3) {
  plot.paleoTS(dataTS[[i]],main=tolower(names(dataTS)[i]),ylim=range(data[,i+1]))
  points(data[,1],data[,i+1])
}; mtext(paste0("Buteo swainsoni TMT"),outer=TRUE,cex=1.5)
#significance tests
correction<-0.05/length(levels(age.fac))
KWtest<-array(dim=c(nvar,4));rownames(KWtest)<-names(data[-1]);colnames(KWtest)<-c("K-W chi-squared","df","p-value",paste0("significant (p<",round(correction,6),")"))
for (i in 1:nvar) {
  test<-kruskal.test(data[,i+1],age.fac)
  KWtest[i,1:4]<-c(round(test$statistic,2),test$parameter,round(test$p.value,8),test$p.value<correction)
}
filename<-paste0(path,taxon,"KWtest.csv")
write.csv(KWtest,file=filename)
MWboot<-array(dim=c(length(levels(age.fac)),nvar));colnames(MWboot)<-names(data)[-1];rownames(MWboot)<-levels(age.fac)
for (j in 1:nvar){
  groups<-split(data[,j+1],age.fac)
  for (i in 1:length(levels(age.fac))) {
    x1<-unlist(groups[i]); x2<-unlist(groups[-i])
    MWboot[i,j]<-round(wilcox.test(x1,x2)$p.value,4)
  }
}
for(i in 1:length(MWboot)) if(MWboot[i]<correction) MWboot[i]<-paste0(MWboot[i],"*")
MWx<-MWboot
for (j in 1:nvar){
  groups<-split(data[,j+1],age.fac)
  for (i in 1:length(levels(age.fac))) {
    x1<-unlist(groups[i]); x2<-unlist(groups[-i])
    MWx[i,j]<-round(wilcox.test(x1,x2)$statistic,4)
  }
}
#MWresults<-cbind(MWx[,"LENGTH"],MWboot[,"LENGTH"],MWx[,"WIDTH"],MWboot[,"WIDTH"],MWx[,"DEPTH"],MWboot[,"DEPTH"],MWx[,"ROBUSTNESS"],MWboot[,"ROBUSTNESS"]);colnames(MWresults)<-c("Length.U","Length.p","Width.U","Width.p","Depth.U","Depth.p","Robustness.U","Robustness.p")
MWresults<-cbind(MWx[,"LENGTH"],MWboot[,"LENGTH"],MWx[,"WIDTH"],MWboot[,"WIDTH"],MWx[,"ROBUSTNESS"],MWboot[,"ROBUSTNESS"]);colnames(MWresults)<-c("Length.U","Length.p","Width.U","Width.p","Robustness.U","Robustness.p")
write.csv(MWresults,paste0(path,taxon,"MW.csv"))

#--------------------
#new dates (Fuller et al. 2015)
#require(Bchron)
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


