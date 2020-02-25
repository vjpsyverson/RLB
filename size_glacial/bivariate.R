require(paleoTS);require(xlsx);require(Hotelling)
path<-"Cats/"
taxon<-"Lynx_rufus"
meas<-"mt3"
data<-read.xlsx(paste0(path,taxon,".xlsx"),sheetName=meas)[,c("Pit","Age","L","W")]
modern<-subset(data,data$Age==0)
fossil<-subset(data,data$Age!=0)
test<-hotelling.test(modern[,c("L","W")],fossil[,c("L","W")])
plot(0,0,xlim=range(data$L),ylim=range(data$W),pch=0,xlab=paste(meas,"length"),ylab=paste(meas,"width"),main=paste(taxon,meas))
  points(modern$L,modern$W,pch=1);points(mean(modern$L),mean(modern$W),pch=16)
  points(fossil$L,fossil$W,pch=2);points(mean(fossil$L),mean(fossil$W),pch=17)
  legend("topright",legend=c("modern","modern mean","fossil","fossil mean"),pch=c(1,16,2,17),cex=0.9,bg="transparent")
  legend("bottomleft",legend=c(paste("Hotelling T2",capture.output(test)[1]),capture.output(test)[4]),bty="n",cex=0.9)
