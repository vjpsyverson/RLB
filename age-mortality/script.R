require(openxlsx);require(diptest);require(mclust);require(LaplacesDemon)

tooth<-openxlsx::read.xlsx("measurements/CamelopsJaws.xlsx")[,1]
dens<-density(tooth)
plot(dens,add=T,ylim=c(0,max(c(hist(tooth,plot=F)$density),dens$y)))
hist(tooth,freq=T,add=T)
lines(dens)

Camelops<-list()
for (i in 1:4){
data<-openxlsx::read.xlsx("measurements/Camelops.xlsx",i)[,2]
print(length(data))
Camelops[[i]]<-data}
names(Camelops)<-c("Humerus","Radius","Femur","Tibia")
openxlsx::getSheetNames("measurements/Camelops.xlsx")
Camelops<-Camelops[order(names(Camelops))]
Smilodon<-list()
for (i in 1:4){
  data<-openxlsx::read.xlsx("measurements/Smilodon.xlsx",i)[,5]
  print(length(data))
  Smilodon[[i]]<-data}
names(Smilodon)<-openxlsx::getSheetNames("measurements/Smilodon.xlsx")
Smilodon<-Smilodon[order(names(Smilodon))]
Canis<-list()
for (i in 1:4){
  data<-openxlsx::read.xlsx("measurements/Canis.xlsx",i)[,2]
  print(length(data))
  Canis[[i]]<-data}
names(Canis)<-openxlsx::getSheetNames("measurements/Canis.xlsx")
Canis<-Canis[order(names(Canis))]



all_results<-list()

data<-Camelops
data<-Canis
data<-Smilodon

results<-array(dim=c(4,5))
colnames(results)<-c("NSpecs","NModes","ModeCI1","ModeCI2","ModeCI3")
rownames(results)<-names(data)
for (i in 1:4){
  test<-capture.output(
    p.interval(na.omit(data[[i]]),MM=TRUE))
  if (length(test)>5) {
    intoutput<-unlist(strsplit(test[[6]]," +"))
    intoutput<-intoutput[5:length(intoutput)]
    } else {
    intoutput<-paste0("(",paste(unlist(strsplit(test[[2]]," +"))[2],unlist(strsplit(test[[2]]," +"))[3],sep=","),")")
    }
  result<-c(length(data[[i]]),
            length(Modes(data[[i]])$modes),
            intoutput)
  results[i,1:length(result)]<-result
}
all_results$Camelops<-results
all_results$Canis<-results
all_results$Smilodon<-results
write.csv(all_results,file="results/modetest.csv")

par(mfrow=c(2,2),mar=c(2,2,2,2),ask=F)
for (i in 1:4) {
  dens<-density(data[[i]],na.rm=T)
  plot(dens,ylim=c(0,max(c(hist(data[[i]],plot=F)$density),dens$y)),
    main=paste("Smilodon",names(data)[[i]]),sub="")
  hist(data[[i]],add=T,freq=T)
  lines(dens)
}

