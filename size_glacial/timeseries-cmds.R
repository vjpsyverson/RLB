require(paleoTS)
setwd("C:/Users/vjsyvers/Documents/R")
write.csv(models,file="RLB/Gymnogyps/models.csv")

G.fem<-read.csv(file="RLB/Gymnogyps/all_femora.csv")
G.fem.aged<-G.fem[-which(G.fem[,3]=="Unk"),]

G.fem.age.means<-array(dim=c(length(levels(G.fem.aged[,3]))-1,dim(G.fem.aged)[2]-1))
colnames(G.fem.age.means)<-c(colnames(G.fem.aged)[3],"n",colnames(G.fem.aged)[4:13])
for (i in 1:dim(G.fem.age.means)[1]){
	age<-levels(G.fem.aged[,3])[i]
	G.fem.age.means[i,1]<-age
	G.fem.age.means[i,2]<-length(which(G.fem.aged[,3]==age))
	G.fem.age.means[i,3:12]<-mean(G.fem.aged[which(G.fem.aged[,3]==age),4:13],na.rm=T)
	}
G.fem.age.means<-apply(G.fem.age.means,2,as.numeric)

G.fem.age.vars<-array(dim=c(length(levels(G.fem.aged[,3]))-1,dim(G.fem.aged)[2]-1))
colnames(G.fem.age.vars)<-c(colnames(G.fem.aged)[3],"n",colnames(G.fem.aged)[4:13])
for (i in 1:dim(G.fem.age.vars)[1]){
	age<-levels(G.fem.aged[,3])[i]
	G.fem.age.vars[i,1]<-age
	G.fem.age.vars[i,2]<-length(which(G.fem.aged[,3]==age))
	G.fem.age.vars[i,3:12]<-diag(var(G.fem.aged[which(G.fem.aged[,3]==age),4:13],na.rm=T))
	}
G.fem.age.vars<-apply(G.fem.age.vars,2,as.numeric)

G.fem.age.sd<-array(dim=c(length(levels(G.fem.aged[,3]))-1,dim(G.fem.aged)[2]-1))
colnames(G.fem.age.sd)<-c(colnames(G.fem.aged)[3],"n",colnames(G.fem.aged)[4:13])
for (i in 1:dim(G.fem.age.sd)[1]){
	age<-levels(G.fem.aged[,3])[i]
	G.fem.age.sd[i,1]<-age
	G.fem.age.sd[i,2]<-length(which(G.fem.aged[,3]==age))
	G.fem.age.sd[i,3:12]<-sd(G.fem.aged[which(G.fem.aged[,3]==age),4:13],na.rm=T)
	}
G.fem.age.sd<-apply(G.fem.age.sd,2,as.numeric)

G.fem.length<-as.paleoTS(mm=G.fem.age.means[,3],vv=G.fem.age.vars[,3],nn=G.fem.age.means[,2],tt=G.fem.age.means[,1])
aG.fem.proxA<-as.paleoTS(mm=G.fem.age.means[,10],vv=G.fem.age.vars[,3],nn=G.fem.age.means[,2],tt=G.fem.age.means[,1])
G.fem.midA<-as.paleoTS(mm=G.fem.age.means[,11],vv=G.fem.age.vars[,3],nn=G.fem.age.means[,2],tt=G.fem.age.means[,1])
G.fem.distA<-as.paleoTS(mm=G.fem.age.means[,12],vv=G.fem.age.vars[,3],nn=G.fem.age.means[,2],tt=G.fem.age.means[,1])

par(mfrow=c(2,2))
plot(G.fem.length,main="length")
plot(G.fem.proxA,main="proximal area")
plot(G.fem.midA,main="midshaft area")
plot(G.fem.distA,main="distal area")

fem.models<-cbind("femur",c(rep("length",3),rep("proximal area",3),rep("midshaft area",3),rep("distal area",3)),rbind(fit3models(G.fem.length),fit3models(G.fem.proxA),fit3models(G.fem.midA),fit3models(G.fem.distA)))
colnames(fem.models)[1:2]<-c("element","dimension")
models<-fem.models

###

G.tmt<-read.csv(file="RLB/Gymnogyps/all_tmt.csv")
G.tmt.aged<-G.tmt[-which(G.tmt[,3]=="Unk"),]

G.tmt.age.means<-array(dim=c(length(levels(G.tmt.aged[,3]))-1,dim(G.tmt.aged)[2]-1))
colnames(G.tmt.age.means)<-c(colnames(G.tmt.aged)[3],"n",colnames(G.tmt.aged)[4:13])
for (i in 1:dim(G.tmt.age.means)[1]){
	age<-levels(G.tmt.aged[,3])[i]
	G.tmt.age.means[i,1]<-age
	G.tmt.age.means[i,2]<-length(which(G.tmt.aged[,3]==age))
	G.tmt.age.means[i,3:12]<-mean(G.tmt.aged[which(G.tmt.aged[,3]==age),4:13],na.rm=T)
	}
G.tmt.age.means<-apply(G.tmt.age.means,2,as.numeric)
G.tmt.age.means<-G.tmt.age.means[1:8,]

G.tmt.age.vars<-array(dim=c(length(levels(G.tmt.aged[,3]))-1,dim(G.tmt.aged)[2]-1))
colnames(G.tmt.age.vars)<-c(colnames(G.tmt.aged)[3],"n",colnames(G.tmt.aged)[4:13])
for (i in 1:dim(G.tmt.age.vars)[1]){
	age<-levels(G.tmt.aged[,3])[i]
	G.tmt.age.vars[i,1]<-age
	G.tmt.age.vars[i,2]<-length(which(G.tmt.aged[,3]==age))
	G.tmt.age.vars[i,3:12]<-diag(var(G.tmt.aged[which(G.tmt.aged[,3]==age),4:13],na.rm=T))
	}
G.tmt.age.vars<-apply(G.tmt.age.vars,2,as.numeric)
G.tmt.age.vars<-G.tmt.age.vars[1:8,]

G.tmt.length<-as.paleoTS(mm=G.tmt.age.means[,3],vv=G.tmt.age.vars[,3],nn=G.tmt.age.means[,2],tt=G.tmt.age.means[,1])
G.tmt.proxA<-as.paleoTS(mm=G.tmt.age.means[,10],vv=G.tmt.age.vars[,3],nn=G.tmt.age.means[,2],tt=G.tmt.age.means[,1])
G.tmt.midA<-as.paleoTS(mm=G.tmt.age.means[,11],vv=G.tmt.age.vars[,3],nn=G.tmt.age.means[,2],tt=G.tmt.age.means[,1])
G.tmt.distA<-as.paleoTS(mm=G.tmt.age.means[,12],vv=G.tmt.age.vars[,3],nn=G.tmt.age.means[,2],tt=G.tmt.age.means[,1])

tmt.models<-cbind("tarsometatarsus",c(rep("length",3),rep("proximal area",3),rep("midshaft area",3),rep("distal area",3)),rbind(fit3models(G.tmt.length),fit3models(G.tmt.proxA),fit3models(G.tmt.midA),fit3models(G.tmt.distA)))
colnames(tmt.models)[1:2]<-c("element","dimension")
models<-rbind(models,tmt.models)

###

G.tbt<-read.csv(file="RLB/Gymnogyps/all_tibiotarsi.csv")
G.tbt.aged<-G.tbt[-which(G.tbt[,3]=="Unk"),]

G.tbt.age.means<-array(dim=c(length(levels(G.tbt.aged[,3]))-1,dim(G.tbt.aged)[2]-1))
colnames(G.tbt.age.means)<-c(colnames(G.tbt.aged)[3],"n",colnames(G.tbt.aged)[4:13])
for (i in 1:dim(G.tbt.age.means)[1]){
	age<-levels(G.tbt.aged[,3])[i]
	G.tbt.age.means[i,1]<-age
	G.tbt.age.means[i,2]<-length(which(G.tbt.aged[,3]==age))
	G.tbt.age.means[i,3:12]<-mean(G.tbt.aged[which(G.tbt.aged[,3]==age),4:13],na.rm=T)
	}
G.tbt.age.means<-apply(G.tbt.age.means,2,as.numeric)

G.tbt.age.vars<-array(dim=c(length(levels(G.tbt.aged[,3]))-1,dim(G.tbt.aged)[2]-1))
colnames(G.tbt.age.vars)<-c(colnames(G.tbt.aged)[3],"n",colnames(G.tbt.aged)[4:13])
for (i in 1:dim(G.tbt.age.vars)[1]){
	age<-levels(G.tbt.aged[,3])[i]
	G.tbt.age.vars[i,1]<-age
	G.tbt.age.vars[i,2]<-length(which(G.tbt.aged[,3]==age))
	G.tbt.age.vars[i,3:12]<-diag(var(G.tbt.aged[which(G.tbt.aged[,3]==age),4:13],na.rm=T))
	}
G.tbt.age.vars<-apply(G.tbt.age.vars,2,as.numeric)

G.tbt.age.means<-G.tbt.age.means[c(1,3:6),]
G.tbt.age.vars<-G.tbt.age.vars[c(1,3:6),]

G.tbt.length<-as.paleoTS(mm=G.tbt.age.means[,3],vv=G.tbt.age.vars[,3],nn=G.tbt.age.means[,2],tt=G.tbt.age.means[,1])
G.tbt.proxA<-as.paleoTS(mm=G.tbt.age.means[,10],vv=G.tbt.age.vars[,3],nn=G.tbt.age.means[,2],tt=G.tbt.age.means[,1])
G.tbt.midA<-as.paleoTS(mm=G.tbt.age.means[,11],vv=G.tbt.age.vars[,3],nn=G.tbt.age.means[,2],tt=G.tbt.age.means[,1])
G.tbt.distA<-as.paleoTS(mm=G.tbt.age.means[,12],vv=G.tbt.age.vars[,3],nn=G.tbt.age.means[,2],tt=G.tbt.age.means[,1])

tbt.models<-cbind("tibiotarsus",c(rep("length",3),rep("proximal area",3),rep("midshaft area",3),rep("distal area",3)),rbind(fit3models(G.tbt.length),fit3models(G.tbt.proxA),fit3models(G.tbt.midA),fit3models(G.tbt.distA)))
colnames(tbt.models)[1:2]<-c("element","dimension")
models<-rbind(models,tbt.models)

par(mfrow=c(2,2))
plot(G.tbt.length,main="length")
plot(G.tbt.proxA,main="proximal area")
plot(G.tbt.midA,main="midshaft area")
plot(G.tbt.distA,main="distal area")

###

G.bills<-read.csv(file="RLB/Gymnogyps/all_bills.csv")
G.bills.aged<-G.bills[-which(G.bills[,3]=="Unk"),]

G.bills.age.means<-array(dim=c(length(levels(G.bills.aged[,3]))-1,dim(G.bills.aged)[2]-1))
colnames(G.bills.age.means)<-c(colnames(G.bills.aged)[3],"n",colnames(G.bills.aged)[4:5])
for (i in 1:dim(G.bills.age.means)[1]){
	age<-levels(G.bills.aged[,3])[i]
	G.bills.age.means[i,1]<-age
	G.bills.age.means[i,2]<-length(which(G.bills.aged[,3]==age))
	G.bills.age.means[i,3:4]<-mean(G.bills.aged[which(G.bills.aged[,3]==age),4:5],na.rm=T)
	}
G.bills.age.means<-apply(G.bills.age.means,2,as.numeric)

G.bills.age.vars<-array(dim=c(length(levels(G.bills.aged[,3]))-1,dim(G.bills.aged)[2]-1))
colnames(G.bills.age.vars)<-c(colnames(G.bills.aged)[3],"n",colnames(G.bills.aged)[4:5])
for (i in 1:dim(G.bills.age.vars)[1]){
		age<-levels(G.bills.aged[,3])[i]
	G.bills.age.vars[i,1]<-age
	G.bills.age.vars[i,2]<-length(which(G.bills.aged[,3]==age))
	G.bills.age.vars[i,3:4]<-diag(var(G.bills.aged[which(G.bills.aged[,3]==age),4:5],na.rm=T))
	}
G.bills.age.vars<-apply(G.bills.age.vars,2,as.numeric)

G.bills.age.means<-G.bills.age.means[c(1,3,5:6),]
G.bills.age.vars<-G.bills.age.vars[c(1,3,5:6),]

G.bills.length<-as.paleoTS(mm=G.bills.age.means[,3],vv=G.bills.age.vars[,3],nn=G.bills.age.means[,2],tt=G.bills.age.means[,1])
G.bills.width<-as.paleoTS(mm=G.bills.age.means[,4],vv=G.bills.age.vars[,3],nn=G.bills.age.means[,2],tt=G.bills.age.means[,1])

plot(G.bills.length)
plot(G.bills.width)

###

G.head<-read.csv(file="RLB/Gymnogyps/all_crania.csv")
G.head.aged<-G.head[-which(G.head[,3]=="Unk"),]

G.head.age.means<-array(dim=c(length(levels(G.head.aged[,3]))-1,dim(G.head.aged)[2]-1))
colnames(G.head.age.means)<-c(colnames(G.head.aged)[3],"n",colnames(G.head.aged)[4:ncol(G.head.aged)])
for (i in 1:dim(G.head.age.means)[1]){
	age<-levels(G.head.aged[,3])[i]
	G.head.age.means[i,1]<-age
	G.head.age.means[i,2]<-length(which(G.head.aged[,3]==age))
	G.head.age.means[i,3:(ncol(G.head.aged)-1)]<-mean(G.head.aged[which(G.head.aged[,3]==age),4:ncol(G.head.aged)],na.rm=T)
	}
G.head.age.means<-apply(G.head.age.means,2,as.numeric)

G.head.age.vars<-array(dim=c(length(levels(G.head.aged[,3]))-1,dim(G.head.aged)[2]-1))
colnames(G.head.age.vars)<-c(colnames(G.head.aged)[3],"n",colnames(G.head.aged)[4:ncol(G.head.aged)])
for (i in 1:dim(G.head.age.vars)[1]){
	age<-levels(G.head.aged[,3])[i]
	G.head.age.vars[i,1]<-age
	G.head.age.vars[i,2]<-length(which(G.head.aged[,3]==age))
	G.head.age.vars[i,3:(ncol(G.head.aged)-1)]<-diag(var(G.head.aged[which(G.head.aged[,3]==age),4:ncol(G.head.aged)],na.rm=T))
	}
G.head.age.vars<-apply(G.head.age.vars,2,as.numeric)
G.head.age.vars[which(is.na(G.head.age.vars),arr.ind=T)]<-0

G.head.height<-as.paleoTS(mm=G.head.age.means[,3],vv=G.head.age.vars[,3],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.length<-as.paleoTS(mm=G.head.age.means[,4],vv=G.head.age.vars[,4],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.temp<-as.paleoTS(mm=G.head.age.means[,5],vv=G.head.age.vars[,5],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.op<-as.paleoTS(mm=G.head.age.means[,6],vv=G.head.age.vars[,6],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.occ<-as.paleoTS(mm=G.head.age.means[,7],vv=G.head.age.vars[,7],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.forH<-as.paleoTS(mm=G.head.age.means[,7],vv=G.head.age.vars[,7],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.forW<-as.paleoTS(mm=G.head.age.means[,7],vv=G.head.age.vars[,7],nn=G.head.age.means[,2],tt=G.head.age.means[,1])
G.head.occ.L<-as.paleoTS(mm=c(G.head.age.means[,10],G.head.age.means[,11]),vv=c(G.head.age.means[,10],G.head.age.means[,11]),nn=rep(G.head.age.means[,2],2),tt=rep(G.head.age.means[,1],2))
G.head.occ.W<-as.paleoTS(mm=c(G.head.age.means[,12],G.head.age.means[,13]),vv=c(G.head.age.means[,12],G.head.age.means[,13]),nn=rep(G.head.age.means[,2],2),tt=rep(G.head.age.means[,1],2))

fit3models(G.head.occ)

###

G.hum<-read.csv(file="RLB/Gymnogyps/all_humeri.csv")
G.hum.aged<-G.hum[-which(G.hum[,3]=="Unk"),]

G.hum.age.means<-array(dim=c(length(levels(G.hum.aged[,3]))-1,dim(G.hum.aged)[2]-1))
colnames(G.hum.age.means)<-c(colnames(G.hum.aged)[3],"n",colnames(G.hum.aged)[4:13])
for (i in 1:dim(G.hum.age.means)[1]){
	age<-levels(G.hum.aged[,3])[i]
	G.hum.age.means[i,1]<-age
	G.hum.age.means[i,2]<-length(which(G.hum.aged[,3]==age))
	G.hum.age.means[i,3:12]<-mean(G.hum.aged[which(G.hum.aged[,3]==age),4:13],na.rm=T)
	}
G.hum.age.means<-apply(G.hum.age.means,2,as.numeric)

G.hum.age.vars<-array(dim=c(length(levels(G.hum.aged[,3]))-1,dim(G.hum.aged)[2]-1))
colnames(G.hum.age.vars)<-c(colnames(G.hum.aged)[3],"n",colnames(G.hum.aged)[4:13])
for (i in 1:dim(G.hum.age.vars)[1]){
	age<-levels(G.hum.aged[,3])[i]
	G.hum.age.vars[i,1]<-age
	G.hum.age.vars[i,2]<-length(which(G.hum.aged[,3]==age))
	G.hum.age.vars[i,3:12]<-diag(var(G.hum.aged[which(G.hum.aged[,3]==age),4:13],na.rm=T))
	}
G.hum.age.vars<-apply(G.hum.age.vars,2,as.numeric)

G.hum.age.vars[which(is.na(G.hum.age.vars),arr.ind=T)]<-0

G.hum.length<-as.paleoTS(mm=G.hum.age.means[,3],vv=G.hum.age.vars[,3],nn=G.hum.age.means[,2],tt=G.hum.age.means[,1])
G.hum.proxA<-as.paleoTS(mm=G.hum.age.means[,10],vv=G.hum.age.vars[,3],nn=G.hum.age.means[,2],tt=G.hum.age.means[,1])
G.hum.midA<-as.paleoTS(mm=G.hum.age.means[,11],vv=G.hum.age.vars[,3],nn=G.hum.age.means[,2],tt=G.hum.age.means[,1])
G.hum.distA<-as.paleoTS(mm=G.hum.age.means[,12],vv=G.hum.age.vars[,3],nn=G.hum.age.means[,2],tt=G.hum.age.means[,1])

hum.models<-cbind("humerus",c(rep("length",
3),rep("proximal area",3),rep("midshaft area",3),rep("distal area",3)),rbind(fit3models(G.hum.length),fit3models(G.hum.proxA),fit3models(G.hum.midA),fit3models(G.hum.distA)))
colnames(hum.models)[1:2]<-c("element","dimension")
models<-rbind(models,hum.models)

par(mfrow=c(2,2))
plot(G.hum.length,main="length")
plot(G.hum.proxA,main="proximal area")
plot(G.hum.midA,main="midshaft area")
plot(G.hum.distA,main="distal area")

###

G.cmc<-read.csv(file="RLB/Gymnogyps/all_cmc.csv")
G.cmc.aged<-G.cmc[-which(G.cmc[,3]=="Unk"),]

G.cmc.age.means<-array(dim=c(length(levels(G.cmc.aged[,3]))-1,dim(G.cmc.aged)[2]-1))
colnames(G.cmc.age.means)<-c(colnames(G.cmc.aged)[3],"n",colnames(G.cmc.aged)[4:13])
for (i in 1:dim(G.cmc.age.means)[1]){
	age<-levels(G.cmc.aged[,3])[i]
	G.cmc.age.means[i,1]<-age
	G.cmc.age.means[i,2]<-length(which(G.cmc.aged[,3]==age))
	G.cmc.age.means[i,3:12]<-mean(G.cmc.aged[which(G.cmc.aged[,3]==age),4:13],na.rm=T)
	}
G.cmc.age.means<-apply(G.cmc.age.means,2,as.numeric)

G.cmc.age.vars<-array(dim=c(length(levels(G.cmc.aged[,3]))-1,dim(G.cmc.aged)[2]-1))
colnames(G.cmc.age.vars)<-c(colnames(G.cmc.aged)[3],"n",colnames(G.cmc.aged)[4:13])
for (i in 1:dim(G.cmc.age.vars)[1]){
	age<-levels(G.cmc.aged[,3])[i]
	G.cmc.age.vars[i,1]<-age
	G.cmc.age.vars[i,2]<-length(which(G.cmc.aged[,3]==age))
	G.cmc.age.vars[i,3:12]<-diag(var(G.cmc.aged[which(G.cmc.aged[,3]==age),4:13],na.rm=T))
	}
G.cmc.age.vars<-apply(G.cmc.age.vars,2,as.numeric)

G.cmc.length<-as.paleoTS(mm=G.cmc.age.means[,3],vv=G.cmc.age.vars[,3],nn=G.cmc.age.means[,2],tt=G.cmc.age.means[,1])
G.cmc.proxA<-as.paleoTS(mm=G.cmc.age.means[,10],vv=G.cmc.age.vars[,3],nn=G.cmc.age.means[,2],tt=G.cmc.age.means[,1])
G.cmc.midA<-as.paleoTS(mm=G.cmc.age.means[,11],vv=G.cmc.age.vars[,3],nn=G.cmc.age.means[,2],tt=G.cmc.age.means[,1])
G.cmc.distA<-as.paleoTS(mm=G.cmc.age.means[,12],vv=G.cmc.age.vars[,3],nn=G.cmc.age.means[,2],tt=G.cmc.age.means[,1])

cmc.models<-cbind("carpometacarpus",c(rep("length",3),rep("proximal area",3),rep("midshaft area",3),rep("distal area",3)),rbind(fit3models(G.cmc.length),fit3models(G.cmc.proxA),fit3models(G.cmc.midA),fit3models(G.cmc.distA)))
colnames(cmc.models)[1:2]<-c("element","dimension")
models<-rbind(models,cmc.models)

##

source(file="C:/Users/vjsyvers/Documents/R/RLB/makepaleoTS.R")
allnames<-dir()[-c(14,19,29,30,31,32)]
allTests<-read.csv(file="00modelTests/allTests.csv")
allTS<-list()

for (i in 1:length(allnames)){
	allTS[[i]]<-make.paleoTS(allnames[i])
	}

allTests.Stasis<-allTests[which(allTests[,7]==3),]
allTests.Random<-allTests[which(allTests[,7]==2),]
round(apply(allTests.Stasis[,4:6],2,mean,na.rm=T),2)
round(apply(allTests.Stasis[,4:6],2,sd,na.rm=T),2)

hist(allTests[,4],xlim=c(0,1),main="Directional",xlab="")
hist(allTests[,5],xlim=c(0,1),main="Random walk",xlab="")
hist(allTests[,6],xlim=c(0,1),main="Stasis",xlab="Akaike weights")

species<-levels(as.factor(substr(levels(as.factor(allTests[,1])),0,5)))
species.rows<-list()
for (i in 1:length(species)) {
	species.rows[[i]]<-grep(a[i],allTests[,1]) }
names(species.rows)<-species

i<-9
length(species.rows[[i]])
allTests[species.rows[[i]],]
theserows<-127:129
theserows<-which(allTests[species.rows[[i]],7]==3)
thoserows<-which(allTests[species.rows[[i]],7]==2)

round(apply(allTests[species.rows[[i]],4:6],2,mean,na.rm=T),2)
round(apply(allTests[theserows,4:6],2,mean,na.rm=T),2)
round(apply(allTests[species.rows[[i]][theserows],4:6],2,mean,na.rm=T),2)
round(apply(allTests[species.rows[[i]][thoserows],4:6],2,mean,na.rm=T),2)

par(mfrow=c(1,3))
hist(allTests[species.rows[[i]],4],xlim=c(0,1),breaks="Scott",main="Directional",xlab="Akaike weights")
hist(allTests[species.rows[[i]],5],xlim=c(0,1),main="Random walk",xlab="Akaike weights")
hist(allTests[species.rows[[i]],6],xlim=c(0,1),main="Stasis",xlab="Akaike weights")

models<-array(dim=c(2,7))
TS<-make.paleoTS("bisonHumeri.csv")
length(TS)
par(mfrow=c(2,4))
for (i in 1:length(TS)) {plot(TS[[i]],main=names(TS)[i])}
TS<-make.paleoTS("gymnogypsHumeri.csv")
gymnogypsHumeri<-test3.paleoTS(TS)
names(TS)
for (i in 1:length(names(TS))){
	fit3models.hacked(TS[[names(TS)[i]]])}