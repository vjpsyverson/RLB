A.tmt<-read.csv(file="RLB/Aquila/AquilaTMT.csv")
A.tmt[,3]<-as.factor(A.tmt[,3])

A.tmt.means<-array(dim=c(length(levels(A.tmt[,3]))-1,dim(A.tmt)[2]-1))
colnames(A.tmt.means)<-c(colnames(A.tmt)[3],"n",colnames(A.tmt)[4:ncol(A.tmt)])
for (i in 1:dim(A.tmt.means)[1]){
	age<-levels(A.tmt[,3])[i]
	A.tmt.means[i,1]<-age
	A.tmt.means[i,2]<-length(which(A.tmt[,3]==age))
	A.tmt.means[i,3:(ncol(A.tmt)-1)]<-mean(A.tmt[which(A.tmt[,3]==age),4:ncol(A.tmt)],na.rm=T)
	}
A.tmt.means<-apply(A.tmt.means,2,as.numeric)

A.tmt.vars<-array(dim=c(length(levels(A.tmt[,3]))-1,dim(A.tmt)[2]-1))
colnames(A.tmt.vars)<-c(colnames(A.tmt)[3],"n",colnames(A.tmt)[4:ncol(A.tmt)])
for (i in 1:dim(A.tmt.vars)[1]){
	age<-levels(A.tmt[,3])[i]
	A.tmt.vars[i,1]<-age
	A.tmt.vars[i,2]<-length(which(A.tmt[,3]==age))
	A.tmt.vars[i,3:(ncol(A.tmt)-1)]<-diag(var(A.tmt[which(A.tmt[,3]==age),4:ncol(A.tmt)],na.rm=T))
	}
A.tmt.vars<-apply(A.tmt.vars,2,as.numeric)
A.tmt.vars[which(is.na(A.tmt.vars),arr.ind=T)]<-0

A.tmt.length<-as.paleoTS(mm=A.tmt.means[,3],vv=A.tmt.vars[,3],nn=A.tmt.means[,2],tt=A.tmt.means[,1])
A.tmt.proxW<-as.paleoTS(mm=A.tmt.means[,4],vv=A.tmt.vars[,4],nn=A.tmt.means[,2],tt=A.tmt.means[,1])
A.tmt.midW<-as.paleoTS(mm=A.tmt.means[,5],vv=A.tmt.vars[,5],nn=A.tmt.means[,2],tt=A.tmt.means[,1])
A.tmt.midD<-as.paleoTS(mm=A.tmt.means[,6],vv=A.tmt.vars[,6],nn=A.tmt.means[,2],tt=A.tmt.means[,1])
A.tmt.distW<-as.paleoTS(mm=A.tmt.means[,7],vv=A.tmt.vars[,7],nn=A.tmt.means[,2],tt=A.tmt.means[,1])

A.tmt.models<-cbind("tarsometatarsus",c(rep("length",3),rep("proximal width",3),rep("midshaft width",3),rep("midshaft depth",3),rep("distal width",3)),rbind(fit3models(A.tmt.length),fit3models(A.tmt.proxW),fit3models(A.tmt.midW),fit3models(A.tmt.midD),fit3models(A.tmt.distW)))
colnames(A.tmt.models)[1:2]<-c("element","dimension")

write.csv(A.tmt.models,file="RLB/Aquila/models.csv")