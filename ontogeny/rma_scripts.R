if (require("xlsx",warn.conflicts=FALSE)==FALSE) {
  install.packages("xlsx",repos="http://cran.cnr.berkeley.edu/");
  library("xlsx");
}
if (require("smatr",warn.conflicts=FALSE)==FALSE) {
  install.packages("smatr",repos="http://cran.cnr.berkeley.edu/");
  library("smatr");
}
options(stringsAsFactors = FALSE,encoding="UTF-8")

#----
#setwd("../Science/RLB/ontogeny")

testData<-read.xlsx("Kilbourne_Felis_silvestris.xlsx",sheetIndex = 1)
cor.test(testData$LN.length.,testData$LN.circumf.,method="spearman")
test<-sma(LN.circumf.~LN.length.,data=testData)
summary(test)
plot(test)

fileNames<-c("Camelops.xlsx")
#fileNames<-c("SmilodonRMA.xlsx","AtroxRMA.xlsx")
#dataNames<-c("Length","Circumference")
dataNames<-c("CIRCUMF","LENGTH")
boneNames<-c("Ulna")

#
write("",file="results.txt")
#
write(paste("RMA results,",date()),file="results.txt",append=T)
for(i in 1:length(fileNames)){
  for(j in 1:length(boneNames)){
    data<-read.xlsx(fileNames[i],sheetIndex=j)
    for(k in 2:length(dataNames)){
      result<-list()
      result$name<-paste(fileNames[i],boneNames[j],"X=",dataNames[1],", Y=",dataNames[k])
      x<-eval(parse(text=paste0("data$",dataNames[1])))
      y<-eval(parse(text=paste0("data$",dataNames[k])))
      result$corr.p.value<-cor.test(x,y,method="spearman")$p.value
      RMA<-sma(log(y)~log(x),data=data)
      result$RMA.coef<-RMA$coef[[1]]
      result$RMA.r2<-RMA$r2[[1]][1]
      result$RMA.p.value<-RMA$pval[[1]][1]
      capture.output(result,file="results.txt",append=T)      
    }
  }
}

