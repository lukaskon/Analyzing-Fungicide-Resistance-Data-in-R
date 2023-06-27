
setwd("C:/Users/nikki/Michigan State University/PSM.Hausbecklab - Nikki Lukasko - Nikki Lukasko/Botrytis/Fungicide Sensitivity/Results")

data<-read.table("MultifungicideData_R.txt",header = T, sep="\t")
head(data)

dat<-reshape(data, idvar = "Isolate", timevar = "Fungicide", direction = "wide")
dat


n<-nrow(dat)
n
v<-c()
for(i in 1:n){
  for(j in 2:9){
    for(k in 2:9){
      v<-c(v,ifelse(dat[i,j]==1&dat[i,k]==1,1,0))
    }
  }
}

x<-array(v,dim=c(8,8,n))
x

co_occur<-apply(simplify2array(x), c(1,2), sum)
diag(co_occur)<-n#diagnal should always match
co_occur
percentage<-co_occur/n
percentage


#1-7 indicates: "Fenhexamid","Iprodione","Fludioxonil","Thiophanate.methyl","Boscalid",
#"Fluopyram","Cyprodinil","Pyraclostrobin" respectively.