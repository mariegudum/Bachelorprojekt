# Bemærk!! Uafhængighedsantagelsen er lavet før sæsonen 23/24 sluttede, 
# og derfor er det kun 554 kampe, der betragtes (og ikke 3*192=576 kampe)

SL <- read_excel("~/Downloads/DNK (2).xlsx")
SL<-SL %>% filter(Season == "2023/2024" | Season == "2022/2023" | Season == "2021/2022")
head(SL)
M<-nrow(SL)


#Test of independence

Q<-table(SL$HG,SL$AG)
sum(Q)
A<-matrix(0,11,10)
A[(1:8),(1:7)]<-Q[(1:8),(1:7)]
for (i in 1:10){
  A[i,10]<-sum(A[i,])}
for (j in 1:10){
  A[11,j]<-sum(A[,j])
}
su<-round(A[11,]/M,digits=4)
sh<-round(A[,10]/M,digits=4)
B<-matrix(0,10,9)
for (i in 1:10){
  for (j in 1:9){
    B[i,j]<-su[j]*sh[i]*M
  }
}
OO<-A[1:10,1:9]
EE<-round(B,digits=0)
kv<-as.matrix((OO-EE)^2)
XX<-0
for (i in 1:10){
  for (j in 1:9){
    if (EE[i,j]!=0)
    XX<-XX+kv[i,j]/EE[i,j]
  }
}
XX
EE

#slår sammen så der er fra 0 til 4+ mål:
#Observerede:

OO<-matrix(0,5,5)
OO[(1:4),(1:4)]<-Q[(1:4),(1:4)]
for (i in 1:4){
  OO[i,5]<-Q[i,5]+Q[i,6]+Q[i,7]
  OO[5,i]<-Q[5,i]+Q[6,i]+Q[7,i]+Q[8,i]
}
for (j in 5:7){
  for (i in 5:8){
    OO[5,5]<-OO[5,5]+(Q[i,j])
  }
}
sum(OO)
M
OO

#Finder sandsynligheder
A<-matrix(0,6,6)
A[(1:5),(1:5)]<-OO[(1:5),(1:5)]
for (i in 1:5){
  A[i,6]<-sum(A[i,])}
for (j in 1:5){
  A[6,j]<-sum(A[,j])
}
A
su<-round(A[6,]/M,digits=4)
sh<-round(A[,6]/M,digits=4)
#Finder forventede
B<-matrix(0,5,5)
for (i in 1:5){
  for (j in 1:5){
    B[i,j]<-su[j]*sh[i]*M
  }
}
EE<-round(B,1)
EE
OO

kv<-as.matrix((OO-EE)^2)
XX<-0
for (i in 1:5){
  for (j in 1:5){
    if (EE[i,j]!=0)
      XX<-XX+kv[i,j]/EE[i,j]
  }
}
XX
pchisq(17.62824, 16, lower.tail = F)

xtable(EE)


# chi^2=17,62824 med DF = (5 - 1) * (5 - 1) =4*4=16 frihedsgrader 
# Hypotesen forkastes ikke. 