

SL <- read_excel("~/Downloads/DNK (2).xlsx")
SL<-SL %>% filter(Season == "2023/2024",
                  Home == "Midtjylland" & Away == "Silkeborg" 
                  | Home == "Silkeborg" & Away == "Lyngby"
                  | Home == "Lyngby" & Away == "Midtjylland")
M<-nrow(SL)
head(SL)

TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
alfa<-gamma<-rep(0,length(TeamNames))
DesignMatrix<-matrix(0,nrow=2*nrow(SL),ncol=2+2*length(TeamNames))
xtable(SL[,6:9])

for (i in 1:nrow(SL)){
  Goals[2*i-1]<-SL$HG[i]
  Goals[2*i]<-SL$AG[i]
  DesignMatrix[c(2*i-1,2*i),1]<-1
  DesignMatrix[2*i-1,2]<-1
  DesignMatrix[2*i-1,2+Index[TeamNames==SL$Home[i]]]<-1
  DesignMatrix[2*i-1,2+length(TeamNames)+Index[TeamNames==SL$Away[i]]]<--1
  DesignMatrix[2*i,2+Index[TeamNames==SL$Away[i]]]<-1
  DesignMatrix[2*i,2+length(TeamNames)+Index[TeamNames==SL$Home[i]]]<--1
}
qr(DesignMatrix)$rank
#Estimate parameters in the independent Poisson-mode 
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))
#  [,c(1,3,23)] fjernes
# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter
qr(DesignMatrix[,c(-1,-3,-15)])
head(DesignMatrix)
head(SL)
summary(testGLM)



#These are now the true parameters
betaALL<-testGLM$coefficients[1]
delta<-testGLM$coefficients[2]
alfa[2:length(TeamNames)]<-testGLM$coefficients[3:(length(TeamNames)+1)]
gamma[2:length(TeamNames)]<-testGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]


Parametre_eks<-list(hold=TeamNames,hjemmebanefordel=delta,betaALL=betaALL, alfa=alfa, gamma=gamma )
print(Parametre_eks)
Alle_parametre<-data.frame(Hold=TeamNames,alfa=alfa, gamma=gamma)
all<-Alle_parametre[order(-Alle_parametre$alfa), ]
xtable(all)



# Calculate home win/dram/Away win probabilities for a specific match
HDAprob<- function(Home, Away, betaALL,delta,alfa,gamma) {
  lamH<-exp(betaALL+delta+alfa[Index[TeamNames==Home]]-gamma[Index[TeamNames==Away]])
  lamA<-exp(betaALL+alfa[Index[TeamNames==Away]]-gamma[Index[TeamNames==Home]])
  pH<-pD<-0
  
  for (i in 0:20){
    pD<-pD+dpois(i,lamH)*dpois(i,lamA)
    for (j in 0:i) if (j < i) pH<-pH+dpois(i,lamH)*dpois(j,lamA)
  }
  
  return(c(pH,pD,1-pH-pD))
  
}
HDAprob("FC Copenhagen", "Brondby", betaALL,delta,alfa,gamma)



SL <- read_excel("~/Downloads/DNK (2).xlsx")


# Betting ssh 

SL<-SL %>% filter(Season == "2023/2024")
SL
Pin_odds<-data.frame(SL[,6:13]) %>% filter(Home == "Nordsjaelland" & Away == "Vejle")
Pin_ssh<-1/Pin_odds[,6:8]*100
s<-HDAprob("Nordsjaelland", "Vejle", betaALL, delta, alfa, gamma)*100
xtable(s/100*Pin_odds[6:8]-1)
xtable(Pin_odds[6:8])
xtable(s)
xtable(Pin_ssh)








