
# Indlæs Superliga-filen Jeg henter min fra "Desktop".

SL <- read_excel("Desktop/Superliga.xlsx")

head(SL)
SL<-data.frame(SL %>% filter(Season == "2023/2024"))
M<-nrow(SL)


TeamNames<-unique(SL$Home)
Index<-1:length(TeamNames)
Goals<-1:(2*nrow(SL)) 
alfa<-gamma<-rep(0,length(TeamNames))
DesignMatrix<-matrix(0,nrow=2*nrow(SL),ncol=2+2*length(TeamNames))


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

#Estimate prameters in the independet Poisson-mode
testGLM<-glm(Goals ~ DesignMatrix[,c(-1,-3,-15)], family = poisson(link = "log"))
# 1: intercept, 2: hjemmebanefordel, 3: første holds angrebsparameter, 15: første holds forsvarsparameter

#These are now the true parameters
betaALL<-testGLM$coefficients[1]
delta<-testGLM$coefficients[2]
alfa[2:length(TeamNames)]<-testGLM$coefficients[3:(length(TeamNames)+1)]
gamma[2:length(TeamNames)]<-testGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]

# Calculate home win/dram/Away win probabilities for a specific match
HDAprob<- function(Home, Away, betaALL,delta,alfa,gamma) {
  nMatches<-length(Home)
  pD<-pH<-pA<-rep(0,nMatches)
  MaxGoals<-10
  for (i in 1:nMatches){
    lamH<-exp(betaALL+delta+alfa[Index[TeamNames==Home[i]]]-gamma[Index[TeamNames==Away[i]]])
    lamA<-exp(betaALL+alfa[Index[TeamNames==Away[i]]]-gamma[Index[TeamNames==Home[i]]])
    pMatrix<-dpois(0:MaxGoals,lamH)%*%t(dpois(0:MaxGoals,lamA))
    pD[i]<-sum(diag(pMatrix))
    pH[i]<-sum(pMatrix[lower.tri(pMatrix)])
    pA[i]<-sum(pMatrix[upper.tri(pMatrix)])
    scale<-pH[i]+pD[i]+pA[i]
    pH[i]<-pH[i]/scale; pD[i]<-pD[i]/scale; pA[i]<-pA[i]/scale
  }
  return(cbind(pH,pD,pA))
  
}


Odds<-matrix(0,nrow=nrow(SL),ncol=3)
HDAprob(SL[i,6], SL[i,7],betaALL,delta,alfa,gamma)

cut<-0.03978533 #Bookmaker's profit margin

#Bookmakers set odds according to true probabilities
for (i in 1:nrow(SL)){
  temp<-HDAprob(SL[i,6], SL[i,7],betaALL,delta,alfa,gamma)
  Odds[i,]<-(1-cut)/temp
}


#Simulate seasons and betting strategies

nSeasons<-1000

RandomWealth<-Wealth<-ExpectedWealth<-matrix(nrow(SL),ncol=nrow(SL)+1,nrow=nSeasons)

lam<-SimGoals<-rep(0,2*nrow(SL))

lam<- exp(DesignMatrix%*%c(betaALL,delta,alfa,gamma))
NEWalfa<-NEWgamma<-rep(0,length(TeamNames))

Probs<-Temp<-rep(0,3)

Begin<-as.vector(c(betaALL,delta,alfa[2:length(TeamNames)],gamma[2:length(TeamNames)]))

for (k in 1:nSeasons){
  
  #Simulate a new season with the true parameters  
  SimGoals<-rpois(length(lam),lam)
  
  #Estimate parameters from the newly simulated season
  
  NEWtestGLM<-glm(SimGoals ~ DesignMatrix[,c(-1,-3,-15)],start=Begin, family = poisson(link = "log"))
  
  
  NEWbetaALL<-NEWtestGLM$coefficients[1]
  NEWdelta<-NEWtestGLM$coefficients[2]
  NEWalfa[2:length(TeamNames)]<-NEWtestGLM$coefficients[3:(length(TeamNames)+1)]
  NEWgamma[2:length(TeamNames)]<-NEWtestGLM$coefficients[(length(TeamNames)+2):(2*length(TeamNames))]
  
  
  Resample1<-FALSE #Bet w/ new estimate on a new season simulated with new estimate
  Resample2<-TRUE #Bet w/ new estimate on a season simulated with true parameter
  # If these are both false: Bet w/ new estimate the season that gave this new estimate
  
  wNew<-0
  
  for (i in 1:nrow(SL)){
    lamH<-wNew*exp(NEWbetaALL+NEWdelta+NEWalfa[Index[TeamNames==SL[i,6]]]-NEWgamma[Index[TeamNames==SL[i,7]]])+(1-wNew)*exp(betaALL+delta+alfa[Index[TeamNames==SL[i,6]]]-gamma[Index[TeamNames==SL[i,7]]])
    lamA<-wNew*exp(NEWbetaALL+NEWalfa[Index[TeamNames==SL[i,7]]]-NEWgamma[Index[TeamNames==SL[i,6]]])+(1-wNew)*exp(betaALL+alfa[Index[TeamNames==SL[i,7]]]-gamma[Index[TeamNames==SL[i,6]]])
    SimGoals[c(2*i-1,2*i)]<-rpois(2,c(lamH,lamA))
  }
  
  
  Ai<-2*(1:nrow(SL))
  Hi<-Ai-1
  AllProbs<-HDAprob(SL[,6],SL[,7],NEWbetaALL,NEWdelta,NEWalfa,NEWgamma)
  AllResults<-(SimGoals[Hi] > SimGoals[Ai])+2*(SimGoals[Hi] == SimGoals[Ai])+3*(SimGoals[Hi] < SimGoals[Ai])
  
  for(j in 1:nrow(SL)) {
    Probs<-AllProbs[j,]
    Result<-AllResults[j]
    # benchmark, sanity check: Bet 1 uniformly randomly 
    RandomBet<-floor(3*runif(1))+1
    RandomWin<-(Odds[j,RandomBet]*(Result==RandomBet)-1)
    RandomWealth[k,j+1]<-RandomWealth[k,j]+RandomWin
    # Bet Kelly on the result with the highest expected return -- if that is >0, else don't bet
    Temp<-Probs*Odds[j,]
    Kelly<-(Probs*(Odds[j,]-1)-(1-Probs))/(Odds[j,]-1)
    Faster<-max(Temp)
    print(Faster)
    NetWin<-0
    ExpectedWealth[k,j+1]<-ExpectedWealth[k,j]
    if(Faster > 1){ 
      Bet<-which.max(Temp)
      Size<-Wealth[k,j]*max(Kelly)
      Size<-1
      NetWin<- (Odds[j,Bet]*(Result==Bet)-1)*Size 
      #How much we think (assuming our model is correct) our wealth will increase
      ExpectedWealth[k,j+1]<-ExpectedWealth[k,j+1]+(Faster-1)*Size 
    }
    Wealth[k,j+1]<-Wealth[k,j]+NetWin
    
  }
  
}


RunningWealth<-RunningRandomWealth<-matrix(0,nrow=3,ncol=nrow(SL)+1)
RunningExpectedWealth<-matrix(0,nrow=1,ncol=nrow(SL)+1)

#Calculate average wealth after i (1...M) matches -- and 2.5% and 97.5% fractiles

for (i in 1:(nrow(SL)+1)){
  dummy1<-sort(Wealth[,i])
  dummy2<-sort(RandomWealth[,i])
  RunningWealth[,i]<-c(dummy1[round(0.025*nSeasons)], mean(Wealth[,i]),dummy1[round(0.975*nSeasons)])
  RunningExpectedWealth[,i]<- mean(ExpectedWealth[,i])
  RunningRandomWealth[,i]<-c(dummy2[round(0.025*nSeasons)], mean(RandomWealth[,i]),dummy2[round(0.975*nSeasons)])
}


yrange<-c(min(RunningWealth,RunningRandomWealth,RunningExpectedWealth),max(RunningWealth,RunningRandomWealth,RunningExpectedWealth))

title<-c("Kelly betting")



plot(0:nrow(SL),RunningWealth[2,],type='l',xlab="Match number",ylab="Wealth",xlim =c(0,200),ylim=yrange,lwd=2,main=title)
points(0:nrow(SL),RunningWealth[1,],type='l',lty=3)
points(0:nrow(SL),RunningWealth[3,],type='l',lty=3)
text(150,1.03*RunningWealth[2,nrow(SL)+1],paste("Average wealth"),adj=0,cex=0.8)
text(150,RunningWealth[2,nrow(SL)+1],paste("Slope = ", round(mean(diff(RunningWealth[2,],1)),4)),adj=0,cex=0.8)

points(0:nrow(SL),RunningExpectedWealth[1,],type='l',col="red",lwd=2)
text(150,0.99*RunningExpectedWealth[1,nrow(SL)+1],paste("Slope = ", round(mean(diff(RunningExpectedWealth[1,],1)),2)),adj=0,cex=0.8,col="red")
text(150,1.02*RunningExpectedWealth[1,nrow(SL)+1],paste("Expected wealth"),adj=0,cex=0.8,col="red")

points(0:nrow(SL),RunningRandomWealth[2,],type='l',col="deepskyblue2",lwd=2)
points(0:nrow(SL),RunningRandomWealth[1,],type='l',lty=3,col="deepskyblue2")
points(0:nrow(SL),RunningRandomWealth[3,],type='l',lty=3,col="deepskyblue2")
text(115,0.94*RunningRandomWealth[2,nrow(SL)+1],paste("Average wealth, random betting"),col="deepskyblue2",adj=0,cex=0.8)
text(150,0.97*RunningRandomWealth[2,nrow(SL)+1],paste("Slope = ", round(mean(diff(RunningRandomWealth[2,],1)),2)),adj=0,col="deepskyblue2", cex=0.8)

text(0,max(yrange),paste(nSeasons, "simulated seasons with",M,"matches"),adj=0, cex=0.8)


RunTime<-toc()



# Tilføj vækstrater 

RunningWealth<-matrix(0,nrow=2,ncol=nrow(SL24)+1)

for (i in 1:(nrow(SL)+1)){
  RunningWealth[,i]<-c(sd(Wealth[,i]), mean(Wealth[,i]))
}


# Growth rates
t<-1:193
t2<-1:192
# RunningWealth
LogRunningWealth <- log(RunningWealth)
GrowthRatesWealth <- ((LogRunningWealth[2,]-LogRunningWealth[2,1])/t) 
sdw<-((LogRunningWealth[1,2:193])/t2) 

# Plot standardafvigelser
clt<-function(n){0.7/sqrt(n)}
plot(clt,1,192,xlab="Kampnummer",ylab="Vækstrater",col="darkgrey",xlim =c(0,200))
plot(1:192,sdw,type='l',xlab="Kampnummer",ylab="Standardafvigelse",xlim =c(0,200),ylim=yrangelog,lwd=2)

# Plot vækstrater
plot(t,GrowthRatesWealth,type='l',xlab="Kampnummer",ylab="Vækstrate",xlim =c(0,200),ylim=c(min(GrowthRatesWealth),max(GrowthRatesWealth)),col="deepskyblue2",lwd=2)
abline(h=mean(GrowthRatesWealth), lty=2)
text(150,mean(GrowthRatesWealth)+0.000025,paste("Gennemsnitlig vækstrate"),adj=0,cex=0.7)
text(150,mean(GrowthRatesWealth)+0.000015,paste(" = ", round(mean(GrowthRatesWealth),5)),adj=0,cex=0.7)
text(20,max(GrowthRatesWealth),paste(nSeasons, "simuleringer ud fra",M,"kampe"),adj=0, cex=0.8)

mean(GrowthRatesWealth)


# Kelly-grafen

HDAprob("Silkeborg","Vejle",NEWbetaALL,NEWdelta,NEWalfa,NEWgamma)*
  (1-cut)/HDAprob("Silkeborg","Vejle",betaALL,delta,alfa,gamma)


f<-function(x){0.5*log(1+x*1.25)+(1-0.5)*log(1-x)}
maximum<-optimize(f, interval=c(0,0.25), maximum=TRUE)

plot(f,0,0.25,lwd=2, xlab="Andel af formue væddet", ylab="Vækstrate", main="Asymptotisk vækstrate af formue")
abline(h=maximum$objective,col="deepskyblue2",lwd=2)
abline(v=maximum$maximum,col="deepskyblue2",lwd=2)
abline(h=mean(GrowthRatesWealth),col="red",lwd=2)
text(0.2,0.0055,paste("Kelly vækstrate"),col="deepskyblue2",adj=0,cex=0.8)
text(0.2,0.0015,paste("Kelly estimeret"),col="red",adj=0,cex=0.8)
text(0.2,0.001,paste("vækstrate",round(mean(GrowthRatesWealth),5)),col="red",adj=0,cex=0.8)
text(0.103,-0.008,paste("Kelly andel"),col="deepskyblue2",adj=0,cex=0.8)
text(0.17,0.0035,paste("Vækstrate"),adj=0,cex=0.8)





# Plot begge samlet
plot(t2,sdw, type='l',xlab="Kampnummer",ylab="Vækstrater",xlim =c(80,200),ylim=c(min(GrowthRatesWealth),0.03),lwd = 1)
points(t,GrowthRatesWealth ,type='l',col="deepskyblue2",lwd = 1)
text(175,0.0015,paste("Vækstrater"),col="deepskyblue2",adj=0,cex=0.7)
text(171,0.02,paste("Standardafvigelser"),adj=0,cex=0.7)


# Plot alt samlet
clt<-function(n){0.3/sqrt(n)}
plot(clt,50,170,xlab="Kampnummer",ylab="Vækstrater",ylim=c(min(GrowthRatesWealth, sd),0.05),col="darkgrey",xlim =c(0,200))
points(1:170,sd[4:173], type='l',xlab="Match number",ylab="Growth rate",xlim =c(0,200),lwd = 1)
points(1:170,GrowthRatesWealth[4:173] ,type='l',col="deepskyblue2",lwd = 1)
text(175,0,paste("Vækstrater"),col="deepskyblue2",adj=0,cex=0.7)
text(171,0.002,paste("Standardafvigelser"),adj=0,cex=0.7)




# Tjek standardafvigelsernes konvergens

n<-1:length(sdw)
plot(n,sdw,log="xy",lwd=2,cex=1.5)
fit<-lm(log(sdw)~log(n))
points(n,exp(fit$coefficients[1])*n^fit$coefficients[2],type='l',lwd=2,col="red")
par(adj=0)
text(n[3],sdw[2],paste("slope = ",round(fit$coefficients[2],3)),cex=1.5, col="red")
par(adj=0.5)















