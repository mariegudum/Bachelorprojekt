# Estimerer parametre fra 2023, og tester af på sæsonen 2024. 
# Hold der rykker op er blot blevet tildelt parametre fra de hold, der rykkede ned
Alle_parametre23 <- data.frame(
  Hold = c("Midtjylland", "Lyngby", "Viborg", "FC Copenhagen", "Brondby", "Odense", "Aarhus", 
           "Randers FC", "Vejle", "Hvidovre IF", "Silkeborg", "Nordsjaelland"),
  alfa = c(0, -0.5790059, -0.1686674, 0.1931593, -0.00003769422, -0.1114784, -0.1967073, 
           -0.2007359, -0.4634908, -0.4570078, -0.1905625, -0.008972265),
  gamma = c(0, -0.1947841, 0.1887942, 0.1298641, -0.2349347, -0.3234213, 0.3076481, 
            -0.1080498, -0.1197290, -0.3741151, -0.2345294, 0.1622380)
)

#Laver ssh-tabel over mulige slutresultater:
ProbTable<- function(Alle_parametre,HT,AT){
  teams<-Alle_parametre$Hold
  delta <-delta 
  betaALL<-betaALL
  a<- which(teams==HT)
  b<-which(teams==AT)
  #a/A svarer til hjemmemål, b/B svarer til udemål
  lambda_a<-exp(betaALL+Alle_parametre[a,]$alfa+Alle_parametre[b,]$gamma+delta)
  lambda_b<-exp(betaALL+Alle_parametre[b,]$alfa+Alle_parametre[a,]$gamma)
  A<-as.numeric()
  B<-as.numeric()
  for (i in 0:6){
    A[(i+1)]<-dpois(i,lambda_a)
    B[(i+1)]<-dpois(i,lambda_b)
  }
  A[8]<-1-sum(A[1:7])
  B[8]<-1-sum(B[1:7])
  name<-c("0","1","2","3","4","5","6","7+")
  zero<-mat.or.vec(8,1)
  C<-data.frame(row.names=name)
  for (j in 1:8){
    for (k in 1:8) {
      C[j,k]<-A[k]*B[j] # simultan fordeling
    }
  }
  colnames(C)<- name
  return(round(C*100,2))
}

#hjemmehold kolonner, udehold rækker

Ssh<-ProbTable(Alle_parametre, "Midtjylland", "Silkeborg")
Ssh
xtable(Ssh)

#Samlede ssh'er for HomeWin,Draw og AwayWin:
ResultProbs<-function(probs){
  R<-matrix(0,3,1)
  n<-length(probs)
  for (i in 1:n){
    for (j in 1:n) {
      if (i>j){ #altså summen af indgangene under diagonalen=AW
        R[3]<-R[3]+probs[i,j]
      } else{ #summen af ssh'er i diagonaln=D
        if (i==j){
          R[2]<-R[2]+probs[i,j]
        }else { #summen af resterende, altså indgangene over diagonalen=HW
          R[1]<-R[1]+probs[i,j]
        }
      }
    }
  }
  return(list(HomeWin=R[1],Draw=R[2],AwayWin=R[3]))
}
sand<-data.frame(Hjemmesejr=ResultProbs(Ssh)$HomeWin,Uafgjort=ResultProbs(Ssh)$Draw,Udesejr=ResultProbs(Ssh)$AwayWin)
xtable(sand)

##Sandsynligheder:
AllProb <- function(Alle_parametre,PProbTable) {
  teams<-Alle_parametre$Hold
  delta<-delta
  n <- length(teams)
  name<-c("Hjemmehold","Udehold","Hjemmesejr","Uafgjort","Udesejr")
  C <- data.frame()
  row <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {#hold kan ikke spille mod sig selv
        #ProbTable finder ssh-tabeller for mulige slutresultater
        ssh<-PProbTable(Alle_parametre,teams[i],teams[j])
        #ResultProbs laver ssh-tabel om til samlede ssh'er for ude-,hjemmesejr og uafgjort
        ResultProbabilities<-ResultProbs(ssh)
        C[row,1] <- teams[i]
        C[row,2] <- teams[j]
        C[row,3] <- ResultProbabilities$HomeWin
        C[row,4] <- ResultProbabilities$Draw
        C[row,5] <- ResultProbabilities$AwayWin
        row <- row + 1
      }
    }
  }
  colnames(C)<- name
  return(C)
}
#SSH for alle 20 hold
AllSSH<-AllProb(Alle_parametre23,ProbTable)
xtable(AllProb(Alle_parametre,ProbTable))
AllSSH




SL <- read_excel("~/Downloads/DNK (3).xlsx")
SL<-SL %>% filter(Season == "2023/2024")
# I AllSSH er ssh'erne ganget med 100. Dividerer derfor med 100:
ssh_model<-cbind(AllSSH[,1:2],AllSSH[,3:5])
#Hold
teams<-Alle_parametre$Hold
head(SL)
#SSmodel skal laves alfabetisk:
SSmodel<-ssh_model[order(ssh_model$Hjemmehold,ssh_model$Udehold),]

#Oddsene skal sorteres alfabetisk:
BookOdds<-BBookOdds[order(BBookOdds$Home,BBookOdds$Away),]
#Rækkefølgen stemmer
SSmodel[,1]==BookOdds[,1]
SSmodel[,2]==BookOdds[,2]
head(SSmodel)
head(BookOdds)



#Undersøger ssh_Model*Odds_Bookmaker, og finder dermed strategien
strategi<-cbind(SSmodel[,1:2],SSmodel[,3:5]*BookOdds[,3:5])
head(strategi)



HWS<-matrix(0,132,1)
for (i in 1:132){
  if (strategi[i,3] > 1){
    HWS[i]<-strategi[i,3]
  } else{
    HWS[i]<-0
  }
}
DS<-matrix(0,132,1)
for (i in 1:132){
  if (strategi[i,4] > 1){
    DS[i]<-strategi[i,4]
  } else{
    DS[i]<-0
  }
}
AWS<-matrix(0,132,1)
for (i in 1:132){
  if (strategi[i,5] > 1){
    AWS[i]<-strategi[i,5]
  } else{
    AWS[i]<-0
  }
}
zero<-matrix(0,132,1)
BestBet<-data.frame(HomeTeam=SSmodel[,1], AwayTeam=SSmodel[,2], SSHOdds=zero, BestBet=zero)
for (i in 1:132){
  BestBet[i,3]<-max(HWS[i],DS[i],AWS[i],0)
  if(BestBet[i,3]==0){
    BestBet[i,4]<-"NoBet"} else{
      if(BestBet[i,3]==DS[i]){
        BestBet[i,4]<-"D"}  else{
          if(BestBet[i,3]==AWS[i]){
            BestBet[i,4]<-"A"}
          else{
            if (BestBet[i,3]==HWS[i])
              BestBet[i,4]<-"H"}
        }  } }
winner<-BookOdds[,6]
formue<-0
formue_vektor <- numeric(132)
for (i in 1:132){
  if(winner[i]=="A" & BestBet[i,4]=="A") 
  {formue<-formue+1*BookOdds[i,5]-1}else{
    if(winner[i]=="A" & BestBet[i,4]=="H") 
    {formue<-formue-1}else{
      if(winner[i]=="A" & BestBet[i,4]=="D") 
      {formue<-formue-1}else{
        
        if(winner[i]=="D" & BestBet[i,4]=="D") 
        {formue<-formue+1*BookOdds[i,4]-1} else{
          if(winner[i]=="D" & BestBet[i,4]=="H") 
          {formue<-formue-1}else{
            if(winner[i]=="D" & BestBet[i,4]=="A") 
            {formue<-formue-1}else{
              
              if(winner[i]=="H" & BestBet[i,4]=="H") 
              {formue<-formue+1*BookOdds[i,3]-1}
              if(winner[i]=="H" & BestBet[i,4]=="A") 
              {formue<-formue-1}else{
                if(winner[i]=="H" & BestBet[i,4]=="D") 
                {formue<-formue-1}else{
                } } } } } } } }
  # Printer formuen for hver iteration
  formue_vektor[i] <- formue
}
print(formue_vektor)

length(formue_vektor)

#Plot formue


title<-c("Formue")

plot(0:131,formue_vektor,type='l',xlab="Kampnummer",ylab="Formue",lwd=2)
abline(h=0, lty=2)







