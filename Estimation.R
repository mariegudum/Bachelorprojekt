# Ret selv hvor I henter filerne fra. Her har jeg hentet fra mit Skrivebord


SL <- read_excel("Desktop/Superliga.xlsx")
SL<-SL %>% filter(Season == "2023/2024")
M<-nrow(SL)
head(SL)
#view(SL)


hjemmebane<-data.frame(Hjemmesejre=length(which(SL$Res=="H"))/nrow(SL),
                       Uafgjorte=length(which(SL$Res=="D"))/nrow(SL),
                       Udesejre=length(which(SL$Res=="A"))/nrow(SL))
xtable(hjemmebane)



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


Parametre<-list(hold=TeamNames,hjemmebanefordel=delta,betaALL=betaALL, alfa=alfa, gamma=gamma )
print(Parametre)
Alle_parametre<-data.frame(Hold=TeamNames,alfa=alfa, gamma=gamma)
all<-Alle_parametre[order(-Alle_parametre$alfa), ]
xtable(Alle_parametre)

# Plot parametre
Alle_parametre$Hold <- factor(Alle_parametre$Hold, levels = Alle_parametre$Hold)
Alle_parametre_long <- Alle_parametre %>%
  gather(key = "Parameter", value = "Værdi", -Hold)
xtable(Alle_parametre)

ggplot(Alle_parametre_long, aes(x = Hold, y = Værdi, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Hold", y = "Værdi") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("alfa" = "skyblue2", "gamma" = rgb(0.69,0.77,0.87)))
