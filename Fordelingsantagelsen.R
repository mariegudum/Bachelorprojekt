# Bemærk!! Fordelingsantagelsen er lavet før sæsonen 23/24 sluttede, 
# og derfor er det kun 554 kampe, der betragtes (og ikke 3*192=576 kampe)

SL <- read_excel("~/Downloads/DNK (2).xlsx")
SL<-SL %>% filter(Season == "2023/2024" | Season == "2022/2023" | Season == "2021/2022")
M<-nrow(SL)


# Plot over fordelinger

plot1<-SL %>%
  ggplot(aes(x = HG)) +
  geom_bar(fill=rgb(0.69,0.77,0.87))
poisH <- dpois(0:max(SL$HG), mean(SL$HG))
p1<-plot1 + geom_line(data = data.frame(x = 0:max(SL$HG), y = poisH * nrow(SL)),
                      aes(x, y), color = "darkred")+xlab("Antal hjemmemål")+ylab("Antal kampe")

plot2<-SL %>%
  ggplot(aes(x = AG)) +
  geom_bar(fill=rgb(0.69,0.77,0.87))
poisA <- dpois(0:max(SL$AG), mean(SL$AG))
p2<-plot2 + geom_line(data = data.frame(x = 0:max(SL$AG), y = poisA * nrow(SL)),
                      aes(x, y), color = "darkred")+xlab("Antal udemål")+ylab("Antal kampe")

grid.arrange(p1,p2)
summarise(SL, mean(HG), mean(AG))
head(SL)



library(gridExtra)

# Find det maksimale antal mål for både hjemme- og udekampe
max_goals <- max(max(SL$HG), max(SL$AG))

# Opret en dataframe med unikke værdier af mål
unique_goals <- data.frame(goals = 0:max_goals)

# Generer Poisson fordeling for antal hjemme- og udemål
poisH <- dpois(0:max_goals, mean(SL$HG))
poisA <- dpois(0:max_goals, mean(SL$AG))

plot1 <- ggplot(SL, aes(x = HG)) +
  geom_bar(fill = rgb(0.69, 0.77, 0.87)) +
  geom_line(data = data.frame(goals = 0:max_goals, prob = poisH * nrow(SL)),
            aes(x = goals, y = prob), color = "darkred") +
  labs(x = "Antal hjemmemål", y = "Antal kampe") +
  scale_x_continuous(breaks = seq(0, max_goals, by = 1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

plot2 <- ggplot(SL, aes(x = AG)) +
  geom_bar(fill = rgb(0.69, 0.77, 0.87)) +
  geom_line(data = data.frame(goals = 0:max_goals, prob = poisA * nrow(SL)),
            aes(x = goals, y = prob), color = "darkred") +
  labs(x = "Antal udemål", y = "Antal kampe") +
  scale_x_continuous(breaks = seq(0, max_goals, by = 1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

grid.arrange(plot1, plot2)



###Laver goodness of fit SL
#Fordelingstest med 0-4+ mål for hjemmemål
GoFHjem<-function(SL){
  ftag<-SL$AG
  TAG<-table(factor(ftag, levels=0:6))
  fthg<-SL$HG
  THG<-table(factor(fthg, levels=0:6))
  #Ændrer obs til TAG.. for hjemmemål
  ObsHjem<-as.vector(THG)
  goals<-rep(0:(length(THG)-2))
  sshHjem<-function(x){dpois(x,mean(SL$HG))}
  #tager ssh for at score 0-x mål, og finder dermed ssh for x+1 mål.
  PropHjem<-c(sshHjem(goals),(1-sum(sshHjem(goals))))
  OO<-matrix(0,5,1)
  OO[(1:4),1]<-ObsHjem[(1:4)]
  OO[5,1]<-ObsHjem[5]+ObsHjem[6]+ObsHjem[7]
  PP<-matrix(0,5,1)
  PP[(1:4),1]<-PropHjem[(1:4)]
  PP[5,1]<-PropHjem[5]+PropHjem[6]+PropHjem[7]
  hejHjem<-data.frame(Mål=rep(0:4),Observeret=OO,SSH=PP)
  chisq.test(hejHjem[,2],p=hejHjem[,3])
}

GoFHjem(SL)
E<-hejHjem[,3]*M
tabel_hjem<-data.frame(hejHjem[,1:2],Forventet=round(E,2))

#Fordelingstest med 0-4+ mål for udemål
GoFUd<-function(SL){
  ftag<-SL$AG
  TAG<-table(factor(ftag, levels=0:6))
  fthg<-SL$HG
  THG<-table(factor(fthg, levels=0:6))
  #Ændrer obs til TAG
  ObsUd<-as.vector(TAG)
  goals<-rep(0:(length(TAG)-2))
  sshUd<-function(x){dpois(x,mean(SL$AG))}
  #tager ssh for at score 0-x mål, og finder dermed ssh for x+1 mål.
  PropUd<-c(sshUd(goals),(1-sum(sshUd(goals))))
  OO<-matrix(0,5,1)
  OO[(1:4),1]<-ObsUd[(1:4)]
  OO[5,1]<-ObsUd[5]+ObsUd[6]+ObsUd[7]
  PP<-matrix(0,5,1)
  PP[(1:4),1]<-PropUd[(1:4)]
  PP[5,1]<-PropUd[5]+PropUd[6]+PropUd[7]
  hejUd<-data.frame(Mål=rep(0:4),Observeret=OO,SSH=PP)
  chisq.test(hejUd[,2],p=hejUd[,3])
}

GoFUd(SL)
hejUd[,3]*M
E2<-hejUd[,3]*M
tabel_ud<-data.frame(hejUd[,1:2],Forventet=round(E2,2))
xtable(tabel_ud)