# Her anvendes filen DNK (1), da denne blev foretaget i april før Superligaen sluttede
SL <- read_excel("~/Downloads/DNK (1).xlsx")
SL<-SL %>% filter(Season == "2023/2024")
M<-nrow(SL)
head(SL)
#view(SL)



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
HDAprob("Midtjylland", "Silkeborg", betaALL,delta,alfa,gamma)*100
Alle_parametre
SL<-cbind(SL,matrix(0,nrow=nrow(SL),ncol=3))
N<-ncol(SL)

# Simulering

# Funktion til at simulere en enkelt sæson
simulate_season <- function(TeamNames, HDAprob) {
  results <- matrix(0, nrow = length(TeamNames), ncol = 3, dimnames = list(TeamNames, c("Wins", "Draws", "Losses")))
  for (i in 1:length(TeamNames)) {
    for (j in 1:length(TeamNames)) {
      if (i != j) {
        probs <- HDAprob(TeamNames[i], TeamNames[j], betaALL, delta, alfa, gamma)
        result <- sample(c("HomeWin", "Draw", "AwayWin"), 1, prob = probs)
        if (result == "HomeWin") {
          results[i, "Wins"] <- results[i, "Wins"] + 1
          results[j, "Losses"] <- results[j, "Losses"] + 1
        } else if (result == "AwayWin") {
          results[i, "Losses"] <- results[i, "Losses"] + 1
          results[j, "Wins"] <- results[j, "Wins"] + 1
        } else {
          results[i, "Draws"] <- results[i, "Draws"] + 1
          results[j, "Draws"] <- results[j, "Draws"] + 1
        }
      }
    }
  }
  
  return(results)
}

simulate_season(TeamNames, HDAprob)


# Simuler 1000 sæsoner af GRUNDSPILLET
all_results_grund <- list()
for (season in 1:1000) {
  season_results_grund <- simulate_season(TeamNames, HDAprob)
  all_results_grund[[season]] <- season_results_grund
}

total_results_grund <- matrix(0, nrow = length(TeamNames), ncol = 3, dimnames = list(TeamNames, c("Wins", "Draws", "Losses")))
for (season_results_grund in all_results_grund) {
  total_results_grund <- total_results_grund + season_results_grund
}
average_results_grund <- total_results_grund / 1000

points_grund <- (average_results_grund[, "Wins"] * 3) + average_results_grund[, "Draws"]
ranked_teams_grund <- data.frame(Team = TeamNames, Points = points_grund)
ranked_teams_grund <- ranked_teams_grund[order(-ranked_teams_grund$Points), ]
ranked_teams_grund$Rank <- 1:nrow(ranked_teams_grund)
TeamNames_Mester<-unique(ranked_teams_grund[1:6,]$Team)
TeamNames_Ned<-unique(ranked_teams_grund[7:12,]$Team)
ranked_teams_grund$Team <- NULL
ranked_teams_grund


# Simuler 1000 sæsoner af MESTERSKABSSPILLET
all_results_mester <- list()
for (season in 1:1000) {
  season_results_mester <- simulate_season(TeamNames_Mester, HDAprob)
  all_results_mester[[season]] <- season_results_mester
}

total_results_mester <- matrix(0, nrow = length(TeamNames_Mester), ncol = 3, dimnames = list(TeamNames_Mester, c("Wins", "Draws", "Losses")))
for (season_results_mester in all_results_mester) {
  total_results_mester <- total_results_mester + season_results_mester
}
average_results_mester <- total_results_mester / 1000

points_mester <- (average_results_mester[, "Wins"] * 3) + average_results_mester[, "Draws"]
ranked_teams_mester <- data.frame(Team = TeamNames_Mester, Points = round(points_mester))
ranked_teams_mester <- ranked_teams_mester[order(-ranked_teams_mester$Points), ]
ranked_teams_mester$Rank <- 1:nrow(ranked_teams_mester)
ranked_teams_mester$Team <- NULL
ranked_teams_mester


# Simuler 100 sæsoner af NEDRYKNINGSSPILLET
all_results_ned <- list()
for (season in 1:1000) {
  season_results_ned <- simulate_season(TeamNames_Ned, HDAprob)
  all_results_ned[[season]] <- season_results_ned
}

total_results_ned <- matrix(0, nrow = length(TeamNames_Ned), ncol = 3, dimnames = list(TeamNames_Ned, c("Wins", "Draws", "Losses")))
for (season_results_ned in all_results_ned) {
  total_results_ned <- total_results_ned + season_results_ned
}
average_results_ned <- total_results_ned / 1000

points_ned <- (average_results_ned[, "Wins"] * 3) + average_results_ned[, "Draws"]
ranked_teams_ned <- data.frame(Team = TeamNames_Ned, Points = points_ned)
ranked_teams_ned <- ranked_teams_ned[order(-ranked_teams_ned$Points), ]
ranked_teams_ned$Rank <- 1:nrow(ranked_teams_ned)
ranked_teams_ned$Team <- NULL
ranked_teams_ned

# Total stilling

points_total1<-points_total2<-list()
TeamNames_frame1<-data_frame(Team=TeamNames_Mester)
TeamNames_frame2<-data_frame(Team=TeamNames_Ned)
for (Team in TeamNames_frame1){
points_total1<-points_grund[Team]+points_mester[Team]
}
for (Team in TeamNames_frame2){
  points_total2<-points_grund[Team]+points_ned[Team]
}

ranked_teams1 <- data.frame(Team = TeamNames_Mester, Points = round(points_total1))
ranked_teams1 <- ranked_teams1[order(-ranked_teams1$Points), ]
ranked_teams1$Rank <- 1:nrow(ranked_teams1)
ranked_teams_final1<-ranked_teams1[,2:3]
winner<-rownames(ranked_teams1[1,])
ranked_teams_final1

ranked_teams2 <- data.frame(Team = TeamNames_Ned, Points = round(points_total2))
ranked_teams2 <- ranked_teams2[order(-ranked_teams2$Points), ]
ranked_teams2$Rank <- 1:nrow(ranked_teams2)
ranked_teams_final2<-ranked_teams2[,2:3]
ranked_teams_final2

cat("Vinderen af sæsonen er:", winner)

latex_table <- xtable(ranked_teams1)
print(latex_table, caption = "Stilling 23/24", caption.placement = "top", include.rownames = FALSE)
latex_table <- xtable(ranked_teams2)
print(latex_table, caption = "Stilling 23/24", caption.placement = "top", include.rownames = FALSE)

