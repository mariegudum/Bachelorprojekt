# Bookmakernes profitmargin

SL <- read_excel("~/Downloads/DNK (2).xlsx")
SL<-SL %>% filter(Season == "2023/2024" | Season == "2022/2023" | Season == "2021/2022")
M<-nrow(SL)
head(SL)
set.seed(10)

SL[,11:13]
for (i in 1:nrow(SL)){
  profit_margin<-(1/SL[i,11]) *100 + (1/SL[i,12]) *100 + (1/SL[i,13]) *100 
}
cut_calc<-(profit_margin-100)/100