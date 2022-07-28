library(ggplot2)
library(grid)
library(gridExtra)
library(neuralnet)
library(nnet)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(dplyr)


moneyball <- read.csv("ORIGINAL.baseball.csv")
manager <- read.csv("manager.csv")


####     billy bean vs other managers
teams <- read.csv("team.csv",na.strings=c("","NA"))
recent_teams <- teams %>%
  filter(year > 1985)
oakland <- recent_teams %>%
  filter(name == "Oakland Athletics") %>%
  mutate(BB = ifelse(year >= 1998, "Billy Beane", "Other Managers"))


g <- ggplot(oakland, aes(x = year, y = w, colour=factor(BB)))
g1 <- g + geom_line() + ggtitle("Oakland's win record \n (1985-2015)")+ xlab("Season") + ylab("Wins") #+ 
  

current_div <- oakland$div_id[oakland$year == 2015] %>%
  as.character

g <- ggplot(oakland, aes(y = rank, x = year, colour=factor(BB)))
g2 <- g + geom_line() + ggtitle(paste("The Oakland A's conference rank", "\n Conference: ", current_div, sep = "")) + 
  xlab("Season") + ylab("") 

grid.arrange(g1, g2,ncol = 1)




######################    cost of wins

salary <- read.csv("salary.csv",na.strings=c("","NA"))

salary_ttl_year <- salary %>%
  group_by(year, team_id) %>%
  summarise(ttl_salary = sum(salary)) %>%
  filter(year >= 1985)

salary_avg_year <- salary_ttl_year %>%
  group_by(year) %>%
  summarise(ttl_salary = mean(ttl_salary, na.rm=TRUE)) %>%
  mutate(team_id = "Avg")

oakland_salary <- salary_ttl_year %>%
  mutate(team_id = as.character(team_id)) %>%
  filter(team_id == "OAK") %>%
  mutate(BB = ifelse(year >= 1998, "Billy Beane", "Other Managers"))

oakland_vs_avg <- salary_ttl_year %>%
  bind_rows(., salary_avg_year) %>%
  filter(team_id == "OAK" | team_id == "Avg")

ggplot(oakland_vs_avg, aes(x = year,
                           y = ttl_salary,
                           color = factor(team_id))) + 
  geom_line() + ggtitle("Oakland vs MLB avg Payroll") + scale_y_continuous(labels = dollar) + 
  ylab("Total Salary") + xlab("Season") + scale_y_continuous(labels = dollar)

recent_teams <- read.csv("team.csv",na.strings=c("","NA")) %>%
  select(team_id, w, year)

dollar_per_win <- recent_teams %>%
  left_join(., salary_ttl_year, by = c("year", "team_id")) %>%
  mutate(dollar_per_win = ttl_salary / w)

dollar_per_win_oak <- dollar_per_win %>%
  filter(team_id == "OAK" & year > 1985)

ggplot(dollar_per_win_oak, aes(y = dollar_per_win, x = year)) + geom_line() + xlab("Season") + ylab("") +
  scale_y_continuous(labels = dollar) + ggtitle("Cost of each win to Oakland A's") 


dollar_per_win_1985 <- dollar_per_win %>% filter(year > 1985) %>% 
  group_by(year) %>% 
  summarise(
    Wins = sum(w),
    Dollar_over_win = (sum(ttl_salary))/(sum(w)),
    Payroll = sum(ttl_salary),
    Year = unique(year)
  )

ggplot(dollar_per_win_1985, aes(y = Dollar_over_win, x = Year)) + geom_line() + 
  xlab("Season") + ylab("") + scale_y_continuous(labels=scales::dollar_format())+ ggtitle ("Cost per win in the MLB")


ggplot() + 
  geom_line(data = dollar_per_win_oak, aes( y = dollar_per_win, x = year, size = "Oakland"), color = "Dark Green") +
  geom_line(data = dollar_per_win_1985, aes(y = Dollar_over_win, x = year)) + 
  xlab("Season") + ylab("") + ggtitle("Cost per win Oakland vs avg MLB roster") +
  scale_y_continuous(labels=scales::dollar_format())

###########################################   How many winns?

dataBefore2002 <- subset(moneyball, Year < 2002)


m <- ggplot(dataBefore2002, aes(x = W,
                                y = Team,
                                color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")

m + geom_vline(aes(xintercept = 95), color = "black", linetype = "dashed", size=1) + xlab("Wins")


dataBefore2002$RD <- dataBefore2002$RS - dataBefore2002$RA



#########################################  run differentail vs wins 
ggplot(dataBefore2002, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")


winsReg <- lm(W ~ RD, data = dataBefore2002)
winsReg


ggplot(dataBefore2002, aes(x = RD,
                           y = W,
                           color = factor(Playoffs))) +
  geom_point() + scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs") +
  geom_vline(aes(xintercept = 133), color = "black", linetype = "dashed", size=1)



###############################  dollar per win 1962 to 2002 A's
dpwo <- dollar_per_win %>%
  filter(year > 1961)


############################### yankees inidpentend stats

stats_62to02 <-read.csv("team.csv",na.strings=c("","NA")) %>%
  select(team_id, franchise_id, h, so, bb, sv, e, year, fp, era) %>%
  filter(franchise_id == "NYY")%>%
  filter(year > 1961)%>%
  filter(year < 2003)


colnames(stats_62to02)[colnames(stats_62to02)=="franchise_id"]<-"Team"
colnames(stats_62to02)[colnames(stats_62to02)=="year"]<-"Year"

NYY2002 <-read.csv("ORIGINAL.baseball.csv",na.strings=c("","NA")) %>%
  select(Team, RS, RA, W, OBP, SLG, BA, OOBP, OSLG, Year)%>%
  filter(Team == "NYY")%>%
  filter(Year < 2003)
  
  
NYY2002T <- merge(stats_62to02,NYY2002,by=c("Team","Year"))  
ind_nyy_reg <- lm(RS ~ h +bb+ +SLG, data = NYY2002T)
summary(ind_nyy_reg)  

ind_nyy_RA <- lm(W ~  + era + h + RS, data = NYY2002T)
summary(ind_nyy_RA)  

 
#####################################  oakland indipendent stats

OAKstats_62to02 <-read.csv("team.csv",na.strings=c("","NA")) %>%
  select(team_id, franchise_id, h, so, bb, sv, e, year) %>%
  filter(franchise_id == "OAK")%>%
  filter(year > 1961)%>%
  filter(year < 2003)


colnames(OAKstats_62to02)[colnames(OAKstats_62to02)=="franchise_id"]<-"Team"
colnames(OAKstats_62to02)[colnames(OAKstats_62to02)=="year"]<-"Year"


OAK2002 <-read.csv("ORIGINAL.baseball.csv",na.strings=c("","NA")) %>%
  select(Team, RS, RA, W, OBP, SLG, BA, OOBP, OSLG, Year)%>%
  filter(Team == "OAK")%>%
  filter(Year < 2002)


OAK2002T <- merge(OAKstats_62to02,OAK2002,by=c("Team","Year"))  


ind_oak_reg <- lm(RS ~ h +bb+ +SLG, data = OAK2002T)
summary(ind_oak_reg)

ind_oak_RA <- lm(RA ~ h +bb+ SLG , data = OAK2002T)
summary(ind_oak_RA)


################################################################

####     NEURALNETWORK
####     Dummies for teams
statBefore02<-read.csv("dataBefore2002csv.csv")

statBefore02$AL<-0
statBefore02$AL[statBefore02$League=="AL"]<-1
statBefore02$NL<-0
statBefore02$NL[statBefore02$League=="NL"]<-1

statBefore02$ANA<-0
statBefore02$ANA[statBefore02$Team=="ANA"]<-1
statBefore02$ARI<-0
statBefore02$ARI[statBefore02$Team=="ARI"]<-1
statBefore02$ATL<-0
statBefore02$ATL[statBefore02$Team=="ATL"]<-1
statBefore02$BAL<-0
statBefore02$BAL[statBefore02$Team=="BAL"]<-1
statBefore02$BOS<-0
statBefore02$BOS[statBefore02$Team=="BOS"]<-1
statBefore02$CAL<-0
statBefore02$CAL[statBefore02$Team=="CAL"]<-1
statBefore02$CHC<-0
statBefore02$CHC[statBefore02$Team=="CHC"]<-1
statBefore02$CHW<-0
statBefore02$CHW[statBefore02$Team=="CHW"]<-1
statBefore02$CIN<-0
statBefore02$CIN[statBefore02$Team=="CIN"]<-1
statBefore02$CLE<-0
statBefore02$CLE[statBefore02$Team=="CLE"]<-1
statBefore02$COL<-0
statBefore02$COL[statBefore02$Team=="COL"]<-1
statBefore02$DET<-0
statBefore02$DET[statBefore02$Team=="DET"]<-1
statBefore02$FLA<-0
statBefore02$FLA[statBefore02$Team=="FLA"]<-1
statBefore02$HOU<-0
statBefore02$HOU[statBefore02$Team=="HOU"]<-1
statBefore02$KCA<-0
statBefore02$KCA[statBefore02$Team=="KCA"]<-1
statBefore02$KCR<-0
statBefore02$KCR[statBefore02$Team=="KCR"]<-1
statBefore02$LAA<-0
statBefore02$LAA[statBefore02$Team=="LAA"]<-1
statBefore02$LAD<-0
statBefore02$LAD[statBefore02$Team=="LAD"]<-1
statBefore02$MIL<-0
statBefore02$MIL[statBefore02$Team=="MIL"]<-1
statBefore02$MIN<-0
statBefore02$MIN[statBefore02$Team=="MIN"]<-1
statBefore02$MON<-0
statBefore02$MON[statBefore02$Team=="MON"]<-1
statBefore02$NYM<-0
statBefore02$NYM[statBefore02$Team=="NYM"]<-1
statBefore02$NYY<-0
statBefore02$NYY[statBefore02$Team=="NYY"]<-1
statBefore02$OAK<-0
statBefore02$OAK[statBefore02$Team=="OAK"]<-1
statBefore02$PHI<-0
statBefore02$PHI[statBefore02$Team=="PHI"]<-1
statBefore02$PIT<-0
statBefore02$PIT[statBefore02$Team=="PIT"]<-1
statBefore02$SDP<-0
statBefore02$SDP[statBefore02$Team=="SDP"]<-1
statBefore02$SEA<-0
statBefore02$SEA[statBefore02$Team=="SEA"]<-1
statBefore02$SEP<-0
statBefore02$SEP[statBefore02$Team=="SEP"]<-1
statBefore02$SFG<-0
statBefore02$SFG[statBefore02$Team=="SFG"]<-1
statBefore02$STL<-0
statBefore02$STL[statBefore02$Team=="STL"]<-1
statBefore02$TBD<-0
statBefore02$TBD[statBefore02$Team=="TBD"]<-1
statBefore02$TEX<-0
statBefore02$TEX[statBefore02$Team=="TEX"]<-1
statBefore02$TOR<-0
statBefore02$TOR[statBefore02$Team=="TOR"]<-1
statBefore02$WSA<-0
statBefore02$WSA[statBefore02$Team=="WSA"]<-1

head(statBefore02)

Before2002<- subset(statBefore02, select = -c(League, Team, X))

head(Before2002)

scaledData<-scale(Before2002)

normalizeData<- function(x) {
  return(((x-min(x))) / (max(x) - min(x)))
}


maxmindf <- as.data.frame(lapply(Before2002, normalizeData))

trainset <- maxmindf[1:722, ]
testset <- maxmindf[723:902, ]


BBnn<-neuralnet(W ~ RS + RA + OBP + SLG + BA + Playoffs + RD + AL + NL + ANA + ARI + ATL + BAL + BOS+ CAL+ CHC+ CHW+ CIN +CLE+ COL+ DET+ FLA+ HOU+KCA +KCR +LAA+ LAD+ MIL+ MIN+ MON+ NYM+NYY+OAK+ PHI+PIT+SDP+SEA+SEP+SFG+STL+TBD+TEX+TOR+WSA , data = trainset, hidden = c(6,6,6), linear.output = FALSE, threshold = 0.01)


BBnn$result.matrix
plot(BBnn)

temp_test <- subset(testset, select = c("RS", "RA","OBP","SLG",  "BA",  "Playoffs", "RD","AL",  "NL", "ANA",  "ARI",  "ATL",  "BAL",  "BOS", "CAL", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "FLA", "HOU", "KCA", "KCR", "LAA", "LAD", "MIL",  "MIN",  "MON",  "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SEA", "SEP", "SFG", "STL", "TBD", "TEX", "TOR", "WSA"))

head(temp_test)
BBnn.result<-neuralnet::compute(BBnn, temp_test)
results<- data.frame(actual = testset$W, prediction = BBnn.result$net.result)

results

###################################################   confucion matrix
roundedresults<- sapply(results, round, digits =0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual, prediction)


#######################################################################


aBBnn<-neuralnet(W ~ RS + RA + OBP + SLG + BA + Playoffs + RD , data = trainset, hidden = c(6,6,6), linear.output = FALSE, threshold = 0.01)


aBBnn$result.matrix
plot(aBBnn)

atemp_test <- subset(testset, select = c("RS", "RA","OBP","SLG",  "BA",  "Playoffs", "RD"))

head(atemp_test)
aBBnn.result<-neuralnet::compute(aBBnn, temp_test)
results<- data.frame(actual = testset$W, prediction = aBBnn.result$net.result)

results

#######################################   confucion matrix
roundedresults<- sapply(results, round, digits =0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual, prediction)


####################################3  Linear regression on all the teams


runsScoredReg <- lm(RS ~ OBP + SLG + BA, data = dataBefore2002)
summary(runsScoredReg)




runsScoredRegNoBA <- lm(RS ~ OBP + SLG, data = dataBefore2002)
summary(runsScoredRegNoBA)




runsAllowedReg <- lm(RA ~ OOBP + OSLG, data = dataBefore2002)
summary(runsAllowedReg)


##################################   Teams Stats in year 2002 

team_data02 <- read.csv("team.csv",na.strings=c("","NA")) %>%
  filter(year == 2002)


ggplot(team, aes(y = h, x = reorder(team_id, h))) + 
  xlab("") + ylab("") + coord_flip()+
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#1 - Hits scored")

ggplot(team, aes(y = r, x = reorder(team_id, r))) + 
  xlab("") + ylab("") + coord_flip()+
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#2 - Runs scored")

ggplot(team_data02, aes(y = bb, x = reorder(team_id, bb))) + 
  coord_flip() + xlab("") + ylab("") +
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#3 - Team walks")

ggplot(team_data02, aes(y = so, x = reorder(team_id, so))) + 
  coord_flip() + xlab("") + ylab("") +
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#4 - Batter outs")

ggplot(team_data02, aes(y = sv, x = reorder(team_id, sv))) + 
  coord_flip() + xlab("") + ylab("") +
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#5 - Saves")

ggplot(team_data02, aes(y = e, x = reorder(team_id, e))) + 
  coord_flip() + xlab("") + ylab("") +
  scale_y_continuous(labels = comma) + geom_bar(stat = "identity", fill = "#0072B2") + ggtitle("#6 - Errors by Fielders")

