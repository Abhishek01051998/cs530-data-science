library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
#library(plyr)
library(dplyr)
library(gridExtra)
library(treemap)
library(RColorBrewer)
library(tidyr)
library(radarchart)

deliveries<-read.csv("deliveries.csv")
matches<-read.csv("matches.csv")
matches<-matches[matches$result=="normal",]

matches[which(as.character(matches$team2)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team2)==as.character(matches$winner)),"team1"]
matches[which(as.character(matches$team1)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team1)==as.character(matches$winner)),"team2"]

#1.	Closeness of the matches when the team batting first wins
matches1<-matches[matches$win_by_runs!=0,]
closeness<-function(x,y = "gold" ){
  data1<-matches1[matches1$winner==x|matches1$loser==x,]
  data1[data1$loser==x,"win_by_runs"]<- -data1[data1$loser==x,"win_by_runs"]
  ggplot(data1,aes(1:nrow(data1),win_by_runs))+ geom_area(fill=y)+ggtitle(x)+
    ylab("Runs")+ xlab("Matches")+ geom_ribbon(aes(ymin=-5, ymax=5),fill="red",alpha=0.4) +geom_ribbon(aes(ymin=-15, ymax=15),fill="red",alpha=0.1) +
    guides(fill=FALSE)+scale_alpha(guide = 'none')+coord_cartesian(ylim = c(-100, 100)) 
}
a<-closeness("Chennai Super Kings")
b<-closeness("Kolkata Knight Riders","purple")
c<-closeness("Sunrisers Hyderabad","orange")
d<-closeness("Mumbai Indians","blue2")
e<-closeness("Royal Challengers Bangalore","red3")
f<-closeness("Delhi Daredevils","firebrick3")
g<-closeness("Rajasthan Royals","blueviolet")
h<-closeness("Kings XI Punjab","salmon")
grid.arrange(a,b,c,e,d,f,g,h,ncol=2)


#2.	Number of matches played in different cities
ggplot(matches[which(!is.na(matches$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Number of Matches Played") +
  guides(fill=FALSE)

#3.	Number of matches played in different stadium
ggplot(matches,aes(venue, rm.na=T)) +geom_bar(fill="#0072B2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Number of Matches Played")


#4.	Is winning the toss really an advantage?
matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")
ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")

#5.	Is home advantage a real thing in IPL?
Data<-matches[matches$season!="2009",]
Data$date<- as.Date(Data$date)
Data1<-Data[Data$date < as.Date("2014-04-16") | Data$date > as.Date("2014-04-30"),]
Data1$home_team[Data1$city=="Bangalore"]<- "Royal Challengers Bangalore"
Data1$home_team[Data1$city=="Chennai"]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Delhi"]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Chandigarh"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Jaipur"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Mumbai"]<- "Mumbai Indians"
Data1$home_team[Data1$city=="Kolkata"]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Kochi"]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season <=2012]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season >2012]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ahmedabad"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Dharamsala"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Visakhapatnam" & Data1$season== 2015]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ranchi" & Data1$season== 2013]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Ranchi" & Data1$season > 2013]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Rajkot" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Kanpur" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Raipur" ]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Nagpur" ]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Indore" ]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Pune" & Data1$season!= 2016]<- "Pune Warriors"
Data1$home_team[Data1$city=="Pune" & Data1$season== 2016]<- "Rising Pune Supergiants"
Data1<-Data1[ which(!is.na(Data1$home_team)),]
Data1$win_host <- ifelse(as.character(Data1$winner)==as.character(Data1$home_team),"Home","Away")

ggplot(Data1[which(!is.na(Data1$win_host)),],aes(win_host,fill= win_host))+geom_bar()+
  ggtitle("Is home advantage a real thing in IPL?")+
  xlab("Team")+
  ylab("Number of Matches won")+labs(aesthetic="Winner")

#6.	Number of matches played by each team
ggplot(as.data.frame(table(matches$team2) + table(matches$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) +geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+
  ylab("Number of Matches") +guides(fill=FALSE)


#7.	Number of Matches won by each teams
ggplot(matches,aes(winner)) +geom_bar(fill="#0072B2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Matches won")

#8.	Win percentage of each team in IPL
matches_won<-as.data.frame(table(matches$winner))
colnames(matches_won)[2]<-"Won"
matches_played<-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2]<-"Played"

ggplot(left_join(matches_played,matches_won ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))

#Top Batsman
df<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%
  filter(runs > 3000) 
df %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F)


#10.	Best seat in the House 
deliveries %>% group_by(non_striker) %>% 
  summarise(Runs= sum(total_runs)) %>% top_n(n=10,wt= Runs) %>% ggplot(aes(reorder(non_striker,-Runs),Runs,fill=non_striker)) + geom_bar(stat="identity")+ xlab("Players") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + guides(fill=F) + ggtitle("Best seat in the house- Non-striker's end!")


#11.	Top Bowlers
df<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
df %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F)


#total runs scored at at each delivery of an over 

df <- deliveries %>% group_by(ball) %>% summarise(Runs = sum(total_runs)) %>% filter(ball<7) 
print(df)
df %>% ggplot(aes(ball,Runs,fill=ball)) +geom_bar(stat = "identity")+scale_x_continuous(breaks = c(1,2,3,4,5,6))+ 
  guides(fill=F) +xlab("Ball") + ylab("Total runs scored") + ggtitle("Total runs scored in each delivery of the over")

#13.	Average number of runs scored in each delivery of the over
df<-deliveries %>% group_by(ball) %>% summarise(Runs = mean(total_runs)) %>% filter(ball<7)
print(df)
df %>% ggplot(aes(ball,Runs,fill=ball)) +geom_bar(stat = "identity")+scale_x_continuous(breaks = c(1,2,3,4,5,6))+ 
  guides(fill=F) +xlab("Ball") + ylab("Average runs scored") + ggtitle("Average number of runs scored in each delivery of the over")


#14.	Total number of runs scored in each over of the innings
df <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= sum(total_runs))
print(df)
df %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity")+scale_x_continuous(breaks = 1:20)+ 
  guides(fill=F) +xlab("Over") + ylab("Total runs scored") + ggtitle("Total number of runs scored in each over of the innings")

#15.	Average number of runs scored in each over of the innings

df <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= mean(total_runs)*6)
print(df)
df %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity")+scale_x_continuous(breaks = 1:20)+ 
  guides(fill=F) +xlab("Over") + ylab("Total runs scored") + ggtitle("Average number of runs scored in each over of the innings")

#16.	Total number of wickets in each over of the innings
deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Wickets= length(player_dismissed)) %>% 
  ggplot(aes(over,Wickets,fill=over))+geom_bar(stat = "identity") +scale_x_continuous(breaks = 1:20)+ 
  guides(fill=F) +xlab("Over") + ylab("Total wickets taken") + ggtitle("Total number of wickets in each over of the innings")

#	S17.	Batsmen with top strike rate
deliveries %>% group_by(batsman) %>% filter(length(total_runs)>500) %>% summarise(strike_rate= mean(batsman_runs)*100) %>% top_n(n=10,wt=strike_rate) %>%
  ggplot(aes(reorder(batsman,-strike_rate),strike_rate,fill=batsman))+ geom_bar(stat="identity")+ xlab("Batsman") + ylab("Strike Rate") +
  ggtitle("Batsmen with top strike rate",subtitle = "Minimum 500 balls faced")+
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + guides(fill=F)


#19.	Strike Rate in different stages of the games
deliveries %>% 
  filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% 
  group_by(batsman,over) %>% summarise(strike= mean(batsman_runs)*100) %>%  
  ggplot(aes(over,strike, col=batsman)) + geom_line(size=2) + ylab("Strike Rate") + 
  ggtitle("Strike rate in different stages of the game ") + scale_x_continuous(breaks = 1:20)

#Virat Kohli
deliveries %>% filter(batsman=="V Kohli") %>%
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
  geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression- Virat Kohli") +  coord_cartesian(ylim = c(0, 120)) 


#Gautam Gambir
deliveries %>% filter(batsman=="G Gambhir") %>%
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
  geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression- G Gambhir")+  coord_cartesian(ylim = c(0, 120)) 

#rohit Sharma
deliveries %>% filter(batsman=="RG Sharma") %>%
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
  geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression- RG Sharma")+  coord_cartesian(ylim = c(0, 120)) 

#Suresh Raina
deliveries %>% filter( batsman=="SK Raina" ) %>%
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
  geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression- SK Raina")+  coord_cartesian(ylim = c(0, 120))




#20.	Relating Innings progression with Result of the match
colnames(matches)[1]<- "match_id" 
deliveries %>% left_join(matches) %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>%
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  ggplot(aes(cum_ball,cum_run,col=ifelse(as.character(batting_team)==as.character(winner),"Winning Cause","Losing Cause"))) + 
  geom_point() + facet_wrap(~batsman,ncol=2) + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle(" Relating Innings Progression with Result of the match")


#21.	Innings progression in different innings
deliveries %>%  left_join(matches) %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% 
  group_by(match_id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>% filter(is_super_over==0) %>%
  ggplot(aes(cum_ball,cum_run,col=as.factor(inning))) + geom_point()  + facet_wrap(~batsman,ncol=2) + labs(col="Innings") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression in Different innings")

#22.	Runs scored vs balls faced in all matches
deliveries %>%filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir") %>% group_by(match_id) %>%
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
  filter(player_dismissed=="V Kohli"|player_dismissed=="SK Raina" |player_dismissed=="RG Sharma"|player_dismissed=="G Gambhir") %>% 
  ggplot(aes(cum_ball,cum_run,col=batsman)) +geom_point() + xlab("Balls") +ylab("Runs")+ ggtitle("Runs scored vs balls faced in all the matches")


