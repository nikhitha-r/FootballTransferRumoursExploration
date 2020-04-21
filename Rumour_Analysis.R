#rm(list=ls())
#gc()


######## Import Libraries ########

library(tidyverse)
library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)

######## Read in the files ########
## TODO : Use these instead of hardcoding the path directly
base_dir<- '/Users/nikhitha/Documents/Seminar/'
news_dir<- 'News_Archive/'

# 1. Parse Betting Odds ######

betting_file<-'/Users/nikhitha/Documents/Seminar/BettingOdds2.csv'
bet_data <- read_csv(betting_file)
bet_data <- bet_data[complete.cases(bet_data),]
bet_data$Date <- gsub('\\"', '', bet_data$Date)
bet_data$Player <- gsub('\\"', '', bet_data$Player)
bet_data$Club <- gsub('\\"', '', bet_data$Club)
bet_data$Odds <- gsub('\\"', '', bet_data$Odds)

bet_data$Date <- as.Date(bet_data$Date, format = "%m-%d")
bet_data <- bet_data %>%
  separate(Odds, c("Temp1", "Temp2"), "/")
bet_data$Odds = as.double(bet_data$Temp2) /(as.integer(bet_data$Temp1) + as.integer(bet_data$Temp2))
bet_data <- select(bet_data,-starts_with("Temp"))

# 2. Read the Rumours from the transfer_rumours csv
rumour_file<-'transfer_rumours.csv'
may_data<- read_csv(paste('D:/A/Seminar/News_Archive/05_2019/',rumour_file,sep=''))
june_data<- read_csv(paste('D:/A/Seminar/News_Archive/06_2019/',rumour_file,sep=''))
july_data<- read_csv(paste('D:/A/Seminar/News_Archive/07_2019/',rumour_file,sep=''))

# 3. Read the Player and the current club list
p2c_map_file<- 'Player2Club_Map.csv' 
p2c_map<- read_csv(paste(base_dir,p2c_map_file,sep='')) # WARNING : Debug later

######## Cleaning the data ########

# 1. Remove rumours mapping a player to its current club 
filterd_May <- anti_join(may_data, p2c_map, by = c("Player" = "Player","Club" = "Current Club"))
filterd_June <- anti_join(june_data, p2c_map, by = c("Player" = "Player","Club" = "Current Club"))
filterd_July <- anti_join(july_data, p2c_map, by = c("Player" = "Player","Club" = "Current Club"))

# 2. Merge the cleaned rumours into one table.
# This is sorted by date by default.
all_rumours <- rbind(filterd_May,filterd_June)
all_rumours <- rbind(all_rumours,filterd_July)
all_rumours <- read_csv('/Users/nikhitha/Documents/Seminar/all_rumours.csv')
all_rumours <- select(all_rumours,-starts_with("Player_Tag"))
all_rumours <- select(all_rumours,-starts_with("Club_Tag"))

all_rumours <- all_rumours %>% distinct()

# Get the dates in correct format
all_rumours$Date <- gsub('_', '/', all_rumours$Date)
all_rumours$Date <- paste(all_rumours$Date,"19",sep="/")
all_rumours$Date <- as.Date(all_rumours$Date, format = "%m/%d/%y")

######## Preprosessing the Data #########

# 1. Count the frequency of players appearing

player_cnt <- count(all_rumours,"Player")
player_cnt <- player_cnt %>% arrange(desc(freq))

# 2. Some Visualization

plyr_freq_vis <- ggplot(player_cnt, aes(x=Player, y=freq))+
  geom_bar(stat="identity")
plyr_freq_vis <- plyr_freq_vis + coord_flip()
ggplotly(plyr_freq_vis)

# 3. Dealing with the Thorgan hazard problem

all_rumours <- all_rumours %>% mutate(Player=replace(Player, which(Player=="Thorgan Hazard" & !(grepl("Thorgan",all_rumours$Rumour))),"Eden Hazard"))
player_cnt <- count(all_rumours,"Player")
player_cnt <- player_cnt %>% arrange(desc(freq))

# 4. Lets see the distribution of the Players

boxplot(player_cnt$freq, horizontal = TRUE)
hist(player_cnt$freq)

######## Analysis starts here #########

# 5. Exploratory Analysis for Players based on Bettind Odds fluctuations

## a.) Harry Maguire
harr_maguire_rum <- filter(all_rumours, Player == "Harry Maguire")
hm_club_dist <- ggplot(harr_maguire_rum, aes(x=Club)) +
  geom_bar(fill='blue')
hm_club_dist + theme(axis.text.x = element_text(angle = 55, hjust = 1))

## b.) Matthijs De Ligt
MdL_rum <- filter(all_rumours, Player == "Matthijs de Ligt")
MdL_club_dist <- ggplot(MdL_rum, aes(x=Club)) +
  geom_bar(fill='orange')
MdL_club_dist + theme(axis.text.x = element_text(angle = 55, hjust = 1))

## b.) Paul Pogba
PP_rum <- filter(all_rumours, Player == "Paul Pogba")
PP_club_dist <- ggplot(PP_rum, aes(x=Club)) +
  geom_bar(fill='lightblue')
PP_club_dist + theme(axis.text.x = element_text(angle = 55, hjust = 1))

# 6. Helper Function that takes PlayerName, ClubName, and DateRange
# to show rumours per day (for top 2/3 clubs) of that player.
# Pass dates in the format : mm/dd/yy"
rum_per_day <- function(player, club, start, end, agnst){
  start_dt = as.Date(start, format = "%m/%d/%y")
  end_dt = as.Date(end, format = "%m/%d/%y")
  filterd_plyr_clb <- filter(all_rumours, Player == player & Club == club)
  filterd_plyr_clb <- filter(filterd_plyr_clb,filterd_plyr_clb$Date>=start_dt & filterd_plyr_clb$Date<=end_dt)
  print(filterd_plyr_clb)
  
  if(agnst == "Date")
  {
    reslt_plot <- ggplot(filterd_plyr_clb, aes(Date)) 
    reslt_plot + geom_bar(aes(fill=Source), width = 0.8) + 
    #scale_fill_manual(values = c("Violet","Purple","lightblue","lightgreen","Orange","firebrick2","antiquewhite3","tomato","magenta","Red","darkgreen","goldenrod4","cadetblue4","darkolivegreen4"))+
      scale_fill_manual(values = c("coral","chocolate","olivedrab3","goldenrod1","brown3","cadetblue2","aquamarine4","azure3","bisque4","mediumseagreen","darkkhaki","firebrick3","lightpink2" ,"mediumpurple3","black"))+
    theme(axis.text.x = element_text(angle=60, vjust=0.6),
          panel.background = element_rect(fill = "ghostwhite",
                                          size = 0.5, linetype = "solid")) +
      labs(title="Paper-wise Daily Distribution", 
           subtitle=paste(player,club,sep=" : "))  
  }
  else
  {
    reslt_plot <- ggplot(filterd_plyr_clb, aes(x=Source)) +
      geom_bar(fill='dodgerblue2') 
    reslt_plot + theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
      labs(title="Reports per Paper", 
           subtitle=paste(player,club,sep=":"))  
  }
}

# 7. Helper Function that takes PlayerName, ClubName, and DateRange
# to show rumours per day (for top 2/3 clubs) of that player and
# the betting odds if existing in the database
rum_n_bets <- function(data1, player, club, start, end){
  start_dt = as.Date(start, format = "%m/%d/%y")
  end_dt = as.Date(end, format = "%m/%d/%y")
  filterd_plyr_clb <- filter(data1, Player == player & Club == club)
   print(filterd_plyr_clb)
  filterd_plyr_clb <- filter(filterd_plyr_clb,filterd_plyr_clb$Date>=start_dt & filterd_plyr_clb$Date<=end_dt)
  #print(filterd_plyr_clb)
  
  # ge the player bets data
  player_bets <- filter(data1, Player == player & Club == club)
  #print(player_bets)
  player_bets$Odds = as.double(player_bets$Odds)
  player_bets$Date <- as.Date(player_bets$Date, format = "%m-%d")
  player_bets$Odds <- round(player_bets$Odds ,2)
 #normalizer <- max(filterd_plyr_clb$freq)
  normalizer <- max(count(filterd_plyr_clb$Date)$freq)
  ggplot() + 
    #geom_bar(aes(x=Date, y=freq), width = 0.8, stat = "identity",fill = "lightblue4") +
    geom_bar(data = filterd_plyr_clb,aes(x=Date, fill=Source), width = 0.8 ) +
    geom_point(data=player_bets,aes(x=Date,y=Odds*normalizer),col="royalblue4",size=3) +
    geom_line(data =player_bets, linetype = "dashed", aes(x=Date,y=Odds*normalizer), color= "Red",size=1) +
    #scale_y_continuous(sec.axis = sec_axis(~ . /.* 10, name = "Odds")) +
    scale_x_date(date_breaks = "weeks",date_labels = "%d-%b" ) +
    #scale_fill_manual(values = c("Violet","Purple","lightblue","lightgreen","Orange","firebrick2","antiquewhite3","tomato","magenta","Red","darkgreen","goldenrod4","cadetblue4","darkolivegreen4"))+
  scale_fill_manual(values = c("coral","chocolate","olivedrab3","goldenrod1","brown3","cadetblue2","aquamarine4","azure3","bisque4","mediumseagreen","darkkhaki","firebrick3","lightpink2" ,"mediumpurple3","black"))+
      theme(axis.text.x = element_text(angle=60, vjust=0.6), panel.background = element_rect(fill = "ghostwhite",size = 0.5, linetype = "solid")) +
      labs( y = "Rumour count", subtitle=paste(player,club,sep=" : "))
}

rum_n_bets("Aaron Wan-Bissaka","Manchester United FC","05/20/19","07/20/19")


rum_n_trends <- function(pc, isPlayer, start, end){
  start_dt = as.Date(start, format = "%m/%d/%y")
  end_dt = as.Date(end, format = "%m/%d/%y")
  if(isPlayer){
    filterd_plyr_clb <- filter(all_rumours, Player == pc)
  }else{
    filterd_plyr_clb <- filter(all_rumours, Club == pc)
  }
  filterd_plyr_clb <- filter(filterd_plyr_clb,filterd_plyr_clb$Date>=start_dt & filterd_plyr_clb$Date<=end_dt)
  #print(filterd_plyr_clb)
  
  # ge the player bets data
  trends <- filter(google_trends, Tag==pc)
  print(trends)
  trends$Rate = as.double(trends$Rate)
  #player_bets$Odds <- round(player_bets$Odds * 10,2)
  normalizer <- max(count(filterd_plyr_clb$Date)$freq)
  ggplot(position = "dodge") + 
    geom_bar(data = filterd_plyr_clb, aes(x=Date,fill=Source), width = 0.8) +
    #geom_bar(data=trends,aes(x=Date),col="greenyellow",width = 1.5) +
    geom_line(data =trends, aes(x=Date, y=Rate/10), color= "royalblue",size=0.2) +
        geom_point(data=trends,aes(x=Date,y=Rate/10),col="royalblue",size=2) +

    #scale_y_continuous(sec.axis = sec_axis(~ . * 10, name = "Odds")) +
    scale_x_date(date_breaks = "weeks",date_labels = "%d-%b" ) +
    #scale_fill_manual(values = c("Violet","Purple","lightblue","lightgreen","Orange","firebrick2","antiquewhite3","tomato","magenta","Red","darkgreen","goldenrod4","cadetblue4","darkolivegreen4"))+
  scale_fill_manual(values = c("coral","chocolate","olivedrab3","goldenrod1","brown3","cadetblue2","aquamarine4","azure3","bisque4","mediumseagreen","darkkhaki","firebrick3","lightpink2" ,"mediumpurple3","black"))+
      theme(axis.text.x = element_text(angle=60, vjust=0.6), panel.background = element_rect(fill = "ghostwhite",size = 0.5, linetype = "solid")) +
      labs(title="Google trends with transfer rumours", subtitle=paste(pc))
}



google_trends = read_csv('/Users/nikhitha/Documents/Seminar/googletrendsnew.csv')
google_trends$Date <- gsub('-', '/', google_trends$Date)
google_trends$Date <- paste(google_trends$Date,"19",sep="/")
google_trends$Date <- as.Date(google_trends$Date, format = "%B/%d/%y")
google_trends$Rate <- sapply(google_trends$Rate, function(x) gsub("%", "", x))
google_trends$Rate <- sapply(google_trends$Rate, function(x) gsub("<", "", x))
google_trends$Rate = as.double(google_trends$Rate)
google_trends$Rate <- sapply(google_trends$Rate, function(x) if(x<20){x*10} else{x})

rum_n_trends("Wilfried Zaha",TRUE, "05/20/19","07/20/19")


grouped_rumours <- count(all_rumours, c("Date", "Player", "Club"))
#bet_data<-bet_data[!(bet_data$Date=="2019-05-20"),]
bet_data$Player[bet_data$Player == "Matthijs De Ligt"] <- "Matthijs de Ligt"
bet_data$Date[bet_data$Date == "2019-05-19"] <- "2019-05-20"
merged_rumours <- merge(grouped_rumours, bet_data, by=c("Date","Player", "Club"), all.x = TRUE)
#######################################################################################################


mer <- merge(merged_rumours, bet_data, by=c("Player", "Club", "Date"), all=TRUE)
#mer <- mer[order(mer$Player, mer$Club),]
mer <- mer %>% distinct(mer$Club, mer$Player, mer$Date, .keep_all = TRUE)
mer$`mer$Club` <- NULL
mer$`mer$Player` <- NULL
mer$`mer$Date` <- NULL
mer$`mer$Odds.x` <- NULL
mer$Odds.x <- NULL

#mer <- mer %>% rename(Date = Date.x, Odds=Odds.x)
mer <- mer[(mer$Player %in% bet_data$Player),]
mer <- mer[order(mer$Player, mer$Club, mer$Date),]

merged_file<-'/Users/nikhitha/Documents/Seminar/mergeddatanew_filtered.csv'
rum_odds <- read_csv(merged_file)
rum_odds[is.na(rum_odds)] <- 0
rum_odds$Date <- as.Date(rum_odds$Date, format = "%d/%m/%y")
# "Aaron Wan-Bissaka" transferred on 30 June 2019
rum_odds <- rum_odds[!( rum_odds$Player == "Aaron Wan-Bissaka" & rum_odds$Club == "Manchester United FC" & rum_odds$Date > "2019-06-30"),]
# Adrien Rabiot transferred as 01 July 2019
rum_odds <- rum_odds[!( rum_odds$Player == "Adrien Rabiot" & rum_odds$Club == "Juventus" & rum_odds$Date > "2019-07-01"),]
#rum_odds$Odds <- trimws(rum_odds$Odds)
rum_odds <- rum_odds[!( rum_odds$Player == "Philippe Coutinho" & rum_odds$Club == "Manchester United FC" & rum_odds$Date > "2019-07-14"),]
rum_odds <- rum_odds[!( rum_odds$freq == 0 |  rum_odds$freq == 1 ),]
rum_odds$Odds <- as.double(rum_odds$Odds)
rum_odds$NormFreq <- ave(rum_odds$freq, c(rum_odds$Player, rum_odds$Club), FUN=function(x) x/max(x))

rum_odds$NormFreq[is.nan(rum_odds$NormFreq)] <- 0 
capture.output(by(rum_odds, list(rum_odds$Player, rum_odds$Club), FUN = function(rum_odds) cor(rum_odds$NormFreq, rum_odds$Odds, method = "pearson")))

odds_rum_mer <- merge(all_rumours, rum_odds, by=c("Player", "Club", "Date"), all =TRUE)
odds_rum_mer <- odds_rum_mer[!is.na(odds_rum_mer$Odds)]

rum_n_bets_paper <- function(data1, player, club, papers, start, end){
  for (paper in papers){
  print(paper[1])
  #print(paper[])
    start_dt = as.Date(start, format = "%m/%d/%y")
    end_dt = as.Date(end, format = "%m/%d/%y")
    filterd_plyr_clb <- filter(data1, Player == player & Club == club & Source == paper)
     print(filterd_plyr_clb)
    filterd_plyr_clb <- filter(filterd_plyr_clb,filterd_plyr_clb$Date>=start_dt & filterd_plyr_clb$Date<=end_dt)
    #print(filterd_plyr_clb)
    
    # ge the player bets data
    player_bets <- filter(data1, Player == player & Club == club & Source == paper)
    #print(player_bets)
    player_bets$Odds = as.double(player_bets$Odds)
    player_bets$Date <- as.Date(player_bets$Date, format = "%m-%d")
    player_bets$Odds <- round(player_bets$Odds ,2)
   #normalizer <- max(filterd_plyr_clb$freq)
    normalizer <- max(count(filterd_plyr_clb$Date)$freq)
   plot1 <- ggplot() + 
      #geom_bar(aes(x=Date, y=freq), width = 0.8, stat = "identity",fill = "lightblue4") +
      geom_bar(data = filterd_plyr_clb,aes(x=Date, fill=Source), width = 0.8 ) +
      geom_point(data=player_bets,aes(x=Date,y=Odds*normalizer),col="royalblue4",size=3) +
      geom_line(data =player_bets, linetype = "dashed", aes(x=Date,y=Odds*normalizer), color= "Red",size=1) +
      #scale_y_continuous(sec.axis = sec_axis(~ . /.* 10, name = "Odds")) +
      scale_x_date(date_breaks = "weeks",date_labels = "%d-%b" ) +
      #scale_fill_manual(values = c("Violet","Purple","lightblue","lightgreen","Orange","firebrick2","antiquewhite3","tomato","magenta","Red","darkgreen","goldenrod4","cadetblue4","darkolivegreen4"))+
    scale_fill_manual(values = c("coral","chocolate","olivedrab3","goldenrod1","brown3","cadetblue2","aquamarine4","azure3","bisque4","mediumseagreen","darkkhaki","firebrick3","lightpink2" ,"mediumpurple3","black"))+
        theme(axis.text.x = element_text(angle=60, vjust=0.6), panel.background = element_rect(fill = "ghostwhite",size = 0.5, linetype = "solid")) +
        labs( y = "Rumour count", subtitle=paste(player,club,sep=" : "))
   print(plot1)
  #  break
 }
}
rum_n_bets(temp_odds_rum_mer,"Paul Pogba","Real Madrid CF","05/20/19","07/14/19")

rum_n_bets_paper(temp_odds_rum_mer, "Philippe Coutinho","Manchester United FC", c("Metro", "Daily_Star"),"05/20/19","07/14/19")
rum_n_bets_paper(temp_odds_rum_mer, "Paul Pogba","Real Madrid CF", c("Marca","Fox"),"05/20/19","07/20/19")

temp_odds_rum_mer <- odds_rum_mer[!duplicated(odds_rum_mer[,c("Rumour", "Club", "Player")]),]


corr_file<-'/Users/nikhitha/Documents/Seminar/Correlation_Odds_Rumors.csv'
corr_data <- read_csv(corr_file)
corr_data$Correlation <- as.double(corr_data$Correlation)
ggcorrplot(corr_data[,2:10], hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)
ggcorrplot(corr_data[,2:10], colors=c("firebrick", "white","chartreuse4"),title="Correlation between betting odds and transfer rumours") + scale_x_discrete(limits=corr_data$X1)
