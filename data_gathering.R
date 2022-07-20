#library world football R can query data from many football datasources like fbref, transfermarkt
library("worldfootballR")
library("dplyr")

#getting Serie A players market value from 2021/2022 season (source:Transfermarkt)
serie_a_league_valuations <- get_player_market_values(country_name = "Italy", start_year = 2021)
dplyr::glimpse(serie_a_league_valuations)

#cleaning redundant information
serie_a_league_valuations$comp_name <- NULL
serie_a_league_valuations$region <- NULL
serie_a_league_valuations$country <- NULL
serie_a_league_valuations$season_start_year <- NULL
serie_a_league_valuations$squad <- NULL
serie_a_league_valuations$player_num <- NULL
serie_a_league_valuations$player_position <- NULL
serie_a_league_valuations$player_dob <- NULL
serie_a_league_valuations$player_age <- NULL
serie_a_league_valuations$player_nationality <- NULL
serie_a_league_valuations$player_foot <- NULL
serie_a_league_valuations$player_url <- NULL
serie_a_league_valuations$current_club <- NULL
serie_a_league_valuations$date_joined <- NULL
serie_a_league_valuations$joined_from <- NULL
serie_a_league_valuations$contract_expiry <- NULL





#getting Serie A player data from 2021/2022 season (source:FBREF)
league_url <- fb_league_urls(country = "ITA", gender = "M", season_end_year = 2022, tier = '1st')
teams_url_list <- fb_teams_urls(league_url)

#playing time data for each player (from start/as a sub)
playing_time <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "playing_time")
standard <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "standard")
#attacking data for each player 
shooting <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "shooting")
#passing data for each player
passing <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "passing")
#defending data for each player
defense <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "defense")
#goalkeeping data for each player (NA for players who don't play in this position)
keeper <- fb_team_player_stats(team_urls= teams_url_list, stat_type= "keeper")

#since a player can play different roles we extract its primary role to simplify
single_pos <- c()
for(role in standard$Pos){
  pos <- strsplit(role,",")[[1]][1]
  single_pos <- c(single_pos, pos)
}
standard <- cbind(standard,single_pos)

#removing redundancy and noisy columns from standard
standard$Season <- NULL
standard$Comp <- NULL
standard$Nation <- NULL
standard$Starts_Playing_Time <- NULL
standard$Mins_Per_90_Playing_Time <- NULL
standard$G_minus_PK <- NULL
standard$PKatt <- NULL
standard$Gls_Per_Minutes <- NULL
standard$Ast_Per_Minutes <- NULL
standard$`G+A_Per_Minutes`<- NULL
standard$G_minus_PK_Per_Minutes<- NULL
standard$`G+A_minus_PK_Per_Minutes`<- NULL
standard$npxG_Expected<- NULL
standard$`npxG+xA_Expected`<- NULL
standard$xG_Per_Minutes<- NULL
standard$xA_Per_Minutes<- NULL
standard$`xG+xA_Per_Minutes`<- NULL
standard$npxG_Per_Minutes<- NULL
standard$`npxG+xA_Per_Minutes`<- NULL
standard$PlayerURL<- NULL
standard$Pos<- NULL

#removing redundancy and noisy columns from playing time

playing_time$Season <- NULL
playing_time$Comp <- NULL
playing_time$Nation <- NULL
playing_time$Pos <- NULL
playing_time$Age <- NULL
playing_time$MP_Playing_Time <- NULL
playing_time$Min_Playing_Time <- NULL
playing_time$Mn_per_MP_Playing_Time <- NULL
playing_time$Mins_Per_90_Playing_Time <- NULL
playing_time$Mn_per_Start_Starts <- NULL
playing_time$Mn_per_Sub_Subs <- NULL
playing_time$Compl_Starts <- NULL
playing_time$Plus_Minus90_Team_Success <- NULL
playing_time$xGPlus_Minus90_Team_Success_xG <- NULL
playing_time$Starts_Starts<- NULL
playing_time$onG_Team_Success <- NULL
playing_time$onGA_Team_Success <- NULL
playing_time$PPM_Team_Success <- NULL
playing_time$On_minus_Off_Team_Success <- NULL
playing_time$onxG_Team_Success_xG_ <- NULL
playing_time$onxGA_Team_Success_xG <- NULL
playing_time$xGPlus_Minus_Team_Success_xG <- NULL
playing_time$PlayerURL <- NULL

#removing redundancy and noisy columns from defense data
defense$Season <- NULL
defense$Comp <- NULL
defense$Nation <- NULL
defense$Pos <- NULL
defense$Age <- NULL
defense$Mins_Per_90 <- NULL
defense$`Def 3rd_Tackles` <- NULL
defense$`Mid 3rd_Tackles` <- NULL
defense$`Att 3rd_Tackles` <- NULL
defense$Tkl_Vs_Dribbles <- NULL
defense$Att_Vs_Dribbles <- NULL
defense$Past_Vs_Dribbles <- NULL
defense$Succ_Pressures <- NULL
defense$`Def 3rd_Pressures` <- NULL
defense$`Mid 3rd_Pressures` <- NULL
defense$`Att 3rd_Pressures` <- NULL
defense$Sh_Blocks <- NULL
defense$ShSv_Blocks <- NULL
defense$Pass_Blocks <- NULL
defense$`Tkl+Int` <- NULL
defense$PlayerURL <- NULL

#removing redundancy and noisy columns from goalkeeping data
keeper$Season <- NULL
keeper$Comp <- NULL
keeper$Nation <- NULL
keeper$Pos <- NULL
keeper$Age <- NULL
keeper$MP_Playing_Time <- NULL
keeper$Starts_Playing_Time <- NULL
keeper$Min_Playing_Time <- NULL
keeper$Mins_Per_90_Playing_Time <- NULL
keeper$PlayerURL <- NULL
keeper$GA90 <- NULL
keeper$Saves <- NULL
keeper$W <- NULL
keeper$D <- NULL
keeper$L <- NULL
keeper$CS_percent <- NULL
keeper$PKatt_Kicks <- NULL
keeper$PKA_Kicks <- NULL
keeper$PKsv_Kicks <- NULL
keeper$PKm_Kicks <- NULL
keeper$Save_percent_Kicks <- NULL

#removing redundancy and noisy columns from passing data
passing$Season <- NULL
passing$Comp <- NULL
passing$Nation <- NULL
passing$Pos <- NULL
passing$Age <- NULL
passing$Mins_Per_90 <- NULL
passing$Cmp_Total <- NULL
passing$TotDist_Total <- NULL
passing$Cmp_Short <- NULL
passing$Att_Short <- NULL
passing$Cmp_percent_Short <- NULL

passing$Cmp_percent_Medium <- NULL
passing$Cmp_Medium <- NULL
passing$Att_Medium <- NULL

passing$Cmp_percent_Long <- NULL
passing$Cmp_Long <- NULL
passing$Att_Long <- NULL

passing$PlayerURL<- NULL
passing$Ast <- NULL
passing$Final_Third <- NULL
passing$A_minus_xA <- NULL

#removing redundancy and noisy columns from passing data
shooting$Season <- NULL
shooting$Comp <- NULL
shooting$Nation <- NULL
shooting$Pos <- NULL
shooting$Age <- NULL
shooting$Mins_Per_90 <- NULL
shooting$PlayerURL <- NULL
shooting$Gls_Standard <- NULL
shooting$SoT_Standard <- NULL
shooting$Sh_per_90_Standard <- NULL
shooting$SoT_per_90_Standard <- NULL
shooting$G_per_Sh_Standard <- NULL
shooting$G_per_SoT_Standard <- NULL
shooting$FK_Standard <- NULL
shooting$PK_Standard <- NULL
shooting$PKatt_Standard <- NULL
shooting$xG_Expected <- NULL
shooting$npxG_Expected <- NULL
shooting$G_minus_xG_Expected <- NULL
shooting$`np:G_minus_xG_Expected` <- NULL
shooting$Dist_Standard <- NULL


#join data tables
serie_a_stats <- inner_join(standard,playing_time, by=c("Player","Squad"))
serie_a_stats <- inner_join(serie_a_stats,shooting, by=c("Player","Squad"))
serie_a_stats <- inner_join(serie_a_stats,passing, by=c("Player","Squad"))
serie_a_stats <- inner_join(serie_a_stats,defense, by=c("Player","Squad"))
serie_a_stats <- left_join(serie_a_stats,keeper, by=c("Player","Squad"))
#change name of the column for join the player market value
names(serie_a_league_valuations)[1] <- paste("Player")
serie_a_stats <- inner_join(serie_a_stats,serie_a_league_valuations, by="Player")


#cleaning duplicate columns
duplicates <- c()
for (feature in colnames(serie_a_stats)){
  if(grepl(".y",feature, fixed=TRUE))
    duplicates <- c(duplicates, feature)
}
features <- colnames(serie_a_stats) 
serie_a_stats <- serie_a_stats[, !names(serie_a_stats) %in% c(duplicates)]

#dummy variables for each role
unique(single_pos)
serie_a_stats$forward <- ifelse(serie_a_stats$single_pos=="FW",1,0)
serie_a_stats$defender <- ifelse(serie_a_stats$single_pos=="DF",1,0)
serie_a_stats$midfield <- ifelse(serie_a_stats$single_pos=="MF",1,0)
serie_a_stats$keeper <- ifelse(serie_a_stats$single_pos=="GK",1,0)
#drop row duplicates
serie_a_stats <- unique(serie_a_stats)
#set NA as 0 since 
serie_a_stats[is.na(serie_a_stats)] <- 0

#cleaning duplicates (not same name)
serie_a_stats$xA <- NULL
serie_a_stats$Min_Playing_Time <- NULL
serie_a_stats$MP_Playing_Time <- NULL
serie_a_stats$TklW_Tackles <- NULL

serie_a_stats <- distinct(serie_a_stats, Player, .keep_all = TRUE)
#export as csv for analysis
write.csv(serie_a_stats,file="Documents/StatLearningProject/serie_a_stats_21_22.csv")
