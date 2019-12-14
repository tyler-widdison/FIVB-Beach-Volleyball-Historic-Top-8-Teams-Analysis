#desired goals. What hour and court are top 8 teams having the success?
#Librarys loaded
library(lubridate)
library(tidyverse)
library(gridExtra)
library(tidyr)
library(ggthemes)
library(viridis)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(tidyquant)
library(gridExtra)
library(grid)
library(lubridate)
library(sm)
library(GGally)
library(stringr)
library(binhf)
library(reshape2)
library(dataMeta)
# ---------------------------------------------------------------

#1. Data access: download the data set into your R environment. ####
#This data set was webscrapped from 2 sources. First from FIVB Beach competiton database. http://www.fivb.org/EN/BeachVolleyball/Competitions/Competitions.htm
#Second, FIVB Beach rankings from http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayW&Date=20191001. 
#Numerous more links were accessed between these two links. But ultimatly I was able to access the data I was looking for.

dfs <- read.csv('C:/Users/Tyler.Widdison/Documents/all_the_things.csv', header = TRUE, as.is=TRUE,  encoding="UTF-8")
# ---------------------------------------------------------------
#2. Use the data repository to get a definition of all the variables in the data set. ####
#   Perform feature engineering to select variables that support your hypothesis.
#Data Dict Building
#Variables selected for my hypothesis will be:
#hour, court, total_matches, match_won, match_lost, player, team_country, tourn, date, phase, year, match_no, time, gender
dict
# ---------------------------------------------------------------
#3  Perform any data transformations you feel are necessary to achieve the desired goals.####
# ---------------------------------------------------------------
#I did some extra transformations for future work. Some are not needed and some are:
names(dfs)[1] <- 'match_no' #changing the column 1 name
dfs<-dfs[!(dfs$tourn == 'wgoo2001'),] #this tournament is in there under the name 'wbri2001' so I am deleting all of the 'wgoo2001' occurances
dfs$match_no <- as.numeric(dfs$match_no) #needs to be a numeric
dfs$date <- as.Date(dfs$date) #needs to be a date
dfs$court <- as.numeric(dfs$court) #change to numeric
dfs$result <- NULL #getting rid of result. Score is later down the line
dfs$team_rank <- as.numeric(dfs$team_rank) #needs to be a numeric
dfs$opp_team_rank <- as.numeric(dfs$opp_team_rank) #needs to be a numeric
dfs$duration<-as.duration(hms(dfs$duration)) #putting duration into a useable format using lubridate
dfs$duration<-as.vector(dfs$duration) #making the class an integer
dfs$opp_game_three[is.na(dfs$opp_game_three)] <- 0 #ignoring NAs in game three
dfs$team_game_three[is.na(dfs$team_game_three)] <- 0 #...
dfs$tourn_rank[is.na(dfs$tourn_rank)] <- 239 #This NA is only one tournament. I gave it a score of 239 based off teams rankings. 
dfs$score_diff_game_one <-(dfs$team_game_one - dfs$opp_game_one)#score difference for all sets
dfs$score_diff_game_two <-(dfs$team_game_two - dfs$opp_game_two)
dfs$score_diff_game_three <-(dfs$team_game_three - dfs$opp_game_three)
dfs$team_final_score <- (dfs$team_game_one + dfs$team_game_two + dfs$team_game_three) #number of total points scored
dfs$opp_team_final_score <- (dfs$opp_game_one + dfs$opp_game_two + dfs$opp_game_three) #number of total opponent points scored
dfs$team <- str_replace(dfs$team, 'humana-paredes/pavan', 'humana/paredes-pavan') #needing some name changes so things match up properly
dfs$team <- str_replace(dfs$team, 'melissa/pavan', 'humana/paredes-pavan')

dfs$team <- str_replace(dfs$team, 'michon/szczytowicz-kolosinska', 'kolosinska/michon-szczytowicz')
dfs$time <- str_replace(dfs$time, '01:00', '13:00')  #some of the times need adjusting (i checked schedule to make sure this is the case)
dfs$time <- str_replace(dfs$time, '02:00', '14:00')  
dfs$time <- str_replace(dfs$time, '04:00', '16:00')  
dfs$time <- str_replace(dfs$time, '04:57', '16:57')  

#melting the data so I can get both players in a unique setting. Players, at times, will play with different partners throughout one year. Looking at one team wouldn't work for the way I wanted this
df <- melt(dfs, id.vars=c('match_no', 'date', 'time', 'court', 'duration', 'tourn', 'year', 'phase', 'tourn_rank', 'winning_team', 'losing_team', 'gender', 'team', 
                          'team_country', 'team_rank', 'team_match_score', 'team_game_one', 'team_game_two', 'team_game_three', 'opp', 'opp_country', 'opp_match_score', 
                          'opp_game_one', 'opp_game_two', 'opp_game_three', 'opp_team_rank', 'matches_played', 'team_match_won', 'opp_match_won', 'opp_player_1', 
                          'opp_player_2', 'score_diff_game_one', 'score_diff_game_two', 'score_diff_game_three', 'team_final_score', 'opp_team_final_score'))
colnames(df)[which(names(df) == "value")] <- "player" #column changing the melt
df$player <- str_replace(df$player, 'thole j.', 'thole, j.') #more name changes are needing to happen due to data quality needing to be updated
df$player <- str_replace(df$player, 'ces a.', 'cès')
df$player <- str_replace(df$player, 'mol, a.', 'mol a.')

df$team_rank[df$team_rank == 0] <- 150 #Due to time, I only ensured teams 1-50 had a correct ranking. I am giving all other teams with 0 a 150 ranking. 
df$opp_team_rank[df$opp_team_rank == 0] <- 150
df<-df[!is.na(df$team_game_one),] #there are matches with injurys. this throws errors. So I am taking out those injury matches
df$game_three_amount<-lapply(df$team_game_three, function(x){ length(which(x!=0))/length(x)}) #counting the number of set 3s played
df$game_three_amount<-as.numeric(df$game_three_amount) #setting the class as numeric

df$phase2 <- ifelse(grepl("Country", df$phase), 'CQ', #phase of the match is not consistent. For example 'Country Quota BRA' ect ect. I want all Country Quota matches played to say CQ
                    ifelse(grepl("Federation", df$phase), 'CQ',
                           ifelse(grepl("Semi", df$phase), 'Semifinals',
                                  ifelse(grepl("Play-Off", df$phase), 'Playoff',
                                         ifelse(grepl("Gold Medal Match", df$phase), 'Playoff',
                                                ifelse(grepl("Finals", df$phase), 'Final 1st Place',
                                                       ifelse(grepl("Bronze", df$phase), 'Final 1st Place',NA)))))))

df$phase2 <- ifelse(is.na(df$phase2), df$phase, df$phase2) #grouping the phases together so I can get one phase
df$phase <- NULL #I don't want to confuse myself. Nulling out the first phase. 
df<-df %>% rename(phase = phase2)
#4. Use various EDA and simple statistical analysis techniques to gain a deep understanding for the data.####


# --------------------------------------------------------------- 
#A bit of exploration. Not necessarily concerning my hypothesis
#NOTE:: Not all variables are used for my hypothesis. This much filtering and summarising is for exploration
#For this project only total matches, match won, match lost, and rank are needed. 
#all_player_df <- df %>% 
#  dplyr::group_by(player, team, team_country, tourn, date, phase2, year, match_no, court, time, team_rank, gender, opp, opp_team_rank) %>% 
#  dplyr::summarise(total_matches = sum(matches_played),
#                   match_won = sum(team_match_won),
#                   match_lost = sum(matches_played - team_match_won),
#                   match_win = match_won / (match_won + match_lost),
#total_set_threes = sum(game_three_amount),
#total_sets_played = (total_matches + total_matches + total_set_threes),
#                   rank = sum(team_rank))
#duration = mean(duration/60),
#tourn_rank = sum(tourn_rank),
#set_1_team_points = sum(team_game_one),
#set_2_team_points = sum(team_game_two),
#set_3_team_points = sum(team_game_three),
#total_points = sum(team_final_score),
#opp_total_points = sum(opp_team_final_score),
#points_diff_g1 = (sum(team_game_one) - sum(opp_game_one)),
#points_diff_g2 = (sum(team_game_two) - sum(opp_game_two)),
#poitns_diff_g3 = (sum(team_game_three) - sum(opp_game_three)),
#avg_g1_diff = mean(score_diff_game_one),
#avg_points_set = (total_points / total_sets_played),
#avg_g2_diff = mean(score_diff_game_two),
#g3_score_sum = sum(score_diff_game_three),
#avg_g3_diff = (g3_score_sum / total_set_threes),
#points_diff = (total_points - opp_total_points))



all_player_df <- df %>% 
  dplyr::group_by(player, team, team_country, tourn_rank, tourn, date, phase, year, match_no, court, time, team_rank, gender, opp, opp_team_rank) %>% 
  dplyr::summarise(total_matches = sum(matches_played),
                   match_won = sum(team_match_won),
                   match_lost = sum(matches_played - team_match_won),
                   match_win = match_won / (match_won + match_lost),
                   total_set_threes = sum(game_three_amount),
                   total_sets_played = (total_matches + total_matches + total_set_threes),
                   rank = sum(team_rank))


summary(all_player_df)
#player              team           team_country          tourn                date               phase2               year         match_no     
#Length:224468      Length:224468      Length:224468      Length:224468      Min.   :2001-04-04   Length:224468      Min.   :2001   Min.   :  1.00  
#Class :character   Class :character   Class :character   Class :character   1st Qu.:2006-07-20   Class :character   1st Qu.:2006   1st Qu.: 12.00  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :2010-08-27   Mode  :character   Median :2010   Median : 25.00  
#Mean   :2011-02-27                      Mean   :2011   Mean   : 29.39  
#3rd Qu.:2016-04-07                      3rd Qu.:2016   3rd Qu.: 45.00  
#Max.   :2019-11-09                      Max.   :2019   Max.   :136.00  

#     court            time             team_rank          gender              opp            opp_team_rank     total_matches     match_won     
# Min.   :1.00     Length:224468      Min.   :   1.00   Length:224468      Length:224468      Min.   :   1.00   Min.   :1.000   Min.   :0.0000  
# 1st Qu.:1.00     Class :character   1st Qu.:  14.00   Class :character   Class :character   1st Qu.:  14.00   1st Qu.:1.000   1st Qu.:0.0000  
# Median :2.00     Mode  :character   Median :  35.00   Mode  :character   Mode  :character   Median :  35.00   Median :1.000   Median :1.0000  
# Mean   :2.45                        Mean   :  66.73                                         Mean   :  66.95   Mean   :1.007   Mean   :0.5037  
# 3rd Qu.:3.00                        3rd Qu.:  83.00                                         3rd Qu.:  84.00   3rd Qu.:1.000   3rd Qu.:1.0000  
# Max.   :8.00                        Max.   :1976.00                                         Max.   :1976.00   Max.   :2.000   Max.   :2.0000  
# NA's   :103735                      
#match_lost       match_win           rank        
#Min.   :0.0000   Min.   :0.0000   Min.   :   1.00  
#1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:  15.00  
#Median :0.0000   Median :1.0000   Median :  35.00  
#Mean   :0.5037   Mean   :0.5004   Mean   :  67.33  
#3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:  84.00  
#Max.   :2.0000   Max.   :1.0000   Max.   :1976.00  

#                NOTES: NA's within court are expected. Prior to 2004 this information was missing from the FIVB sites.
#                 I want each row to represent one match. In the total matches column some matches are doubling up. I discovered this is happening when two players with the
#                 same names are playing together. For this reason I will filter total matches to be 1 and exclude any matches that have been doubled up.
#                 To keep it consistent. I will only look at matcches with a time and court. 

# ---------------------------------------------------------------
#Filtering for each row to have one match. This consists of MENS and WOMENS matches there are 56 matches where they are listed twice.
#In future work, this will need to be fixed. I will stay focused on top 8 teams during exploration
ppy_df <- all_player_df %>%
  dplyr::filter(total_matches == 1& team_rank<=8)

ppy_df$hour <- substring(ppy_df$time, 0, 2) #I only want to see hour of each day. Not the Minutes
ppy_df$hour<-as.integer(ppy_df$hour) #Make as integer so it is easy to deal with
ppy_df$time[ppy_df$time == ''] <- NA #35,266 matches
ppy_hour <- ppy_df
ppy_hour<-na.omit(ppy_hour) # 17,824 matches

#How many matches do top 8 teams average per season?
test<-ppy_df %>%
  dplyr::group_by(player, year) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won)) 

test <- ppy_df %>%
  dplyr::group_by(player, year) %>%
  dplyr::filter(total_matches > 29) %>%
  dplyr::summarise(total_wins = sum(match_won))
ggplot(aes(match_won)) + 
  geom_density(fill = 'mediumaquamarine')) + 
  geom_vline(aes(xintercept = mean(match_won)), linetype= 'dashed')




# ---------------------------------------------------------------
#5. Use R's plotting features to produce both exploratory and expository data visualizations.
#Onto some data exploration!
#HOUR
#Filtering for hour of match being played

ppy_hour <- ppy_df %>%
  dplyr::group_by(hour) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won),
                   match_lost = sum(match_lost),
                   match_win_perce = match_won/total_matches)

ppy_hour <- ppy_hour %>% #I want total matches to be above 100
  dplyr::filter(ppy_hour$total_matches > 99)

ppy_hour <- na.omit(ppy_hour)
# ---------------------------------------------------------------
#Total of matches played for each hour by teams in the top 8 and those teams win percentage for that hour
top_teams_vs_hour<-ggplot(ppy_hour, aes(hour, total_matches)) + 
  geom_col(aes(hour, total_matches), color = 'green', fill = 'lightgreen') +
  geom_text(aes(label = formattable::percent(match_win_perce)), nudge_y = 20, alpha = 0.5) + 
  scale_x_continuous(breaks = seq(08, 21, by = 1)) +
  scale_y_continuous(breaks = seq(0, 3200, by = 100)) +
  theme_tq()+
  ggtitle('FIVB top 8 teams total matches and win% on each hour')
top_teams_vs_hour

top_teams_vs_court <- 
  ggplot(ppy_hour, aes(hour, total_matches)) + 
  geom_col(aes(hour, total_matches), color = 'green', fill = 'lightgreen') +
  geom_line(aes(hour, match_win_perce*2500)) + 
  geom_text(aes(label = formattable::percent(match_win_perce)), nudge_y = 145, alpha = 0.5) + 
  geom_text(aes(label = total_matches),nudge_y = -145, alpha = 0.5) +
  scale_x_continuous(breaks = seq(08, 21, by = 1)) +
  scale_y_continuous(breaks = seq(0, 3200, by = 200)) +
  theme_tq()+
  labs(title = 'Total matches and win% per hour', x = 'Hour', y = 'Games')
top_teams_vs_court

#A lot of matches happen during the 11:00 & 13:00 hour. Top teams as a cumulitive have lower win% in the 15:00, 18:00, 20:00, & 22:00 hour(s).
#Teams within the top 8 are likley beating each other giving the win% a lower rate. 
#On the flip side. Top teams are very good between 08:00 & 09:00 hour. These are likley pool play matches.



#Linear regression on match hour and win percentage
#More hour and win percentage
plot(ppy_hour$hour,ppy_hour$match_win_perce,pch=19,col="blue", type = 'o')
#Base R showing the hour and win percent. Another observation similar to the previous statement 
# ---------------------------------------------------------------
# Linear model using basic least squares to predict win% and hour
hm1 <- lm(ppy_hour$match_win_perce ~ ppy_hour$hour)

#Call:
#  lm(formula = ppy_hour$match_win_perce ~ ppy_hour$hour)
#
#Coefficients:
#  (Intercept)  ppy_hour$hour  
#0.841531      -0.008797 
#Regresstion line through match win% per hour
lines(ppy_hour$hour,hm1$fitted,col="red",lwd=3)
# ---------------------------------------------------------------
summary(hm1)
#Call:
#  lm(formula = ppy_hour$match_win_perce ~ ppy_hour$hour)
#
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.037019 -0.015980  0.001088  0.019147  0.041320 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    0.841531   0.026461  31.802 5.89e-13 ***
#  ppy_hour$hour -0.008797   0.001758  -5.003 0.000308 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.02652 on 12 degrees of freedom
#Multiple R-squared:  0.676,	Adjusted R-squared:  0.649 
#F-statistic: 25.03 on 1 and 12 DF,  p-value: 0.0003076

names(hm1)
#[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"        "qr"            "df.residual"   "xlevels"       "call"         
#[11] "terms"         "model" 
# ---------------------------------------------------------------
#Coefficient Vector
hm1$coeff
#(Intercept) ppy_hour$hour 
#0.841531265  -0.008797031
# ---------------------------------------------------------------
#Using the trained linear model to predict top 8 teams wins during playing hours
predict_hour <- hm1$coeff[1] + hm1$coeff[2]
predict_hour
#(Intercept) 
#0.8327342

# ---------------------------------------------------------------
#COURT
#Filtering for the court being played
ppy_court <- ppy_df %>%
  dplyr::group_by(court) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won),
                   match_lost = sum(match_lost),
                   match_win_perce = match_won/total_matches)

ppy_court <- na.omit(ppy_court)
#I want total matches to be above 100
ppy_court <- ppy_court %>%
  dplyr::filter(total_matches > 99)

# ---------------------------------------------------------------
#Total of matches played for each hour by teams in the top 8 and those teams win percentage for that hour
top_teams_vs_court <- ggplot(ppy_court, aes(court, total_matches)) + 
  geom_col(aes(court, total_matches), color = 'green', fill = 'lightgreen') +
  geom_line(aes(court, match_win_perce)) + 
  geom_text(aes(label = formattable::percent(match_win_perce)), nudge_y = 145, alpha = 0.5) + 
  geom_text(aes(label = total_matches),nudge_y = -145, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 7200, by = 200)) +
  theme_tq()+
  labs(title = 'Total matches and win% per court', x = 'Court', y = 'Games')
top_teams_vs_court
#A lot of matches happen on court 1 for the top teams. That is expected. Teams within the top 8 are likley beating each other giving the win% a lower rate. 


#A different look using base R
plot(ppy_court$court,ppy_court$match_win_perce,pch=19,col="blue", type = 'o')
# ---------------------------------------------------------------
# Linear model using basic least squares to predict win% and court
#cm1 <- lm(ppy_court$match_win_perce ~ ppy_court$court)
#Call:
#  lm(formula = ppy_court$match_win_perce ~ ppy_court$court)
#
#Coefficients:
#  (Intercept)  ppy_court$court  
#0.68341          0.01757 
# ---------------------------------------------------------------
#Regresstion line through match win% per hour
lines(ppy_court$court,cm1$fitted,col="red",lwd=3)

#Again, likely top teams are beating each other on court 1. However, if I was a lower ranked team I would want to be playing a top ranked team on court 4. 
#The matches played here are likely pool play matches. 
# ---------------------------------------------------------------
#6. Select one or more of R's statistical learning algorithms to make predictions, and/or discoveries.
#Using the trained linear model to predict top 8 teams wins on courts
predict_court <- cm1$coeff[1] + 60 * cm1$coeff[2]
predict_court
#(Intercept) 
#1.736203 
# ---------------------------------------------------------------
#More filtering to look at both hour and court
hour_court <- ppy_df %>%
  dplyr::group_by(hour, court) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won),
                   match_lost = sum(match_lost),
                   match_win_perce = match_won/total_matches)
summary(hour_court)
#hour            court          total_matches    match_won       match_lost       match_win_perce 
#Min.   : 8.00   Min.   :1.000   Min.   :  2.0   Min.   :  0.0   Min.   :  0.00   Min.   :0.0000  
#1st Qu.:11.00   1st Qu.:2.000   1st Qu.: 20.0   1st Qu.: 14.5   1st Qu.:  4.00   1st Qu.:0.6857  
#Median :15.00   Median :4.000   Median : 88.0   Median : 72.0   Median : 22.00   Median :0.7500  
#Mean   :14.85   Mean   :3.724   Mean   :181.9   Mean   :130.8   Mean   : 51.04   Mean   :0.7604  
#3rd Qu.:18.00   3rd Qu.:5.000   3rd Qu.:307.0   3rd Qu.:236.5   3rd Qu.: 73.50   3rd Qu.:0.8142  
#Max.   :23.00   Max.   :8.000   Max.   :758.0   Max.   :508.0   Max.   :254.00   Max.   :1.0000  


#I want the total mean matches
hour_court <- hour_court %>%
  dplyr::filter(total_matches >= 88)

# ---------------------------------------------------------------
# Predictors for hour and court
formula <- match_win_perce ~hour + court

mhlm2 <- glm(formula, data=hour_court)
summary(mhlm2)
#Call:
#  glm(formula = formula, data = hour_court)
#
#Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
#-0.079251  -0.024858  -0.004246   0.017203   0.115857  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.785438   0.030583  25.683  < 2e-16 ***
#  hour        -0.008279   0.001857  -4.459 5.09e-05 ***
#  court        0.022084   0.005310   4.159 0.000135 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for gaussian family taken to be 0.002004866)
#
#Null deviance: 0.180346  on 49  degrees of freedom
#Residual deviance: 0.094229  on 47  degrees of freedom
#AIC: -163.81
#
#Number of Fisher Scoring iterations: 2
# ---------------------------------------------------------------
summary(mhlm2)$coef
#             Estimate    Std. Error   t value     Pr(>|t|)
#(Intercept)  0.785437655 0.030582566 25.682530 2.610528e-29
#hour        -0.008279455 0.001856673 -4.459297 5.093964e-05
#court        0.022083741 0.005309875  4.158994 1.345763e-04
# ---------------------------------------------------------------
prob <- predict(mhlm2, newdata=hour_court, type="response")
prob <- round(prob,3)
prob
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    21    22    23    24    25    26 
#0.741 0.763 0.785 0.808 0.733 0.755 0.777 0.799 0.725 0.747 0.769 0.791 0.716 0.739 0.761 0.783 0.805 0.708 0.730 0.752 0.774 0.797 0.700 0.722 0.744 0.766 
#27    28    29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49    50 
#0.692 0.714 0.736 0.758 0.683 0.705 0.727 0.750 0.675 0.697 0.719 0.741 0.667 0.689 0.711 0.733 0.658 0.681 0.703 0.725 0.650 0.672 0.694 0.642 
# ---------------------------------------------------------------
#add the prob to the hour_court dataframe
hour_court$prob <- prob

#Models, probability and win%

hour_court %>%
  arrange(desc(prob)) %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(x=prob, y=match_win_perce, size=total_matches, color=court)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 15))


f %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(court, hour)) +
  geom_point(aes(size = match_win_perce, color = prob), alpha = 0.5) + 
  scale_size(range = c(.1, 15)) +
  scale_y_continuous(breaks = seq(8, 22, by = 1)) +
  labs(title = 'FIVB top 8 team probability of winning by court and hour',
       subtitle = 'Data from 2004 - 2019',
       size = 'Win%',
       color ='Probability',
       x = 'Court',
       y = 'Hour')+
  theme_minimal()

#QQplot of prob
qqnorm(hour_court$prob, pch = 1, frame = FALSE)
qqline(hour_court$prob, col = "steelblue", lwd = 2)

#7. In the case of predictions, use the trained algorithm on new data and make a case for the algorithm's accuracy.
#Linear model to predict win% using court and hour
# Split data set into training set and test set
n <- nrow(hour_court)  # Number of observations = 102
ntrain <- round(n*0.6)    # 60% for training set
set.seed(1)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

trainhc <- hour_court[tindex,]  # Create training set
testhc <- hour_court[-tindex,]  # Create test set

lm2 <- lm(match_win_perce~court + hour, data=trainhc)
summary(lm2)

#Call:
#  lm(formula = match_win_perce ~ court + hour, data = trainhc)
#
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.085206 -0.030327 -0.006951  0.018540  0.118856 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.768180   0.048041  15.990 2.71e-15 ***
#  court        0.020426   0.008366   2.441   0.0215 *  
#  hour        -0.006290   0.002881  -2.183   0.0379 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.05237 on 27 degrees of freedom
#Multiple R-squared:   0.34,	Adjusted R-squared:  0.2911 
#F-statistic: 6.955 on 2 and 27 DF,  p-value: 0.003661


# The predicted vs. residual plot  
plot(lm2$fitted, lm2$residuals)


# Using the trained model to predict
predict2 <- predict(lm2, newdata=testhc) 

cor(predict2, testhc$match_win_perce)
#[1] 0.8807696


# ---------------------------------------------------------------
# QQ plot for residuals: shows if residuals are normally distributed.
# Good if residuals are close to straight line
rs <- residuals(lm2)
qqnorm(rs)     
qqline(rs)

# Plot predicted vs. actual
plot(testhc$match_win_perce,predict2)
# ---------------------------------------------------------------


#######################




library(randomForest)
# Train randomForest to predict win% using all predictors
rf <- randomForest(match_win_perce~hour + court, data=hour_court, ntree=500, 
                   mtry=2, importance=TRUE)

varUsed(rf, by.tree=FALSE, count=TRUE)
#[1] 5165 2400

par(mfrow = c(2,1))
plot(hour_court$match_win_perce, hour_court$hour)
plot(hour_court$match_win_perce, as.factor(hour_court$court))

#Hour seems to be the best variable to predict if a top 8 team will win. In future work 

#coorlation of two plots shows Hour as a greater predictor
library(caret)
#################################
# Split data set into training set and test set

#df.percent <- ppy_hour[c('hour', 'match_win_perce')] %>%
#  gather(variable, value, -hour)

ggplot(ppy_hour, aes(hour, match_won)) + 
  geom_col(aes(hour, match_won), color = 'black', fill = 'lightgreen') +
  geom_col(aes(hour, match_lost), color = 'black', fill = 'pink') +
  #geom_text(aes(label = formattable::percent(match_win_perce)), nudge_y = 145, alpha = 0.5) + 
  geom_text(aes(label = match_won),nudge_y = 40, alpha = 0.7) +
  geom_text(aes(label = match_lost),nudge_y = -40, alpha = 0.7)+
  scale_x_continuous(breaks = seq(08, 21, by = 1)) +
  scale_y_continuous(breaks = seq(0, 2400, by = 150),sec.axis = sec_axis(~./20, name = "win %")) +
  scale_color_manual(labels = c("Games won", "Games played"), values = c("blue", "red")) 
  

ppy_hour$match_win_perce <- NULL
ppy_hour$total_matches <- NULL

t<- melt(ppy_hour, id.vars = c('hour'))

ggplot(t, aes(hour, value, fill = variable)) + geom_col()



ggplot(data=t, aes(x=value, group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha = 0.6) + geom_vline(xintercept = 58.67, linetype = 'dotted', size = 1.5, color = '#9c5959') +
  geom_vline(xintercept = 42.21, linetype = 'dotted', size = 1.5, color = '#50948a') +
  facet_wrap(~year)



ggplot(year_mp, aes(year, total_matches)) + 
  geom_line() + 
  geom_line(aes(year, total_wins))

d<-melt(year_mp, id = 'year')


#````````````````````````````






formula <- match_win_perce ~hour + court

mhlm2 <- glm(formula, data=hour_court)
summary(mhlm2)
#Call:
#  glm(formula = formula, data = hour_court)
#
#Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
#-0.079251  -0.024858  -0.004246   0.017203   0.115857  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.785438   0.030583  25.683  < 2e-16 ***
#  hour        -0.008279   0.001857  -4.459 5.09e-05 ***
#  court        0.022084   0.005310   4.159 0.000135 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for gaussian family taken to be 0.002004866)
#
#Null deviance: 0.180346  on 49  degrees of freedom
#Residual deviance: 0.094229  on 47  degrees of freedom
#AIC: -163.81
#
#Number of Fisher Scoring iterations: 2
# ---------------------------------------------------------------
summary(mhlm2)$coef
#             Estimate    Std. Error   t value     Pr(>|t|)
#(Intercept)  0.785437655 0.030582566 25.682530 2.610528e-29
#hour        -0.008279455 0.001856673 -4.459297 5.093964e-05
#court        0.022083741 0.005309875  4.158994 1.345763e-04
# ---------------------------------------------------------------
prob <- predict(mhlm2, newdata=hour_court, type="response")
prob <- round(prob,3)
prob
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    21    22    23    24    25    26 
#0.741 0.763 0.785 0.808 0.733 0.755 0.777 0.799 0.725 0.747 0.769 0.791 0.716 0.739 0.761 0.783 0.805 0.708 0.730 0.752 0.774 0.797 0.700 0.722 0.744 0.766 
#27    28    29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49    50 
#0.692 0.714 0.736 0.758 0.683 0.705 0.727 0.750 0.675 0.697 0.719 0.741 0.667 0.689 0.711 0.733 0.658 0.681 0.703 0.725 0.650 0.672 0.694 0.642 
# ---------------------------------------------------------------
#add the prob to the hour_court dataframe
hour_court$prob <- prob

#Models, probability and win%

hour_court %>%
  arrange(desc(prob)) %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(x=prob, y=match_win_perce, size=total_matches, color=court)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 15))


hour_court %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(court, hour)) +
  geom_point(aes(size = match_win_perce, color = prob), alpha = 0.5) + 
  scale_size(range = c(.1, 15)) +
  scale_y_continuous(breaks = seq(8, 22, by = 1)) +
  labs(title = 'FIVB top 8 team probability of winning by court and hour',
       subtitle = 'Data from 2004 - 2019',
       size = 'Win%',
       color ='Probability',
       x = 'Court',
       y = 'Hour')+
  theme_minimal()

#QQplot of prob
qqnorm(hour_court$prob, pch = 1, frame = FALSE)
qqline(hour_court$prob, col = "steelblue", lwd = 2)
