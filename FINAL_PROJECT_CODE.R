# ---------------------------------------------------------------
# Desired goals. What hour and court are top 8 teams having the success?
# ---------------------------------------------------------------



# ---------------------------------------------------------------
# Librarys loaded
# ---------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(ggthemes)
library(viridis)
library(knitr)
library(kableExtra)
library(tidyquant)
library(gridExtra)
library(grid)
library(sm)
library(GGally)
library(dataMeta)



# ---------------------------------------------------------------
# Data access: 
# ---------------------------------------------------------------
#This data set was webscrapped from 2 sources. First from FIVB Beach competiton database. http://www.fivb.org/EN/BeachVolleyball/Competitions/Competitions.htm
#Second, FIVB Beach rankings from http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayW&Date=20191001. 
#Numerous more links were accessed between these two links. But ultimatly I was able to access the data I was looking for. 
#How data was compiled and wrangled won't be discussed here




# ---------------------------------------------------------------
# Load csv
# ---------------------------------------------------------------
df <- read.csv('final_df.csv', header = TRUE, as.is=TRUE,  encoding="UTF-8")
glimpse(df)
# Rows: 224,468
# Columns: 23
# $ player            <chr> "a-kim jihee"
# $ hour              <int> 16, 17, 12, 14
# $ team              <chr> "a-kim jihee/choi
# $ team_country      <chr> "KOR", "KOR"
# $ tourn_rank        <dbl> 149.47, 149.47
# $ tourn             <chr> "w13avckr", 
# $ date              <chr> "2013-07-04"
# $ phase             <chr> "Pool A"
# $ year              <int> 2013, 2013
# $ match_no          <int> 17, 31, 27, 15
# $ court             <int> 1, 1, NA, NA, 3 
# $ time              <chr> "16:10", "17:00"
# $ team_rank         <int> 150, 150, 235, 23
# $ gender            <chr> "w", "w", "m"
# $ opp               <chr> "cavestro/tonon"
# $ opp_team_rank     <int> 260, 55, 50, 150
# $ total_matches     <int> 1, 1, 1, 1, 1, 1
# $ match_won         <int> 0, 0, 0, 1, 0, 1
# $ match_lost        <int> 1, 1, 1, 0, 1, 0
# $ match_win         <int> 0, 0, 0, 1, 0, 1
# $ total_set_threes  <int> 0, 0, 1, 0, 0, 1
# $ total_sets_played <int> 2, 2, 3, 2, 2, 3
# $ rank              <int> 150, 150 
# $ rank              <int> 150, 150 
# $ rank              <int> 150, 150 
# ---------------------------------------------------------------
# Notes
# ---------------------------------------------------------------
# NA's within court are expected. Prior to 2004 this information was missing from the FIVB sites.



# ---------------------------------------------------------------
# EDA
# ---------------------------------------------------------------

# filter for top 8 teams
top_8 <- df %>%
  filter(team_rank <= 8)
  
# How many matches do top 8 teams average per season? 2001 - 2019
top_8 %>%
  group_by(player, year, gender) %>%
  summarise(total_matches = sum(total_matches),
            match_won = sum(match_won)) %>% 
  pivot_longer(c('match_won', 'total_matches')) %>% 
  ggplot(aes(value)) + 
  geom_density(aes(fill = name), alpha = .4) + 
  #geom_vline(color = 'deepskyblue',aes(xintercept = ifelse(name == 'total_matches', mean(value), NA))) +
  geom_vline(color = 'coral',aes(xintercept = ifelse(name == 'match_won', mean(value), NA))) +
  theme_bw() +
  scale_fill_manual(values = c("deepskyblue","coral")) + 
  facet_wrap(~gender) +
  
  
  ggplot(aes(gender, value)) + 
  geom_boxplot(aes(fill = name)) +
  theme_bw()

# HOUR
# Filtering for hour of match being played
ppy_hour <- top_8 %>%
  dplyr::group_by(hour) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won),
                   match_lost = sum(match_lost),
                   match_win_perce = match_won/total_matches)
ppy_hour
#  hour    total_matches match_won match_lost match_win_perce
# <int>            <int>     <int>      <int>           <dbl>
# 1      8           899       710        190           0.790
# 2      9          2201      1730        478           0.786
# 3     10          2786      2068        722           0.742
# 4     11          3086      2278        812           0.738
# 5     12          3033      2226        812           0.734
# 6     13          3139      2202        946           0.701
# 7     14          2969      2128        844           0.717
# 8     15          2940      2016        928           0.686
# 9     16          3070      2182        894           0.711
# 10    17          2180      1544        638           0.708
# 11    18          1801      1182        626           0.656
# 12    19          1030       692        342           0.672
# 13    20           330       216        114           0.655
# 14    21           218       142         76           0.651


ppy_hour <- ppy_hour %>% #I want total matches to be above 100
  dplyr::filter(ppy_hour$total_matches > 99, !is.na(hour))
ppy_hour
#  hour    total_matches match_won match_lost match_win_perce
# <int>            <int>     <int>      <int>           <dbl>
# 1      8           899       710        190           0.790
# 2      9          2201      1730        478           0.786
# 3     10          2786      2068        722           0.742
# 4     11          3086      2278        812           0.738
# 5     12          3033      2226        812           0.734
# 6     13          3139      2202        946           0.701
# 7     14          2969      2128        844           0.717
# 8     15          2940      2016        928           0.686
# 9     16          3070      2182        894           0.711
# 10    17          2180      1544        638           0.708
# 11    18          1801      1182        626           0.656
# 12    19          1030       692        342           0.672
# 13    20           330       216        114           0.655
# 14    21           218       142         76           0.651

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
ggplot(ppy_hour, aes(hour, match_won)) + 
  geom_col(aes(hour, match_won), color = 'black', fill = 'lightgreen') +
  geom_col(aes(hour, match_lost), color = 'black', fill = 'pink') +
  #geom_text(aes(label = formattable::percent(match_win_perce)), nudge_y = 145, alpha = 0.5) + 
  geom_text(aes(label = match_won),nudge_y = 40, alpha = 0.7) +
  geom_text(aes(label = match_lost),nudge_y = -40, alpha = 0.7)+
  scale_x_continuous(breaks = seq(08, 21, by = 1)) +
  scale_y_continuous(breaks = seq(0, 2400, by = 150),sec.axis = sec_axis(~./20, name = "win %")) +
  scale_color_manual(labels = c("Games won", "Games played"), values = c("blue", "red")) 


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
#0.8465157 

# ---------------------------------------------------------------
#COURT
#Filtering for the court being played
ppy_court <- top_8 %>%
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
  geom_line(aes(court, match_win_perce*2500)) + 
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
cm1 <- lm(ppy_court$match_win_perce ~ ppy_court$court)
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
#1.813666 
# ---------------------------------------------------------------
#More filtering to look at both hour and court
hour_court <- top_8 %>%
  dplyr::group_by(hour, court) %>%
  dplyr::summarise(total_matches = sum(total_matches),
                   match_won = sum(match_won),
                   match_lost = sum(match_lost),
                   match_win_perce = match_won/total_matches)
summary(hour_court)
#hour            court           total_matches    match_won        match_lost        match_win_perce 
#Min.   : 8.00   Min.   :1.000   Min.   :   2.0   Min.   :   0.0   Min.   :   0.00   Min.   :0.0000  
#1st Qu.:11.00   1st Qu.:2.000   1st Qu.:  25.5   1st Qu.:  19.5   1st Qu.:   6.00   1st Qu.:0.6834  
#Median :15.00   Median :4.000   Median : 131.0   Median :  88.0   Median :  33.00   Median :0.7386  
#Mean   :14.94   Mean   :3.697   Mean   : 304.5   Mean   : 219.3   Mean   :  85.67   Mean   :0.7513  
#3rd Qu.:18.75   3rd Qu.:5.000   3rd Qu.: 376.2   3rd Qu.: 257.0   3rd Qu.: 104.50   3rd Qu.:0.7927  
#Max.   :23.00   Max.   :8.000   Max.   :5494.0   Max.   :4032.0   Max.   :1462.00   Max.   :1.0000 


#I want the total mean matches
hour_court <- hour_court %>%
  dplyr::filter(total_matches >= 88, !is.na(court))

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
#-0.081940  -0.025814   0.000715   0.017408   0.115414  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.784843   0.030525  25.712  < 2e-16 ***
#  hour        -0.008228   0.001853  -4.440 5.42e-05 ***
#  court        0.022713   0.005300   4.286 8.96e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.001997261)
#
#Null deviance: 0.181759  on 49  degrees of freedom
#Residual deviance: 0.093871  on 47  degrees of freedom
#AIC: -164
#
#Number of Fisher Scoring iterations: 2
# ---------------------------------------------------------------
summary(mhlm2)$coef
#             Estimate    Std. Error   t value     Pr(>|t|)
#(Intercept)  0.784842960 0.030524509 25.711895 2.482685e-29
#hour        -0.008228478 0.001853148 -4.440270 5.421070e-05
#court        0.022713398 0.005299795  4.285712 8.957655e-05
# ---------------------------------------------------------------
prob <- predict(mhlm2, newdata=hour_court, type="response")
prob <- round(prob,3)
prob
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    21    22    23    24    25    26    27    28 
#0.742 0.764 0.787 0.810 0.734 0.756 0.779 0.802 0.725 0.748 0.771 0.793 0.717 0.740 0.762 0.785 0.808 0.709 0.732 0.754 0.777 0.800 0.701 0.723 0.746 0.769 0.692 0.715 
#29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49    50 
#0.738 0.760 0.684 0.707 0.730 0.752 0.676 0.699 0.721 0.744 0.668 0.690 0.713 0.736 0.659 0.682 0.705 0.728 0.651 0.674 0.697 0.643 
# ---------------------------------------------------------------
#add the prob to the hour_court dataframe
hour_court$prob <- prob

#Models, probability and win%

hour_court %>%
  arrange(desc(prob)) %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(x=prob, y=match_win_perce, size=total_matches, color=court)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 15)) + 
  theme_bw()


hour_court %>%
  mutate(court = factor(court, court)) %>%
  ggplot(aes(court, hour)) +
  geom_point(aes(size = match_win_perce, color = prob), alpha = 0.5) + 
  scale_size(range = c(.1, 15)) +
  scale_y_continuous(breaks = seq(8, 22, by = 1)) +
  labs(title = 'FIVB top 8 team probability of winning by court and hour',
       size = 'Win%',
       color ='Probability',
       x = 'Court',
       y = 'Hour')+
  theme_minimal()

#QQplot of prob
qqnorm(hour_court$prob, pch = 1, frame = FALSE)
qqline(hour_court$prob, col = "steelblue", lwd = 2)

#################################
formula <- match_win_perce ~hour + court

mhlm2 <- glm(formula, data=hour_court)
summary(mhlm2)
#Call:
#  glm(formula = formula, data = hour_court)
#
#Deviance Residuals: 
#  Min         1Q     Median         3Q        Max  
#-0.081940  -0.025814   0.000715   0.017408   0.115414  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.784843   0.030525  25.712  < 2e-16 ***
#  hour        -0.008228   0.001853  -4.440 5.42e-05 ***
#  court        0.022713   0.005300   4.286 8.96e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.001997261)
#
#Null deviance: 0.181759  on 49  degrees of freedom
#Residual deviance: 0.093871  on 47  degrees of freedom
#AIC: -164
#
#Number of Fisher Scoring iterations: 2

# ---------------------------------------------------------------
summary(mhlm2)$coef
#             Estimate    Std. Error   t value     Pr(>|t|)
#(Intercept)  0.784842960 0.030524509 25.711895 2.482685e-29
#hour        -0.008228478 0.001853148 -4.440270 5.421070e-05
#court        0.022713398 0.005299795  4.285712 8.957655e-05
# ---------------------------------------------------------------
prob <- predict(mhlm2, newdata=hour_court, type="response")
prob <- round(prob,3)
prob
#1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20    21    22    23    24    25    26    27    28 
#0.742 0.764 0.787 0.810 0.734 0.756 0.779 0.802 0.725 0.748 0.771 0.793 0.717 0.740 0.762 0.785 0.808 0.709 0.732 0.754 0.777 0.800 0.701 0.723 0.746 0.769 0.692 0.715 
#29    30    31    32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49    50 
#0.738 0.760 0.684 0.707 0.730 0.752 0.676 0.699 0.721 0.744 0.668 0.690 0.713 0.736 0.659 0.682 0.705 0.728 0.651 0.674 0.697 0.643 
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
#################################

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
#-0.088829 -0.031526 -0.002019  0.015414  0.119899 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.764199   0.047740  16.008 2.64e-15 ***
#  court        0.021412   0.008314   2.576   0.0158 *  
#  hour        -0.006046   0.002863  -2.112   0.0441 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.05204 on 27 degrees of freedom
#Multiple R-squared:  0.347,	Adjusted R-squared:  0.2987 
#F-statistic: 7.175 on 2 and 27 DF,  p-value: 0.003169


# The predicted vs. residual plot  
plot(lm2$fitted, lm2$residuals)


# Using the trained model to predict
predict2 <- predict(lm2, newdata=testhc) 

cor(predict2, testhc$match_win_perce)
#[1] 0.8767765


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
#[1] 5281 2302
vip::vip(rf)
par(mfrow = c(2,1))
plot(hour_court$match_win_perce, hour_court$hour)
plot(hour_court$match_win_perce, as.factor(hour_court$court))

#Hour seems to be the best variable to predict if a top 8 team will win. In future work 
#coorlation of two plots shows Hour as a greater predictor
