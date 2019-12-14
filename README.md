#                       FIVB Beach Volleyball Historic Top 8 Teams Analysis



![Image of Yaktocat](https://www.sportface.it/wp-content/uploads/2018/05/FIVB-Beach-Volley-World-Tour-Logo.jpg)
## Project Description
This is a repositiory for my final project in my UCLA Extension class, Introduction to Data Science.
For this project I web scrapped the FIVB Beach Volleyball schedule/results and rankings from 2001 - 2019. To gain additional data wrangling, EDA and visualization skills and build a database of historical performances that can be used for future research.

## Source of the Data Set
The schedule data was web scrapped from 753 tournament web pages from [old fivb](http://www.fivb.org/EN/BeachVolleyball/Competitions/Competitions.htm), [new fivb](https://www.fivb.org/EN/BeachVolleyball/calendar.asp) and [fivb vis](https://www.fivb.org/visasp/JS_BMatchList.aspx?TournCode=&Phase=2) public pages.

The end of year ranking was taken on Oct 1 at the end of each year from 2001 - 2019 using [FIVB ranking pages](http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayW&Date=20191001).

Originally I was going to use BigTimeStats [dataset](https://github.com/BigTimeStats/beach-volleyball/tree/master/data), however after investigating naming convetions for players I found it difficult to add a end of the year ranking column matching player names. In the end I had to doctor the naming convention in order to match correct ranking and schedule player names. The final dataset looked like this (each row representing one match):

|no|date|time|court|result|duration|tourn|year|phase|team_a|team_a_country|team_b|team_b_country|winning_country|team_a_rank|player_1_team_a|player_2_team_a|team_b_rank|player_1_team_b|player_2_team_b|gender|team_a_sets_won|team_b_sets_won|team_a_game_one_points|team_b_game_one_points|team_a_game_two_points|team_b_game_two_points|team_a_game_three_points|team_b_game_three_points|tourn_rank
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
|35|2016-08-25|16:00|2|2-1 (23-21, 19-21, 15-7)|0:45:00|wlob2016|2016|Pool B|larissa/talita|bra|van der vlist/van gestel|ned|bra|3|larissa|talita|25|van der vlist|van gestel|w|2|1|23|21|10|21|15|7|43.03|

 ## Contents
 
|File|Description|
|-|-|
|finishes_main.csv|CSV file from webscrape of rankings|
|fivb_2001_to_2019_early_nov_results.csv|CSV file from webscrape of scheudle/results|
|Report.pdf|Report about findings|
|DS_Report.pdf|Report about code|

### Tools
- R for data science means
- Python for web scrapping
- Excel/Google sheets for manual doctor edits

### Future works
- Adding additional columns of height, weight, birthdate, fivb player number, and appending .dvw scouted matches data.
- Shiny apps for quick comparisons of players, teams, or countries

## Author
Tyler Widdison





