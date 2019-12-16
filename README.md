#                       FIVB Beach Volleyball Historic Top 8 Teams Analysis



![Image of Yaktocat](https://www.sportface.it/wp-content/uploads/2018/05/FIVB-Beach-Volley-World-Tour-Logo.jpg)
## Project Description
This is a repositiory for my final project in my UCLA Extension class, Introduction to Data Science.
For this project I web scraped the FIVB Beach Volleyball schedule/results and rankings from 2001 - 2019. The goal of this project is to  expand current data wrangling, EDA, and visualization skills, and to build a database of historical performances that can be used for future research.

## Source of the Data Set
The schedule data was web scraped from 753 tournament web pages from [the old FIVB](http://www.fivb.org/EN/BeachVolleyball/Competitions/Competitions.htm), [the current FIVB](https://www.fivb.org/EN/BeachVolleyball/calendar.asp) and [FIVB V.I.S.](http://www.fivb.org/visasp/JS_BMatchList.aspx?TournCode=MSTA2009&Phase=2) public pages.

The end of year ranking was taken on Oct. 1 at the end of each year from 2001 - 2019, using [FIVB ranking pages](http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayW&Date=20191001).

Originally, I was going to use BigTimeStats [dataset](https://github.com/BigTimeStats/beach-volleyball/tree/master/data), however after investigating naming conventions, I found it difficult to add an end of the year ranking column matching player names. In the end I had to doctor the naming convention in order to match correct ranking and schedule player names. Below is an example of one row of the final dataset:

player|hour|team|team_country|tourn_rank|tourn|date|phase|year|match_no|court|time|team_rank|gender|opp|opp_team_rank|total_matches|match_won|match_lost|match_win|total_set_threes|total_sets_played|rank
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
a-kim jihee"|16|"a-kim jihee/choi dan"|"KOR"|149.47|"w13avckr"|2013-07-04|"Pool A"|2013|17|1|"16:10"|150|"w"|"cavestro/tonon"|260|1|0|1|0|0|2|150

 ## Contents
 
|File|Description|
|-|-|
|FINAL_PROJECT_CODE.R|Code used for the final portion used in R|
|final_df.csv|Finalalized CSV file from webscrape of scheudle/results|
|Report.pdf|Report about hypothesis|
|Tech_Report.pdf|Report about code and EDA|

### Tools
- R for final code and reports
- Python for web scrapping
- Excel/Google sheets for manual doctor edits of naming convetions

## Author
Tyler Widdison





