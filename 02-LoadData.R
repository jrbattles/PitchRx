## scrape 2016 game data and store in the database
library(pitchRx)
scrape(start = "2016-04-03", end = "2016-11-02", suffix = "inning/inning_all.xml", connect = my_db1$con)


# To speed up execution time, create an index on these three fields.
library("dbConnect", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

dbSendQuery(my_db1$con, "CREATE INDEX url_atbat ON atbat(url)") 
dbSendQuery(my_db1$con, "CREATE INDEX url_pitch ON pitch(url)")
dbSendQuery(my_db1$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_db1$con, "CREATE INDEX des_index ON pitch(des)")
