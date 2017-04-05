## Use dplyer to create SQLite database
install.packages("dplyr")
library(dplyr)
install.packages("RSQLite")
my_db1 <- src_sqlite("pitchRx.sqlite3", create = TRUE)

#confirm empty
my_db1
