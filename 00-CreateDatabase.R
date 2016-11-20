## Use dplyer to create SQLite database
library(dplyr)
my_db1 <- src_sqlite("pitchRx.sqlite3", create = TRUE)

#confirm empty
my_db1
