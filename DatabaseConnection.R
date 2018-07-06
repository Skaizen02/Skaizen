library(RMySQL)

#Connection to the database
con <- dbConnect(MySQL(), user="skaizen2", password="kzm38gw35f", dbname="webcrawler", host="localhost")

#Prepare dataframe for database write
df$id <- seq.int(length(df$title))

tablename <- "webcrawler"

#Read database table
df <- dbReadTable(con, tablename)

#Overwrite table
dbWriteTable(conn=con, name=tablename, value=df, row.names = FALSE, overwrite = TRUE)

#Append to table
dbWriteTable(conn=con, name=table, value=df, row.names = TRUE, append = TRUE)