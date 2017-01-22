library(DBI)
host <-  '192.168.0.53'
user <- 'root'
pwd <- 'dcbicc106'

con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd)

result = dbGetQuery(con, "show processlist")

result <- result[!is.na(result$Id),]
#View(result)
for (i in 1:length(result)) {
  Command <- result[i,]$Command
  Time <- result[i,]$Time
  Host <- result[i,]$Host
  id <- result[i,]$Id
  if (Command == 'Sleep' && Time > 120 && Host != 'localhost' )
  {  print(paste("killed process ", id ))
    dbGetQuery(con, paste0("kill ", id) )
  }
}

dbDisconnect(con)

#source('/home/crm/BBF/mysql_connects_monitoring.R')