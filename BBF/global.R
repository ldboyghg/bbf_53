#
# global.R
# source by app.R
#
# host <-  '192.168.0.49'
# user <- 'crm'
# pwd <- 'crm2016@root'


host <-  '192.168.0.53'
user <- 'root'
pwd <- 'dcbicc106'
drv_mysql <- RMySQL::MySQL(max.con=200, fetch.default.rec=1000)

# db:bbf_shiny/ecommerce
dbGetQuery0 <- function(db, sql){
  con <- dbConnect(drv_mysql, host=host, user=user, password=pwd, dbname=db)
  dbSendQuery(con,'SET NAMES utf8')
  df <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(df)
}



# library(RMySQL)
# 
# sqlQuery <- function (query) {
#   # creating DB connection object with RMysql package
#   DB <- dbConnect(MySQL(), user="youruser", password='yourpassword', dbname='yourdb', host='192.168.178.1')
#   # send Query to btain result set
#   rs <- dbSendQuery(DB, query)
#   # get elements from result sets and convert to dataframe
#   result <- fetch(rs, -1)
#   # close db connection
#   dbDisconnect(DB)
#   # return the dataframe
#   return(result)
# }



dbWriteTable0 <- function(db, tbl, df) {
  con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd, dbname=db)
  dbWriteTable(con, tbl, df, append = TRUE, row.names = FALSE)
  dbDisconnect(con)
}

dbReadTable0 <- function(db, tbl) {
  con <- dbConnect(RMySQL::MySQL(), host=host, user=user, password=pwd, dbname=db)
  dbSendQuery(con,'SET NAMES utf8')
  df <- dbReadTable(con, tbl)
  dbDisconnect(con)
  return(df)
}


#####################Oracle##################

host1 <-  '192.168.0.51'
user1 <- 'C##_PUBLIC'
pwd1 <- 'pub800pharm'
drv2<-JDBC("oracle.jdbc.OracleDriver","/home/crm/BBF/ojdbc6.jar")


dbGetQuery1 <- function(sql){
  conn2<-dbConnect(drv2,"jdbc:oracle:thin:@192.168.0.51:1521:emember",user1,pwd1)
  df <- dbGetQuery(conn2,sql)
  dbDisconnect(conn2)
  return(df)
}


