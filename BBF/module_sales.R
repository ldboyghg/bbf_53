
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 活动评估  这里先做一个根据商品查询客户购买订单导出
# 根据商品通用名查询
marketingEvaluationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('dt'), '成单时间', start = Sys.Date()-8, end = Sys.Date()-1)),
             column(3, textInput(ns('txt'), '商品名(可从excel黏贴或用空格隔开)', value = '卡马西平片')),
             #column(3, textInput(ns('txt1'), '商品pid(可从excel黏贴或用空格隔开)', value = '1251860')),
             column(2, downloadButton(ns('btn_export'), '导出购买客户清单表', class = 'btn_nowrap'))

    ),
    fluidRow(
      DT::dataTableOutput(ns('tbl'))
    )
  )
}
marketingEvaluation <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  txt <- reactive({
    req(input$txt)
    stringr::str_replace_all(input$txt, " ", "','")
  })
  # txt1 <- reactive({
  #   req(input$txt1)
  #   stringr::str_replace_all(input$txt1, " ", "','")
  # })
  df <- reactive({
    dbGetQuery0('e_member', paste0("
      SELECT
           t.mid,date(post_date_str) as order_time,  concat('''',t.orderno) orderno  ,t.linkman, t.name,t.mobile
           FROM
           tb_porder t
            JOIN tb_porder_line b ON t.oid = b.oid
           WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
               (
               PAY_STATUS = 1
               AND date(payDates) >= '",dt()[1],"'  
               AND date(payDates) <= '",dt()[2],"'
               )
               OR (
               date(POST_DATE_STR) >= '",dt()[1],"'  
               AND date(POST_DATE_STR) <= '",dt()[2],"'
               AND PAY_TYPE = 10002
               )
             )
          AND  b.name  in ('",txt(),"') 
              
            
     "))%>% 
      rename(`会员编号`=mid, `下单时间`=order_time,`订单号`=orderno,`下单人`=linkman, `收货人`=name,`收货人电话`=mobile)
  })
  # use " " to seperate 
  # and t.mid in ('",txt(),"') 
  # or b.pid in ('",txt1(),"')   txt1 或者的选项还是不行。
  # and b.proName in ('')  通用名
  # and t.merchant_name in ('')  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf_customer_list',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
  })
  
  output$tbl <- DT::renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      extensions = list(Scroller=list()),
       options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '50px', targets = c(1))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=100,
         scrollX=TRUE,
         scrollCollapse = TRUE,
      #   order = list(list(10, 'desc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
         pageLength = 19
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
       )
    )
  })
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ## ------------------------地域销售概况-------------------
# salesMapUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(class='toolbar',
#              column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
#              #column(2, selectInput(ns('field'), '指标', choices = c('订单数','客户数','销售额','销售量') ))
#              column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ))
#     ),
#     fluidRow(
#       column(3,dataTableOutput(ns('tbl'))) ,
#       column(9,eChartOutput(ns('map') ) )
#     )
# 
#   )
# }
# salesMap <- function(input, output, session) {
# 
#   #付款日期值获取
#   pay_date <- reactive({
#     req(input$pay_date)
#     input$pay_date
#   })
# 
# 
#   field <- reactive({
#     req(input$field)
#     input$field
#   })
# 
#   #and date(dt) >= '",pay_date()[1],"'
#   #and date(dt) <= '",pay_date()[2],"'
# 
# 
#   df <- reactive({
#     sql <-
#       paste0("
#              select
#              x.PROVINCE_NAME province ,
#              ifnull(y.oids ,0) oids,
#              ifnull(y.mids ,0) mids,
#              ifnull(y.amount ,0) amount,
#              ifnull(y.quantity ,0) quantity
#              from bbf_shiny.tb_param_PROVINCE_NAME_short x
#              left join (
#              SELECT
#              t.PROVINCE_NAME ,
#              count(distinct t.oid) oids ,
#              count(distinct t.mid) mids ,
#              round(sum(b.amount),0) amount ,
#              sum(b.quantity) quantity
#              FROM
#              (select oid , mid ,
#              replace(replace(
#              case when PROVINCE_NAME like '%内蒙古%' then '内蒙'
#              when PROVINCE_NAME like '%宁夏%' then '宁夏'
#              when PROVINCE_NAME like '%广西%' then '广西'
#              when PROVINCE_NAME like '%西藏%' then '西藏'
#              when PROVINCE_NAME like '%新疆%' then '新疆'
#              else PROVINCE_NAME end ,'省',''),'市','')
#              PROVINCE_NAME ,
#              PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
#              ecommerce.tb_porder )t
#              JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
#              WHERE
#              1 = 1
#              AND (
#              (
#              PAY_STATUS = 1
#              AND date(payDates) >= '",pay_date()[1],"'
#              AND date(payDates) <= '",pay_date()[2],"'
#              )
#              OR (
#              HANDLE_STATUS NOT IN (5, 7)
#              AND PAY_TYPE = 10002
#              AND date(POST_DATE_STR) >= '",pay_date()[1],"'
#              AND date(POST_DATE_STR) <= '",pay_date()[2],"'
#              )
#              )
#              group by t.PROVINCE_NAME )  y
#              on x.PROVINCE_NAME = y.PROVINCE_NAME
#              ")
#     dd <- dbGetQuery0('ecommerce',sql)
# 
#     df <- dd %>% select(province,oids,mids,amount,quantity)
#     #df <- dd %>% rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
#     #df <- dd %>% rename(NAME=province)
#     df
#   })
# 
# 
# 
#   output$tbl <- renderDataTable({
#     datatable(
#       #df() %>% rename(`省份`=province),
#       df <- df() %>% rename(`省份`=province,`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity),
#       escape = FALSE,
#       rownames = FALSE,
#       selection = 'multiple',
#       #filter = 'top',
#       extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
#       options = list(
#         searching=FALSE,
#         lengthChange = TRUE
#       )
#     )
#   })
# 
# 
#   output$map <- renderEChart({
# 
#     #print(field())
#     #eMap(df(), namevar=~province, datavar = ~oids )
#     eMap(df(), namevar=~province, datavar = ~get(field()) )
#    
#     #mapData <- data.frame(province=c("上海", "江苏", "广东", "黑龙江"), val1=c(100, 200, 300, 500), val2=c(200,300,400,200), val3=c(1,2,3,5), stringsAsFactors=F)
#     #eMap(mapData, namevar=~province, datavar = ~val1+val2)
# 
# 
#   })
# 
# }




# #------------------------地域销售概况---rechart3----------------
salesMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
             #column(2, selectInput(ns('field'), '指标', choices = c('订单数','客户数','销售额','销售量') ))
             column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ))
    ),
    fluidRow(
      column(4,dataTableOutput(ns('tbl'))) ,
      column(8,plotOutput(ns('map')))
      #)
    )

  )
}
salesMap <- function(input, output, session) {

  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  })


  field <- reactive({
    req(input$field)
    input$field
  })

  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'


  df <- reactive({
    sql <-
      paste0("
             select x.PROVINCE_NAME province_name ,
             ifnull(y.oids ,0) oids,
             ifnull(y.mids ,0) mids,
             ifnull(y.amount ,0) amount,
             ifnull(y.quantity ,0) quantity
             from bbf_shiny.tb_param_PROVINCE_NAME x
             left join (
             SELECT
             t.PROVINCE_NAME ,
             count(distinct t.oid) oids ,
             count(distinct t.mid) mids ,
             round(sum(b.amount),0) amount ,
             sum(b.quantity) quantity
             FROM
             (select oid , mid ,
             case when PROVINCE_NAME like '%内蒙古%' then '内蒙古自治区'
             when PROVINCE_NAME like '%宁夏%' then '宁夏回族自治区'
             when PROVINCE_NAME like '%广西%' then '广西壮族自治区'
             when PROVINCE_NAME like '%西藏%' then '西藏自治区'
             when PROVINCE_NAME like '%新疆%' then '新疆维吾尔自治区'
             when PROVINCE_NAME like '%青海%' then '青海省'
             when PROVINCE_NAME like '%青海%' then '青海省'
             else PROVINCE_NAME end PROVINCE_NAME ,PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
             ecommerce.tb_porder )t
             JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR ( PAY_TYPE = 10002
             AND date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             )
             )
             group by t.PROVINCE_NAME )  y
             on x.PROVINCE_NAME = y.PROVINCE_NAME
             ")
    dd <- dbGetQuery0('ecommerce',sql)

    #df <- dd %>% rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
    df <- dd %>% rename(NAME=province_name)

    df
  })



  output$tbl <- renderDataTable({
    datatable(
      #df() %>% rename(`省份`=NAME),
      df <- df() %>% rename(`省份`=NAME,`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )
  })


  output$map <- renderPlot({
    print(field())

    china_map<-readShapePoly("/home/crm/BBF/data/bou2_4p.shp")
    x<-china_map@data
    xs<-data.frame(x,id=seq(0:924)-1)
    china_map1<-fortify(china_map) #转化为数据框

    china_mapdata<-join(china_map1, xs, type = "full") #合并两个数据框

    china_mapdata$NAME <-iconv(china_mapdata$NAME,"GBK","UTF-8")

    china_pop<-join(china_mapdata, df(), type = "full")

    ggplot(china_pop, aes(x = long, y = lat, group = group, fill= get(field())  ))+
    geom_polygon( ) +  geom_path(colour = "grey40")

  })

}













# #------------------------地域销售概况---rechart3----------------
salesMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
             #column(2, selectInput(ns('field'), '指标', choices = c('订单数','客户数','销售额','销售量') ))
             column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ))
    ),
    fluidRow(
      column(4,dataTableOutput(ns('tbl'))) ,
      column(8,plotOutput(ns('map')))
      #)
    )
    
  )
}
salesMap <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  })
  
  
  field <- reactive({
    req(input$field)
    input$field
  })
  
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0("
             select x.PROVINCE_NAME province_name ,
             ifnull(y.oids ,0) oids,
             ifnull(y.mids ,0) mids,
             ifnull(y.amount ,0) amount,
             ifnull(y.quantity ,0) quantity
             from bbf_shiny.tb_param_PROVINCE_NAME x
             left join (
             SELECT
             t.PROVINCE_NAME ,
             count(distinct t.oid) oids ,
             count(distinct t.mid) mids ,
             round(sum(b.amount),0) amount ,
             sum(b.quantity) quantity
             FROM
             (select oid , mid ,
             case when PROVINCE_NAME like '%内蒙古%' then '内蒙古自治区'
             when PROVINCE_NAME like '%宁夏%' then '宁夏回族自治区'
             when PROVINCE_NAME like '%广西%' then '广西壮族自治区'
             when PROVINCE_NAME like '%西藏%' then '西藏自治区'
             when PROVINCE_NAME like '%新疆%' then '新疆维吾尔自治区'
             when PROVINCE_NAME like '%青海%' then '青海省'
             when PROVINCE_NAME like '%青海%' then '青海省'
             else PROVINCE_NAME end PROVINCE_NAME ,PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
             ecommerce.tb_porder )t
             JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR ( PAY_TYPE = 10002
             AND date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             )
             )
             group by t.PROVINCE_NAME )  y
             on x.PROVINCE_NAME = y.PROVINCE_NAME
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    #df <- dd %>% rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
    df <- dd %>% rename(NAME=province_name)
    
    df
  })
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      #df() %>% rename(`省份`=NAME),
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'multiple',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE
      )
    )
  })
  
  
  output$map <- renderPlot({
    
  })
  
}