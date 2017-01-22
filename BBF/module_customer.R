#------------------------客单价区间-------------------
customerArpuUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(2)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
customerArpu <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  
  # df <- reactive({
  #   sql <-
  #     paste0("
  #            select * from (
  #            select  case
  #            when sum_price >= 0 and sum_price <= 100 then
  #            'A 0-100'
  #            when sum_price > 100 and sum_price <= 200 then
  #            'B 100-200'
  #            when sum_price > 200 and sum_price <= 300 then
  #            'C 200-300'
  #            when sum_price > 300 and sum_price <= 500 then
  #            'D 300-500'
  #            when sum_price > 500 and sum_price <= 1000 then
  #            'E 500-1000'
  #            when sum_price > 1000 then
  #            'F 1000+'
  #            end  price_type,
  #            count(oid) oids
  #            from (select case
  #            when PAY_STATUS = 1 then
  #            payDates
  #            when HANDLE_STATUS not in (5, 7) and PAY_TYPE = 10002 then
  #            POST_DATE_STR
  #            end dt ， oid,
  #            sum_price
  #            from tb_porder t
  #            where 1 = 1
  #            and (PAY_STATUS = 1 or
  #            (HANDLE_STATUS not in (5, 7) and PAY_TYPE = 10002)))
  #            where 1 = 1
  #            and to_char(dt, 'yyyymmdd') between '",format(pay_date()[1],'%Y%m%d') ,"' and '", format(pay_date()[2],'%Y%m%d'),"'
  #            group by case
  #            when sum_price >= 0 and sum_price <= 100 then
  #            'A 0-100'
  #            when sum_price > 100 and sum_price <= 200 then
  #            'B 100-200'
  #            when sum_price > 200 and sum_price <= 300 then
  #            'C 200-300'
  #            when sum_price > 300 and sum_price <= 500 then
  #            'D 300-500'
  #            when sum_price > 500 and sum_price <= 1000 then
  #            'E 500-1000'
  #            when sum_price > 1000 then
  #            'F 1000+'
  #            end
  #            ) order by 1 ")
  # 
  #   df <- dbGetQuery1(sql)
  # 
  # })
  
  
  
  
  
  df <- reactive({
    sql <-
      paste0("
             select * from (
             select  case
             when sum_price >= 0 and sum_price <= 100 then
             'A 0-100'
             when sum_price > 100 and sum_price <= 200 then
             'B 100-200'
             when sum_price > 200 and sum_price <= 300 then
             'C 200-300'
             when sum_price > 300 and sum_price <= 500 then
             'D 300-500'
             when sum_price > 500 and sum_price <= 1000 then
             'E 500-1000'
             when sum_price > 1000 then
             'F 1000+'
             end  price_type,
             count(oid) oids
             from (select case
             when PAY_STATUS = 1 then
             payDates
             when  PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt , oid,
             sum_price
             from tb_porder t
             where 1 = 1
             and HANDLE_STATUS not in (5, 7) 
             and (PAY_STATUS = 1 or PAY_TYPE = 10002)) y
             where 1 = 1
             and date(dt) >= '",pay_date()[1],"'
             and date(dt) <= '",pay_date()[2],"'
             group by case
             when sum_price >= 0 and sum_price <= 100 then
             'A 0-100'
             when sum_price > 100 and sum_price <= 200 then
             'B 100-200'
             when sum_price > 200 and sum_price <= 300 then
             'C 200-300'
             when sum_price > 300 and sum_price <= 500 then
             'D 300-500'
             when sum_price > 500 and sum_price <= 1000 then
             'E 500-1000'
             when sum_price > 1000 then
             'F 1000+'
             end
             ) x order by 1 ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      sum_oids = sum(dd[,2]),
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      oid_rat = dd[,2]/sum(dd[,2])
    ) %>%  select(-sum_oids) %>% rename(`价格区间`=price_type , `订单数`=oids,`订单占比`=oid_rat)
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
      # options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '200px', targets = c(0))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=200,
      #   scrollX=TRUE,
      #   scrollCollapse = TRUE,
      #   order = list(list(0, 'asc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
      #   pageLength = 200,
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      # )
    )%>%  # datatable end
      formatPercentage(c('订单占比'), 2)%>%
      formatStyle(
        '订单数',
        background = styleColorBar(df()$订单数, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        '价格区间',
        transform = 'rotateX(0deg) rotateY(0deg) rotateZ(0deg)',
        backgroundColor = styleEqual(
          unique(df()$价格区间), c('lightblue', 'lightgreen', 'lightpink','blue','pink','green')
        )
      )  %>%
      formatStyle(
        '订单占比',
        color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = styleInterval(3.4, c('green', 'yellow'))
      )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rfm table  rfm分布
# rfmTableUI <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     HTML('
#          <style>
#          .table {
#          width: 100% !important;
#          }
#          .table > tbody > tr > td:nth-child(1) {
#          text-align: left;
#          }
#          .table > tbody > tr > th {
#          text-align:right !important;
#          }
#          </style>'
#     ),
#     titlePanel('RFM'),
#     helpText(paste0('订单范围：2015-01-01至', as.Date(file.mtime('./data/rfm.rds')), ', 计算时间：', file.mtime('./data/rfm.rds'), ', 每月月初重新计算,计算比较费时请耐心等待')),
#     helpText(textOutput(ns('desc_text'))),
#     fluidRow(
#       column(3, h5('RFM分布'), DT::dataTableOutput(ns('rfm_total'))),
#       column(3, offset = 1, h5('全部占比%'), DT::dataTableOutput(ns('rfm_total_pct'))),
#       column(5)
#       # column(3, h5('F比例分布%'), DT::dataTableOutput(ns('rfm_row_pct'))),
#       # column(3, h5('R比例分布%'), DT::dataTableOutput(ns('rfm_col_pct')))
#     )
#     
#     )
#   
#   }
# rfmTable <- function(input, output, session) {
#   rfm <- reactive({
#     # 缓存不存在或者距离上次更新超过30天则会触发缓存更新
#     if (as.numeric(Sys.Date() - as.Date(file.info('./data/rfm.rds')$mtime)) >= 30 | !file.exists('./data/rfm.rds')){
#       x <- dbGetQuery0('ecommerce', "select mid, substring(post_date_str, 1, 10) as dt, actual_pay as pay from tb_porder where post_date_str>='2015-01-01 00:00:00' and handle_status in (1, 4, 6, 99)")
#       r.interval = seq(0,360, by=30)
#       r.interval = c(0, 30, 90, 180, 360)
#       lbl = paste(sprintf('%02d', 1:length(r.interval)), paste(r.interval, c(r.interval[-1], 'inf'),sep = '-'), sep=':')
#       x <- x %>%
#         select(id=mid, r=dt, m=pay) %>%
#         mutate(r=as.Date(r)) %>% 
#         group_by(id, r) %>%  # 人店天合并
#         summarise(
#           m = sum(m)
#         ) %>% 
#         group_by(id) %>% 
#         summarise(
#           #r = as.numeric(Sys.Date() - max(r)),
#           r = as.numeric(Sys.Date() - max(r)),
#           f = n(),
#           m = sum(m),
#           label = cut(r, c(r.interval, Inf), labels=lbl)
#         ) %>% 
#         mutate(
#           f = ifelse(f>4, 'F5+', paste0('F', f))
#         )
#       saveRDS(x, './data/rfm.rds')
#       session$reload()
#     } else {
#       x <- read_rds('./data/rfm.rds')
#     }
#     x
#   })
#   
#   output$desc_text <- renderText({
#     rfm <- colSums(reshape2::acast(rfm(), label~f))
#     
#     paste0('1次到2次的重复购买率',round(sum(rfm[2:5])/sum(rfm)*100, 2), '%，2次到3次的重复购买率',round(sum(rfm[3:5])/sum(rfm[2:5])*100, 2), '%，3次到4次的重复购买率',round(sum(rfm[4:5])/sum(rfm[3:5])*100, 2), '%\n')
#   })
#   
#   output$rfm_total <- DT::renderDataTable({
#     df <- reshape2::acast(rfm(), label~f) %>% as.data.frame()
#     brks <- quantile(df, probs = seq(.05, .95, .037), na.rm = TRUE)
#     clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
#     
#     datatable(df,
#               escape = FALSE,
#               rownames = TRUE,
#               selection = 'none',
#               options = list(
#                 autoWidth = TRUE,
#                 searching = FALSE,
#                 deferRender = TRUE,
#                 scrollCollapse = TRUE,
#                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
#                 paging = FALSE,
#                 lengthChange = FALSE,
#                 bPaginate = FALSE,
#                 bFilter = FALSE,
#                 bInfo = FALSE,
#                 initComplete = JS("function(settings, json) {}"),
#                 fnDrawCallback = JS('function (oSettings, json) {}')
#               )
#     ) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
#   })
#   output$rfm_total_pct <- DT::renderDataTable({
#     df <- prop.table(reshape2::acast(rfm(), label~f)) %>% as.data.frame()
#     brks <- quantile(df, probs = seq(.05, .95, .037), na.rm = TRUE)
#     clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
#     
#     datatable(df,
#               escape = FALSE,
#               rownames = TRUE,
#               selection = 'none',
#               options = list(
#                 autoWidth = TRUE,
#                 searching = FALSE,
#                 deferRender = TRUE,
#                 scrollCollapse = TRUE,
#                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
#                 paging = FALSE,
#                 lengthChange = FALSE,
#                 bPaginate = FALSE,
#                 bFilter = FALSE,
#                 bInfo = FALSE,
#                 initComplete = JS("function(settings, json) {}"),
#                 fnDrawCallback = JS('function (oSettings, json) {}')
#               )
#     ) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs)) %>% formatPercentage(colnames(df), 2)
#   })
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------当天注册购买转化-------------------
registerBuyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(2)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}
registerBuy <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    sql <-
      paste0("
             select  x.dt,x.mids_register,y.mids_buy  from 
             ( select date(post_date_str) as dt,count(mid) as mids_register
             from ana_member
             where  1=1
             and date(post_date_str) >= '",pay_date()[1],"'
             and date(post_date_str) <= '",pay_date()[2],"'
             group by date(post_date_str)) x
             join 
             (
             select dt, count(mid) as mids_buy from 
             (  select a.mid,  date(a.dt) as dt from 
             (             select case
             when PAY_STATUS = 1 then
             payDates
             when  PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt, 
             oid,
             mid,
             sum_price,
             PAY_STATUS
             from tb_porder
             where 1 = 1
             and HANDLE_STATUS not in (5, 7)
             and (PAY_STATUS = 1 or  PAY_TYPE = 10002)
             ) a
             join 
             ana_member b 
             on a.mid=b.mid
             where   
             1=1
             and date(a.dt) >= '",pay_date()[1],"'
             and date(a.dt) <= '",pay_date()[2],"'
             and  date(b.post_date_str)=date(a.dt)
             
             ) c
             group by dt
             ) y
             on x.dt=y.dt ")
    
    
    
    dd <- dbGetQuery0('e_member',sql)
    
    df <- dd %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      buy_rate = dd[,3]/dd[,2]
    ) %>% rename(`日期`=dt , `注册人数`=mids_register,`购买人数`=mids_buy,`注册用户购买转化率`=buy_rate)
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = TRUE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
      # options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '200px', targets = c(0))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=200,
      #   scrollX=TRUE,
      #   scrollCollapse = TRUE,
      #   order = list(list(0, 'asc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
      #   pageLength = 200,
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      # 
    )%>%  # datatable end
      formatPercentage(c('注册用户购买转化率'), 2)
    #)%>%  # datatable end
    # formatPercentage(c('订单占比'), 2)%>%
    # formatStyle(
    #   '订单数',
    #   background = styleColorBar(df()$订单数, 'steelblue'),
    #   backgroundSize = '100% 90%',
    #   backgroundRepeat = 'no-repeat',
    #   backgroundPosition = 'center'
    # ) %>%
    # formatStyle(
    #   '价格区间',
    #   transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    #   backgroundColor = styleEqual(
    #     unique(df()$价格区间), c('lightblue', 'lightgreen', 'lightpink','blue','pink','green')
    #   )
    # )  %>%
    # formatStyle(
    #   '订单占比',
    #   color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    #   backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
    # )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 客单价趋势
kdUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             column(3, dateRangeInput(ns('dt'), '购买时间', Sys.Date()-90, Sys.Date()-1)),
             column(3, radioButtons(ns('freq'), '频率', choices = c('按月'='month','按周'='week', '按天'='day'), inline = TRUE))
    ),
    fluidRow(
      column(12, plotlyOutput(ns('p')))
    )
  )
}
kd <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  freq <- reactive({
    req(input$freq)
    input$freq
  })
  df <- reactive({
    if(freq()=='month') {
      prefix_sql <- 'substring(dt,1,7)'
    } else if(freq()=='week') {
      prefix_sql <- 'weekofyear(dt)'
    } else if(freq()=='day') {
      prefix_sql <- 'substring(dt,1,10)'
    } else {
      prefix_sql <- 'substring(dt,1,7)'
    }
    sql <- paste0("
            select " ,prefix_sql, " as dt1, sum(SUM_PRICE) /count(DISTINCT oid) AS m
            FROM
                  (
                  select case
                  when PAY_STATUS = 1 then  date(payDates)
                  when PAY_TYPE = 10002 then date(POST_DATE_STR)
                  end dt , oid,
                  sum_price
                  from tb_porder 
                  where 1 = 1
                  and HANDLE_STATUS not in (5, 7) 
                  and (PAY_STATUS = 1 or PAY_TYPE = 10002)
                  ) a
                  WHERE 1=1
                  #and date(dt) >= '20161001'
                  #and date(dt) <= '20161031'
                  AND	date(dt) >= '",dt()[1],"'
                  AND date(dt) <= '",dt()[2],"'
                  GROUP BY
                  dt1 
                  ")
    x <- dbGetQuery0('e_member', sql) %>% mutate(dt = as.factor(dt1))
    if(freq()=='day') {
      x$dt <- as.Date(x$dt)
    }
    x
  })
  
  output$p <- renderPlotly({
    g <- df() %>% 
      ggplot(aes(dt, m))+geom_bar(aes(fill=3), stat='identity')+labs(x='客单价趋势')+theme(legend.position='none')
    #ggplot()+geom_point(aes(dt, m, color=2))+geom_smooth()+labs(x='客单价趋势')+theme(legend.position='none')
    ggplotly(g)
  })
}


#------------------------购买次数汇总-------------------
customerBuyCntsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-months(2)-1, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    )
  )
}


customerBuyCnts <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  
  df <- reactive({
    sql <-
      paste0(" 
             select case when oids = 1 then 'A 购买1次'
             when oids = 2 then 'B 购买2次'
             when oids = 3 then 'C 购买3次'
             when oids = 4 then 'D 购买4次'
             when oids = 5 then 'E 购买5次'
             when oids = 6 then 'F 购买6次'  
             when oids = 7 then 'G 购买7次'
             when oids = 8 then 'H 购买8次'
             when oids = 9 then 'I 购买9次'
             when oids = 10 then 'J 购买10次'
             when oids = 11 then 'K 购买11次'
             when oids = 12 then 'L 购买12次'
             else 'M 12次以上' end buy_cnts_type ,
             case when oids = 1 then '1'
             when oids = 2 then '2'
             when oids = 3 then '3'
             when oids = 4 then '4'
             when oids = 5 then '5'
             when oids = 6 then '6'  
             when oids = 7 then '7'
             when oids = 8 then '8'
             when oids = 9 then '9'
             when oids = 10 then '10'
             when oids = 11 then '11'
             when oids = 12 then '12'
             else '13' end buy_cnts ,
             count(mid) mids ,
             sum(amount) amount from (
             select mid , sum(sum_price) amount , count(distinct oid) oids  from (
             select a.mid , 
             case when a.PAY_STATUS = 1 then a.payDates when a.PAY_TYPE = 10002 then a.POST_DATE_STR end dt,
             a.sum_price,
             a.oid
             FROM
             tb_porder a
             left join tb_member b  on a.mid = b.mid 
             WHERE
             1 = 1
             and a.HANDLE_STATUS not in (5, 7 )
             and ( a.PAY_STATUS = 1 or  a.PAY_TYPE = 10002 )  
             and not EXISTS (select 'X' from tb_b_type_member d where a.mid = d.mid )  -- 排除掉  B类客户
             ) x
             where 1 = 1 
             and date(dt) >= '",pay_date()[1],"'
             and date(dt) <= '",pay_date()[2],"'
             group by mid  ) y
             group by 
             case when oids = 1 then 'A 购买1次'
             when oids = 2 then 'B 购买2次'
             when oids = 3 then 'C 购买3次'
             when oids = 4 then 'D 购买4次'
             when oids = 5 then 'E 购买5次'
             when oids = 6 then 'F 购买6次'  
             when oids = 7 then 'G 购买7次'
             when oids = 8 then 'H 购买8次'
             when oids = 9 then 'I 购买9次'
             when oids = 10 then 'J 购买10次'
             when oids = 11 then 'K 购买11次'
             when oids = 12 then 'L 购买12次'
             else 'M 12次以上' end ,case when oids = 1 then '1'
             when oids = 2 then '2'
             when oids = 3 then '3'
             when oids = 4 then '4'
             when oids = 5 then '5'
             when oids = 6 then '6'  
             when oids = 7 then '7'
             when oids = 8 then '8'
             when oids = 9 then '9'
             when oids = 10 then '10'
             when oids = 11 then '11'
             when oids = 12 then '12'
             else '13' end    
             ")
    
    dd <- dbGetQuery0('e_member',sql)
    #  df <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      sum_mids = sum(dd[,3]),
      sum_amount = sum(dd[,4]),
      #mid_rat = dd[,3]/sum(dd[,3])
      #/客户数占比
      mid_rat = ifelse(sum_mids==0  , 0 , dd[,3]/sum_mids),
      #/销售额占比
      amount_rat = ifelse(sum_amount==0 , 0 , dd[,4]/sum_amount),
      #/客户单价
      mid_per_price = ifelse(dd[,3] == 0 , 0 ,  dd[,4]/dd[,3] ) ,
      
      ##客单价字符转换
      
      #/客单价
      # mid_price =ifelse(buy_cnts==13 ,'/',as.character( round(ifelse(as.integer(dd[,2])== 0 , 0 , mid_per_price/as.integer(dd[,2]) )),2) )
      mid_price =ifelse(buy_cnts==13 ,'/',as.character( round(ifelse(as.integer(dd[,2])== 0 , 0 , mid_per_price/as.integer(dd[,2]) )),2) )
      
    ) %>%  select(-sum_mids,-buy_cnts ,-sum_amount) %>% rename(`购买次数`=buy_cnts_type , `客户数`=mids,`销售额`=amount,
                                                               `客户数占比`=mid_rat , `销售额占比`=amount_rat,`客户单价`=mid_per_price,`客单价`=mid_price)
  })
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
      # options = list(
      #   autoWidth = TRUE,
      #   columnDefs = list(list(width = '200px', targets = c(0))),
      #   searching=TRUE,
      #   deferRender=TRUE,
      #   scrollY=200,
      #   scrollX=TRUE,
      #   scrollCollapse = TRUE,
      #   order = list(list(0, 'asc')),
      #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
      #   pageLength = 200,
      #   lengthChange = FALSE,
      #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
      #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
      # )
    )%>%  # datatable end
      formatPercentage(c('客户数占比','销售额占比'), 2) %>% formatRound(c('客户单价'), 2)
  })
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------省份城市分析-------------------
provinceCityUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             box(
               title = '省份城市分析', status = 'primary', solidHeader = T, width = 12,
               column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-30, end = Sys.Date()-1))
             )
    ),
    fluidRow(
      column(6,dataTableOutput(ns('tbl'))),
      column(6,dataTableOutput(ns('tbl2')))
    )
  )
}
provinceCity <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    sql <-
      paste0("
             select province, count(DISTINCT oid) as count_orders,count(DISTINCT mid) as count_mids,sum(actual_pay) as sum_pay
from (
		select a.oid,a.mid,
				replace(REPLACE(REPLACE(a.province_name,CHAR(10),''),CHAR(13),''),char(9),'') as province,
				replace(REPLACE(REPLACE(a.city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city,
				a.actual_pay 
		from tb_porder a
		join (SELECT	CASE
										WHEN PAY_STATUS = 1 THEN	payDates
										WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
										END dt,
										 oid, mid, sum_price, PAY_STATUS
										FROM	tb_porder
										WHERE	1 = 1 
                    and HANDLE_STATUS NOT IN (5, 7) 
										AND (	PAY_STATUS = 1	OR PAY_TYPE = 10002)
				) b
		on a.oid=b.oid
		where 1=1
		and province_name is not NULL
		and city_name is not NULL
    #and   date(b.dt) between '20161024' and '20161024'
	  and date(dt) >= '",pay_date()[1],"'
	  and date(dt) <= '",pay_date()[2],"'
) c
where province is not NULL
group by province
order by sum_pay desc  ")
    
    dd <- dbGetQuery0('e_member',sql)
    
    df <- dd %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      sum_pay = round(sum_pay),
      oid_pay = round(dd[,4]/dd[,2])
    ) %>% rename(`省份`=province , `购买单数`=count_orders,`购买人数`=count_mids,`销售额`=sum_pay,`客单价`=oid_pay)
  }) 
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
      df(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '200px', targets = c(0))),
        searching=TRUE,
        #   deferRender=TRUE,
        #   scrollY=200,
        #   scrollX=TRUE,
        #   scrollCollapse = TRUE,
        #   order = list(list(0, 'asc')),
        #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 20,
        lengthChange = TRUE
        #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
        # 
      )
    ) #%>%  # datatable end
    #formatPercentage(c('注册用户购买转化率'), 2)
  })
  
  
  
  df1 <- reactive({
    sql <-
      paste0("
             select city, count(DISTINCT oid) as count_orders,count(DISTINCT mid) as count_mids,sum(actual_pay) as sum_pay
from (
		select a.oid,a.mid,
				replace(REPLACE(REPLACE(a.province_name,CHAR(10),''),CHAR(13),''),char(9),'') as province,
				replace(REPLACE(REPLACE(a.city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city,
				a.actual_pay 
		from tb_porder a
		join (SELECT	CASE
										WHEN PAY_STATUS = 1 THEN	payDates
										WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
										END dt,
										 oid, mid, sum_price, PAY_STATUS
										FROM	tb_porder
										WHERE	1 = 1
                    and HANDLE_STATUS NOT IN (5, 7)
										AND (PAY_STATUS = 1	OR  PAY_TYPE = 10002)		
				) b
		on a.oid=b.oid
		where 1=1
		and province_name is not NULL
		and city_name is not NULL
    #and   date(b.dt) between '20161024' and '20161024'
	  and date(dt) >= '",pay_date()[1],"'
	  and date(dt) <= '",pay_date()[2],"'
		
) c
where city is not NULL
group by city
order by sum_pay desc")
    dd1 <- dbGetQuery0('e_member',sql)
    
    df1 <- dd1 %>%mutate(
      #oid_rat = round( dd[,2]/sum(dd[,2]))
      sum_pay = round(sum_pay),
      oid_pay = round(dd1[,4]/dd1[,2])
    ) %>% rename(`城市`=city , `购买单数`=count_orders,`购买人数`=count_mids,`销售额`=sum_pay,`客单价`=oid_pay)
  }) 
  
  
  output$tbl2 <- renderDataTable({
    datatable(
      df1(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
      options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '200px', targets = c(0))),
        searching=TRUE,
        #   deferRender=TRUE,
        #   scrollY=200,
        #   scrollX=TRUE,
        #   scrollCollapse = TRUE,
        #   order = list(list(0, 'asc')),
        #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
        pageLength = 20,
        lengthChange = TRUE
        #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
        #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
        # 
      )
    ) 
  })
  
  
} 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RFM维度分布
RFMDistributionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(class='toolbar',
             box(
               title = 'RFM维度分布', status = 'primary', solidHeader = T, width = 12,
               column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-8, end = Sys.Date()-1))
             )
    ),
    fluidRow(
      box(title = 'R最近一次购买分布', status = 'primary', solidHeader = T, width = 12,
        column(6,plotlyOutput(ns('trend1'))),
        column(6,dataTableOutput(ns('r_distribution'))) 
        )
    ),
     fluidRow(
        box(title = 'F购买频次分布', status = 'primary', solidHeader = T, width = 12,
        column(6,plotlyOutput(ns('trend2'))),
        column(6,dataTableOutput(ns('f_distribution')))
        )
    ),
    fluidRow(
      box(title = 'M累计购买金额分布', status = 'primary', solidHeader = T, width = 12,
          column(6,plotlyOutput(ns('trend3'))),
          column(6,dataTableOutput(ns('M_distribution'))) 
      )
    )
    
    # fluidRow(
    #   box(column(12,dygraphOutput(ns('trend4'))))
    # )
  )
}


RFMDistribution <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  
  # 会员号 最后一次购买时间 订单数（购买次数）累计购买金额 客单价 ;r距离现在多少天放在r里面计算。datediff(Now(), dt) as r
  # tb_b_type_member是小b端客户, 排除小b客户与金额超过3万的订单。值要是从选择时间段以来所以购买的值【每个人要获取多次的】。
df <- reactive({
  sql<-paste0("
              select * from (   
              SELECT	mid,
              CASE
              WHEN PAY_STATUS = 1 THEN date(max(payDates))
              WHEN PAY_TYPE = 10002 THEN date(max(POST_DATE_STR))
              END dt_max,
              count(DISTINCT oid) AS oid_count,  
              sum(sum_price) as CUM_pay,
              sum(sum_price) / count(DISTINCT oid) AS oid_pay
              FROM	tb_porder
              WHERE	1 = 1
              and HANDLE_STATUS NOT IN (5, 7)
              and SUM_PRICE<30000
              AND (	PAY_STATUS = 1	OR PAY_TYPE = 10002)
              #AND date(post_date_str) > '20151101'
              #AND date(post_date_str) <= '20161031' 
              and mid not in (select mid from tb_b_type_member )
              group by mid
              ) x
              where 1=1
              #and date(dt_max) >= '2016-10-01' 
              #and date(dt_max) <= '2016-10-30'
              and date(dt_max)>='",dt()[1],"' 
              and date(dt_max)<='",dt()[2],"'
              ")
dbGetQuery0('e_member', sql) 
})
# 用于计算R，最近一次购买时间分布
df1 <- reactive({
  r <- df()
  r1<-r%>%
    mutate(rday=as.numeric(Sys.Date()-date(r$dt_max)))
  r_cut <-cut(r1$rday, c(0,30,60,90,120,180,270,360,Inf),dig.lab = 10)
  r1$r_cut = r_cut
  r2<-r1 %>% group_by(r_cut) %>% summarise(mids=n_distinct(mid), 
                                           CUM_pay=round(sum(CUM_pay)),
                                           oid_counts=round(sum(oid_count)),
                                           mean_r=round(mean(rday)))%>%
    mutate(meanmid_pay=round(CUM_pay/mids),
           meanoid_pay=round(CUM_pay/oid_counts))
  
  # 将列表转化成数据类型，并且过滤掉NA值
  r3<-as.data.frame(r2) 
  r3<-r3%>% rename(`日期`=r_cut , `人数`=mids,`累计金额数`=CUM_pay,`订单数`=oid_counts,
                                   `平均天数`=mean_r,`人均购买金额`=meanmid_pay,`客单价`=meanoid_pay)
    
  r4<-subset(r3,r3[,1]!='NA')
  r4
})


#这里的Sys.Date()要改成dt()[2]
output$trend1 <- renderPlotly({
		g <- df1() %>%
		  ggplot(aes(`日期`, `人数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='R客户最近一次购买时间分布')+theme(legend.position='none')
		ggplotly(g)
  })

output$r_distribution <- DT::renderDataTable({
  datatable(
    df1(), 
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
    )
  )
})


# F购买频次
# df2 <- reactive({
#   a <- df()
#   a1<-subset(a,a$oid_count<=quantile(a$oid_count,  probs = c(95)/100))
#   a1
# })
 #频次统计
df2_1 <- reactive({
  a <- df()
  a1<-subset(a,a$oid_count<=quantile(a$oid_count,  probs = c(95)/100))
  a2<-as.data.frame(table(a1$oid_count))
  a2<-a2%>% rename(`购买次数`=Var1 , `频次`=Freq)
  a2
})
output$f_distribution <- DT::renderDataTable({
  datatable(
    df2_1(),
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
      )
  )
})
# # output$trend2 <- renderPlotly({
# #   a1<-df2()
# #   plot(a1$oid_count)
# # })
output$trend2 <- renderPlotly({
  a1<-df2_1()
  g <- a1 %>%
  ggplot(aes(`购买次数`, `频次`))+geom_bar(aes(fill=3), stat='identity')+labs(x='R客户最近一次购买时间分布')+theme(legend.position='none')
ggplotly(g)
})

# M累计购买金额分布
df3 <- reactive({
  tmp <-df()
  tmp1<-tmp
  lbl <-cut(tmp$CUM_pay, c(0,200,1000,2000,5000,10000,50000,100000,Inf),dig.lab = 10)
  # cut(tmp$CUM_pay[1:20], c(0,50,100,200,300,400,500,600,800,1000,Inf),dig.lab = 10)
  tmp1$lbl = lbl
  tmp2<-tmp1 %>% group_by(lbl) %>% summarise(mids=n_distinct(mid), CUM_pay=sum(CUM_pay),oid_counts=sum(oid_count))%>%
    mutate(meanmid_pay=CUM_pay/mids,meanoid_pay=CUM_pay/oid_counts) 
  
  tmp3<-as.data.frame(tmp2)
  
  tmp3<-tmp3%>% rename(`金额区间`=lbl , `人数`=mids, `累计金额数`=round(CUM_pay),`订单数`=oid_counts,
                       `人均购买金额`=meanmid_pay,`客单价`=round(meanoid_pay))
  tmp3<-subset(tmp3,tmp3[,1]!='NA')
  tmp3
})
output$trend3 <- renderPlotly({
  g <- df3() %>%
    ggplot(aes(`金额区间`, `人数`))+geom_bar(aes(fill=3), stat='identity')+labs(x='M客户累计金额区间分布')+theme(legend.position='none')
  ggplotly(g)
})
output$M_distribution <- DT::renderDataTable({
  datatable(
    df3(), 
    rownames = FALSE,
    selection = 'multiple',
    extensions = list(Scroller=list()),
    options = list(
      searching=FALSE,
      lengthChange = TRUE
    )
  )
})

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------关联规则-------------------
rulesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-31, end = Sys.Date()-1))
    ),
    fluidRow(
      dataTableOutput(ns('tbl'))
    ),
    fluidRow(
      plotOutput(ns('rules1'))
    )
  )
}
rules <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  df <- reactive({
    sql <-
      paste0("
             select distinct a.mid,c.ctitle2
            from (
             SELECT	CASE
             WHEN PAY_STATUS = 1 THEN	payDates
             WHEN PAY_TYPE = 10002 THEN POST_DATE_STR
             END dt,
             oid, mid
             FROM	tb_porder
             WHERE	1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (	PAY_STATUS = 1	OR   PAY_TYPE = 10002) ) a
             join tb_porder_line b on a.oid=b.oid
             join tb_product_catalogbase c on b.pid=c.pid
             where date(a.dt)>'20161001'
             ")
dbGetQuery0('ecommerce',sql)
  }) 
# 数据处理
  df1 <- reactive({
    data1<-df()
    trans_data1<- as(split(data1[,"ctitle2"], data1[,"mid"]), "transactions")
    # a100=0.001
    rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    #inspectDT(rules)
   # as.data.frame(inspect(rules))
    inspect(rules)
  })
  df2 <- reactive({
    data1<-df()
    trans_data1<- as(split(data1[,"ctitle2"], data1[,"mid"]), "transactions")
    # a100=0.001
    rules<-apriori(trans_data1, parameter = list(minlen=1,supp=0.001, conf=0.1))
    rules
  })

  output$tbl <- renderDataTable({
    datatable(
      df1(),
      escape = FALSE,
      rownames = FALSE,
      selection = 'none',
      #filter = 'top',
      extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list())
    )
  })
  #renderPlotly  renderPlot
  output$rules1 <- renderPlot({
    # data(Groceries)
    # rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
    
    #plotly_arules(df2())
    
    # subrules<- head(sort(df2(), by="support"),100)
    #plotly_arules(df2(), measure=c("support", "lift"), shading="confidence")
    #plotly_arules(subrules, method="graph")
    # plot(subrules, method="graph")
    
    data(Groceries)
    rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
    subrules2 <- sample(rules, 10)
    plot(subrules2, method="graph")
    
    #plot(df2(), method="graph")
   
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





#------------------------会员等级分布-------------------
customerLevelUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = '会员等级分布', status = 'primary', solidHeader = T, width = 12,
      fluidRow(class='toolbar',
               column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1))
      ),
      fluidRow(
  
        # column(6,plotlyOutput(ns('level')))
          column(6,dataTableOutput(ns('tbl'))) 
        #)
      )
    )
  )
}
customerLevel <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
   
  
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0("
               select 
             ifnull(account_level,'未知') account_level ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount,
             sum(quantity) quantity,
             count(distinct x.oid) oids
             from (
             select case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt , t.oid, t.mid ,
             b.amount ,b.quantity
             from tb_porder t
             join tb_porder_line b
             on t.oid = b.oid 
             where 1 = 1
             and HANDLE_STATUS not in (5, 7) 
             -- and not exists(select 'X' from e_member.tb_b_type_member c where t.mid = c.mid )
             and (PAY_STATUS = 1 or  PAY_TYPE = 10002)
             ) x 
             left join (select mid , 
             CASE 
             WHEN IFNULL(SCORETOTAL,0) BETWEEN 0 and 2000 THEN
             'E 普通会员'
             WHEN IFNULL(SCORETOTAL,0) BETWEEN 2001 and  10000 THEN
             'D 铜牌会员'
             WHEN IFNULL(SCORETOTAL,0) BETWEEN 10001 and 50000 THEN
             'C 银牌会员'
             WHEN IFNULL(SCORETOTAL,0) BETWEEN 50001 and 100000 THEN
             'B 金牌会员'
             else	'A 钻石会员' END AS account_level from tb_member_score  ) y
             on x.mid = y.mid 
             where 1 = 1 
             and date(dt ) >= '",pay_date()[1],"'
             and date(dt ) <= '",pay_date()[2],"'
             group by  account_level 
              ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,5],2)
    ) %>% rename(`会员等级`=account_level , `购买人数`=mids,`销售额`=amount, 
                 `销售量`= quantity, `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-oids)
  }) 
   
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
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
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  # output$level <- renderPlotly({
  #   # g <- df3() %>%
  #   #   ggplot(aes(lbl, CUM_pay))+geom_bar(aes(fill=3), stat='identity')+labs(x='M客户累计金额区间分布')+theme(legend.position='none')
  #   # ggplotly(g) 
  #   
  #   
  #   dm = data.frame(df()$'会员等级',df()$'人数占比')
  #    
  #   p = ggplot(dm, aes(x = '会员等级', y = '人数占比')) 
  #   # + 
  #   #   geom_bar(stat = "identity", width = 1) + 
  #   #   coord_polar(theta = "y") + 
  #   #   labs(x = "", y = "", title = "") + 
  #   #   theme(axis.ticks = element_blank()) + 
  #   #   theme(legend.title = element_blank(), legend.position = "top") 
  #   p
  #   
  #     
  # })
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 日销售曲线
salesHourUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = '日销售曲线', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(3, dateInput(ns('dt'), '成单时间', Sys.Date()-1))
    ),
    fluidRow(
      column(8, plotlyOutput(ns('trend1')) ),
      column(4,dataTableOutput(ns('tb1')) )
    )
  )
  )
}
salesHour <- function(input, output, session) {
  dt <- reactive({
    req(input$dt)
    input$dt
  })
  df <- reactive({
    
    sql <- paste0("
                  select DATE_FORMAT(b.dt,'%H') h,round(sum(sum_price)) amount2 , count(oid) oid2 
                  from (
                  select case
                  when PAY_STATUS = 1 then
                  payDates
                  when PAY_TYPE = 10002 then
                  POST_DATE_STR
                  end dt , sum_price ,oid 
                  from tb_porder a
                  where 
                   HANDLE_STATUS not in (5, 7) 
                    and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                  ) b
                  where 1 = 1 
                  and date(dt) = '",dt(),"'
                  group by DATE_FORMAT(b.dt,'%H')
                  order by DATE_FORMAT(b.dt,'%H') 
                  ")
    x <- dbGetQuery0('e_member', sql)
    x1<-x%>%
    rename(`小时`=h , `销售额`=amount2,`订单数量`=oid2)
    x1
  })


  output$trend1 <- renderPlotly({
    x1 <- df()
    plot_ly(x1, x = ~`小时`) %>%
      add_lines(y = ~`销售额`)%>% add_markers(y = ~`销售额`)
    # plot_ly(x1, x = ~x1[,1]) %>%
    #      add_lines(y = ~x1[,2]) %>% add_markers(y = ~x1[,2])
  })
  output$tb1 <- DT::renderDataTable({
    datatable(
      df(), 
      rownames = FALSE,
      selection = 'multiple',
      extensions = list(Scroller=list()),
      options = list(
        searching=FALSE,
        lengthChange = TRUE,
        pageLength = 24
       
      )
    )
  })
  
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#------------------------会员性别分布-------------------
customerSexUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = '会员性别分布', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1))
        ),
        fluidRow(
          column(6,dataTableOutput(ns('tbl'))) ,
          column(6,RECharts3Output(ns('sex')))
          #)
        )
    )
  )
}
customerSex <- function(input, output, session) {
  
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0("
             select  case when y.sex = 0 then '男' 
             when y.sex = 1 then '女'
             else '未知' end sex ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount,
             sum(quantity) quantity,
             count(distinct x.oid) oids
             from (
             SELECT
             CASE
             WHEN PAY_STATUS = 1 THEN
             payDates
             WHEN  PAY_TYPE = 10002 THEN
             POST_DATE_STR
             END dt,
             t.oid,
             t.mid,
             b.amount,
             b.quantity
             FROM
             tb_porder t
             JOIN tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR ( date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             AND PAY_TYPE = 10002
             )
             )
             ) x 
             left join (select sex , mid  from tb_member a
             where 1 = 1) y
             on x.mid = y.mid 
             group by
             case when y.sex = 0 then '男' 
             when y.sex = 1 then '女'
             else '未知' end 
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,5],2)
    ) %>% dplyr::rename(`性别`=sex , `购买人数`=mids,`销售额`=amount, 
                 `销售量`= quantity, `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-oids)
  }) 
  
  
  
  
  
  output$tbl <- renderDataTable({
    datatable(
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
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  # output$sex <- renderPlotly({
  #   # g <- df3() %>%
  #   #   ggplot(aes(lbl, CUM_pay))+geom_bar(aes(fill=3), stat='identity')+labs(x='M客户累计金额区间分布')+theme(legend.position='none')
  #   # ggplotly(g)
  # 
  #   p <- plot_ly(
  #     df(), 
  #     values = df()$'人数占比', labels = df()$'性别',
  #     type = 'pie'
  #     #marker =  df()$'性别'
  #   )
  # })
  
  
  
  output$sex <- renderREcharts3({ 
    # dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    # bar(dat1, feed, weight, label = round(weight, 0), title = 'test')
    # x<-df()
    # x[,1] <-as.factor(x[,1])
    # pie(x,`性别`,`购买人数`)
    pie(df(),'性别', '人数占比',title = '人数占比', height = 400)
    
    # bar(df, as.factor(df$'性别'), df$'人数占比')
    # REcharts3::pie(df() , df()$'性别' , df()$'人数占比', title = '人数占比', height = 400)
    # REcharts3::pie(df , df$'性别' , df$'人数占比', title = '人数占比', height = 400)
    #p02
    
  })
}


#------------------------会员年龄分布-------------------
customerAgeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # box(title = '会员年龄分布', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-1, end = Sys.Date()-1))
    ),
    fluidRow(
      column(6,dataTableOutput(ns('tbl'))) ,
      column(6,RECharts3Output(ns('age')))
    )
  )
}
customerAge <- function(input, output, session){
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  #and date(dt) >= '",pay_date()[1],"'
  #and date(dt) <= '",pay_date()[2],"'
  
  
  df <- reactive({
    sql <-
      paste0("
             select  case 
             when age <= 10 then '小于10岁'
             when age > 10 and age <= 19 then '10-19'
             when age >= 20 and age <= 29 then '20-29'
             when age >= 30 and age <= 39 then '30-39'
             when age >= 40 and age <= 49 then '40-49'
             when age >= 50 and age <= 59 then '50-59'
             when age >= 60 then '60岁以上'
             else '未知' end age ,
             count(distinct x.mid) mids ,
             sum(x.amount) amount ,
             sum(quantity) quantity,
             count(distinct x.oid) oids
             from (
             SELECT
             CASE
             WHEN PAY_STATUS = 1 THEN
             payDates
             WHEN PAY_TYPE = 10002 THEN
             POST_DATE_STR
             END dt,
             t.oid,
             t.mid,
             b.amount,
             b.quantity
             FROM
             tb_porder t
             JOIN tb_porder_line b ON t.oid = b.oid
             WHERE
             1 = 1
             and HANDLE_STATUS NOT IN (5, 7)
             AND (
             (
             PAY_STATUS = 1
             AND date(payDates) >= '",pay_date()[1],"'
             AND date(payDates) <= '",pay_date()[2],"'
             )
             OR ( date(POST_DATE_STR) >= '",pay_date()[1],"'
             AND date(POST_DATE_STR) <= '",pay_date()[2],"'
             AND PAY_TYPE = 10002
             )
             )
             ) x 
             left join (
             select ifnull(round(DATEDIFF(now() , date(birthday) )/365),'未知') age ,mid 
             from tb_member 
             where birthday <> ''
             ) y
             on x.mid = y.mid 
             group by case 
             when age <= 10 then '小于10岁'
             when age > 10 and age <= 19 then '10-19'
             when age >= 20 and age <= 29 then '20-29'
             when age >= 30 and age <= 39 then '30-39'
             when age >= 40 and age <= 49 then '40-49'
             when age >= 50 and age <= 59 then '50-59'
             when age >= 60 then '60岁以上'
             else '未知' end
             ")
    dd <- dbGetQuery0('ecommerce',sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = dd[,3]/sum(dd[,3]),
      per_price = round(dd[,3]/dd[,5],2)
    ) %>% dplyr::rename(`年龄`=age , `购买人数`=mids,`销售额`=amount, 
                 `销售量`= quantity, `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-oids)
    
    
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
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
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  output$age <- renderREcharts3({ 
    # dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    # pie(dat1, feed, weight, label = round(weight*10, 0), title = 'Pie Plot')
    # donut(dat1, feed, weight, title = 'Pie Plot')
    pie(df(),`年龄`,`销售额`, title = '销售额', height = 400)
    #p02
    
  })
  
}







#------------------------会员购买间隔分布-------------------
customerBuyDiffUI <- function(id) {
  ns <- NS(id)
  tagList(
    # box(title = '会员年龄分布', status = 'primary', solidHeader = T, width = 12,
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-120, end = Sys.Date()-1))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,RECharts3Output(ns('buy')))
    )
  )
}
customerBuyDiff <- function(input, output, session){
  #付款日期值获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
 
  
  df <- reactive({
    #print(pay_date()[1])
    #print(format(pay_date()[1],'%Y%m%d') ) 
    
    sql <-
      paste0("
              select case 
          when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end diff , count(distinct mid) mids , sum(sum_price) AMOUNT, count(distinct oid) oids from (
             with temp as (
             select mid, dt, sum_price , oid ,  row_number() over(partition by mid order by dt desc) rn 
             from (select mid,sum_price ,oid,
             case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt
             from tb_porder a
             WHERE 1 = 1
             and HANDLE_STATUS not in (5, 7)
             AND ((PAY_STATUS = 1 AND
             to_char(payDates, 'yyyymmdd') >= '", format(pay_date()[1],'%Y%m%d') ,"' AND
             to_char(payDates, 'yyyymmdd') <= '", format(pay_date()[2],'%Y%m%d'),"') OR
             (  PAY_TYPE = 10002 AND
             to_char(POST_DATE_STR, 'yyyymmdd') >= '",format(pay_date()[1],'%Y%m%d'),"' AND
             to_char(POST_DATE_STR, 'yyyymmdd') <= '",format(pay_date()[2],'%Y%m%d'),"')))
             ) select a.mid  , round(a.dt-b.dt) difftime , a.sum_price ,a.oid from temp a
             join temp b  on a.mid = b.mid and  a.rn = b.rn - 1 
             )
             group by case 
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>%mutate(
      mid_rat = dd[,2]/sum(dd[,2]),
      amount_rat = round(dd[,3]/sum(dd[,3]),4),
      per_price = round(dd[,3]/dd[,4],2)
    ) %>% dplyr::rename(`购买间隔`=DIFF  , `购买人数`=MIDS ,`销售额`=AMOUNT,  `客单价`=per_price,`人数占比`=mid_rat,`销售额占比`=amount_rat) %>% select(-OIDS)
    
    
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(
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
    )%>%  # datatable end
      formatPercentage(c('人数占比','销售额占比'), 2)
  })
  
  
  output$buy <- renderREcharts3({
    
    dat <- df()[c('购买间隔','销售额','销售额占比')]
    
    dat$销售额 <- dat$销售额/10000
    
    dat2 = gather(dat, key, value, -购买间隔)
    
    p = his(dat2, '购买间隔', value, key, #label = percent(value, 0), 
            title = '单位:万', label.show = F, label.position = 'top',yAxis.max = 1000)
    p2 = p %>% addSecAxis(series = '销售额占比', type = 'line' ,yAxis.max = 1)
    p2
  })
  
}







#------------------------会员生命周期-------------------
customerLifeCycleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(2, sliderInput(ns("logday"),label = "未登陆天数", min = 1, max = 1000, value = 365  ))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,RECharts3Output(ns('life')))
    )
  )
}
customerLifeCycle <- function(input, output, session){
  #  未登陆天数  获取
  logday <- reactive({
    req(input$logday)
    input$logday
  }) 
  
  
  df <- reactive({
    sql <-
      paste0("
             select  case  
          when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end diff , count(mid) mids   
             from (
             select mid , round(last-first) difftime  from (
             select mid,
             min(dt) keep (dense_rank first order by dt desc) last , 
             min(dt) keep (dense_rank last order by dt desc) first
             from (
             with temp as (select mid
             from ana_member a
             where 1 = 1
             and sysdate - a.LAST_LOGIN_DATE_STR > ",logday(),")
             select a.mid,
             case
             when PAY_STATUS = 1 then
             payDates
             when PAY_TYPE = 10002 then
             POST_DATE_STR
             end dt
             from tb_porder a
             join temp b
             on a.mid = b.mid
             WHERE 1 = 1
              and HANDLE_STATUS not in (5, 7) 
             AND (PAY_STATUS = 1 OR PAY_TYPE = 10002))
             group by mid 
             ) x
             where first < last )
             group by case  
             when difftime >= 0 and difftime <= 7 then 'A:0-7' 
             when difftime > 7 and difftime <= 14 then 'B:7-14'             
             when difftime > 14 and difftime <= 30 then 'C:14-30'             
             when difftime > 30 and difftime <= 60 then 'D:30-60'             
             when difftime > 60 and difftime <= 90 then 'E:60-90'             
             when difftime > 90 and difftime <= 120 then 'F:90-120'             
             when difftime > 120 and difftime <= 180 then 'G:120-180'             
             when difftime > 180 and difftime <= 270 then 'H:180-270'             
             when difftime > 270 and difftime <= 360 then 'I:270-360'             
             when difftime > 360  then 'J:360+' 
             end 
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>%mutate(
      mid_rat = round(dd[,2]/sum(dd[,2]),4)
    )%>% arrange(DIFF)  %>% dplyr::rename(`生命周期`=DIFF  , `购买人数`=MIDS , `人数占比`=mid_rat)
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(  
      #df() <- df()[order(df()$生命周期),],
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
    )%>%  # datatable end
      formatPercentage(c('人数占比'), 2)
  })
  
  
  output$life <- renderREcharts3({

    #dat1 = aggregate(weight ~ feed, data = chickwts, mean)
    donut(df(), 生命周期, 购买人数, title = '会员生命周期分布')
    
  })
  
}
