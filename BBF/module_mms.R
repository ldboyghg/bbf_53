#------------------------付费渠道成单跟踪-------------------
mmsChnlUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='toolbar',
             column(4, dateRangeInput(ns('pay_date'), '日期',  start = Sys.Date()-30, end = Sys.Date()-1)),
             column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
    ),
    fluidRow(
      column(5,dataTableOutput(ns('tbl'))) ,
      column(7,plotlyOutput(ns('chnl')))
    )
  )
}
mmsChnl <- function(input, output, session){
  #  未登陆天数  获取
  pay_date <- reactive({
    req(input$pay_date)
    input$pay_date
  }) 
  
  
  df <- reactive({
    sql <-
      paste0(" select * from tb_chnl_sale_trace 
                 where timest >= '", format(pay_date()[1],'%Y%m%d') ,"'
                  and timest <=  '",format(pay_date()[2],'%Y%m%d') ,"'
             ")
    
    
    
    dd <- dbGetQuery1(sql)
    
    df <- dd %>% arrange(desc(TIMEST)) %>% dplyr::rename(`流量日期`=TIMEST  , `渠道`=SRC , `销售额`=SUM_PRICE ,`客户数`=MIDS , `订单数`=OIDS ) 
    
  }) 
  
  
  output$tbl <- renderDataTable({
    datatable(  
      #df() <- df()[order(df()$流量日期),],
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
  
  
  # export
  output$btn_export <- downloadHandler(paste('bbf-data-mms',Sys.Date(),'.csv',sep=''), content = function(file) {
    if(TRUE){
      #tmp <- df()[input$tbl_rows_all, , drop = FALSE]
      #write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
      write.csv(df(), file, fileEncoding='gbk',row.names=FALSE)
    } else {
      write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
    }
    
  })
  
  output$chnl <- renderPlotly({
    
    mytheme <- theme(plot.title=element_text(face="bold.italic",
                                             size="14", color="brown"),
                     axis.title=element_text(face="bold.italic",
                                             size=10, color="brown"),
                     axis.text=element_text(face="bold", size=9,
                                            color="darkblue"),
                     panel.background=element_rect(fill="white",
                                                   color="darkblue"),
                     panel.grid.major.y=element_line(color="grey",
                                                     linetype=1),
                     panel.grid.minor.y=element_line(color="grey",
                                                     linetype=2),
                     panel.grid.minor.x=element_blank(),
                     axis.text.x=element_text(angle = 315,vjust = 0.5,hjust = 0.5),
                     legend.position="none")
    
    
    ggplot(df <-  df() %>% mutate(`流量日期`=substr(`流量日期`,5,8)),aes(`流量日期`,`销售额`))+
      facet_grid(`渠道`~.)+
      #coord_flip()+
      #geom_line()+
      geom_bar(aes(fill=`渠道`),stat="identity",position="dodge",width=0.8)+    
      #geom_area(aes(fill=`渠道`),position="stack",alpha=0.5 )+
      
      # theme(
      #   #text=element_text(family = "myfont"),
      #       legend.position="none",
      #       axis.text.x=element_text(angle = 315,vjust = 0.5,hjust = 0.5),
      #       title=element_text(family = "myfont",face="bold")
      #     )
      mytheme
  })
  
}
