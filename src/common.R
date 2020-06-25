# 定义获取股票函数
# @params: stock_name 股票名称
# @return: stock 获得的股票
getStockByName <- function(stock_name, stock, type, label) {
    # quantmod 方法
    if (type == 'quantmod') {
        return(getStockByQuantmod(stock_name))
    }
    # web 方法
    else if (type == 'web') {
        if (label == 'sz') {
            return(getSpecialStockByWeb(stock, label))
        } else {
            return(getStockByWeb(stock))
        }
    }
    # Tushare 方法
    else if (type == 'Tushare') {
        print(stock_name)
        return(getStockByTushare(stock_name))
    }
    # 报错
    else {
        throwWrong()
    }
}

# Tushare 获取股票
    # @params: stock_name 股票名称
    # @return: stock 获得的股票
getStockByTushare <- function(stock_name) {
    api <- pro_api(token="afc328b8c1724efce0fdfdc00126e4cd0e72eaef28f64428e11c0dfb")
    # 获取数据
    stock <- api(api_name = 'daily', ts_code=stock_name, start_date='20170525', end_date='20200529')
    stock_new_1<-stock[order(stock[,2],decreasing=F),]
    head(stock_new_1)
}

getSZ <- function() {

}

# Quantmod 获取股票
    # @params: stock_name 股票名称
    # @return: stock 获得的股票
getStockByQuantmod <- function(stock_name) {
    print(stock_name)
    # 获取该股票交易数据
    stock <- getSymbols(stock_name, src = "yahoo", from = "2017-01-01", auto.assign = F)
    # 检查是否获取成功
    if (is.OHLC(stock) == TRUE) {
        print("股票获取成功")
        return(stock)
    } else {
        return(NULL)
        throwWrong()
    }
}

# web 获取股票
    # @params: stock_name 股票名称
    # @return: OHLC_ful 获得的股票
getStockByWeb <- function(stock_name) {
    symbol = stock_name # stock code
    from = as.Date("2003-07-03")
    to=Sys.Date()+90 #设置最后日期，seq函数不能识别当季值，截止日期向前推一个季度
    time.index=seq.Date(from=from,to=to,by="quarter") #设置季度节点的数据索引
    year.id=year(time.index)
    quarter.id=quarter(time.index) #设置日期元素，与网页数据特征对应

    OHLC=list() 
    for(t in 1:length(time.index)){ 
        year=year.id[t] 
        season=quarter.id[t] 
        url=paste0("http://quotes.money.163.com/trade/lsjysj_",symbol,".html?year=",year,"&season=",season) 
        web=read_html(url) #Read HTML or XML 
        xpath="/html/body/div[2]/div[4]/table" 
        web.table=html_table(html_nodes(web,xpath=xpath))
        OHLC[[t]]= web.table[[1]] 
    }

    OHLC_1<-OHLC[[1]]
    OHLC_ful<-OHLC_1[order(OHLC_1[,1],decreasing=F),]
    for(i in 2:length(OHLC)){
        OHLC_conti=OHLC[[i]]
        OHLC_conti_ful=OHLC_conti[order(OHLC_conti[,1],decreasing=F),]
        OHLC_ful<-rbind(OHLC_ful,OHLC_conti_ful)
    }
    return(OHLC_ful)
}

# web 获取特殊股票f
    # @params: stock_name 股票名称
    # @return: OHLC_ful 获得的股票
getSpecialStockByWeb <- function(stock_name, label) {
    symbol = stock_name # stock code
    from = as.Date("2003-07-03")
    to=Sys.Date()+90 #设置最后日期，seq函数不能识别当季值，截止日期向前推一个季度
    time.index=seq.Date(from=from,to=to,by="quarter") #设置季度节点的数据索引
    year.id=year(time.index)
    quarter.id=quarter(time.index) #设置日期元素，与网页数据特征对应

    OHLC=list() 
    for(t in 1:length(time.index)){ 
        year=year.id[t] 
        season=quarter.id[t]
        url=paste0("http://quotes.money.163.com/trade/lsjysj_zhishu_","399001",".html?year=",year,"&season=",season) 
        web=read_html(url) #Read HTML or XML 
        xpath="/html/body/div[2]/div[3]/table" 
        web.table=html_table(html_nodes(web,xpath=xpath))
        OHLC[[t]]= web.table[[1]] 
    }
    OHLC_1<-OHLC[[1]]
    OHLC_ful<-OHLC_1[order(OHLC_1[,1],decreasing=F),]
    for(i in 2:length(OHLC)){
        OHLC_conti=OHLC[[i]]
        OHLC_conti_ful=OHLC_conti[order(OHLC_conti[,1],decreasing=F),]
        OHLC_ful<-rbind(OHLC_ful,OHLC_conti_ful)
    }
    return(OHLC_ful)
}

# 定义绘图函数
    # @params: stock 绘制股票
    # @params: needs 需要绘制哪些图
    # @return: 
paintNeedGraphs <- function(stock, type) {
    # 日数据图和成交量
    # 取名个推是因为选取股票是300766
    if (type == 'quantmod') {
        chartSeries(stock, type = "line", minor.ticks = FALSE, theme = "white", name="STOCK")
    }
    else if(type == 'web') {
        stock.heavy.ohlc<-as.data.frame(lapply(stock,as.numeric))
        stock.heavy.ohlc<-as.xts(stock.heavy.ohlc,order.by=as.Date(stock[,1]))
        vec<-c(1,8,9)
        stock.heavy.ohlc_2<-stock.heavy.ohlc[,-vec]
        stock.heavy.price<-stock.heavy.ohlc_2[,4]
        plot(stock.heavy.price,type="l")
        hist(stock.heavy.price,nclass=30,main="STOCK")
    }
}

# 基本数据分析
    # @params: stock 股票
    # @return: 
BasicAnalysis <- function(stock, stock_data) {
    print("基本数据分析信息：")
    a <- summary(stock)
    b <- basicStats(stock_data) 
    c <- stat.desc(stock_data, basic = TRUE, desc = TRUE, norm = TRUE, p = 0.9)
    print(a)
    print('调用fBasics--------------------------------------')
    print(b)
    print('调用pastecs 数据分析--------------------------------------')
    print(c)
}

# 报错
    # @return: string
throwWrong <- function() {
    print("出现异常失败，请重试")
}


