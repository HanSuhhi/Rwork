# ------------------------------------------------------
# 项目名称: 金融时间序列的线性模型及其案例分析
# 文件名称: Part2
# 选择股票: 300766
# @author: 卢大佐
# @version: v1.0
# @description: 金融数据分析-王许老师-第二份作业
# ------------------------------------------------------
#   
#    给出价格序列和收益率序列的样本自相关图和偏自相关图，并对其特征加以说明
#    对价格序列和收益率序列进行混成检验 
#    运用多种方法对价格和收益率序列进行单位根（平稳性）检验（至少3种） 
#    运用本模块讲授的的多种方法验证该只股票的价格波动是否符合有效市场 假说（至少4种）
#
# -------------------------------------------------------
# 
# 下载包 && 已下载，注释之
# install.packages("quantmod")
# install.packages("fBasics")
# install.packages("pastecs")
# install.packages("rvest")
# install.packages("Tushare")
# install.packages("fUnitRoots")
# install.packages("urca")
#
    # 引入公共方法包
    stock <- "300766"
    source('common.R')
    # 加载相关包
    library(zoo)
    library(xts)
    library(TTR)
    library(quantmod)
    library(lubridate)
    library(xts)
    library(timeDate)
    library(timeSeries)
    library(fBasics)
    library(pastecs)
    library(xml2)
    library(rvest)
    library(lubridate)
    library(httr)
    library(tidyverse)
    library(Tushare)
    library(fUnitRoots)
    library(urca)
# 
# main
# -------------------------------------------------------
# 1 给出价格序列和收益率序列的样本自相关图和偏自相关图，并对其特征加以说明
# -------------------------------------------------------

    # 定义变量及函数
    title <- "300766.sz"
    log.return <- function(x) {
        c(diff(log(x)))
    }

    # 每日互动收盘价
    GETUI_price <- getStockByQuantmod(title)
    GETUI_price <- na.approx(GETUI_price)[, 1]
    acf(GETUI_price, lag=36)

    # 每日互动收益率
    GETUI.logreturn <- log.return(GETUI_price)
    GETUI.logreturn <- na.approx(GETUI.logreturn)[, 1]
    acf(GETUI.logreturn, lag=36)

    # 自相关函数计算结果的分析 
    f1 <- acf(GETUI_price)
    f1$acf
    2/sqrt(length(GETUI_price))

    # 偏自相关系数图
    pacf(GETUI.logreturn)
    pacf(GETUI_price)

# -------------------------------------------------------
# 2 对价格序列和收益率序列进行混成检验 
# -------------------------------------------------------

    # 混成检验的R实现：平稳时间序列与非平稳时间序列的比较
    Box.test(GETUI.logreturn, type="Ljung")
    Box.test(GETUI_price,type="Ljung")

# -------------------------------------------------------
# 3 运用多种方法对价格和收益率序列进行单位根（平稳性）检验（至少3种）
# -------------------------------------------------------

    #######
    # 1 fUnitRoots
    #######
    fUnitRoots <- function(vel) {
        plot(vel)
        acf(vel)
        adfTest(vel,type=c("ct"))
        adfTest(diff(vel),type=c("ct"))
    }
    fUnitRoots(GETUI_price)
    # fUnitRoots(diff(GETUI_price))

    #######
    # 2 unitRootTest
    #######
    unitRootTest <- function(vel) {
        unitrootTest(vel, type = c("ct"))
    }
    # unitRootTest(GETUI_price)
    unitRootTest(diff(GETUI_price))

    #######
    # 3 urca
    #######
    urca <- function(vel) {
        adf_vel <- ur.df(vel, type = 'trend', selectlags = 'AIC')
        summary(adf_vel)
    }
    urca(GETUI_price)
    # urca(diff(GETUI_price))

# -------------------------------------------------------
# 4 运用本模块讲授的的多种方法验证该只股票的价格波动是否符合有效市场假说（至少4种）
# -------------------------------------------------------

    #######
    # 1 市场弱式有效假说 - 直接检验方法
    #######
    market1 <- function(vel) {
        f1 <- acf(vel) 
        plot(f1, main = "自相关图", xlab = "滞后阶数", col = 4 , lwd = 2)
        for (i in 1:20)
            print(Box.test(GETUI_price, type="Ljung-Box", lag = i))
    }
    market1(GETUI_price)
    
    #######
    # 2 市场弱式有效假说 - 建立股价序列的自回归模型，并对其系数进行显著性检验
    #######
    # 深圳成指收盘价
    SZCZ_price <- getStockByQuantmod("399001.sz")
    SZCZ_price <- na.approx(SZCZ_price[, 6])
    SZCZ_price <- ts(SZCZ_price, start = c(1997,7,2) , freq=365)
    SZCZ_ar <- ar(SZCZ_price, method = "yule-walker") 
    SZCZ_ar$order

    #######
    # 3 市场弱式有效假说 - 对价格序列或其对数序列进行单位根检验，以判断其是否符合随机 游走
    #######
    adfTest(SZCZ_price, type="ct")
    adfTest(diff(SZCZ_price), type="nc")
    Box.test(SZCZ_price, type="Ljung-Box")

    #######
    # 4 市场弱式有效假说 - 对价格序列或其对数序列进行单位根检验，以判断其是否符合随机 游走
    #######
    SZCZ_price <- getStockByQuantmod("399001.sz")
    shenzhen.r <- dailyReturn(SZCZ_price[, 6], type="log")[-1]
    Box.test(shenzhen.r, type="Ljung-Box")
