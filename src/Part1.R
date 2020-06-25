# ------------------------------------------------------
# 项目名称: 金融数据特征分析与R软件入门
# 文件名称: Part1
# 选择股票: 300766
# @author: 卢大佐
# @version: v1.0
# @description: 金融数据分析-王许老师-第一份作业
# ------------------------------------------------------
#
#  练习R软件的基本操作
#  运用所学习的三种网络爬虫方法（quantmod程序包、网页爬取技术、Tushare数据库）任意抓取深圳证券交易所上市的股票，并分别进行绘图和基本统计分析
#  注：保证时间段一致，比较分析三种方法所获取股票的差异
#  运用上述三种网络爬虫方法抓取深圳市场指数数据，并运用单一指数模型测度所选取股票的风险
#
# ------------------------------------------------------
#
# 下载包 && 已下载，注释之
# install.packages("quantmod")
# install.packages("fBasics")
# install.packages("pastecs")
# install.packages("rvest")
# install.packages("Tushare")
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
# 
# main
# -------------------------------------------------------
# 1 运用所学习的三种网络爬虫方法（quantmod程序包、网页爬取技术、Tushare数据库）任意抓取深圳证券交易所上市的股票，并分别进行绘图和基本统计分析
# -------------------------------------------------------

    # @params: stock 股票代码
    # @params: v 股票板块
    # @params: type 获取股票方式
    # @params: label 是否获取特殊股票
    # @return: 
    Q1 <- function(stock, v, type, label) {
        # 拼接股票名称
        stock_name <- paste(stock, '.', v, sep = "", NULL)
        paste('所需获取的股票名称为', stock_name, "", NULL)
        print(label)
        # 获取股票
        stock <- getStockByName(stock_name, stock, type, label)
        Sys.sleep(10)
        # 绘图
        paintNeedGraphs(stock, type)
        # 基本数据分析
        if (label == "sz") {
            BasicAnalysis(stock, stock[, 5])
        } else {
            BasicAnalysis(stock, stock[, 4])
        }
    }
    # -------------------------------------------------------
    # | <quantmod> | <web> | <Tushare> |
    Q1(stock, 'sz', 'quantmod', 'undefind')
    # Q1(stock, 'sz', 'web', 'undefind')
    # Q1(stock, 'sz', 'Tushare', 'undefind')
    # -------------------------------------------------------

# -------------------------------------------------------
# 2 运用上述三种网络爬虫方法抓取深圳市场指数数据
# -------------------------------------------------------

    # @params: stock 股票代码
    # @params: v 股票板块
    # @return: 
    Q2 <- function(stock, v, type) {
        # 方法与第一题一致
        # 为表不同，闭包调用
        Q1(stock, v, type, 'sz')
    }
    # -------------------------------------------------------
    # | <quantmod> | <web> | <Tushare> |
    SZZS <- "399001"
    Q2(SZZS, 'sz', 'quantmod')
    # Q2(SZZS, 'sz', 'web')
    # Tushare 暂未实现
    # Q2(SZZS, 'sz', 'Tushare')
    # -------------------------------------------------------

# -------------------------------------------------------
# 3 运用单一指数模型测度所选取股票的风险
# -------------------------------------------------------

    title <- "300766.sz"

    # 计算调整收盘价
    # 每日互动收盘价
    GETUI_price <- getStockByQuantmod(title)
    GETUI_price <- na.approx(GETUI_price)[, 6]
    # 深圳成指收盘价
    SZCZ_price <- getStockByQuantmod("399001.sz")
    SZCZ_price <- na.approx(SZCZ_price)[, 6]

    Sys.sleep(2)

    # 依据调整收盘价计算收益率
    GETUI.r <- dailyReturn(GETUI_price, subset = "2019-03-25/2020-06-24",type = "log")
    SZCZ.r <- dailyReturn(SZCZ_price, subset = "2019-03-25/2020-06-24",type = "log")

    # 将序列转换为时间序列对象
    GETUI.r<-ts(GETUI.r, start = c(2019,03,25), freq = 60)
    SZCZ.r<-ts(SZCZ.r, start = c(2019,03,25), freq = 60)

    print("单一资产和市场收益率的相关性分析")

    # 运用cor函数计算两序列的相关系数
    cor(GETUI.r, SZCZ.r)

    # 绘制散点图
    plot(GETUI.r, SZCZ.r)

    # 构建市场模型并估计参数
    market <- lm(GETUI.r~SZCZ.r)
    summary(market)

    market<-lm(GETUI.r~0+SZCZ.r)
    summary(market)

    # 模型残差序列的检验
    # 观察残差序列及其自相关性
    par(mfrow=c(2,1))
    plot(market$residuals,type="l")
    acf(market$residuals,lag=36)
    # 对残差序列进行白噪声检验
    Box.test(market$residuals,lag=36)

    # 单一资产与市场指数收益率最小二乘线性拟合图解
    par(mfrow=c(1,1))
    plot(GETUI.r, SZCZ.r, cex = 1)
    abline(0, market$coefficients)
