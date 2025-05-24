rm(list=ls())
setwd('E:/LZ/25014')
if(! dir.exists("00_rawdata")) dir.create("00_rawdata")
setwd('./00_rawdata')

# 加载必要的包
if (!require("glue")) install.packages("glue", dependencies = TRUE)
if (!require("gtsummary")) install.packages("gtsummary", dependencies = TRUE)
library(dietaryindex)
library(glue)
library(gtsummary)
library(dplyr)
library(foreign)
library(haven)
library(openxlsx)

# 定义年份和对应的字母
years <- c("2005-2006", "2007-2008", "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018")
letters_code <- c("C", "D", "F", "G", "H", "I", "J")  # 从C开始对应2005-2006

# 创建空列表存储结果
all_dii_results <- list()

# 循环处理每个年份
for(i in seq_along(years)) {
  year <- years[i]
  letter <- letters_code[i]
  
  # 转换年份格式 (例如: "2017-2018" -> "1718")
  year_code <- substr(year, 3, 4) %>% paste0(substr(year, 8, 9))
  
  # 构建文件路径
  demo_file <- sprintf("./%s/%s_DEMO_%s.XPT", year, year, letter)
  dr1_file <- sprintf("./%s/%s_DR1TOT_%s.XPT", year, year, letter)
  dr2_file <- sprintf("./%s/%s_DR2TOT_%s.XPT", year, year, letter)
  fped1_file <- sprintf("./%s/fped_dr1tot_%s.sas7bdat", year, year_code)
  fped2_file <- sprintf("./%s/fped_dr2tot_%s.sas7bdat", year, year_code)
  
  # 读取数据
  tryCatch({
    cat(sprintf("正在处理%s年度数据...\n", year))
    DEMO_PATH <- read_xpt(demo_file)
    cat("已读取人口统计数据\n")
    NUTRIENT_PATH_1 <- read_xpt(dr1_file)
    cat("已读取第一天营养数据\n")
    NUTRIENT_PATH_2 <- read_xpt(dr2_file)
    cat("已读取第二天营养数据\n")
    FPED_PATH_1 <- read_sas(fped1_file)
    cat("已读取第一天FPED数据\n")
    FPED_PATH_2 <- read_sas(fped2_file)
    cat("已读取第二天FPED数据\n")
    
    # 计算DII
    cat("开始计算DII...\n")
    dii_result <- DII_NHANES_FPED(
      FPED_PATH = FPED_PATH_1,
      NUTRIENT_PATH = NUTRIENT_PATH_1,
      DEMO_PATH = DEMO_PATH,
      FPED_PATH2 = FPED_PATH_2,
      NUTRIENT_PATH2 = NUTRIENT_PATH_2
    )
    
    # 添加年份信息
    dii_result$year <- year
    
    # 存储结果
    all_dii_results[[year]] <- dii_result
    
    # 保存单年度结果
    saveRDS(dii_result, sprintf("DII_%s.rds", gsub("-", "_", year)))
    
    cat(sprintf("完成%s年度DII计算\n", year))
    
  }, error = function(e) {
    cat(sprintf("处理%s年度数据时出错: %s\n", year, e$message))
  })
}

# 合并所有结果
combined_dii <- bind_rows(all_dii_results)

# 保存合并后的结果
saveRDS(combined_dii, "DII_2005_2018_combined.rds")



# 使用gtsummary创建更详细的统计表
dii_table <- combined_dii %>%
  select(year, DII_NOETOH) %>%
  tbl_summary(
    by = year,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})\n[{min}, {max}]"
    )
  )

# 打印基本统计信息
print(dii_table)





