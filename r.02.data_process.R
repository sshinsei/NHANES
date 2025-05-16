rm(list=ls())
setwd('E:/LZ')
if(! dir.exists("25014")) dir.create("25014")
setwd('./25014')

library(tidyverse)

####对数据进行整理####
# dat <- data09
new_dat <- read.csv("./00_rawdata/mergeData.csv", row.names = 1)
dat <- new_dat




# 分析样本量变化
analyze_sample_size <- function(data, age_range = c(20, 85)) {
  # 原始样本量
  n_original <- nrow(data)
  cat(sprintf("原始样本量: %d\n", n_original))
  
  # 年龄筛选
  age_filtered <- data[data$Age >= age_range[1] & data$Age <= age_range[2], ]
  n_age <- nrow(age_filtered)
  n_removed_age <- n_original - n_age
  cat(sprintf("年龄筛选后样本量: %d (删除 %d 个样本)\n", n_age, n_removed_age))
  
  # 怀孕状态筛选
  pregnancy_filtered <- age_filtered[age_filtered$Pregnancy %in% c(2, 9, NA), ]
  n_pregnancy <- nrow(pregnancy_filtered)
  n_removed_pregnancy <- n_age - n_pregnancy
  cat(sprintf("怀孕状态筛选后样本量: %d (删除 %d 个样本)\n", n_pregnancy, n_removed_pregnancy))
  
  return(pregnancy_filtered)
}



# 执行分析
sink("age_pregnent_filter.txt")
dat <- analyze_sample_size(dat)
sink()



# ----------------------1. 协变量----------------------
####__________年龄#### 
dat$Age_cat <- ifelse(dat$Age<=60, 1, 2)
#加标签
dat$Age_cat <- factor(dat$Age_cat,
                          levels = c(1,2),
                          labels = c("<=60",">60"))
table(dat$Age_cat)


####__________性别####
table(dat$Gender)

#加标签
dat$Gender <- factor(dat$Gender,
                          levels = c(1,2),
                          labels = c("male","female"))
table(dat$Gender)



####__________种族####
table(dat$Race,useNA = "ifany")  
# 1	Mexican American
# 2	Other Hispanic
# 3	Non-Hispanic White
# 4	Non-Hispanic Black
# 5	Other Race - Including Multi-Racial
#dat$Race <- ifelse(dat$Race==1,1,
#                           ifelse(dat$Race==3,2,
#                                  ifelse(dat$Race==4,3,4)))
dat$Race <- factor(dat$Race,
                           levels = c(1,2,3,4,5),
                           labels = c("Mexican American",
                                      "Other Hispanic",
                                      "Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Other Race"))
table(dat$Race,useNA = "ifany") 


####__________婚姻####
table(dat$Marital,useNA = "ifany") 
# 1	Married		
# 2	Widowed	
# 3	Divorced
# 4	Separated	
# 5	Never married
# 6	Living with partner	
# 77	Refused	
# 99	Don't Know	
#文献的做法是已婚/未婚，根据样本量，Married	和Living with partner	进行的合并
dat$Marital <- ifelse(dat$Marital %in% c(1,6),1,2)
dat$Marital <- factor(dat$Marital,
                              levels = c(1,2),
                              labels = c("Yes","No"))
table(dat$Marital,useNA = "ifany") 


####__________文化程度####
table(dat$EDUcation2,useNA = "ifany") 
# 1	Less than 9th grade	
# 2	9-11th grade (Includes 12th grade with no diploma)
# 3	High school graduate/GED or equivalent
# 4	Some college or AA degree	
# 5	College graduate or above	
# 7	Refused
# 9	Don't Know
# .	Missing
#文献的做法是将Less than 9th grade 和9-11th grade (Includes 12th grade with no diploma)合并
# 分类为：Below high school 和 High School or above
dat$EDUcation <- ifelse(dat$EDUcation2 %in% c(1,2),1,
                        ifelse(dat$EDUcation2 %in% c(3,4,5,7,9),2,3))
dat$EDUcation <- factor(dat$EDUcation,c(1,2,3),
                                labels = c("Below high school",
                                           "High School or above",
                                           NA))
table(dat$EDUcation,useNA = "ifany") 
dat <- subset(dat, select=-c(EDUcation2,EDUcation3))

####__________PIR####
summary(dat$PIR) #可以发现，缺失了 3629
# <1.3、1.3-3.5、> 3.5、未记录 PMID:4033390
dat$PIR_cat <- ifelse(dat$PIR<1.3,1,
                      ifelse(dat$PIR>=1.3 & dat$PIR<=3.5,2,
                             ifelse(dat$PIR>3.5,3,4)))
table(dat$PIR_cat,useNA = "ifany") #很明显，文献将NA转在2组，也就是Not poor
dat$PIR_cat <- ifelse(is.na(dat$PIR_cat),2,dat$PIR_cat)
dat$PIR_cat <- factor(dat$PIR_cat,levels = c(1,2,3,4),
                      labels = c("Poor","Not poor","Not poor","Not poor"))
table(dat$PIR_cat,useNA = "ifany") #再次分组



####__________BMI ####
summary(dat$BMI) # 597
dat$BMI_cat <- cut(dat$BMI,
                     breaks = c(0, 25, Inf),
                     labels = c("<=25",">25"),
                     left = TRUE, # 左开右闭
                     include.lowest = TRUE 
)                                      

table(dat$BMI_cat,useNA = "ifany") #再次分组


####__________吸烟####
table(dat$Smoke_history,useNA = "ifany")  #有60个人有吸烟史，141人无吸烟史
# Smoke_history
# 1	Yes
# 2	No
# 7	Refused	
# 9	Don't know
# .	Missing
table(dat$smoke_now,useNA = "ifany") # 29人
# smoke_now
# 1	Every day
# 2	Some days
# 3	Not at all
# 7	Refused
# 9	Don't know
# .	Missing
dat$Smoke <- ifelse(dat$Smoke_history == 1,
                        ifelse(dat$smoke_now %in% c(1,2),1,2), 3)
dat$Smoke <- factor(dat$Smoke,
                        levels = c(1,2,3),
                        labels = c("smoker","smoked","no-smoke"))
table(dat$Smoke,useNA = "ifany")  # 297
#删除临时的无用变量
dat <- subset(dat, select=-c(Smoke_history,smoke_now))
#查看去除无用变量的数据结构
str(dat)


####__________饮酒####
# 2017以前和以后的变量定义不一致，需要更换定义
if(T){
  table(dat$Drink,useNA = "ifany") # 8299
  # 1	Yes
  # 2	No
  # 7	Refused
  # 9	Don't know
  # .	Missing
  
  # 2017-2018
  # 0	去年从未	269	269	
  # 1	每天	2	271	
  # 2	几乎每天	6	277	
  # 3	每周3至4次	8	285	
  # 4	每周2次	14	299	
  # 5	每周一次	二十九	328	
  # 6	每月2至3次	二十七	355	
  # 7	每月一次	二十五	380	
  # 8	去年有 7 至 11 次	19	399	
  # 9	去年有 3 至 6 次	二十九	428	
  # 10	去年有 1 至 2 次	89	517	
  # 77	被拒	1	518	
  # 99	不知道	4	522	
  # .	丢失的	5011	5533
  dat$Drink <- ifelse(dat$Drink == 1,1,2)
  dat$Drink <- factor(dat$Drink,
                      levels = c(1,2),
                      labels = c("Yes","No"))
  table(dat$Drink,useNA = "ifany")
}






####__________中风####
table(dat$Stroke,useNA = "ifany")
# 1 yes
# 2 no 
# 7 refused
# 9 unkown


dat$Stroke <- ifelse(dat$Stroke==1,1,2)
dat$Stroke <- factor(dat$Stroke,levels = c(1,2),labels = c("Yes","No"))
table(dat$Stroke,useNA = "ifany")





####__________冠心病####
table(dat$CHD,useNA = "ifany")
# 1 yes
# 2 no 
# 7 refused
# 9 unkown


dat$CHD <- ifelse(dat$CHD==1,1,2)
dat$CHD <- factor(dat$CHD,levels = c(1,2),labels = c("Yes","No"))
table(dat$CHD,useNA = "ifany")



####__________高血脂####
if(F){
  summary(dat$TC_mg)  # 2440
  summary(dat$TG_mg)  # 2588
  summary(dat$HDL_mg) # 2440
  summary(dat$LDL_mg) # 22004
  table(dat$cholesterol,useNA = "ifany") # 5286
  dat$TC_mg <- ifelse(is.na(dat$TC_mg),0,dat$TC_mg)
  dat$TG_mg <- ifelse(is.na(dat$TG_mg),0,dat$TG_mg)
  dat$HDL_mg <- ifelse(is.na(dat$HDL_mg),100,dat$HDL_mg) #低水平为正常
  dat$LDL_mg <- ifelse(is.na(dat$LDL_mg),0,dat$LDL_mg)
  dat$lipids <- ifelse(dat$TC_mg>200 | 
                         dat$TG_mg>150 | 
                         (dat$HDL_mg<40 & dat$Gender==1) | 
                         (dat$HDL_mg<50 & dat$Gender==2) | 
                         dat$LDL_mg>=130 |
                         dat$cholesterol==1,1,0)
  # dat$lipids <- ifelse(dat$lipids == 1, 1,0)
  table(dat$lipids,useNA = "ifany") # 2691
  dat <- subset(dat, select=-c(TC_mg,TG_mg,HDL_mg,LDL_mg,cholesterol))  
}



####__________SBP####
summary(dat$BPXSY1) # 3487
summary(dat$BPXSY2) # 3070
summary(dat$BPXSY3) # 3234
summary(dat$BPXSY4) # 36743
#可以发现，4次的SBP都有缺失的存在，但是我们需要对4次的SBP求均值
#为了能够计算，先将缺失定义为0
dat$SBP1 <- ifelse(is.na(dat$BPXSY1),0,dat$BPXSY1)
dat$SBP2 <- ifelse(is.na(dat$BPXSY2),0,dat$BPXSY2)
dat$SBP3 <- ifelse(is.na(dat$BPXSY3),0,dat$BPXSY3)
dat$SBP4 <- ifelse(is.na(dat$BPXSY4),0,dat$BPXSY4)
#看看有多少个SBP来参与计算平均
dat$SBPn1 <- ifelse(is.na(dat$BPXSY1),0,1)
dat$SBPn2 <- ifelse(is.na(dat$BPXSY2),0,1)
dat$SBPn3 <- ifelse(is.na(dat$BPXSY3),0,1)
dat$SBPn4 <- ifelse(is.na(dat$BPXSY4),0,1)
dat$SBPn <- dat$SBPn1 + dat$SBPn2 + dat$SBPn3 + dat$SBPn4
#计算均值
dat$SBP <- (dat$SBP1 + dat$SBP2 + dat$SBP3 + dat$SBP4)/dat$SBPn
summary(dat$SBP) #发现有 1718 人的4次SBP均缺失，暂时不处理

sbp_temp <- subset(dat,is.na(dat$BPXSY1) & 
                     is.na(dat$BPXSY2) & 
                     is.na(dat$BPXSY3) & 
                     is.na(dat$BPXSY4))
nrow(sbp_temp) #验证得到，确实有 1718 个人的四次血压均为缺失
#删除临时的无用变量
dat <- subset(dat, select=-c(SBP1,SBP2,SBP3,SBP4,
                                     BPXSY1,BPXSY2,BPXSY3,BPXSY4,
                                     SBPn1,SBPn2,SBPn3,SBPn4,SBPn
))
str(dat)


####__________DBP ####
summary(dat$BPXDI1) 
summary(dat$BPXDI2) 
summary(dat$BPXDI3) 
summary(dat$BPXDI4)
#可以发现，4次的DBP都有缺失的存在，但是我们需要对4次的DBP求均值
#为了能够计算，先将缺失定义为0
dat$DBP1 <- ifelse(is.na(dat$BPXDI1),0,dat$BPXDI1)
dat$DBP2 <- ifelse(is.na(dat$BPXDI2),0,dat$BPXDI2)
dat$DBP3 <- ifelse(is.na(dat$BPXDI3),0,dat$BPXDI3)
dat$DBP4 <- ifelse(is.na(dat$BPXDI4),0,dat$BPXDI4)
#看看有多少个SBP来参与计算平均
dat$DBPn1 <- ifelse(is.na(dat$BPXDI1),0,1)
dat$DBPn2 <- ifelse(is.na(dat$BPXDI2),0,1)
dat$DBPn3 <- ifelse(is.na(dat$BPXDI3),0,1)
dat$DBPn4 <- ifelse(is.na(dat$BPXDI4),0,1)
dat$DBPn <- dat$DBPn1 + dat$DBPn2 + dat$DBPn3 + dat$DBPn4
#计算均值
dat$DBP <- (dat$DBP1 + dat$DBP2 + dat$DBP3 + dat$DBP4)/dat$DBPn

summary(dat$DBP) #发现有 1718 个人的4次DBP均缺失，暂时不处理

dbp_temp <- subset(dat,is.na(dat$BPXDI1) & 
                     is.na(dat$BPXDI2) & 
                     is.na(dat$BPXDI3) & 
                     is.na(dat$BPXDI4))
nrow(dbp_temp) #验证得到，确实有 1718 个人的四次血压均为缺失
#删除临时的无用变量
dat <- subset(dat, 
                  select=-c(DBP1,DBP2,DBP3,DBP4,
                            BPXDI1,BPXDI2,BPXDI3,BPXDI4,
                            DBPn1,DBPn2,DBPn3,DBPn4,DBPn
                  ))


####__________高血压####
table(dat$hypter,useNA = "ifany")
#需要综合：
#  高血压
#  SBP>=140 
#  DBP>=90 
summary(dat$SBP) #查看变量的分布
dat$SBP_new <- ifelse(is.na(dat$SBP),0,dat$SBP) #将缺失赋值为0
summary(dat$DBP) #查看变量的分布
dat$DBP_new <- ifelse(is.na(dat$DBP),0,dat$DBP) #将缺失赋值为0
dat$hyptersion <- ifelse(dat$hypter==1|dat$SBP_new>=140|dat$DBP_new>=90,1,2)
dat$hyptersion <- factor(dat$hyptersion,levels = c(1,2),labels = c("Yes","No"))
table(dat$hyptersion,useNA = "ifany")
dat <- subset(dat, select=-c(hypter,SBP_new,DBP_new))
#查看去除无用变量的数据结构
str(dat)


####__________糖尿病####
# 需要综合：
#  糖尿病史
#  注射胰岛素
#  服药降糖药
#  Glycohemoglobin>=6.5
#  Glucose>=126
#判断缺失情况
table(dat$Diabetes1,useNA = "ifany")
table(dat$Insulin,useNA = "ifany") # 1
table(dat$Sugar,useNA = "ifany") #有缺失，将缺失赋值为999
dat$Sugar <- ifelse(is.na(dat$Sugar),
                        999,dat$Sugar)
summary(dat$Glycohemoglobin) # 2041
summary(dat$Glucose) #有缺失，先将缺失赋值为0
dat$Glucose <- ifelse(is.na(dat$Glucose),0,dat$Glucose)
dat$Glycohemoglobin <- ifelse(is.na(dat$Glycohemoglobin),0,dat$Glycohemoglobin)
dat$Diabetes <- ifelse(dat$Diabetes1==1 |
                   dat$Insulin==1 | 
                   dat$Sugar==1|
                   dat$Glycohemoglobin>=6.5|
                   dat$Glucose>=126 ,1,2)
dat$Diabetes <- factor(dat$Diabetes,levels = c(1,2),labels = c("Yes","No"))
table(dat$Diabetes,useNA = "ifany")
#删除临时的无用变量
dat <- subset(dat, select=-c(Diabetes1,Insulin,Sugar,Glycohemoglobin,Glucose))



####__________身体活动####
table(dat$med,useNA = "ifany")
table(dat$high,useNA = "ifany")

summary(dat$med) #查看变量的分布
summary(dat$high) #查看变量的分布

dat$pa <- ifelse(dat$med==1|dat$high==1,1,2)
dat$pa <- factor(dat$pa,levels = c(1,2),labels = c("high","low"))
table(dat$pa,useNA = "ifany")
dat <- subset(dat, select=-c(med,high))
#查看去除无用变量的数据结构
str(dat)



# ---------------------- 2. 自变量 ----------------
# PIV 不显著
# ABSI = WC/(BMI²/³ × Height¹/²) 不显著
# UHR (%) = (UA [mg/dL] ÷ HDL [mg/dL]) × 100 PMID:40241174 不显著
# BUCR PMID:40082883 不显著
# MHR PMID:39985043 单核细胞计数 (1,000 个细胞/微升)/高密度脂蛋白胆固醇 (HDL-C) 水平 (毫摩尔/升)
# BRI 不显著
# NHR PMID:40205514 中性粒细胞计数(1000/微升)/高密度脂蛋白胆固醇 (HDL-C)

# combined_dii <- readRDS("./00_rawdata/DII_2005_2018_combined.rds") 
# dii <- combined_dii[,c(1,2,3)]
# colnames(dii)[2] <- "index" 

dat$index <- (dat$Neutrophils)/dat$HDL_mmol
dat$index_log <-log(dat$index)


# ---------------------- 3. 因变量 ----------------
table(dat$MI,useNA = "ifany") # 1
# 1 yes
# 2 no 
# 7 refused
# 9 unkown


dat$MI <- ifelse(dat$MI==1,1,0)
dat$MI <- factor(dat$MI,levels = c(1,0))
table(dat$MI,useNA = "ifany")



# 去除多于变量
# dat <- subset(dat, select=-c(WC))

# ---------------------- 4. 数据筛选 --------------

# 定义需要检查缺失值的变量列表
vars_to_check <- list(
  # 自变量
  independent = c("index"),
  
  # 因变量
  dependent = c("MI"),
  
  # 协变量
  covariates = c("Age", "Race", "EDUcation", "PIR", "Gender", 
  "Smoke", "pa","Marital","Drink")
  
  
)

# 数据筛选主函数
clean_data <- function(data, filter_func, vars_list) {
  
  # 2. 逐个检查变量的缺失值
  for (category in names(vars_list)) {
    cat(sprintf("\n检查%s变量的缺失值:\n", category))
    
    for (var in vars_list[[category]]) {
      n_before <- nrow(data)
      data <- data[!is.na(data[[var]]), ]
      n_after <- nrow(data)
      n_missing <- n_before - n_after
      
      cat(sprintf("%s: 删除 %d 个缺失值, 剩余样本量 %d\n", 
                  var, n_missing, n_after))
    }
  }
  
  return(data)
}

# 创建输出文件,数据清洗的结果保存在txt文件
sink("data_processing_log.txt")
# 执行数据清洗
data_clean <- clean_data(dat, filter_conditions, vars_to_check)
sink()

# 19252


# ---------------------- 5. 转换权重 ------------------
# 选择适用范围最小且与分析最相关的权重
#data_clean <- data_clean %>% 
#  mutate(wt = ifelse(is.na(wt_LDL),WTMEC2YR, wt_LDL))
data_clean$wt <- data_clean$WTMEC2YR/7 # 2005-2018 7个周期


# 检查权重是否正确赋值
summary(data_clean$wt)  # 查看权重的分布情况 


# 保存结果
write.csv(data_clean, "cleanData.csv")
# write.csv(dat, "cleanData.csv")






# 特征相对于MI的基线表------------------

library(tableone)
library(survey)
library(dplyr)

# 定义变量
allVars <- c("Age", "Race", "EDUcation", "PIR", "Gender", 
             "Smoke", "pa","Diabetes","Drink","Marital")

# 定义分类变量
fvars <- c("Race", "EDUcation", "Gender", 
           "Smoke", "pa","Diabetes","Drink","Marital")

# 1. 生成完全未加权的表1
table1_unweighted <- CreateTableOne(vars = allVars, 
                                   strata = "MI", 
                                   data = data_clean, 
                                   factorVars = fvars)

# 获取未加权的结果
unweighted_stats <- print(table1_unweighted, 
                         nonnormal = c("Age", "PIR"),
                         showAllLevels = TRUE, 
                         printToggle = FALSE) %>%
  as.data.frame()

# 2. 生成完全加权的表2
# 创建survey对象
bcSvy2 <- svydesign(ids = ~SDMVPSU,   
                    strata = ~SDMVSTRA,   
                    weights = ~wt,
                    nest = TRUE,
                    survey.lonely.psu = "adjust",  
                    data = data_clean)

# 计算加权统计量
table2_weighted <- svyCreateTableOne(vars = allVars, 
                                    strata = "MI", 
                                    data = bcSvy2, 
                                    factorVars = fvars)

# 获取加权的结果
weighted_stats <- print(table2_weighted, 
                       nonnormal = c("Age", "PIR"),
                       showAllLevels = TRUE, 
                       printToggle = FALSE,
                       smd = TRUE) %>%
  as.data.frame()

# 3. 合并表1和表2
# 提取表1中的样本量列
n_cols <- unweighted_stats[, grep("^n$|^n_", names(unweighted_stats), value = TRUE)]

# 从加权结果中提取统计量和p值
stat_cols <- weighted_stats[, !grepl("^n$|^n_", names(weighted_stats))]

# 合并数据
final_table <- cbind(n_cols, stat_cols)

# 添加特征名称列
table_data <- final_table %>%
  rownames_to_column(var = "Characteristics")

# 4. 输出到Excel
library(openxlsx)

# 创建工作簿
wb <- createWorkbook()

# 添加未加权表
addWorksheet(wb, "Table1_Unweighted")
writeData(wb, "Table1_Unweighted", 
          unweighted_stats %>% rownames_to_column(var = "Characteristics"))

# 添加加权表
addWorksheet(wb, "Table2_Weighted")
writeData(wb, "Table2_Weighted", 
          weighted_stats %>% rownames_to_column(var = "Characteristics"))

# 添加合并表
addWorksheet(wb, "Table3_Combined")

# 设置列宽
setColWidths(wb, "Table3_Combined", cols = 1:ncol(table_data), 
             widths = c(30, rep(15, ncol(table_data)-1)))

# 创建标题样式
headerStyle <- createStyle(
  fontSize = 11,
  fontName = "Times New Roman",
  halign = "center",
  border = "bottom",
  borderStyle = "medium",
  textDecoration = "bold"
)

# 创建数据样式
dataStyle <- createStyle(
  fontSize = 10,
  fontName = "Times New Roman",
  halign = "center",
  border = "bottom",
  borderStyle = "thin"
)

# 第一列左对齐样式
firstColStyle <- createStyle(
  fontSize = 10,
  fontName = "Times New Roman",
  halign = "left",
  border = "bottom",
  borderStyle = "thin"
)

# 写入合并的数据
writeData(wb, "Table3_Combined", table_data, headerStyle = headerStyle)

# 应用样式
for(col in 2:ncol(table_data)) {
  addStyle(wb, "Table3_Combined", dataStyle, 
           rows = 2:nrow(table_data), 
           cols = col, 
           gridExpand = TRUE)
}

# 应用第一列样式
addStyle(wb, "Table3_Combined", firstColStyle, 
         rows = 2:nrow(table_data), 
         cols = 1, 
         gridExpand = TRUE)

# 保存Excel文件
saveWorkbook(wb, "baseline_tables.xlsx", overwrite = TRUE)

# 打印结果用于检查
print(table_data)
