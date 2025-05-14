rm(list=ls())
setwd('E:/LZ')
if(! dir.exists("25014")) dir.create("25014")
setwd('./25014')

library(tidyverse)

####对数据进行整理####
# dat <- data09
new_dat <- read.csv("./00_rawdata/mergeData.csv", row.names = 1)
dat <- new_dat

# 定义筛选条件函数
filter_conditions <- function(data) {
  # 年龄条件
  age_condition <- data$Age >= 20 & data$Age <= 85
  
  # 怀孕条件 (2=否, 9=不知道, NA=缺失)
  pregnancy_condition <- data$Pregnancy %in% c(2, 9, NA)
  
  return(age_condition & pregnancy_condition)
}
# 执行分析
dat <- dat[filter_conditions(dat), ]
cat(sprintf("基本条件筛选后样本量: %d\n", nrow(dat)))
# 原始样本量：39346
# 基本条件筛选后样本量: 37619
# 年龄>=20筛选后样本量: 32920  (删除 893  个样本)
# 怀孕状态筛选后样本量: 32397  (删除 523  个样本)


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


####__________PIR####
summary(dat$PIR) #可以发现，缺失了 3629
dat$PIR_cat <- ifelse(dat$PIR<1,1,2)
table(dat$PIR_cat,useNA = "ifany") #很明显，文献将NA转在2组，也就是Not poor
dat$PIR_cat <- ifelse(is.na(dat$PIR_cat),2,dat$PIR_cat)
dat$PIR_cat <- factor(dat$PIR_cat,levels = c(1,2),labels = c("Poor","Not poor"))
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
if(F){
  table(dat$Drink,useNA = "ifany") # 8299
  # 1	Yes
  # 2	No
  # 7	Refused
  # 9	Don't know
  # .	Missing
  
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
#  SBP>=130 40130253
#  DBP>=80 40130253
summary(dat$SBP) #查看变量的分布
dat$SBP_new <- ifelse(is.na(dat$SBP),0,dat$SBP) #将缺失赋值为0
summary(dat$DBP) #查看变量的分布
dat$DBP_new <- ifelse(is.na(dat$DBP),0,dat$DBP) #将缺失赋值为0
dat$hyptersion <- ifelse(dat$hypter==1|dat$SBP_new>=130|dat$DBP_new>=80,1,2)
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
dat$DM <- ifelse(dat$Diabetes1==1 |
                   dat$Insulin==1 | 
                   dat$Sugar==1|
                   dat$Glycohemoglobin>=6.5|
                   dat$Glucose>=126 ,1,2)
table(dat$DM,useNA = "ifany")
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



# 自变量----------------
# RAR  = (中性粒细胞 (NEU) * 血小板 (PLT) * 单核细胞 (MONO))/淋巴细胞 (LYM)

dat$RAR  <- dat$RDW/dat$albumin
# 取log
dat$RAR_log <- log(dat$RAR )

summary(dat$RAR) # 2646

# 因变量----------------
table(dat$MI,useNA = "ifany") # 1162
# 1 yes
# 2 no 
# 7 refused
# 9 unkown


dat$MI <- ifelse(dat$MI==1,1,0)
dat$MI <- factor(dat$MI,levels = c(1,0))
table(dat$MI,useNA = "ifany")



# 去除多于变量
dat <- subset(dat, select=-c(WC,EDUcation3,Neutrophils,Lymphocyte,
                                     albumin, EDUcation2
                                     ))


####转换权重####
dat <- dat %>% 
  mutate(
    # 处理权重
    wt = case_when(
      wt_LDL == 0 | is.na(wt_LDL) ~ WTMEC2YR,  # 如果LDL权重缺失或为0，使用WTMEC2YR（测试权重）
      TRUE ~ wt_LDL  # 否则使用LDL权重
    )
  )

dat$wt <- dat$wt/7 # 2010-2018 7个周期


# 检查权重是否正确赋值
summary(dat$wt)  # 查看权重的分布情况






# 5. 数据筛选 --------------


# 分析样本量变化
analyze_sample_size <- function(data) {
  # 原始样本量
  n_original <- nrow(data)
  cat(sprintf("原始样本量: %d\n", n_original))
  
  # 年龄筛选
  age_filtered <- data[data$Age >= 20 & data$Age <= 85, ]
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



# Age+Race+EDUcation+PIR+Gender+
#  Marital+pa+Drink+BMI+Smoke+hyptersion+Stroke+lipids


# 定义需要检查缺失值的变量列表
vars_to_check <- list(
  # 自变量
  independent = c("RAR"),
  
  # 协变量
  covariates = c('Age','Race','EDUcation','PIR','Gender',
                 "BMI", "Smoke", 
                 "hyptersion", "lipids", "pa","Stroke","Marital","DM"),
  
  # 因变量
  dependent = c("MI")
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

# 执行数据清洗
data_clean <- clean_data(dat, filter_conditions, vars_to_check)

#检查independent变量的缺失值:
#  RAR: 删除 2482 个缺失值, 剩余样本量 35137

#检查covariates变量的缺失值:
#  BMI: 删除 459 个缺失值, 剩余样本量 34678
#Smoke: 删除 0 个缺失值, 剩余样本量 34678
#Age: 删除 0 个缺失值, 剩余样本量 34678
#Race: 删除 0 个缺失值, 剩余样本量 34678
#EDUcation: 删除 0 个缺失值, 剩余样本量 34678
#PIR: 删除 3048 个缺失值, 剩余样本量 31630
#Gender: 删除 0 个缺失值, 剩余样本量 31630
#hyptersion: 删除 0 个缺失值, 剩余样本量 31630
#lipids: 删除 1856 个缺失值, 剩余样本量 29774
#pa: 删除 0 个缺失值, 剩余样本量 29774
#Stroke: 删除 0 个缺失值, 剩余样本量 29774
#Marital: 删除 0 个缺失值, 剩余样本量 29774
#DM: 删除 0 个缺失值, 剩余样本量 29774

#检查dependent变量的缺失值:
#  MI: 删除 0 个缺失值, 剩余样本量 29774



# 保存结果
write.csv(data_clean, "cleanData.csv")
# write.csv(dat, "cleanData.csv")




# 特征相对于MI的基线表------------------
library(tableone)
# install.packages(c("flextable","officer"))
library(flextable)
library(officer)

# 定义变量
vars <- c("Age", "Gender", "Race", "Marital", "EDUcation", "PIR", 
          "BMI", "Smoke", "Drink", "hyptersion", "CHD", "Stroke", "lipids","pa")

# 定义分类变量
catVars <- c("Age", "Gender", "Race", "Marital", "EDUcation", "PIR", 
             "BMI", "Smoke", "Drink", "hyptersion", "CHD", "Stroke", "lipids","pa")

# 创建Table 1
table1 <- CreateTableOne(vars = vars, 
                        strata = "MI",  # 按MI分组
                        data = dat, 
                        factorVars = catVars)

# 打印表格
print(table1, 
      nonnormal = vars,  # 所有变量都作为分类变量处理
      showAllLevels = TRUE,  # 显示所有水平
      formatOptions = list(big.mark = ","))

# 将表格转换为flextable格式
table1_flex <- print(table1, 
                    nonnormal = vars,
                    showAllLevels = TRUE,
                    printToggle = TRUE,
                    formatOptions = list(big.mark = ",")) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Characteristics") %>%
  flextable() %>%
  set_header_labels(
    level = "Characteristics",
    Overall = "Total (n = 12,689)",
    `0` = "No (n = 10,964)",
    `1` = "Yes (n = 1,725)",
    test = "P value"
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  autofit() %>%
  width(width = 1.5) %>%
  align(align = "left", part = "all") %>%
  align(align = "center", j = c(2:5), part = "all") %>%
  border_outer() %>%
  border_inner_h() %>%
  border_inner_v()

# 保存为Word文档
doc <- read_docx()
doc <- body_add_flextable(doc, table1_flex)
print(doc, target = "Table1.docx")



# 生成基线表------------------

# 定义变量
vars <- c("Drink", "Gender", "EDUcation", "Age", "Race", "Marital", "PIR", 
          "BMI", "Smoke", "hyptersion", "CHD", "Stroke", "lipids","pa")

# 定义分类变量
catVars <- c("Drink", "Gender", "EDUcation", "Age", "Race", "Marital", "PIR", 
             "BMI", "Smoke", "hyptersion", "CHD", "Stroke", "lipids","pa")

# 创建Table 2
table2 <- CreateTableOne(vars = vars, 
                        data = dat, 
                        factorVars = catVars)




