rm(list=ls())
setwd('E:/LZ')
if(! dir.exists("25014")) dir.create("25014")
setwd('./25014')

library(tidyverse)

####对数据进行整理####
# dat <- data09
new_dat <- read.csv("./00_rawdata/mergeData.csv", row.names = 1)
dat <- new_dat

####__________年龄#### 
dat$Age <- ifelse(dat$Age<=45,1,
                          ifelse(dat$Age<=69,2,3))
#加标签
dat$Age <- factor(dat$Age,
                          levels = c(1,2,3),
                          labels = c("18-45","46-69",">69"))
table(dat$Age)


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
dat$Race <- ifelse(dat$Race==1,1,
                           ifelse(dat$Race==3,2,
                                  ifelse(dat$Race==4,3,4)))
dat$Race <- factor(dat$Race,
                           levels = c(1,2,3,4),
                           labels = c("Mexican American",
                                      "Non-Hispanic White",
                                      "Non-Hispanic Black",
                                      "Other"))
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
dat$EDUcation <- ifelse(dat$EDUcation2 %in% c(1,2),1,2)
dat$EDUcation <- factor(dat$EDUcation,c(1,2),
                                labels = c("Below high school",
                                           "High School or above"))
table(dat$EDUcation,useNA = "ifany") 


####__________PIR####
summary(dat$PIR) #可以发现，缺失了 294
dat$PIR <- ifelse(dat$PIR<1,1,2)
table(dat$PIR,useNA = "ifany") #很明显，文献将NA转在2组，也就是Not poor
dat$PIR <- ifelse(is.na(dat$PIR),2,dat$PIR)
dat$PIR <- factor(dat$PIR,levels = c(1,2),labels = c("Poor","Not poor"))
table(dat$PIR,useNA = "ifany") #再次分组



####__________BMI ####
summary(dat$BMI) 
dat$BMI <- cut(dat$BMI,
                       breaks = c(0,18.5, 25, 30, Inf),
                       labels = c("<18.5", "18.5-25","25-30", ">30"),
                       left = TRUE, # 左开右闭
                       include.lowest = TRUE 
)                                      

table(dat$BMI,useNA = "ifany") #再次分组


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
table(dat$Smoke,useNA = "ifany")  
#删除临时的无用变量
dat <- subset(dat, select=-c(Smoke_history,smoke_now))
#查看去除无用变量的数据结构
str(dat)


####__________饮酒####
table(dat$Drink,useNA = "ifany")
# 1	Yes
# 2	No
# 7	Refused
# 9	Don't know
# .	Missing
dat$Drink <- ifelse(dat$Drink == 1,1,2)
dat$Drink <- factor(dat$Drink,
                        levels = c(1,2),
                        labels = c("Yes","No"))
table(dat$Drink,useNA = "ifany")





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
summary(dat$TC_mg)  # 
summary(dat$TG_mg)  # 1
summary(dat$HDL_mg) # 
summary(dat$LDL_mg) # 2441
table(dat$cholesterol,useNA = "ifany")
dat$TC_mg <- ifelse(is.na(dat$TC_mg),0,dat$TC_mg)
dat$HDL_mg <- ifelse(is.na(dat$HDL_mg),100,dat$HDL_mg) #低水平为正常
dat$LDL_mg <- ifelse(is.na(dat$LDL_mg),0,dat$LDL_mg)
dat$lipids <- ifelse(dat$TC_mg>200 | 
                           dat$TG_mg>150 | 
                           (dat$HDL_mg<40 & dat$Gender==1) | 
                           (dat$HDL_mg<50 & dat$Gender==2) | 
                           dat$LDL_mg>=130 |
                           dat$cholesterol==1,1,0)

table(dat$lipids,useNA = "ifany")
  


####__________SBP####
summary(dat$BPXSY1) # 288
summary(dat$BPXSY2) # 355
summary(dat$BPXSY3) # 394
summary(dat$BPXSY4) # 4336
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
summary(dat$SBP) #发现有  74 人的4次SBP均缺失，暂时不处理

sbp_temp <- subset(dat,is.na(dat$BPXSY1) & 
                     is.na(dat$BPXSY2) & 
                     is.na(dat$BPXSY3) & 
                     is.na(dat$BPXSY4))
nrow(sbp_temp) #验证得到，确实有 74 个人的四次血压均为缺失
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

summary(dat$DBP) #发现有3个人的4次DBP均缺失，暂时不处理

dbp_temp <- subset(dat,is.na(dat$BPXDI1) & 
                     is.na(dat$BPXDI2) & 
                     is.na(dat$BPXDI3) & 
                     is.na(dat$BPXDI4))
nrow(dbp_temp) #验证得到，确实有3个人的四次血压均为缺失
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



# 自变量----------------
# RAR  = (中性粒细胞 (NEU) * 血小板 (PLT) * 单核细胞 (MONO))/淋巴细胞 (LYM)

dat$RAR  <- dat$RDW/dat$albumin
# 取log
dat$RAR_log <- log(dat$RAR )


# 因变量----------------
table(dat$MI,useNA = "ifany")
# 1 yes
# 2 no 
# 7 refused
# 9 unkown


dat$MI <- ifelse(dat$MI==1,1,0)
dat$MI <- factor(dat$MI,levels = c(1,0))
table(dat$MI,useNA = "ifany")



# 去除多于变量
dat <- subset(dat, select=-c(WC,EDUcation3,Neutrophils,Lymphocyte,
                                     albumin, CRP_mg_l, EDUcation2
                                     ))


####转换权重####
dat$wt_LDL <- ifelse(dat$wt_LDL==0 | is.na(dat$wt_LDL),dat$WTMEC2YR,dat$wt_LDL)
# dat$wt_glu <- ifelse(dat$wt_glu==0 | is.na(dat$wt_glu),dat$WTMEC2YR,dat$wt_glu)
dat$wt <- apply(dat[,c("wt_LDL","WTMEC2YR"),],1,min) #取出最小值
dat$wt <- dat$WTMEC2YR/3 # 3个周期


write.csv(dat, "cleanData.csv")




# 生成基线表------------------
# install.packages("tableone")
library(tableone)

# 定义变量
vars <- c("Drink", "Gender", "EDUcation", "Age", "Race", "Marital", "PIR", 
          "BMI", "Smoke", "hyptersion", "CHD", "Stroke", "lipids")

# 定义分类变量
catVars <- c("Drink", "Gender", "EDUcation", "Age", "Race", "Marital", "PIR", 
             "BMI", "Smoke", "hyptersion", "CHD", "Stroke", "lipids")

# 创建Table 1
table1 <- CreateTableOne(vars = vars, 
                        data = dat, 
                        factorVars = catVars)

# 打印表格
print(table1, 
      nonnormal = c("Drink", "Gender", "EDUcation",  
                    "Age", "Race", "Marital", "PIR", "BMI",
                    "Smoke", "hyptersion", "CHD", "Stroke"),  # 非正态分布的连续变量
      showAllLevels = TRUE,  # 显示所有水平
      formatOptions = list(big.mark = ","))
