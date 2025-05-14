rm(list=ls())
setwd('E:/LZ/25014')
if(! dir.exists("03_results")) dir.create("03_results")
setwd('./03_results')
library(survey)
library(dplyr)
library(forestplot)
library(rms)
library(ggplot2)

# load functions
source("../functions.R")

# 设置保留小数点后3位
options(digits=3)

## ----------------- 协变量: 模型3调整此处---------------------- ##
covariates = c(                
  "Age", "Race", "EDUcation", 
  "PIR", "Gender", "BMI", "Smoke", 
  "hyptersion", "Stroke", "lipids","Marital","pa","DM"   
)


new_dat <- read.csv("../cleanData.csv",row.names = 1)

####研究设计####
options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)



#均数和标准误
svymean(~Age,study_design)


#频数+构成比
freq <- table(new_dat$Age)
prop <- svytable(~Age,study_design)
per <- rbind(prop[1]/sum(prop),prop[2]/sum(prop),prop[2]/sum(prop))
paste0(freq," (",sprintf("%0.2f",per),")")


# Age
# Race
# Marital
# EDUcation
# PIR
# sleep_time
# Depress
# BMI
# BMI
# WC
# ABMI
# SBP 
# DBP
# Smoke
# Drink
# DM
# hypter
# lipids






# -------------- 1. 模型构建 -----------------
options(survey.lonely.psu = "adjust")

##### 四分位数分组 ###########
CDAI_m <- svyquantile(~CDAI,study_design,c(0.25,0.5,0.75))
CDAI_m

CDAI_q <- quantile(new_dat$CDAI, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
new_dat$CDAI_q4 <- cut(new_dat$CDAI,
                       breaks = c(-Inf, CDAI_q[1], CDAI_q[2], CDAI_q[3], Inf),
                       labels = c("Q1","Q2","Q3","Q4"),
                       right = TRUE,
                       include.lowest = TRUE)


new_dat$CDAI_q4 <- factor(new_dat$CDAI_q4,
                        levels = c("Q1","Q2","Q3","Q4"))
table(new_dat$CDAI_q4)
# Q1   Q2   Q3   Q4 
# 6496 6496 6496 6496

#转化参照
# new_dat$CDAI_q4 <- relevel(new_dat$CDAI_q4,"Q4") # Q4为reference
table(new_dat$MI)
#   0    1 
# 28135  1314  

#转化结局的编码
new_dat$status <- new_dat$MI

# 重新设计采样
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)



#### _________四分位数分组进行回归 ####
mod1 <- svyglm(status~CDAI_q4, study_design,family = quasibinomial)
summary(mod1)
results_mod1 <- extract_regression_results(mod1, "model1")

mod2 <- svyglm(status~CDAI_q4+Age+Race+EDUcation+PIR+Gender, 
                   study_design,family = quasibinomial)
summary(mod2)
results_mod2 <- extract_regression_results(mod2, "model2")

mod3 <- svyglm(as.formula(
  paste0("status~CDAI_q4+",paste(covariates, collapse = " + "))), 
                   study_design,family = quasibinomial)
summary(mod3)
results_mod3 <- extract_regression_results(mod3, "model3")

# 打印所有结果
cat("\nModel 1 Results:\n")
print(results_mod1)
cat("\nModel 2 Results:\n")
print(results_mod2)
cat("\nModel 3 Results:\n")
print(results_mod3)



######## ______趋势分析 ######
table(new_dat$CDAI)
CDAI_q
new_dat$CDAI_T <- ifelse(new_dat$CDAI < -2.57,1,
                       ifelse(new_dat$CDAI < -0.626,2,
                              ifelse(new_dat$CDAI < 1.842,3,4)))
table(new_dat$CDAI_T)
# 1    2    3    4 
# 7250 7933 7233 7358 


study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

Tmod1 <- svyglm(status~CDAI_T, study_design,family = quasibinomial)
summary(Tmod1)
results_Tmod1 <- extract_regression_results(Tmod1, "Tmodel1")

Tmod2 <- svyglm(status~CDAI_T+
                          Age+Race+EDUcation+PIR+Gender, 
                        study_design,family = quasibinomial)
summary(Tmod2)
results_Tmod2 <- extract_regression_results(Tmod2, "Tmodel2")

formula_str <- paste0("status ~ CDAI_T + ", paste(covariates, collapse = " + "))
Tmod3 <- svyglm(as.formula(formula_str),
                design = study_design,
                family = quasibinomial)
summary(Tmod3)
results_Tmod3 <- extract_regression_results(Tmod3, "Tmodel3")


####  _______ 自变量作为连续变量分析_________ ####
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

#### 单因素线性回归 ####
mod <- svyglm(MI~CDAI, study_design)
summary(mod)


#### continuous mod1 ####
Cmod1 <- svyglm(MI~CDAI, study_design, family="quasibinomial")
summary(Cmod1)


#### continuous mod2 ####
Cmod2 <- svyglm(MI~CDAI+Age + Race + EDUcation + 
                     PIR, study_design, family="quasibinomial")
summary(Cmod2)


####  continuous mod3 #####
Cmod3 <- svyglm(as.formula(
  paste0("status~CDAI+",paste(covariates, collapse = " + "))), 
                study_design, family="quasibinomial")
summary(Cmod3)


# -------------- 2. 平滑曲线拟合图 -----------------
## 使用全调整模型进行平滑曲线拟合

study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

# 模型用到的所有自变量
model_vars <- c("CDAI", "Age", "Race", "EDUcation", 
                "PIR", "Gender", "BMI", "Smoke", "DM",
                "hyptersion", "Stroke", "lipids","Marital","pa")

# 检查每个变量的缺失情况
sapply(new_dat[, model_vars], function(x) sum(is.na(x)))

# 全调整模型
mod3 <- svyglm(as.formula(
  paste0("status~CDAI+",paste(covariates, collapse = " + "))), 
                   study_design,family = quasibinomial)

# 预测概率
new_dat$predicted_prob <- predict(mod3, newdata = new_dat, type = "response")



# 绘制平滑曲线
ggplot(new_dat, aes(x = CDAI, y = predicted_prob)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "CDAI", y = "forcasted probe", 
       title = "") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("smooth_plot.png", width = 8, height = 6, dpi = 300)
ggsave("smooth_plot.pdf", width = 8, height = 6, dpi = 300)


# -------------- 3. RCS分析 -----------------
## 使用全调整模型进行RCS分析



# 准备数据
data <- new_dat
ori.weight <- 1/(study_design$prob) # 原始权重是抽样概率的倒数
mean.weight <- mean(ori.weight) # 计算平均权重
data$weights <- ori.weight/mean.weight # 标准化权重

dd <- datadist(data)
options(datadist="dd")

# 将分类变量转换为因子
factor_vars <- c("Race", "EDUcation", "Gender", 
                "Smoke", "hyptersion", "Stroke", 
                 "lipids","Marital","pa","DM")


data[factor_vars] <- lapply(data[factor_vars], as.factor)

# 运行RCS模型

# 3个结点
fit.rcs3 <- Glm(as.formula(
  paste0("status~","rcs(CDAI,3)","+",paste(covariates, collapse = " + "))), 
                data=data, family="quasibinomial", 
                weights=weights)


# 4个结点
fit.rcs4 <- Glm(as.formula(
  paste0("status~","rcs(CDAI,4)","+",paste(covariates, collapse = " + "))), 
                data=data, family="quasibinomial", 
                weights=weights)


# 5个结点
fit.rcs5 <- Glm(as.formula(
  paste0("status~","rcs(CDAI,5)","+",paste(covariates, collapse = " + "))), 
                data=data, family="quasibinomial", 
                weights=weights)


# 比较三个模型的偏差（deviance）
models_comparison <- data.frame(
  Knots = c(3, 4, 5),
  Parameters = c(18, 19, 20),
  Deviance = c(fit.rcs3$deviance, 
               fit.rcs4$deviance, 
               fit.rcs5$deviance),
  Pseudo_R2 = c(1 - fit.rcs3$deviance/fit.rcs3$null.deviance,
                1 - fit.rcs4$deviance/fit.rcs4$null.deviance,
                1 - fit.rcs5$deviance/fit.rcs5$null.deviance)
)
print(models_comparison)

# 计算三个模型的OR并绘图比较
OR3 <- Predict(fit.rcs3, CDAI, type="predictions", fun=exp, ref.zero=TRUE)
OR4 <- Predict(fit.rcs4, CDAI, type="predictions", fun=exp, ref.zero=TRUE)
OR5 <- Predict(fit.rcs5, CDAI, type="predictions", fun=exp, ref.zero=TRUE)


# 绘制三个模型的OR曲线比较
p <- ggplot() +
  geom_line(data=OR3, aes(x=CDAI, y=yhat, color="3 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR4, aes(x=CDAI, y=yhat, color="4 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR5, aes(x=CDAI, y=yhat, color="5 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_ribbon(data=OR4, aes(x=CDAI, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  scale_color_manual(values=c("3 knots"="#77bbdd", "4 knots"="#ff8899", "5 knots"="#ffdd88")) +
  labs(x = "CDAI", y = "OR (95% CI)", 
       title = "比较不同结点数的RCS模型",
       color = "结点数") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

p


# 非线性检验解释
nonlin_test <- anova(fit.rcs3)
cat("\n非线性检验结果解释：\n")
num <- as.numeric(dim(nonlin_test)[1])
cat("1. 整体效应 P =", round(nonlin_test[num,"P"], 4), "\n")
cat("2. 非线性效应 P =", round(nonlin_test[2,"P"], 4), "\n") # 0.0507


# 保存最佳的RCS图
# 绘制三个模型的OR曲线比较
p1 <- ggplot() +
  geom_line(data=OR3, aes(x=CDAI, y=yhat,), 
            linetype="solid", size=1, alpha=0.7,color="blue") +
  geom_ribbon(data=OR3, aes(x=CDAI, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  geom_text(aes(x=0.3,y=7.5,
                label = paste0("p for nonlinear = ",round(nonlin_test[2,"P"], 4))))+
  labs(x = "CDAI", y = "OR (95% CI)") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        legend.position="bottom")

p1
ggsave("RCS_plot.pdf", p1, width = 8, height = 6, dpi = 300)
ggsave("RCS_plot.png", p1, width = 8, height = 6, dpi = 300)






# -------------- 4. 阈值效应分析 -----------------

# 使用函数
covariates <- c("Age", "Race", "EDUcation", 
                "PIR", "Gender", "DM", "Smoke", "BMI",
                "hyptersion", "Stroke", "lipids","Marital","pa")

# 首先获取阈值点
result <- weighted_segmented_regression_nhanes(
  data = new_dat,
  y_var = "MI",
  x_var = "CDAI",
  covariates = covariates
)

# 保存阈值效应分析的结果表格
write.table(as.data.frame(result[["table"]]), "Threshold_results.csv",sep=",",
            quote=F, col.names = T, row.names = F)

# 使用获得的阈值点绘制分段拟合图
p2 <- plot_segmented_fit(
  data = new_dat,
  x_var = "CDAI",
  y_var = "MI",
  cutpoint = result$cutpoint,
  covariates = covariates,
  p_nonlinear = nonlin_test[2,"P"]
)
p2
# 保存图形
ggsave("segmented_regression_plot.png", p2, width = 8, height = 6, dpi = 300)
ggsave("segmented_regression_plot.pdf", p2, width = 8, height = 6, dpi = 300)


##### ________平滑曲线添加拐点画图___________ #######

p3 <- ggplot(new_dat, aes(x = CDAI, y = predicted_prob)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "CDAI", y = "forcasted probe",
       title = "") +
  geom_vline(xintercept = result$cutpoint,
             linetype = "dashed",
             color = "red",
             alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
p3
# 保存图形
ggsave("smooth_threshold.pdf", p3, width = 8, height = 6, dpi = 300)
ggsave("smooth_threshold.png", p3, width = 8, height = 6, dpi = 300)





# -------------- 5. 亚组分析 -----------------
source("../functions.R") # 调试用

##### __________性别亚组分析__________ #####
gender_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Gender",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa","DM"   
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)


##### __________年龄亚组分析__________ #####
age_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Age_cat",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa","DM"  
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)



##### __________BMI亚组分析__________ #####
BMI_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "BMI_cat",           # 分组变量
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa","DM"
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)



##### __________RACE亚组分析__________ #####
RACE_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Race",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa","DM"   
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)


##### __________ Smoke 亚组分析__________ #####
Smoke_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Smoke",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa"
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)



##### __________pa亚组分析__________ #####
PA_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "pa",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "CDAI",      # 自变量：对数转换后的CDAI
  covariates = c(                 # 协变量清单
    "Age", "Race", "EDUcation", 
    "PIR", "Gender", "BMI", "Smoke", 
    "hyptersion", "Stroke", "lipids","Marital","pa","DM"   
  ),
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)


# bind datas 
data2plot <- rbind(PA_results,Smoke_results,age_results,gender_results,
                   BMI_results,RACE_results)
# view(data2plot)

# 处理数据并绘图,函数返回的只有森林图的信息
p <- plot_subgroup_forest(data2plot, # 数据
                          clip = c(0, 10),# OR的绘图范围
                          xticks=seq(0, 10, by = 1) # x轴的刻度
                          )
p
# save plot
png("subgroup_plot.png",width = 14,height = 10, res=300,units = "in")
print(p)
dev.off()

pdf("subgroup_plot.pdf",width = 14,height = 10)
print(p)
dev.off()
