rm(list=ls())
setwd('E:/LZ/25014')
if(! dir.exists("03_results")) dir.create("03_results")
setwd('./03_results')
library(survey)
library(dplyr)
library(forestplot)
# load functions
source("../functions.R")




# 设置保留小数点后3位
options(digits=3)


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
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)
#### 单因素线性回归 ####
mod <- svyglm(MI~RAR_log, study_design)
summary(mod)


#### 单因素logistic回归 ####
mod <- svyglm(MI~RAR_log, study_design, family="quasibinomial")
summary(mod)


#### 多因素logstic回归 ####
mod_mult <- svyglm(MI~RAR_log+Age + Race + EDUcation + 
                  PIR, study_design, family="quasibinomial")
summary(mod_mult)


##### 四分位数分组 ###########
RAR_m <- svyquantile(~RAR_log,study_design,c(0.25,0.5,0.75))
RAR_m

RAR_q <- quantile(new_dat$RAR_log, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
new_dat$RAR_q4 <- cut(new_dat$RAR_log,
                       breaks = c(-Inf, RAR_q[1], RAR_q[2], RAR_q[3], Inf),
                       labels = c("Q1","Q2","Q3","Q4"),
                       right = TRUE,
                       include.lowest = TRUE)


new_dat$RAR_q4 <- factor(new_dat$RAR_q4,
                        levels = c("Q1","Q2","Q3","Q4"))
table(new_dat$RAR_q4)
# Q1   Q2   Q3   Q4 
# 1161  1148 1176 1132

#转化参照
new_dat$RAR_q4 <-relevel(new_dat$RAR_q4,"Q4")
table(new_dat$MI)
#   0    1 
# 4229  388

#转化结局的编码
new_dat$status <- new_dat$MI

# 重新设计采样
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)



#### 四分位数分组进行回归 ####
mod1 <- svyglm(status~RAR_q4, study_design,family = quasibinomial)
summary(mod1)
results_mod1 <- extract_regression_results(mod1, "model1")

mod2 <- svyglm(status~RAR_q4+Age+Race+Marital+
                         EDUcation+PIR+Gender, 
                   study_design,family = quasibinomial)
summary(mod2)
results_mod2 <- extract_regression_results(mod2, "model2")

mod3 <- svyglm(status~RAR_q4+
                   Age+Race+EDUcation+PIR+Gender+
                   Drink+BMI+Smoke+hyptersion+Stroke+lipids, 
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


# -------------- 2. 平滑曲线拟合图 -----------------
## 使用全调整模型进行平滑曲线拟合

study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)
# 模型用到的所有自变量
model_vars <- c("RAR_log", "Age", "Race", "EDUcation", 
                "PIR", "Gender", "Drink", "BMI", "Smoke", 
                "hyptersion", "Stroke", "lipids")

# 检查每个变量的缺失情况
sapply(new_dat[, model_vars], function(x) sum(is.na(x)))

# 全调整模型
mod3 <- svyglm(status~RAR_q4+
                   Age+Race+EDUcation+PIR+Gender+
                   Drink+BMI+Smoke+hyptersion+Stroke+lipids, 
                   study_design,family = quasibinomial)

# 预测概率
new_dat$predicted_prob <- predict(mod3, newdata = new_dat, type = "response")

library(ggplot2)

# 绘制平滑曲线
ggplot(new_dat, aes(x = RAR_log, y = predicted_prob)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "RAR_log", y = "forcasted probe", 
       title = "") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("smooth_plot.png", width = 8, height = 6, dpi = 300)
ggsave("smooth_plot.pdf", width = 8, height = 6, dpi = 300)


# -------------- 3. RCS分析 -----------------
## 使用全调整模型进行RCS分析

library(rms)

# 准备数据
data <- new_dat
ori.weight <- 1/(study_design$prob) # 原始权重是抽样概率的倒数
mean.weight <- mean(ori.weight) # 计算平均权重
data$weights <- ori.weight/mean.weight # 标准化权重

dd <- datadist(data)
options(datadist="dd")

# 将分类变量转换为因子
factor_vars <- c("Age", "Race", "EDUcation", 
                 "PIR", "Gender", "Drink", "BMI", "Smoke", 
                 "hyptersion", "Stroke", "CHD")

data[factor_vars] <- lapply(data[factor_vars], as.factor)

# 运行RCS模型

# 3个结点
fit.rcs3 <- Glm(status ~ rcs(RAR_log,3)+
                Age+Race+EDUcation+PIR+Gender+
                Drink+BMI+Smoke+hyptersion+Stroke+lipids, 
                data=data, family="quasibinomial", 
                weights=weights)


# 4个结点
fit.rcs4 <- Glm(status ~ rcs(RAR_log,4)+
                Age+Race+EDUcation+PIR+Gender+
                Drink+BMI+Smoke+hyptersion+Stroke+lipids, 
                data=data, family="quasibinomial", 
                weights=weights)


# 5个结点
fit.rcs5 <- Glm(status ~ rcs(RAR_log,5)+
                Age+Race+EDUcation+PIR+Gender+
                Drink+BMI+Smoke+hyptersion+Stroke+lipids, 
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
OR3 <- Predict(fit.rcs3, RAR_log, type="predictions", fun=exp, ref.zero=TRUE)
OR4 <- Predict(fit.rcs4, RAR_log, type="predictions", fun=exp, ref.zero=TRUE)
OR5 <- Predict(fit.rcs5, RAR_log, type="predictions", fun=exp, ref.zero=TRUE)

# 绘制三个模型的OR曲线比较
p <- ggplot() +
  geom_line(data=OR3, aes(x=RAR_log, y=yhat, color="3 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR4, aes(x=RAR_log, y=yhat, color="4 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR5, aes(x=RAR_log, y=yhat, color="5 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_ribbon(data=OR4, aes(x=RAR_log, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  scale_color_manual(values=c("3 knots"="#77bbdd", "4 knots"="#ff8899", "5 knots"="#ffdd88")) +
  labs(x = "RAR_log", y = "OR (95% CI)", 
       title = "比较不同结点数的RCS模型",
       color = "结点数") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

p


# 非线性检验解释
nonlin_test <- anova(fit.rcs4)
cat("\n非线性检验结果解释：\n")
num <- as.numeric(dim(nonlin_test)[1])
cat("1. 整体效应 P =", round(nonlin_test[num,"P"], 4), "\n")
cat("2. 非线性效应 P =", round(nonlin_test[2,"P"], 4), "\n") # 0.0507


# 保存最佳的RCS图
# 绘制三个模型的OR曲线比较
p1 <- ggplot() +
  geom_line(data=OR4, aes(x=RAR_log, y=yhat,), 
            linetype="solid", size=1, alpha=0.7,color="blue") +
  geom_ribbon(data=OR4, aes(x=RAR_log, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  geom_text(aes(x=-1.25,y=7.5,
                label = paste0("p for nonlinear = ",round(nonlin_test[2,"P"], 4))))+
  labs(x = "RAR_log", y = "OR (95% CI)") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        legend.position="bottom")

p1
ggsave("RCS_plot.pdf", p1, width = 8, height = 6, dpi = 300)
ggsave("RCS_plot.png", p1, width = 8, height = 6, dpi = 300)






# -------------- 4. 阈值效应分析 -----------------

# 使用函数
covariates <- c("Age", "Race", "EDUcation", 
                "PIR", "Gender", "Drink", "BMI", "Smoke", 
                "hyptersion", "Stroke", "CHD")

# 首先获取阈值点
result <- weighted_segmented_regression_nhanes(
  data = new_dat,
  y_var = "MI",
  x_var = "RAR_log",
  covariates = covariates
)

# 保存阈值效应分析的结果表格
write.table(as.data.frame(result[["table"]]), "Threshold_results.csv",sep=",",
            quote=F, col.names = T, row.names = F)

# 使用获得的阈值点绘制分段拟合图
p2 <- plot_segmented_fit(
  data = new_dat,
  x_var = "RAR_log",
  y_var = "MI",
  cutpoint = result$cutpoint,
  covariates = covariates,
  p_nonlinear = nonlin_test[2,"P"]
)

# 保存图形
ggsave("segmented_regression_plot.png", p2, width = 8, height = 6, dpi = 300)
ggsave("segmented_regression_plot.pdf", p2, width = 8, height = 6, dpi = 300)


##### ________平滑曲线添加拐点画图___________ #######

p3 <- ggplot(new_dat, aes(x = RAR_log, y = predicted_prob)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "RAR_log", y = "forcasted probe",
       title = "") +
  geom_vline(xintercept = result$cutpoint,
             linetype = "dashed",
             color = "red",
             alpha = 0.5) +
  labs(x = "log(RAR)", y = "OR (95% CI)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
p3
# 保存图形
ggsave("smooth_threshold.pdf", p3, width = 8, height = 6, dpi = 300)
ggsave("smooth_threshold.png", p3, width = 8, height = 6, dpi = 300)





# -------------- 5. 亚组分析 -----------------

##### __________性别亚组分析__________ #####
gender_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Gender",
  group_levels = c("male", "female"),
  outcome_vars = c(primary = "status"),
  exposure_vars = c(primary = "RAR_log"),
  covariates = c("Age", "Race", "EDUcation", "PIR",
                 "Drink", "BMI", "Smoke", 
                 "hyptersion", "Stroke", "CHD"),
  design_vars = c(id = "SDMVPSU", 
                 strata = "SDMVSTRA", 
                 weights = "wt"),
  family = "quasibinomial",
  interaction_test = TRUE
)



##### __________年龄亚组分析__________ #####
age_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Age",
  group_levels = c("18-45","46-60",">60"),
  outcome_vars = c(primary = "status"),
  exposure_vars = c(primary = "RAR_log"),
  covariates = c("Gender", "Race", "EDUcation", "PIR",
                 "Drink", "BMI", "Smoke", 
                 "hyptersion", "Stroke", "CHD"),
  design_vars = c(id = "SDMVPSU", 
                 strata = "SDMVSTRA", 
                 weights = "wt"),
  family = "quasibinomial",
  interaction_test = TRUE
)



##### __________BMI亚组分析__________ #####
BMI_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "BMI",
  group_levels = c("<18.5", "18.5-25", "25-30",">30"),
  outcome_vars = c(primary = "status"),
  exposure_vars = c(primary = "RAR_log"),
  covariates = c("Gender", "Race", "EDUcation", "PIR",
                 "Drink", "BMI", "Smoke", 
                 "hyptersion", "Stroke", "CHD"),
  design_vars = c(id = "SDMVPSU", 
                  strata = "SDMVSTRA", 
                  weights = "wt"),
  family = "quasibinomial",
  interaction_test = TRUE
)

# bind datas 
data2plot <- rbind(age_results,gender_results,BMI_results)




# 处理数据并绘图,函数返回的只有森林图的信息
p <- plot_subgroup_forest(data2plot, # 数据
                          clip = c(0, 10),# OR的绘图范围
                          xticks=seq(0, 10, by = 1) # x轴的刻度
                          )
# save plot
png("subgroup_plot.png",width = 14,height = 6, res=300,units = "in")
print(p)
dev.off()

pdf("subgroup_plot.pdf",width = 14,height = 6)
print(p)
dev.off()
