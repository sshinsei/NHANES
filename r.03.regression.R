rm(list=ls())
setwd('E:/LZ/25014')
if(! dir.exists("03_results")) dir.create("03_results")
setwd('./03_results')
library(survey)
library(dplyr)
library(forestplot)
library(rms)
library(ggplot2)
library(plotRCS)
library(mgcv) # 平滑曲线
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
# Diabetes
# hypter
# lipids






# -------------- 1. 模型构建 -----------------
options(survey.lonely.psu = "adjust")

##### 四分位数分组 ###########
index_log_m <- svyquantile(~index_log,study_design,c(0.25,0.5,0.75))
index_log_m

# index_log_q <- quantile(new_dat$index_log, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

new_dat$index_log_q4 <- cut(new_dat$index_log,
                       breaks = c(-Inf, index_log_m$index_log[1], index_log_m$index_log[2], 
                       index_log_m$index_log[3], Inf),
                       labels = c("Q1","Q2","Q3","Q4"),
                       right = TRUE,
                       include.lowest = TRUE)


new_dat$index_log_q4 <- factor(new_dat$index_log_q4,
                        levels = c("Q1","Q2","Q3","Q4"))
table(new_dat$index_log_q4)
# Q1   Q2   Q3   Q4 
# 6550 6260 6402 6318 

#转化参照
# new_dat$index_log_q4 <- relevel(new_dat$index_log_q4,"Q4") # Q4为reference
table(new_dat$MI)
#   0    1 
# 24427  1103  

#转化结局的编码
new_dat$status <- new_dat$MI

# 重新设计采样
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)



#### _________四分位数分组进行回归 ####
mod1 <- svyglm(status~index_log_q4, study_design,family = quasibinomial)
summary(mod1)
results_mod1 <- extract_regression_results(mod1, "model1")

mod2 <- svyglm(status~index_log_q4+Age+Race+EDUcation+PIR+Gender, 
                   study_design,family = quasibinomial)
summary(mod2)
results_mod2 <- extract_regression_results(mod2, "model2")

## ----------------- 协变量: 模型3调整此处---------------------- 
covariates = c(                
  "Age", "Race", "EDUcation", "PIR", "Gender", 
  "Smoke", "pa","Drink","Marital","BMI",
  "Hypertension","Diabetes"
)

mod3 <- svyglm(as.formula(
  paste0("status~index_log_q4+",paste(covariates, collapse = " + "))), 
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
table(new_dat$index_log)
index_log_m
new_dat$index_log_T <- ifelse(new_dat$index_log < index_log_m$index_log[1],1,
                       ifelse(new_dat$index_log < index_log_m$index_log[2],2,
                              ifelse(new_dat$index_log < index_log_m$index_log[3],3,4)))
table(new_dat$index_log_T)
# 1    2    3    4 
# 6529 6263 6412 6326 


study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

Tmod1 <- svyglm(status~index_log_T, study_design,family = quasibinomial)
summary(Tmod1)
results_Tmod1 <- extract_regression_results(Tmod1, "Tmodel1")

Tmod2 <- svyglm(status~index_log_T+
                          Age+Race+EDUcation+PIR+Gender, 
                        study_design,family = quasibinomial)
summary(Tmod2)
results_Tmod2 <- extract_regression_results(Tmod2, "Tmodel2")

formula_str <- paste0("status ~ index_log_T + ", paste(covariates, collapse = " + "))
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
mod <- svyglm(status~index_log, study_design)
summary(mod)


#### continuous mod1 ####
Cmod1 <- svyglm(MI~index_log, study_design, family="quasibinomial")
summary(Cmod1)
results_Cmod1 <- extract_regression_results(Cmod1, "Cmodel1")

#### continuous mod2 ####
Cmod2 <- svyglm(status~index_log+Age + Race + EDUcation + 
                     PIR, study_design, family="quasibinomial")
summary(Cmod2)
results_Cmod2 <- extract_regression_results(Cmod2, "Cmodel2")

####  continuous mod3 #####
Cmod3 <- svyglm(as.formula(
  paste0("status~index_log+",paste(covariates, collapse = " + "))), 
                study_design, family="quasibinomial")
summary(Cmod3)
results_Cmod3 <- extract_regression_results(Cmod3, "Cmodel3")



# -------------- 2. 平滑曲线拟合图 -----------------
## 使用全调整模型进行平滑曲线拟合
## 最好不要用这个，拟合的曲线没有加权，使用的是ggplot自己的拟合方式
# study_design <- svydesign(data=new_dat, 
#                           id=~SDMVPSU, 
#                           strata=~SDMVSTRA, 
#                           weights=~wt, nest=TRUE)
# 
# # 模型用到的所有自变量
# model_vars <- c("index_log", "Age", "Race", "EDUcation", 
#                 "PIR", "Gender", "BMI", "Smoke", "Diabetes",
#                 "Hypertension", "Stroke","Marital","pa")
# 
# # 检查每个变量的缺失情况
# sapply(new_dat[, model_vars], function(x) sum(is.na(x)))
# 
# # 全调整模型
# model <- svyglm(as.formula(
#   paste0("status~index_log+",paste(covariates, collapse = " + "))), 
#                    study_design,family = quasibinomial)
# 
# # 预测概率
# new_dat$predicted_prob <- predict(model, newdata = new_dat, type = "response")
# 
# 
# 
# # 绘制平滑曲线
# ggplot(new_dat, aes(x = index_log, y = predicted_prob)) +
#   geom_smooth(se = TRUE, color = "blue") + # method = "gam", 
#   labs(x = "index_log", y = "forcasted probe", 
#        title = "") +
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggsave("smooth_plot.png", width = 8, height = 6, dpi = 300)
# ggsave("smooth_plot.pdf", width = 8, height = 6, dpi = 300)



######## __________构建GAM模型_________ #########
gam_model <- gam(formula = as.formula(
                            paste0("status ~ s(index_log, k = 3) + ", 
                            paste(covariates, collapse = " + "))),
                 data = new_dat,
                 weights = wt,
                 family = quasibinomial)

# 1. 生成等距预测点
x_seq <- seq(min(new_dat$index_log, na.rm=TRUE),
             max(new_dat$index_log, na.rm=TRUE),
             length.out = 200)

# 2. 构造预测数据框（协变量用均值/众数/参考水平）
newdata_pred <- new_dat[1, , drop=FALSE]
for (v in covariates) {
  if (is.numeric(new_dat[[v]])) {
    newdata_pred[[v]] <- mean(new_dat[[v]], na.rm=TRUE)
  } else {
    newdata_pred[[v]] <- as.character(stats::na.omit(new_dat[[v]])[1])
  }
}
newdata_pred <- newdata_pred[rep(1, length(x_seq)), ]
newdata_pred$index_log <- x_seq

# 3. 预测
pred <- predict(gam_model, newdata = newdata_pred, type = "link", se.fit = TRUE)
fit <- pred$fit
se <- pred$se.fit
fit_low <- fit - 1.96 * se
fit_high <- fit + 1.96 * se

# 4. 转换为概率
prob <- plogis(fit)
prob_low <- plogis(fit_low)
prob_high <- plogis(fit_high)

# 5. 画图
ymax <- max(prob_high, na.rm = TRUE)
ymin <- min(prob_low, na.rm = TRUE)
# 适当留白
ymax <- min(1, ymax * 1.05)
ymin <- max(0, ymin * 0.95)

# 画图
png("smooth_plot_gam.png", width = 720, height = 560)
par(mar = c(5, 5, 4, 2))
plot(x_seq, prob, type = "l", col = "blue", lwd = 2,
     ylim = c(ymin, ymax), xlab = "index_log", ylab = "Predicted Probability",
     main = "GAM Smooth Curve")
lines(x_seq, prob_low, col = "red", lty = 2)
lines(x_seq, prob_high, col = "red", lty = 2)
legend("topright", legend = c("Fitted curve", "95% CI"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1), bty = "n")
dev.off()

# PDF同理
pdf("smooth_plot_gam.pdf", width = 8, height = 6)
par(mar = c(5, 5, 4, 2))
plot(x_seq, prob, type = "l", col = "blue", lwd = 2,
     ylim = c(ymin, ymax), xlab = "index_log", ylab = "Predicted Probability",
     main = "GAM Smooth Curve")
lines(x_seq, prob_low, col = "red", lty = 2)
lines(x_seq, prob_high, col = "red", lty = 2)
legend("topright", legend = c("Fitted curve", "95% CI"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1), bty = "n")
dev.off()

# 输出模型诊断信息
summary(gam_model)



# -------------- 3. RCS分析 -----------------
## 使用全调整模型进行RCS分析
rcsplot(
  data=new_dat,
  outcome = 'MI', exposure='index_log',
  covariates = covariates,
  xlab="index_log",ylab="OR (95% CI)",knots = knot(3),pvalue = TRUE,
  fontfamily = 'sans',linecolor = 'red',linesize = 0.5
)

data <- new_dat
# 准备数据
# 将分类变量转换为因子
factor_vars <- c("Race", "EDUcation", "Gender", 
                 "Smoke", "Drink", "Stroke", 
                 "Marital","pa","Diabetes")


data[factor_vars] <- lapply(data[factor_vars], as.factor)




# 使用函数拟合3、4、5个结点的模型
fit.rcs3 <- fit_rcs_model("index_log", 3, study_design, covariates)
fit.rcs4 <- fit_rcs_model("index_log", 4, study_design, covariates)
fit.rcs5 <- fit_rcs_model("index_log", 5, study_design, covariates)

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
OR3 <- Predict(fit.rcs3, "index_log", type="predictions", fun=exp, ref.zero=TRUE)
OR4 <- Predict(fit.rcs4, "index_log", type="predictions", fun=exp, ref.zero=TRUE)
OR5 <- Predict(fit.rcs5, "index_log", type="predictions", fun=exp, ref.zero=TRUE)



# 对三个模型都计算OR=1的点
or1_point3 <- find_or_1(OR3)
or1_point4 <- find_or_1(OR4)
or1_point5 <- find_or_1(OR5)

# 打印结果
cat("\nOR=1对应的index_log值：\n")
cat("3结点模型：", round(or1_point3, 3), "\n")
cat("4结点模型：", round(or1_point4, 3), "\n")
cat("5结点模型：", round(or1_point5, 3), "\n")

# 绘制三个模型的OR曲线比较
p <- ggplot() +
  geom_line(data=OR3, aes(x=index_log, y=yhat, color="3 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR4, aes(x=index_log, y=yhat, color="4 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR5, aes(x=index_log, y=yhat, color="5 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_ribbon(data=OR4, aes(x=index_log, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  scale_color_manual(values=c("3 knots"="#77bbdd", "4 knots"="#ff8899", "5 knots"="#ffdd88")) +
  labs(x = "index_log", y = "OR (95% CI)", 
       title = "比较不同结点数的RCS模型",
       color = "结点数") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
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
  geom_line(data=OR3, aes(x=index_log, y=yhat), 
            linetype="solid", size=1, alpha=0.7,color="blue") +
  geom_ribbon(data=OR3, aes(x=index_log, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  geom_vline(xintercept=or1_point3, linetype=2, color="red") +
  geom_text(aes(x=or1_point3+1,y=2,
                label = paste0("p for nonlinear: ",round(nonlin_test[2,"P"], 4))))+
  geom_text(aes(x=or1_point3+1,y=2.25,
                label = paste0("p for all: ",
                               ifelse(round(nonlin_test[ncol(nonlin_test),"P"], 4) == 0,
                                      "p<0.0001",round(nonlin_test[ncol(nonlin_test),"P"], 4)))))+
  labs(x = "index_log", y = "OR (95% CI)") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        legend.position="bottom")

p1
ggsave("RCS_plot.pdf", p1, width = 8, height = 6, dpi = 300)
ggsave("RCS_plot.png", p1, width = 8, height = 6, dpi = 300)



# -------------- 4. 阈值效应分析 -----------------
# p for nonlinear 不显著没必要做了


# 首先获取阈值点
result <- weighted_segmented_regression_nhanes(
  data = new_dat,
  y_var = "MI",
  x_var = "index_log",
  covariates = covariates
)

# 保存阈值效应分析的结果表格
write.table(as.data.frame(result[["table"]]), "Threshold_results.csv",sep=",",
            quote=F, col.names = T, row.names = F)

# 使用获得的阈值点绘制分段拟合图
p2 <- plot_segmented_fit(
  data = new_dat,
  x_var = "index_log",
  y_var = "MI",
  cutpoint = result$cutpoint, # or1_point3
  covariates = covariates,
  p_nonlinear = nonlin_test[2,"P"]
)
p2
# 保存图形
ggsave("segmented_regression_plot.png", p2, width = 8, height = 6, dpi = 300)
ggsave("segmented_regression_plot.pdf", p2, width = 8, height = 6, dpi = 300)




##### ________平滑曲线添加拐点画图___________ #######

# p3 <- ggplot(new_dat, aes(x = index_log, y = predicted_prob)) +
#   geom_smooth(se = TRUE, color = "blue") +
#   labs(x = "index_log", y = "forcasted probe",
#        title = "") +
#   geom_vline(xintercept = result$cutpoint, # or1_point3
#              linetype = "dashed",
#              color = "red",
#              alpha = 0.5) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
#   
# p3
# # 保存图形
# ggsave("smooth_threshold.pdf", p3, width = 8, height = 6, dpi = 300)
# ggsave("smooth_threshold.png", p3, width = 8, height = 6, dpi = 300)





# -------------- 5. 亚组分析 -----------------
# source("../functions.R") # 调试用

##### __________性别亚组分析__________ #####
gender_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Gender",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
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
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
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
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
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
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
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
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)



##### __________ Drink 亚组分析__________ #####
Drink_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Drink",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU",   
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)


##### __________ Diabetes 亚组分析__________ #####
Diabetes_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Diabetes",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
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
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)

##### __________ PIR 亚组分析__________ #####
PIR_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "PIR_cat",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)

##### __________ Hypertension 亚组分析__________ #####
Hypertension_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Hypertension",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index_log",      # 自变量：对数转换后的index_log
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)

# bind datas 
data2plot <- rbind(age_results,gender_results,RACE_results,BMI_results,
                   PA_results,Hypertension_results, Diabetes_results)
# view(data2plot)

# 处理数据并绘图,函数返回的只有森林图的信息
p <- plot_subgroup_forest(data2plot, # 数据
                          clip = c(0, 3),# OR的绘图范围
                          xticks=seq(0, 3, by = 1) # x轴的刻度
                          )
p
# save plot
png("subgroup_plot.png",width = 14,height = 10, res=300,units = "in")
print(p)
dev.off()

pdf("subgroup_plot.pdf",width = 14,height = 10)
print(p)
dev.off()


# 基于以上运行的所有结果，将结果整理为表格

# 首先打印模型系数名称以进行检查
print("Continuous Models Coefficients:")
print("Model 1:")
print(names(coef(Cmod1)))
print("Model 2:")
print(names(coef(Cmod2)))
print("Model 3:")
print(names(coef(Cmod3)))

print("\nQuartile Models Coefficients:")
print("Model 1:")
print(names(coef(mod1)))
print("Model 2:")
print(names(coef(mod2)))
print("Model 3:")
print(names(coef(mod3)))

print("\nTrend Models Coefficients:")
print("Model 1:")
print(names(coef(Tmod1)))
print("Model 2:")
print(names(coef(Tmod2)))
print("Model 3:")
print(names(coef(Tmod3)))

# 然后定义函数
format_regression_table <- function(continuous_models, quartile_models, trend_models) {
  # 创建一个数据框来存储结果
  results_table <- data.frame(
    METS_VF = character(),
    Model1 = character(),
    Model2 = character(),
    Model3 = character(),
    stringsAsFactors = FALSE
  )
  
  # 获取模型系数和统计量
  get_model_stats <- function(model, coef_name) {
    coef_summary <- summary(model)$coefficients
    if(coef_name %in% rownames(coef_summary)) {
      estimate <- coef_summary[coef_name, "Estimate"]
      std_error <- coef_summary[coef_name, "Std. Error"]
      p_value <- coef_summary[coef_name, "Pr(>|t|)"]
      
      or <- exp(estimate)
      ci_lower <- exp(estimate - 1.96 * std_error)
      ci_upper <- exp(estimate + 1.96 * std_error)
      
      p_text <- ifelse(p_value < 0.0001, "<0.0001", sprintf("%.3f", p_value))
      
      return(sprintf("%.2f(%.2f,%.2f)\n%s", or, ci_lower, ci_upper, p_text))
    } else {
      return("NA")
    }
  }
  
  # 添加连续变量结果
  continuous_row <- data.frame(
    Index = "Continuous",
    Model1 = get_model_stats(Cmod1, "index_log"),
    Model2 = get_model_stats(Cmod2, "index_log"),
    Model3 = get_model_stats(Cmod3, "index_log")
  )
  
  # 获取四分位数的范围
  q_ranges <- sprintf("%.3f~%.3f", 
                     c(-Inf, as.numeric(index_log_m$index_log)),
                     c(as.numeric(index_log_m$index_log), Inf))
  
  # 添加四分位数分组结果
  quartile_rows <- data.frame(
    Index = c("Interquartile",
                paste("Q1\n", q_ranges[1]),
                paste("Q2\n", q_ranges[2]),
                paste("Q3\n", q_ranges[3]),
                paste("Q4\n", q_ranges[4])),
    Model1 = c("", "Ref",
               get_model_stats(mod1, "index_log_q4Q2"),
               get_model_stats(mod1, "index_log_q4Q3"),
               get_model_stats(mod1, "index_log_q4Q4")),
    Model2 = c("", "Ref",
               get_model_stats(mod2, "index_log_q4Q2"),
               get_model_stats(mod2, "index_log_q4Q3"),
               get_model_stats(mod2, "index_log_q4Q4")),
    Model3 = c("", "Ref",
               get_model_stats(mod3, "index_log_q4Q2"),
               get_model_stats(mod3, "index_log_q4Q3"),
               get_model_stats(mod3, "index_log_q4Q4"))
  )
  
  # 添加趋势性检验结果
  trend_row <- data.frame(
    Index = "P for trend",
    Model1 = get_model_stats(Tmod1, "index_log_T"),
    Model2 = get_model_stats(Tmod2, "index_log_T"),
    Model3 = get_model_stats(Tmod3, "index_log_T")
  )
  
  # 合并所有结果
  results_table <- rbind(continuous_row, quartile_rows, trend_row)
  
  # 保存为CSV文件
  write.csv(results_table, "regression_results_table.csv", row.names = FALSE)
  
  # 使用openxlsx包创建Excel文件
  library(openxlsx)
  wb <- createWorkbook()
  addWorksheet(wb, "Regression Results")
  
  # 写入数据
  writeData(wb, "Regression Results", results_table)
  
  # 设置列宽
  setColWidths(wb, "Regression Results", cols = 1:4, widths = c(20, 25, 25, 25))
  
  # 创建样式
  headerStyle <- createStyle(
    fontSize = 11,
    fontName = "Times New Roman",
    halign = "center",
    border = "bottom",
    borderStyle = "medium",
    textDecoration = "bold"
  )
  
  dataStyle <- createStyle(
    fontSize = 10,
    fontName = "Times New Roman",
    halign = "center",
    border = "bottom",
    borderStyle = "thin"
  )
  
  # 应用样式
  addStyle(wb, "Regression Results", headerStyle, rows = 1, cols = 1:4)
  addStyle(wb, "Regression Results", dataStyle, rows = 2:nrow(results_table), cols = 1:4, gridExpand = TRUE)
  
  # 保存Excel文件
  saveWorkbook(wb, "regression_results_table.xlsx", overwrite = TRUE)
  
  return(results_table)
}

# 调用函数生成表格
regression_table <- format_regression_table(
  continuous_models = list(Cmod1, Cmod2, Cmod3),
  quartile_models = list(mod1, mod2, mod3),
  trend_models = list(Tmod1, Tmod2, Tmod3)
)

# 打印表格
print(regression_table)

