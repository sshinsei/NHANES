rm(list=ls())
setwd('E:/LZ/25070')
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




new_dat <- read.csv("../02_data/cleanData.csv",row.names = 1)

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
# Education
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

## ----------------- 协变量: 模型3调整此处---------------------- 
covariates = c("Age", "Race", "Education", "PIR", "Gender", 
               # 总胆固醇  白蛋白(g/L)
               "TG_mg","albumin","ALT","AST",
               "Smoke", "pa","Marital","Drink","BMI",
               "Hypertension","Diabetes","CHD")





# -------------- 1. 模型构建 ----------------- 
options(survey.lonely.psu = "adjust")

##### 四分位数分组 ###########
index_m <- svyquantile(~index,study_design,c(0.25,0.5,0.75))
index_m

# index_q <- quantile(new_dat$index, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

new_dat$index_q4 <- cut(new_dat$index,
                       breaks = c(min(new_dat$index), index_m$index[1], index_m$index[2], 
                       index_m$index[3], max(new_dat$index)),
                       labels = c("Q1","Q2","Q3","Q4"),
                       right = TRUE,
                       include.lowest = TRUE)


new_dat$index_q4 <- factor(new_dat$index_q4,
                        levels = c("Q1","Q2","Q3","Q4"))
table(new_dat$index_q4)
# Q1   Q2   Q3   Q4 
# 5901 6486 6393 6421 

#转化参照
# new_dat$index_q4 <- relevel(new_dat$index_q4,"Q4") # Q4为reference
table(new_dat$Stroke)
#   0    1 
# 24279   922 

## 转化结局的编码------
new_dat$status <- new_dat$Stroke

# 重新设计采样
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)



#### _________四分位数分组进行回归 ####
mod1 <- svyglm(status~index_q4, study_design,family = quasibinomial)
summary(mod1)
results_mod1 <- extract_regression_results(mod1, "model1")

mod2 <- svyglm(status~index_q4+Age+Race+Education+PIR+Gender, 
                   study_design,family = quasibinomial)
summary(mod2)
results_mod2 <- extract_regression_results(mod2, "model2")



mod3 <- svyglm(as.formula(
  paste0("status~index_q4+",paste(covariates, collapse = " + "))), 
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
table(new_dat$index)
index_m
new_dat$index_T <- ifelse(new_dat$index < index_m$index[1],1,
                       ifelse(new_dat$index < index_m$index[2],2,
                              ifelse(new_dat$index < index_m$index[3],3,4)))
table(new_dat$index_T)
# 1    2    3    4 
# 6529 6263 6412 6326 


study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

Tmod1 <- svyglm(status~index_T, study_design,family = quasibinomial)
summary(Tmod1)
results_Tmod1 <- extract_regression_results(Tmod1, "Tmodel1")

Tmod2 <- svyglm(status~index_T+
                          Age+Race+Education+PIR+Gender, 
                        study_design,family = quasibinomial)
summary(Tmod2)
results_Tmod2 <- extract_regression_results(Tmod2, "Tmodel2")

formula_str <- paste0("status ~ index_T + ", paste(covariates, collapse = " + "))
Tmod3 <- svyglm(as.formula(formula_str),
                design = study_design,
                family = quasibinomial)
summary(Tmod3)
results_Tmod3 <- extract_regression_results(Tmod3, "Tmodel3")
# 打印所有结果
cat("\nTModel 1 Results:\n")
print(results_Tmod1)
cat("\nTModel 2 Results:\n")
print(results_Tmod2)
cat("\nTModel 3 Results:\n")
print(results_Tmod3)

####  _______ 自变量作为连续变量分析_________ ####
study_design <- svydesign(data=new_dat, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~wt, nest=TRUE)

#### 单因素线性回归 ####
mod <- svyglm(status~index, study_design)
summary(mod)


#### continuous mod1 ####
Cmod1 <- svyglm(status~index, study_design, family="quasibinomial")
summary(Cmod1)
results_Cmod1 <- extract_regression_results(Cmod1, "Cmodel1")

#### continuous mod2 ####
Cmod2 <- svyglm(status~index+Age + Race + Education + 
                     PIR, study_design, family="quasibinomial")
summary(Cmod2)
results_Cmod2 <- extract_regression_results(Cmod2, "Cmodel2")

####  continuous mod3 ####
Cmod3 <- svyglm(as.formula(
  paste0("status~index+",paste(covariates, collapse = " + "))), 
                study_design, family="quasibinomial")
summary(Cmod3)
results_Cmod3 <- extract_regression_results(Cmod3, "Cmodel3")
# 打印所有结果
cat("\nCModel 1 Results:\n")
print(results_Cmod1)
cat("\nCModel 2 Results:\n")
print(results_Cmod2)
cat("\nCModel 3 Results:\n")
print(results_Cmod3)



# -------------- 2. RCS分析 -----------------
######## ________使用 rcsplot() ___________ ##########
rcsplot(
  data=new_dat,
  outcome = 'status', exposure='index',
  covariates = covariates,
  xlab="index",ylab="OR (95% CI)",knots = knot(4),pvalue = TRUE,
  fontfamily = 'sans',linecolor = 'red',linesize = 0.5
)


######## ________手动构建___________ ##########
data <- new_dat
# 准备数据
# 将分类变量转换为因子
factor_vars <- c("Race", "Education", "Gender", 
                 "Smoke", "pa","Marital","Drink",
                 "Hypertension","Diabetes", "CHD","MI")


data[factor_vars] <- lapply(data[factor_vars], as.factor)




# 使用函数拟合3、4、5个结点的模型
fit.rcs3 <- fit_rcs_model("status","index", 3, study_design, covariates)
fit.rcs4 <- fit_rcs_model("status","index", 4, study_design, covariates)
fit.rcs5 <- fit_rcs_model("status","index", 5, study_design, covariates)

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
OR3 <- Predict(fit.rcs3, "index", type="predictions", fun=exp, ref.zero=TRUE)
OR4 <- Predict(fit.rcs4, "index", type="predictions", fun=exp, ref.zero=TRUE)
OR5 <- Predict(fit.rcs5, "index", type="predictions", fun=exp, ref.zero=TRUE)



# 对三个模型都计算OR=1的点
or1_point3 <- find_or_1(OR3)
or1_point4 <- find_or_1(OR4)
or1_point5 <- find_or_1(OR5)

# 打印结果
cat("\nOR=1对应的index值：\n")
cat("3结点模型：", round(or1_point3, 4), "\n")
cat("4结点模型：", round(or1_point4, 4), "\n")
cat("5结点模型：", round(or1_point5, 4), "\n")

# 绘制三个模型的OR曲线比较
p <- ggplot() +
  geom_line(data=OR3, aes(x=index, y=yhat, color="3 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR4, aes(x=index, y=yhat, color="4 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_line(data=OR5, aes(x=index, y=yhat, color="5 knots"), 
            linetype="solid", size=1, alpha=0.7) +
  geom_ribbon(data=OR4, aes(x=index, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  scale_color_manual(values=c("3 knots"="#77bbdd", "4 knots"="#ff8899", "5 knots"="#ffdd88")) +
  labs(x = "index", y = "OR (95% CI)", 
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
  geom_line(data=OR3, aes(x=index, y=yhat), 
            linetype="solid", size=1, alpha=0.7,color="blue") +
  geom_ribbon(data=OR3, aes(x=index, ymin=lower, ymax=upper), 
              alpha=0.1, fill="grey") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  geom_vline(xintercept=or1_point4, linetype=2, color="red") +
  geom_text(aes(x=or1_point3+1,y=2,
                label = paste0("p for nonlinear: ",round(nonlin_test[2,"P"], 4))))+
  geom_text(aes(x=or1_point3+1,y=2.25,
                label = paste0("p for all: ",
                               ifelse(round(nonlin_test[ncol(nonlin_test),"P"], 4) == 0,
                                      "p<0.0001",round(nonlin_test[ncol(nonlin_test),"P"], 4)))))+
  labs(x = "index", y = "OR (95% CI)") +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        legend.position="bottom")

p1
ggsave("RCS_plot.pdf", p1, width = 8, height = 6, dpi = 300)
ggsave("RCS_plot.png", p1, width = 8, height = 6, dpi = 300)


# # -------------- 3. 平滑曲线拟合图 -----------------



# 使用示例：
model <- plot_smooth_curve(
  data = new_dat,
  x_var = "index",
  y_var = "Stroke",
  covariates = covariates,
  study_design = study_design,
  output_prefix = "smooth"
)

# # -------------- 4. 阈值效应分析 -----------------
# # p for nonlinear 不显著没必要做了
# 
# 
result <- weighted_segmented_regression_nhanes(
  data = new_dat,
  y_var = "Stroke",
  x_var = "index",
  weight_var = "wt",
  covariates = covariates
)

# 保存阈值效应分析的结果表格
write.table(as.data.frame(result[["table"]]), "Threshold_results.csv",sep=",",
            quote=F, col.names = T, row.names = F)

# # 使用获得的阈值点绘制分段拟合图
# p2 <- plot_segmented_fit(
#   data = new_dat,
#   x_var = "index",
#   y_var = "MI",
#   cutpoint = result$cutpoint, # or1_point3
#   covariates = covariates,
#   p_nonlinear = nonlin_test[2,"P"]
# )
# p2
# # 保存图形
# ggsave("segmented_regression_plot.png", p2, width = 8, height = 6, dpi = 300)
# ggsave("segmented_regression_plot.pdf", p2, width = 8, height = 6, dpi = 300)
# 
# 
# 
# ##### ________平滑曲线添加拐点画图___________ #######
# 
# # p3 <- ggplot(new_dat, aes(x = index, y = predicted_prob)) +
# #   geom_smooth(se = TRUE, color = "blue") +
# #   labs(x = "index", y = "forcasted probe",
# #        title = "") +
# #   geom_vline(xintercept = result$cutpoint, # or1_point3
# #              linetype = "dashed",
# #              color = "red",
# #              alpha = 0.5) +
# #   theme_bw() +
# #   theme(panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank())
# #   
# # p3
# # # 保存图形
# # ggsave("smooth_threshold.pdf", p3, width = 8, height = 6, dpi = 300)
# # ggsave("smooth_threshold.png", p3, width = 8, height = 6, dpi = 300)
# 
# 



# -------------- 5. 亚组分析 -----------------
# source("../functions.R") # 调试用

##### __________性别亚组分析__________ #####
gender_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "Gender",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
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
  exposure_vars = "index",      # 自变量：对数转换后的index
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)


##### __________ CHD 亚组分析__________ #####
CHD_results <- perform_subgroup_analysis(
  data = new_dat,
  group_var = "CHD",           # 分组变量：性别
  outcome_vars = "status",        # 因变量：疾病状态
  exposure_vars = "index",      # 自变量：对数转换后的index
  covariates = covariates,
  design_vars = c(                # 复杂抽样设计变量
    id = "SDMVPSU", 
    strata = "SDMVSTRA", 
    weights = "wt"
  )
)




# bind datas 
data2plot <- rbind(age_results,gender_results,BMI_results,PA_results,
                   Drink_results,Smoke_results,
                   Hypertension_results, Diabetes_results,CHD_results)
# view(data2plot)

# 处理数据并绘图,函数返回的只有森林图的信息
p <- plot_subgroup_forest(data2plot, # 数据
                          clip = c(0.8, 1.3),# OR的绘图范围
                          xticks=seq(0.8, 1.3, by = 0.1) # x轴的刻度
                          )
p
# save plot
png("subgroup_plot.png",width = 14,height = 10, res=300,units = "in")
print(p)
dev.off()

pdf("subgroup_plot.pdf",width = 14,height = 10)
print(p)
dev.off()


# 将结果整理为表格-----------

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



# 调用函数生成表格
regression_table <- format_regression_table(
  continuous_models = list(Cmod1, Cmod2, Cmod3),
  quartile_models = list(mod1, mod2, mod3),
  trend_models = list(Tmod1, Tmod2, Tmod3),
  exposure_var = "index",
  quantile_data = index_m
)

# 打印表格
# print(regression_table)









