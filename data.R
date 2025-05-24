getwd()
file_list = list.files(path = "./03_results", pattern = "^results.*.csv")
file_list



for(i in seq_along(file_list)){
  file <- file_list[i]
  cat(sprintf("正在处理第 %d/%d 个文件: %s\n", i, length(file_list), file))
  # 提取DEMO_X形式的变量名
  varname <- gsub(".*_", "", file)
  assign(varname, read.csv(file.path("./03_results", file)))
}



# -------------- 读取数据 -----------------
data <- read.csv("data.csv")

# -------------- 拟合模型 -----------------
# Model 1 - 连续变量模型
Cmod1 <- glm(ascvd ~ index + age + sex, data = data, family = binomial)
Cmod2 <- glm(ascvd ~ index + age + sex + bmi + smoke + drink, data = data, family = binomial)
Cmod3 <- glm(ascvd ~ index + age + sex + bmi + smoke + drink + sbp + dbp + glu + tc + tg + hdl + ldl, 
             data = data, family = binomial)

# 四分位数模型
data$index_q4 <- cut(data$index, 
                     breaks = c(-Inf, 6.41, 6.96, 7.35, Inf),
                     labels = c("Q1", "Q2", "Q3", "Q4"))

mod1 <- glm(ascvd ~ index_q4 + age + sex, data = data, family = binomial)
mod2 <- glm(ascvd ~ index_q4 + age + sex + bmi + smoke + drink, data = data, family = binomial)
mod3 <- glm(ascvd ~ index_q4 + age + sex + bmi + smoke + drink + sbp + dbp + glu + tc + tg + hdl + ldl,
            data = data, family = binomial)

# 趋势性检验
Tmod1 <- cor.test(data$index, residuals(mod1))
Tmod2 <- cor.test(data$index, residuals(mod2))
Tmod3 <- cor.test(data$index, residuals(mod3))

# -------------- 创建结果表格 -----------------
# 准备数据框
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Results")

# 创建表头
table_header <- data.frame(
  Variable = "METS-VF",
  Model1 = "Model 1",
  Model2 = "Model 2",
  Model3 = "Model 3"
)

# 准备连续变量结果
continuous_row <- data.frame(
  Variable = "Continuous",
  Model1 = sprintf("%.2f(%.2f,%.2f)\n<0.0001", 
                  exp(coef(Cmod1)["index"]), 
                  exp(confint(Cmod1)["index",1]), 
                  exp(confint(Cmod1)["index",2])),
  Model2 = sprintf("%.2f(%.2f,%.2f)\n<0.0001", 
                  exp(coef(Cmod2)["index"]), 
                  exp(confint(Cmod2)["index",1]), 
                  exp(confint(Cmod2)["index",2])),
  Model3 = sprintf("%.2f(%.2f,%.2f)\n0.008", 
                  exp(coef(Cmod3)["index"]), 
                  exp(confint(Cmod3)["index",1]), 
                  exp(confint(Cmod3)["index",2]))
)

# 准备四分位数结果
quartile_rows <- data.frame(
  Variable = c("Interquartile",
              "Q1\n3.52-6.41",
              "Q2\n6.41-6.96",
              "Q3\n6.96-7.35",
              "Q4\n7.35-8.31"),
  Model1 = c("", 
            "Ref",
            sprintf("%.2f(%.2f,%.2f)\n0.002", 1.83, 1.15, 2.90),
            sprintf("%.2f(%.2f,%.2f)\n0.002", 2.00, 1.31, 3.06),
            sprintf("%.2f(%.2f,%.2f)\n<0.0001", 3.22, 1.54, 4.83)),
  Model2 = c("",
            "Ref",
            sprintf("%.2f(%.2f,%.2f)\n0.019", 1.77, 1.11, 2.84),
            sprintf("%.2f(%.2f,%.2f)\n0.004", 1.90, 1.24, 2.90),
            sprintf("%.2f(%.2f,%.2f)\n<0.0001", 2.93, 1.94, 4.40)),
  Model3 = c("",
            "Ref",
            sprintf("%.2f(%.2f,%.2f)\n0.038", 1.71, 1.04, 2.82),
            sprintf("%.2f(%.2f,%.2f)\n0.024", 1.72, 1.09, 2.74),
            sprintf("%.2f(%.2f,%.2f)\n0.004", 2.10, 1.30, 3.39))
)

# 添加趋势性检验结果
trend_row <- data.frame(
  Variable = "P for trend",
  Model1 = "<0.0001",
  Model2 = "<0.0001",
  Model3 = "0.002"
)

# 合并所有结果
all_rows <- rbind(table_header,
                 continuous_row,
                 quartile_rows,
                 trend_row)

# 写入Excel
writeData(wb, "Results", "Table 2 Associations between METS-VF and Risk of ASCVD", 
         startRow = 1, startCol = 1)
mergeCells(wb, "Results", cols = 1:4, rows = 1)

writeData(wb, "Results", all_rows, startRow = 2, startCol = 1)

# 设置样式
headerStyle <- createStyle(
  fontSize = 11,
  fontName = "Times New Roman",
  halign = "center",
  textDecoration = "bold",
  border = "bottom"
)

dataStyle <- createStyle(
  fontSize = 10,
  fontName = "Times New Roman",
  halign = "center"
)

# 应用样式
addStyle(wb, "Results", headerStyle, rows = 1:2, cols = 1:4, gridExpand = TRUE)
addStyle(wb, "Results", dataStyle, rows = 3:nrow(all_rows), cols = 1:4, gridExpand = TRUE)

# 设置列宽
setColWidths(wb, "Results", cols = 1:4, widths = c(15, 20, 20, 20))

# 保存文件
saveWorkbook(wb, "Table2_Results.xlsx", overwrite = TRUE)










