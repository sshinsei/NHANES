# 加载必要的包
library(survey)
library(dplyr)
library(ggplot2)
library(knitr)
library(rms)
library(mgcv)

#### __________定义提取回归结果的函数__________ ####
extract_regression_results <- function(model, model_name) {
  # 捕获回归结果
  reg_output <- capture.output(summary(model))
  
  # 提取系数表格部分
  coef_lines <- reg_output[grep("^\\s*\\w+\\s+[-0-9.]+\\s+[-0-9.]+\\s+[-0-9.]+\\s+[-0-9.e]+", reg_output)]
  
  # 如果没有找到匹配的行,返回NULL
  if(length(coef_lines) == 0) {
    warning("No coefficient lines found in regression output")
    return(NULL)
  }
  
  # 只保留前4行
  coef_lines <- head(coef_lines, 3)
  
  # 创建数据框
  coef_df <- data.frame(
    do.call(rbind, strsplit(trimws(coef_lines), "\\s+"))
  )
  
  # 检查数据框是否为空
  if(nrow(coef_df) == 0) {
    warning("Empty coefficient data frame")
    return(NULL)
  }
  
  # 设置列名
  names(coef_df) <- c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
  
  # 转换为数值型
  coef_df$Estimate <- as.numeric(as.character(coef_df$Estimate))
  coef_df$`Std. Error` <- as.numeric(as.character(coef_df$`Std. Error`))
  
  # 计算OR和CI
  coef_df$OR <- exp(coef_df$Estimate)
  coef_df$lower <- exp(coef_df$Estimate - 1.96 * coef_df$`Std. Error`)
  coef_df$upper <- exp(coef_df$Estimate + 1.96 * coef_df$`Std. Error`)
  
  # 保留三位小数
  coef_df$OR <- round(coef_df$OR, 3)
  coef_df$lower <- round(coef_df$lower, 3)
  coef_df$upper <- round(coef_df$upper, 3)
  
  # 保存为CSV
  write.csv(coef_df, paste0("results_", model_name, ".csv"), row.names = FALSE)
  
  return(coef_df)
}



#### __________创建RCS模型拟合函数__________ ##############
fit_rcs_model <- function(outcome_var,exposure_var, knots, study_design,
                          covariates) {
  formula_str <- paste0(outcome_var,"~rcs(", exposure_var, ",", knots, ")+", paste(covariates, collapse = "+"))
  fit <- svyglm(formula_str, study_design, family = quasibinomial)
  
  data <- fit$survey.design$variables
  ori.weight <- 1/(study_design$prob)
  mean.weight <- mean(ori.weight)
  data$weights <- ori.weight/mean.weight
  
  # 设置datadist
  dd <- datadist(data)
  assign("dd", dd, envir = .GlobalEnv)  # 将dd保存到全局环境
  options(datadist="dd")
  
  fit.rcs <- Glm(formula = formula(formula_str), 
                 data = data, 
                 family = "quasibinomial", 
                 weights = weights)
  
  return(fit.rcs)
}



##### __________找到OR=1的index值（使用3结点模型）__________ ###########
find_or_1 <- function(OR_data) {
  # 找到OR跨过1的位置
  idx <- which(diff(sign(OR_data$yhat - 1)) != 0)
  
  if(length(idx) > 0) {
    # 使用线性插值找到精确的x值
    x1 <- OR_data$index[idx]
    x2 <- OR_data$index[idx + 1]
    y1 <- OR_data$yhat[idx]
    y2 <- OR_data$yhat[idx + 1]
    
    # 线性插值公式：x = x1 + (1 - y1)*(x2 - x1)/(y2 - y1)
    x_intercept <- x1 + (1 - y1) * (x2 - x1)/(y2 - y1)
    return(x_intercept)
  } else {
    return(NA)
  }
}


#### __________阈值效应分析__________ ####
weighted_segmented_regression_nhanes <- function(data, 
                                                 y_var,           
                                                 x_var,           
                                                 covariates,      
                                                 weight_var = "WTMEC2YR",
                                                 strata_var = "SDMVSTRA",
                                                 psu_var = "SDMVPSU") {
  # 1. 构建加权设计
  design <- svydesign(
    id = as.formula(paste0("~", psu_var)),
    strata = as.formula(paste0("~", strata_var)),
    weights = as.formula(paste0("~", weight_var)),
    nest = TRUE,
    data = data
  )
  
  # 构建协变量公式部分
  if(length(covariates) > 0) {
    cov_formula <- paste(covariates, collapse = " + ")
  } else {
    cov_formula <- ""
  }
  
  # 内部函数：给定cutpoint计算logLik
  get_loglik <- function(cutpoint, data_tmp) {
    # 创建分段变量
    data_tmp$x_split <- ifelse(data_tmp[[x_var]] <= cutpoint,
                              data_tmp[[x_var]],
                              cutpoint + (data_tmp[[x_var]] - cutpoint))
    
    # 构建完整公式
    if(cov_formula != "") {
      full_formula <- paste(y_var, "~ x_split +", cov_formula)
    } else {
      full_formula <- paste(y_var, "~ x_split")
    }
    
    # 使用gam拟合模型
    model_tmp <- gam(as.formula(full_formula), 
                    data = data_tmp,
                    family = quasibinomial(),
                    weights = data_tmp[[weight_var]])
    
    return(list(logLik = as.numeric(logLik(model_tmp)), model = model_tmp))
  }
  
  # Step 1: 初筛5%-95%之间，每隔5%
  x_values <- data[[x_var]]
  quantiles_all <- quantile(x_values, probs = seq(0.05, 0.95, 0.05), na.rm = TRUE)
  loglik_list <- lapply(quantiles_all, function(cp) get_loglik(cp, data))
  loglik_values <- sapply(loglik_list, function(x) x$logLik)
  best_idx <- which.max(loglik_values)
  best_cut_init <- quantiles_all[best_idx]
  
  # Step 2: ±4%范围内缩小
  tp1 <- seq(0.05, 0.95, 0.05)[best_idx]
  tp2.min <- max(tp1 - 0.04, 0.05)
  tp2.max <- min(tp1 + 0.04, 0.95)
  
  tp.pctlrange <- quantile(x_values, probs = c(tp2.min, tp2.max), na.rm = TRUE)
  tp.range <- unique(x_values[x_values > tp.pctlrange[1] & x_values < tp.pctlrange[2]])
  
  # Step 3: 递归缩小范围直到找到最优切点
  while(length(tp.range) > 5) {
    tmp.pct3 <- quantile(tp.range, probs = c(0, 0.25, 0.5, 0.75, 1), type = 3)
    tmp.llk3 <- rep(NA, 3)
    
    for(k in 2:4) {
      tmp.X <- (x_values > tmp.pct3[k]) * (x_values - tmp.pct3[k])
      data_tmp <- cbind(data, tmp.X)
      tmp.mdl <- gam(as.formula(paste(y_var, "~", x_var, "+ tmp.X +", cov_formula)),
                    data = data_tmp,
                    family = quasibinomial(),
                    weights = data_tmp[[weight_var]])
      tmp.llk3[k-1] <- logLik(tmp.mdl)
    }
    
    tmp.min3 <- which.max(tmp.llk3)
    tp.range <- tp.range[tp.range >= tmp.pct3[tmp.min3] & tp.range <= tmp.pct3[tmp.min3+2]]
  }
  
  # 在最终范围内找到最优切点
  if(length(tp.range) > 0) {
    if(length(tp.range) == 1) {
      final_cut <- tp.range[1]
    } else {
      tmp.llk <- rep(NA, length(tp.range))
      for(k in 1:length(tp.range)) {
        tmp.X <- (x_values > tp.range[k]) * (x_values - tp.range[k])
        data_tmp <- cbind(data, tmp.X)
        tmp.mdl <- gam(as.formula(paste(y_var, "~", x_var, "+ tmp.X +", cov_formula)),
                      data = data_tmp,
                      family = quasibinomial(),
                      weights = data_tmp[[weight_var]])
        tmp.llk[k] <- logLik(tmp.mdl)
      }
      final_cut <- tp.range[which.max(tmp.llk)]
    }
  } else {
    final_cut <- tp.pctlrange[1]
  }
  
  # 创建分段变量用于最终模型
  data$x_below <- ifelse(data[[x_var]] <= final_cut, data[[x_var]], final_cut)
  data$x_above <- ifelse(data[[x_var]] > final_cut, data[[x_var]] - final_cut, 0)
  
  # 构建三个不同的模型公式
  if(length(covariates) > 0) {
    cov_formula <- paste(covariates, collapse = " + ")
    # 模型0：包含x_var和x_above
    formula0 <- paste0(y_var, " ~ ", x_var, " + x_above + ", cov_formula)
    # 模型1：包含x_below和x_above
    formula1 <- paste0(y_var, " ~ x_below + x_above + ", cov_formula)
    # 模型2：只包含x_var
    formula2 <- paste0(y_var, " ~ ", x_var, " + ", cov_formula)
  } else {
    formula0 <- paste0(y_var, " ~ ", x_var, " + x_above")
    formula1 <- paste0(y_var, " ~ x_below + x_above")
    formula2 <- paste0(y_var, " ~ ", x_var)
  }
  
  # 拟合三个模型
  model0 <- gam(as.formula(formula0), 
                data = data,
                family = quasibinomial(),
                weights = data[[weight_var]])
  
  model1 <- gam(as.formula(formula1), 
                data = data,
                family = quasibinomial(),
                weights = data[[weight_var]])
  
  model2 <- gam(as.formula(formula2), 
                data = data,
                family = quasibinomial(),
                weights = data[[weight_var]])
  
  # 计算对数似然比
  # 使用binomial分布重新拟合模型用于计算对数似然比
  model0_ll <- gam(as.formula(formula0), 
                   data = data,
                   family = binomial(),
                   weights = data[[weight_var]])
  
  model1_ll <- gam(as.formula(formula1), 
                   data = data,
                   family = binomial(),
                   weights = data[[weight_var]])
  
  model2_ll <- gam(as.formula(formula2), 
                   data = data,
                   family = binomial(),
                   weights = data[[weight_var]])
  
  # 计算对数似然比
  # 模型0 vs 模型2的比较
  ll_ratio_0_2 <- 2 * (logLik(model0_ll)[1] - logLik(model2_ll)[1])
  p_value_0_2 <- 1 - pchisq(ll_ratio_0_2, df = 1)
  
  # 模型1 vs 模型2的比较
  ll_ratio_1_2 <- 2 * (logLik(model1_ll)[1] - logLik(model2_ll)[1])
  p_value_1_2 <- 1 - pchisq(ll_ratio_1_2, df = 1)
  
  # 提取结果并计算OR
  coef_summary <- summary(model1)$p.table
  
  # 创建结果表格（保持原有格式）
  results_table <- data.frame(
    "Inflection point" = c(paste0("<=", round(final_cut, 1)), 
                           paste0(">", round(final_cut, 1)),
                           "Log-likelihood ratio"),
    "Adjusted OR (95% CI)" = c(
      sprintf("%.2f (%.2f-%.2f)", 
              exp(coef_summary["x_below", "Estimate"]),
              exp(coef_summary["x_below", "Estimate"] - 1.96 * coef_summary["x_below", "Std. Error"]),
              exp(coef_summary["x_below", "Estimate"] + 1.96 * coef_summary["x_below", "Std. Error"])),
      sprintf("%.2f (%.2f-%.2f)", 
              exp(coef_summary["x_above", "Estimate"]),
              exp(coef_summary["x_above", "Estimate"] - 1.96 * coef_summary["x_above", "Std. Error"]),
              exp(coef_summary["x_above", "Estimate"] + 1.96 * coef_summary["x_above", "Std. Error"])),
      sprintf("%.3f", ll_ratio_1_2)
    ),
    "P-value" = c(
      sprintf("%.3f", coef_summary["x_below", "Pr(>|t|)"]),
      sprintf("%.3f", coef_summary["x_above", "Pr(>|t|)"]),
      sprintf("%.3f", p_value_1_2)
    )
  )
  
  # 打印表格
  print(knitr::kable(results_table, 
                     format = "pipe",
                     caption = "Threshold effect analysis by the two-piecewise linear regression"))
  
  return(list(
    table = results_table,
    cutpoint = final_cut,
    model = model1,  # 使用model1作为最终模型
    linear_model = model2,  # 使用model2作为线性模型
    ll_ratio = ll_ratio_1_2,
    ll_pvalue = p_value_1_2,
    model0 = model0,  # 额外返回model0以供参考
    ll_ratio_0_2 = ll_ratio_0_2,
    p_value_0_2 = p_value_0_2
  ))
}


#### __________分段拟合和可视化函数__________ ####
plot_segmented_fit <- function(data, x_var, y_var, cutpoint, covariates,
                               weight_var = "WTMEC2YR",
                               strata_var = "SDMVSTRA",
                               psu_var = "SDMVPSU",
                               p_nonlinear = NULL) {
  
  # 创建分段变量
  data$x_below <- ifelse(data[[x_var]] <= cutpoint, data[[x_var]], cutpoint)
  data$x_above <- ifelse(data[[x_var]] > cutpoint, data[[x_var]] - cutpoint, 0)
  
  # 构建survey设计
  design <- svydesign(
    id = as.formula(paste0("~", psu_var)),
    strata = as.formula(paste0("~", strata_var)),
    weights = as.formula(paste0("~", weight_var)),
    nest = TRUE,
    data = data
  )
  
  # 构建分段模型公式
  formula_str <- paste0(y_var, " ~ x_below + x_above + ", 
                        paste(covariates, collapse = " + "))
  
  # 根据p_nonlinear选择拟合方法
  if(!is.null(p_nonlinear) && p_nonlinear < 0.05) {
    # 非线性拟合
    model <- svyglm(as.formula(formula_str), design = design, family = quasibinomial())
  } else {
    # 线性拟合
    model <- svyglm(as.formula(formula_str), design = design)
  }
  
  # 生成预测数据
  x_range <- seq(min(data[[x_var]]), max(data[[x_var]]), length.out = 100)
  pred_data <- data.frame(x = x_range)
  
  # 计算预测值
  pred_data$x_below <- ifelse(pred_data$x <= cutpoint, pred_data$x, cutpoint)
  pred_data$x_above <- ifelse(pred_data$x > cutpoint, pred_data$x - cutpoint, 0)
  
  # 添加协变量的平均值或众数
  for(cov in covariates) {
    if(is.numeric(data[[cov]])) {
      pred_data[[cov]] <- mean(data[[cov]], na.rm = TRUE)
    } else {
      pred_data[[cov]] <- names(which.max(table(data[[cov]])))
    }
  }
  
  # 预测
  pred_data$fit <- predict(model, newdata = pred_data, type = "response")
  
  # 绘图
  p <- ggplot() +
    # 添加原始数据点
    geom_point(data = data, 
               aes(x = .data[[x_var]], y = .data[[y_var]]),
               alpha = 0.1, size = 1) +
    # 添加拟合线
    geom_line(data = pred_data,
              aes(x = x, y = fit),
              color = "blue", size = 1) +
    # 添加垂直线表示拐点
    geom_vline(xintercept = cutpoint, 
               linetype = "dashed", 
               color = "red",
               alpha = 0.5) +
    # 添加非线性检验P值标注
    {if(!is.null(p_nonlinear)) 
      geom_text(aes(x = min(data[[x_var]]), 
                    y = max(pred_data$fit),
                    label = sprintf("p for nonlinear = %.4f", p_nonlinear)),
                hjust = 0, vjust = 1)} +
    # 设置标签
    labs(x = x_var,
         y = paste("Effect of", y_var),
         title = ifelse(!is.null(p_nonlinear) && p_nonlinear < 0.05,
                        "Nonlinear Segmented Regression Fit",
                        "Linear Segmented Regression Fit")) +
    # 设置主题
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "black"),
      axis.line = element_line(color = "black"),
      plot.title = element_text(hjust = 0.5)
    )
  
  return(p)
}




######## ______________通用亚组分析函数______________ #########
perform_subgroup_analysis <- function(data, 
                                    group_var,           
                                    outcome_vars,
                                    exposure_vars,
                                    covariates,
                                    design_vars = c(id = "SDMVPSU", 
                                                    strata = "SDMVSTRA", 
                                                    weights = "wt")) {
  
  # 检查分组变量的水平
  group_levels <- unique(data[[group_var]])
  if(length(group_levels) < 2) {
    warning(sprintf("分组变量 %s 只有一个水平: %s", group_var, paste(group_levels, collapse=", ")))
    return(NULL)
  }
  
  # 检查group_var是否包含下划线并处理
  if(grepl("_", group_var)) {
    group_var_prefix <- sub("_.*", "", group_var)
  } else {
    group_var_prefix <- group_var
  }
  
  # 创建结果存储数据框
  group_results <- data.frame(
    Various = character(),
    Estimate = character(),      
    SE = character(),
    tvalue = character(),
    pvalue = character(),
    pForInt = character(),
    stringsAsFactors = FALSE
  )
  
  # 对每个分组水平进行分析
  for(level in group_levels) {
    # 创建子集数据
    subset_data <- subset(data, data[[group_var]] == level)
    
    # 为子集数据创建新的survey设计对象
    sub_design <- svydesign(
      data = subset_data,
      id = as.formula(paste0("~", design_vars["id"])),
      strata = as.formula(paste0("~", design_vars["strata"])),
      weights = as.formula(paste0("~", design_vars["weights"])),
      nest = TRUE
    )
    
    # 构建模型公式
    formula_str <- paste0(outcome_vars, " ~ ", 
                         exposure_vars, " + ",
                         paste(covariates[!grepl(group_var_prefix, covariates)], 
                               collapse = " + "))
    
    # 运行模型
    sub_model <- try({
      svyglm(
        as.formula(formula_str),
        design = sub_design,
        family = quasibinomial()
      )
    })
    
    # 提取结果并格式化
    if(!inherits(sub_model, "try-error")) {
      coef_summary <- summary(sub_model)$coefficients
      rar_row <- coef_summary[2, ]
      
      new_row <- data.frame(
        Various = level,
        Estimate = sprintf("%.6f", rar_row["Estimate"]),
        SE = sprintf("%.6f", rar_row["Std. Error"]),
        tvalue = sprintf("%.6f", rar_row["t value"]),
        pvalue = sprintf("%.6E", rar_row["Pr(>|t|)"]),
        pForInt = NA,
        stringsAsFactors = FALSE
      )
      group_results <- rbind(group_results, new_row)
    }
  }
  
  # 创建完整数据的survey设计对象用于交互检验
  full_design <- svydesign(
    data = data,
    id = as.formula(paste0("~", design_vars["id"])),
    strata = as.formula(paste0("~", design_vars["strata"])),
    weights = as.formula(paste0("~", design_vars["weights"])),
    nest = TRUE
  )
  
  # 添加交互检验
  full_formula <- paste0(outcome_vars, " ~ ", 
                        exposure_vars, " * ", group_var, " + ",
                        paste(covariates[!grepl(group_var_prefix, covariates)], 
                              collapse = " + "))
  
  interaction_model <- try(svyglm(as.formula(full_formula), 
                                 design = full_design, 
                                 family = quasibinomial()))
  
  if(!inherits(interaction_model, "try-error")) {
    coef_summary <- summary(interaction_model)$coefficients
    interaction_terms <- grep(paste0(exposure_vars, ":", group_var), 
                            rownames(coef_summary), value = TRUE)
    if(length(interaction_terms) > 0) {
      p_interaction <- coef_summary[interaction_terms[1], "Pr(>|t|)"]
    } else {
      p_interaction <- NA
    }
  } else {
    p_interaction <- NA
  }
  
  # 添加分组变量名作为第一行
  group_row <- data.frame(
    Various = paste0(group_var, "(%)"),
    Estimate = NA,
    SE = NA,
    tvalue = NA,
    pvalue = NA,
    pForInt = sprintf("%.3f", p_interaction),
    stringsAsFactors = FALSE
  )
  
  # 合并结果
  results_df <- rbind(group_row, group_results)
  
  return(results_df)
}




######## ______________添加termsGlobeTest函数______________ #########
termsGlobeTest <- function(model, test.terms) {
  canonicalOrder <- function(term) sapply(lapply(strsplit(term, ":"), sort), paste, collapse = ":")
  okbeta <- !is.na(coef(model, na.rm = FALSE))
  tt <- attr(terms(model), "term.labels")
  aa <- attr(model.matrix(model), "assign")[okbeta]
  index <- which(aa %in% match(canonicalOrder(test.terms), canonicalOrder(tt)))
  beta <- coef(model)[index]
  V <- vcov(model)[index, index]  
  chisq <- beta %*% solve(V) %*% beta
  p <- pchisq(chisq, length(index), lower.tail = FALSE)
  return(p)
}




######## ______________绘制森林图函数______________ #########
plot_subgroup_forest <- function(data, clip, xticks) {
  # 将Estimate和SE转换为数值型
  data$Estimate <- as.numeric(data$Estimate)
  data$SE <- as.numeric(data$SE)
  data$pvalue <- as.numeric(data$pvalue)
  data$pForInt <- as.numeric(data$pForInt)
  data$pForInt <- sprintf("%.3f", data$pForInt)


  # 计算OR和CI
  data$OR <- exp(data$Estimate)
  data$lower <- exp(data$Estimate - 1.96 * data$SE)
  data$upper <- exp(data$Estimate + 1.96 * data$SE)
  
  # 直接使用已经格式化的pForInt值，不再进行额外的格式化
  data <- data %>%
    mutate(pForInt = ifelse(grepl("\\(\\%\\)", data$Various), pForInt, ""),
           `OR(95% CI)` = case_when(
             is.na(OR) | is.na(lower) | is.na(upper) | is.na(pvalue)  ~ NA_character_,
             TRUE ~ sprintf("%.3f (%.3f-%.3f)", OR, lower, upper, pvalue))
    )
  
  # 1. 合并分组变量信息为新的一列
  # 首先打印列名，确认位置
  print(names(data))

  # 使用列位置索引选择
  tempd <- data[, c("Various", "OR(95% CI)", "pvalue", "pForInt", "OR", "lower", "upper")]

  # 或者使用dplyr的方式
  # tempd <- data %>% 
  #   select(all_of(c("Various", "OR(95% CI)", "pvalue", "P.for.int", "pForInt", "OR", "lower", "upper")))

  colnames(tempd) <- c("Variables", "OR(95% CI)", "p_value", "P for interaction", "OR", "lower", "upper")
  
  # 2. 构造新表格
  table_out <- tempd
  
  # 3. 遍历每一行，遇到分组标题行时，将其余列设为NA或""
  for(i in seq_len(nrow(table_out))) {
    if(is.na(table_out[i, 2])) { # 第二列为空，说明是分组标题行
      # tempd原本的字符删除
      tempd[i,1] <- ""
      # 新数据中生成一列用于合并
      table_out[i, 4] <- gsub("\\(\\%\\)","",table_out[i, 1])  
    }
  }
  
  tempd <- cbind(table_out[,4],tempd)

  # 1. 生成labeltext矩阵并插入列名
  labeltext <- as.matrix(tempd[, c(1, 2, 3, 4, 5)])
  colnames(labeltext) <- NULL  # forestplot不需要矩阵的colnames
  # 插入列名作为第一行
  labeltext <- rbind(
    c("", "Variables", "OR(95% CI)", "p value","p for interaction"),
    labeltext
  )
  
  # save labeltext文件
  write.table(labeltext,"subgroup_results.csv",sep = ",",quote = F,
              col.names = F,row.names = F)
  
  # 2. 绘制森林图
  return(
    forestplot(
      labeltext = labeltext,
      mean = c(NA, tempd$OR),      # 第一行是列名，mean等数值列前面补NA
      lower = c(NA, tempd$lower),
      upper = c(NA, tempd$upper),
      zero = 1,
      boxsize = 0.2,
      lineheight = unit(8, "mm"),
      cex = 0.9,
      col = fpColors(
        box = "royalblue",
        line = "darkblue",
        summary = "royalblue"
      ),
      xlab = "OR (95% CI)",
      title = " ",
      txt_gp = fpTxtGp(
        ticks = gpar(cex = 0.9),
        xlab = gpar(cex = 1),
        title = gpar(cex = 1.2)
      ),
      clip = clip,
      xticks = xticks,
      grid = TRUE,
      graphwidth = unit(4, "inches"),
      is.summary = c(TRUE, rep(FALSE, nrow(tempd))),
      align = c("l", "l", "c", "c"),
      hrzl_lines = TRUE,
      vertices = TRUE,
      col.label = "black",
      new_page = TRUE,
      fn.ci_norm = fpDrawNormalCI,
      graph.pos = 4
    )
  )
}


######## ______________平滑曲线拟合函数______________ #########
plot_smooth_curve <- function(data, x_var, y_var, covariates, 
                            study_design, output_prefix = "smooth") {
  
  # 1. 构建GAM模型
  formula_str <- paste0(y_var, " ~ s(", x_var, ", k=3) + ", 
                       paste(covariates, collapse = " + "))
  gam_model <- gam(as.formula(formula_str),
                   data = data,
                   family = quasibinomial(link = "logit"),  # 改为quasibinomial
                   weights = wt)
  
  # 2. 获取预测值和标准误
  pred <- predict.gam(gam_model, type="terms", se.fit=TRUE)
  
  # 3. 提取平滑项的预测值和标准误
  smooth_terms <- grep(paste0("s\\(", x_var), colnames(pred$fit), value = TRUE)
  mfit <- apply(pred$fit[,smooth_terms,drop=FALSE], 1, sum)
  sfit <- apply(pred$se.fit[,smooth_terms,drop=FALSE], 1, sum)
  
  # 4. logit转换处理
  mfit0 <- mfit
  mfit0.low <- mfit0 - 1.96 * sfit
  mfit0.upp <- mfit0 + 1.96 * sfit
  
  # 转换到概率尺度
  mfit <- exp(mfit)/(1 + exp(mfit))
  mfit.low <- exp(mfit0.low)/(1 + exp(mfit0.low))
  mfit.upp <- exp(mfit0.upp)/(1 + exp(mfit0.upp))
  
  # 5. 绘图
  x_tmp <- data[[x_var]]
  
  # 计算图形边界
  xy <- legLocate(c(x_tmp, x_tmp), c(mfit.low, mfit.upp))
  
  # PNG格式
  png(paste0(output_prefix, "_smooth.png"), width = 720, height = 560)
  # 绘制散点图和置信区间
  plot(mfit ~ x_tmp, 
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "red",
       type = "p", 
       pch = 20, 
       ylab = y_var, 
       xlab = x_var)
  
  par(new = TRUE)
  plot(mfit.low ~ x_tmp,
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "blue", 
       type = "p", 
       pch = 1,
       ylab = "", 
       xlab = "")
  
  par(new = TRUE)
  plot(mfit.upp ~ x_tmp,
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "blue", 
       type = "p", 
       pch = 1,
       ylab = "", 
       xlab = "")
  
  # 添加平滑线
  tmp.ord <- order(x_tmp)
  x_tmp0 <- x_tmp[tmp.ord]
  y_tmp0 <- mfit[tmp.ord]
  y_low0 <- mfit.low[tmp.ord]
  y_upp0 <- mfit.upp[tmp.ord]
  
  lines(x_tmp0, y_tmp0, col = "red", lty = 1, lwd = 2)
  lines(x_tmp0, y_low0, col = "blue", lty = 3, lwd = 1)
  lines(x_tmp0, y_upp0, col = "blue", lty = 3, lwd = 1)
  
  # 添加数据分布标记
  rug(x_tmp0,col = "gray70")
  
  dev.off()
  
  # PDF格式
  pdf(paste0(output_prefix, "_smooth.pdf"), width = 8, height = 6)
  # 重复相同的绘图代码
  plot(mfit ~ x_tmp, 
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "red",
       type = "p", 
       pch = 20, 
       ylab = y_var, 
       xlab = x_var)
  
  par(new = TRUE)
  plot(mfit.low ~ x_tmp,
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "blue", 
       type = "p", 
       pch = 1,
       ylab = "", 
       xlab = "")
  
  par(new = TRUE)
  plot(mfit.upp ~ x_tmp,
       ylim = c(xy[3], xy[4]),
       xlim = c(xy[1], xy[2]),
       col = "blue", 
       type = "p", 
       pch = 1,
       ylab = "", 
       xlab = "")
  
  lines(x_tmp0, y_tmp0, col = "red", lty = 1, lwd = 2)
  lines(x_tmp0, y_low0, col = "blue", lty = 3, lwd = 1)
  lines(x_tmp0, y_upp0, col = "blue", lty = 3, lwd = 1)
  
  rug(x_tmp0,col = "gray70")
  
  dev.off()
  
  # 返回模型对象和预测值
  return(list(
    model = gam_model,
    predictions = data.frame(
      x = x_tmp,
      fit = mfit,
      lower = mfit.low,
      upper = mfit.upp
    )
  ))
}

######## ______________计算图形边界______________ #########
legLocate <- function(x, y) {
  x[is.infinite(y)] <- NA
  y[is.infinite(y)] <- NA
  xmin <- min(x, na.rm = TRUE)
  xmax <- max(x, na.rm = TRUE)
  ymin <- min(y, na.rm = TRUE)
  ymax <- max(y, na.rm = TRUE)
  yoff <- (ymax - ymin)
  
  # 添加边距
  ymax <- ymax + yoff * 0.1
  ymin <- ymin - yoff * 0.1
  
  # 返回边界值
  return(c(xmin, xmax, ymin, ymax))
}



######## ______________整理所有回归结果______________ #########
format_regression_table <- function(continuous_models, quartile_models, trend_models, 
                                   exposure_var = "index", quantile_data = NULL) {
  # 创建一个数据框来存储结果
  results_table <- data.frame(
    Variable = character(),
    Model1 = character(),
    Model2 = character(),
    Model3 = character(),
    stringsAsFactors = FALSE
  )
  
  # 获取模型系数和统计量
  get_model_stats <- function(model, coef_name) {
    if(is.null(model)) return("NA")
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
    Variable = "Continuous",
    Model1 = get_model_stats(continuous_models[[1]], exposure_var),
    Model2 = get_model_stats(continuous_models[[2]], exposure_var),
    Model3 = get_model_stats(continuous_models[[3]], exposure_var)
  )
  
  # 获取四分位数的范围
  # 如果提供了quantile_data参数，则使用它
  if(!is.null(quantile_data)) {
    q_values <- as.numeric(quantile_data[[exposure_var]])
    q_ranges <- sprintf("%.3f~%.3f", 
                        c(-Inf, q_values),
                        c(q_values, Inf))
  } else {
    # 使用默认值或从模型中提取
    q_ranges <- c("Min~Q1", "Q1~Q2", "Q2~Q3", "Q3~Max")
  }
  
  # 构建四分位数变量名
  quartile_var <- paste0(exposure_var, "_q4")
  
  # 添加四分位数分组结果
  quartile_rows <- data.frame(
    Variable = c("Interquartile",
                paste("Q1\n", q_ranges[1]),
                paste("Q2\n", q_ranges[2]),
                paste("Q3\n", q_ranges[3]),
                paste("Q4\n", q_ranges[4])),
    Model1 = c("", "Ref",
              get_model_stats(quartile_models[[1]], paste0(quartile_var, "Q2")),
              get_model_stats(quartile_models[[1]], paste0(quartile_var, "Q3")),
              get_model_stats(quartile_models[[1]], paste0(quartile_var, "Q4"))),
    Model2 = c("", "Ref",
              get_model_stats(quartile_models[[2]], paste0(quartile_var, "Q2")),
              get_model_stats(quartile_models[[2]], paste0(quartile_var, "Q3")),
              get_model_stats(quartile_models[[2]], paste0(quartile_var, "Q4"))),
    Model3 = c("", "Ref",
              get_model_stats(quartile_models[[3]], paste0(quartile_var, "Q2")),
              get_model_stats(quartile_models[[3]], paste0(quartile_var, "Q3")),
              get_model_stats(quartile_models[[3]], paste0(quartile_var, "Q4")))
  )
  
  # 构建趋势变量名
  trend_var <- paste0(exposure_var, "_T")
  
  # 添加趋势性检验结果
  trend_row <- data.frame(
    Variable = "P for trend",
    Model1 = get_model_stats(trend_models[[1]], trend_var),
    Model2 = get_model_stats(trend_models[[2]], trend_var),
    Model3 = get_model_stats(trend_models[[3]], trend_var)
  )
  
  # 合并所有结果
  results_table <- rbind(continuous_row, quartile_rows, trend_row)
  
  # 重命名列名
  colnames(results_table)[1] <- exposure_var
  
  # 保存为CSV文件
  output_file <- paste0("regression_results_", exposure_var, ".csv")
  write.csv(results_table, output_file, row.names = FALSE)
  
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
  excel_file <- paste0("regression_results_", exposure_var, ".xlsx")
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  
  return(results_table)
}


