# 加载必要的包
library(survey)
library(dplyr)
library(ggplot2)
library(knitr)
library(rms)

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
  get_loglik <- function(cutpoint) {
    # 创建分段变量
    data$x_split <- ifelse(data[[x_var]] <= cutpoint,
                           data[[x_var]],
                           cutpoint + (data[[x_var]] - cutpoint))
    
    design_tmp <- update(design, x_split = data$x_split)
    
    # 构建完整公式
    if(cov_formula != "") {
      full_formula <- paste(y_var, "~ x_split +", cov_formula)
    } else {
      full_formula <- paste(y_var, "~ x_split")
    }
    
    model_tmp <- svyglm(as.formula(full_formula), design = design_tmp)
    return(list(logLik = as.numeric(logLik(model_tmp)), model = model_tmp))
  }
  
  # Step 1: 初筛5%-95%之间，每隔5%
  quantiles_all <- quantile(data[[x_var]], probs = seq(0.05, 0.95, 0.05), na.rm = TRUE)
  loglik_list <- lapply(quantiles_all, get_loglik)
  loglik_values <- sapply(loglik_list, function(x) x$logLik)
  best_idx <- which.max(loglik_values)
  best_cut_init <- quantiles_all[best_idx]
  
  # Step 2: ±4%范围内缩小
  refined_range <- quantile(data[[x_var]], 
                            probs = seq(0.01 * (best_idx*5 - 4), 
                                        0.01 * (best_idx*5 + 4), 
                                        length.out = 3), 
                            na.rm = TRUE)
  
  quartile_points <- quantile(refined_range, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  loglik_q <- lapply(quartile_points, get_loglik)
  loglik_q_vals <- sapply(loglik_q, function(x) x$logLik)
  best_q_idx <- which.max(loglik_q_vals)
  best_q <- quartile_points[best_q_idx]
  
  # Step 3: ±25%范围内再次递归缩小
  final_range <- seq(
    best_q - 0.25 * (best_q - min(refined_range)),
    best_q + 0.25 * (max(refined_range) - best_q),
    length.out = 3
  )
  loglik_final <- lapply(final_range, get_loglik)
  loglik_final_vals <- sapply(loglik_final, function(x) x$logLik)
  final_idx <- which.max(loglik_final_vals)
  final_cut <- final_range[final_idx]
  final_model <- loglik_final[[final_idx]]$model
  
  # 创建分段变量用于最终模型
  data$x_below <- ifelse(data[[x_var]] <= final_cut, data[[x_var]], final_cut)
  data$x_above <- ifelse(data[[x_var]] > final_cut, data[[x_var]] - final_cut, 0)
  
  # 更新设计对象
  design <- svydesign(
    id = as.formula(paste0("~", psu_var)),
    strata = as.formula(paste0("~", strata_var)),
    weights = as.formula(paste0("~", weight_var)),
    nest = TRUE,
    data = data
  )
  
  # 构建分段模型公式
  if(length(covariates) > 0) {
    formula_str <- paste0(y_var, " ~ x_below + x_above + ", paste(covariates, collapse = " + "))
  } else {
    formula_str <- paste0(y_var, " ~ x_below + x_above")
  }
  
  # 拟合最终模型
  final_model <- svyglm(as.formula(formula_str), design = design, family = quasibinomial())
  
  # 提取结果并计算OR
  coef_summary <- summary(final_model)$coefficients
  
  # 创建结果表格
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
      sprintf("%.3f", logLik(final_model)[1])
    ),
    "P-value" = c(
      sprintf("%.3f", coef_summary["x_below", "Pr(>|t|)"]),
      sprintf("%.3f", coef_summary["x_above", "Pr(>|t|)"]),
      ""
    )
  )
  
  # 打印表格
  print(knitr::kable(results_table, 
                     format = "pipe",
                     caption = "Threshold effect analysis by the two-piecewise linear regression"))
  
  return(list(
    table = results_table,
    cutpoint = final_cut,
    model = final_model
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





# -------------- 通用亚组分析函数 -----------------
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




# ------------添加termsGlobeTest函数--------------
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




# ------------绘制森林图函数------------------
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



# ------------添加空行来分隔不同组别,暂时没用但是可以留着学习------------
if(F){
  add_spacing_rows <- function(data) {
    result <- data.frame()
    current_group <- ""
    
    for(i in 1:nrow(data)) {
      if(grepl("\\(%\\)", data$Various[i])) {  # 如果是组标题行
        if(nrow(result) > 0) {  # 如果不是第一组，添加空行
          empty_row <- data[1,]
          empty_row[1,] <- NA
          result <- rbind(result, empty_row)
        }
      }
      result <- rbind(result, data[i,])
    }
    
    return(result)
  }
  data2plot_spaced <- add_spacing_rows(data2plot)
}






