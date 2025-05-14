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
  
  # 创建整体survey design对象
  design <- svydesign(
    id = as.formula(paste0("~", design_vars["id"])),
    strata = as.formula(paste0("~", design_vars["strata"])),
    weights = as.formula(paste0("~", design_vars["weights"])),
    data = data,
    nest = TRUE
  )
  
  # 提取group_var的_之前的字符
  group_var_prefix <- sub("_.*", "", group_var)

  # 构建模型公式
  formula_str <- paste0(outcome_vars, " ~ ", 
                       exposure_vars, " + ",
                       paste(covariates[!grepl(group_var_prefix, covariates)], 
                             collapse = " + "))
  
  # 使用svyby进行分组分析
  results <- try({
    svyby(formula = as.formula(formula_str),
          by = as.formula(paste0("~", group_var)),
          design = design,
          FUN = svyglm,
          family = quasibinomial())
  })
  
  if(inherits(results, "try-error")) {
    warning("svyby analysis failed")
    return(NULL)
  }
  
  # 提取每个亚组的结果
  group_results <- data.frame(
    Various = results[[group_var]],
    Estimate = NA,
    SE = NA,
    tvalue = NA,
    pvalue = NA,
    stringsAsFactors = FALSE
  )
  
  # 从svyby结果中提取系数
  coef_idx <- which(colnames(results) == exposure_vars)
  se_idx <- which(colnames(results) == paste0("se.", exposure_vars))
  
  if(length(coef_idx) > 0 && length(se_idx) > 0) {
    group_results$Estimate <- results[, coef_idx]
    group_results$SE <- results[, se_idx]
    group_results$tvalue <- group_results$Estimate / group_results$SE
    group_results$pvalue <- 2 * (1 - pnorm(abs(group_results$tvalue)))
  }
  
  # 添加交互检验
  full_formula <- paste0(outcome_vars, " ~ ", 
                        exposure_vars, " * ", group_var, " + ",
                        paste(covariates[covariates != group_var], 
                              collapse = " + "))
  
  interaction_model <- try(svyglm(as.formula(full_formula), 
                                 design = design, 
                                 family = quasibinomial()))
  
  if(!inherits(interaction_model, "try-error")) {
    # 提取交互项的p值
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
  
  # 添加交互p值
  group_results$pForInt <- NA
  group_results$pForInt[1] <- p_interaction
  
  # 添加分组变量名作为第一行
  group_row <- data.frame(
    Various = paste0(group_var, "(%)"),
    Estimate = NA,
    SE = NA,
    tvalue = NA,
    pvalue = NA,
    pForInt = p_interaction,
    stringsAsFactors = FALSE
  )
  
  results_df <- rbind(group_row, group_results)
  
  return(results_df)
}

# 添加termsGlobeTest函数
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


