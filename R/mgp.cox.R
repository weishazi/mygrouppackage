#' @Title Cox Proportional Hazard Regression Model
#'
#' @param x a character string.
#' @param y a character string.
#' @param time_var a character string indicating the time series variable.
#' @param covariable a character string indicating the covariates.
#' @param data a data.frame.
#'
#' @return fit_list
#' @export
#'
#' @examples
mgp.cox <- function(x = x,
                   y = y,
                   time_var = time_var,
                   covariable = NULL,
                   data = data) {
  # 判断因变量类型
  if (!is.factor(data[, y])) {
    data[, y] <- data[, y] %>% as.factor()
  }
  if (length(levels(data[, y])) != 2) {
    return(print(paste(y, "因变量不是二分类变量")))
  }

  # 判断是否存在协变量
  if(is.null(covariable)) {
    variable_set <- x
  } else {
    variable_set <- c(x, covariable)
  }

  # 变量名反引号，解决不规范列名
  y2 <- paste0("`", y, "`")
  time_var2 <- paste0("`", time_var, "`")
  variable_set <- purrr::map_chr(variable_set, ~ paste0("`", .x, "`"))

  # 构建公式
  f_Surv <- survival::Surv(data[, time_var], data[, y] == levels(data[, y])[1])
  formula <- paste0("f_Surv~", paste0(variable_set, collapse = "+")) %>% formula()
  # formula <- paste0("Surv(", time_var2, ",", y2, ")~", paste0(variable_set, collapse = "+")) %>% formula()

  # 回归分析
  # library(survival) #调用包“survival”
  fit <- survival::coxph(formula, data = data)

  # 结果整理
  if(is.factor(data[, x])) {
    k <- levels(data[, x]) %>% length()
    if(k > 2) {
      fit_result <- cbind(summary(fit)$coefficients[1:(k-1), ], summary(fit)$conf.int[1:(k-1), 3:4]) %>% as.data.frame()
    } else {
      fit_result <- c(summary(fit)$coefficients[1, ], summary(fit)$conf.int[1, 3:4]) %>%
        as.data.frame() %>% t() %>% as.data.frame()
    }
  } else {
    k <- 2
    fit_result <- c(summary(fit)$coefficients[1, ], summary(fit)$conf.int[1, 3:4]) %>%
      as.data.frame() %>% t() %>% as.data.frame()
  }
  # fit_result <- fit_result %>% dplyr::select()
  colnames(fit_result) <- c("coef", "HR", "se", "z", "p", "lci", "hci")
  #
  fit_result <- fit_result %>%
    mutate(xvar = paste0(x, "_", levels(data[, x])[-1]), yvar = rep(y, k-1),
           n = rep((data[, c(x, y, time_var, covariable)] %>% tidyr::drop_na() %>% dim())[1], k-1),
           covars = ifelse(is.null(covariable), NA, paste0(covariable, collapse = "+")) %>% rep(., k-1)) %>%
    dplyr::select(xvar, yvar, n, everything())

  # 结果保存
  fit_list <- list(fit = fit,
                   fit_result = fit_result)
  return(fit_list)

}
