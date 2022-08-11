#' @title Ordinal Logistic Regression Analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param data a data.frame.
#'
#' @return fit_list
#' @export
#'
#' @examples
mgp.glm_ord <- function(x = x,
                       y = y,
                       covariable = NULL,
                       data = data) {
  # 判断因变量类型
  if (!is.factor(data[, y])) {
    data[, y] <- data[, y] %>% as.factor()
  }

  # 判断是否存在协变量
  if(is.null(covariable)) {
    variable_set <- x
  } else {
    variable_set <- c(x, covariable)
  }

  # 变量名反引号，解决不规范列名
  y2 <- paste0("`", y, "`")
  variable_set <- purrr::map_chr(variable_set, ~ paste0("`", .x, "`"))

  # 构建公式
  formula <- paste0( y2, "~", paste0(variable_set, collapse = "+")) %>% formula()

  # 回归分析
  # library(MASS)
  fit <- MASS::polr(formula, data = data, Hess = T)
  # return(fit)

  # 结果整理
  if(is.factor(data[, x])) {
    k <- levels(data[, x]) %>% length()
    if(k > 2) {
      fit_result <- cbind(summary(fit)$coefficients[1:k-1, ], confint(fit)[1:k-1, ]) %>% as.data.frame()
    } else {
      fit_result <- c(summary(fit)$coefficients[k-1, ], confint(fit)[k-1, ]) %>%
        as.data.frame() %>% t() %>% as.data.frame()
    }
  } else {
    k <- 2
    fit_result <- c(summary(fit)$coefficients[k-1, ], confint(fit)[k-1, ]) %>%
      as.data.frame() %>% t() %>% as.data.frame()
  }
  # 效应值计算
  fit_result <- fit_result %>%
    mutate(xvar = paste0(x, "_", levels(data[, x])[-1]), yvar = rep(y, k-1),
           n = rep((data[, c(x, y, covariable)] %>% tidyr::drop_na() %>% dim())[1], k-1),
           OR = exp(Value), lci = exp(`2.5 %`), hci = exp(`97.5 %`),
           p.value = drop1(fit, test = "Chi")$`Pr(>Chi)`[2:k],
           covars = ifelse(is.null(covariable), NA, paste0(covariable, collapse = "+")) %>% rep(., k-1)) %>%
    dplyr::select(xvar, yvar, n, everything())

  # 结果保存
  fit_list <- list(fit = fit,
                   fit_result = fit_result)
  return(fit_list)

}
