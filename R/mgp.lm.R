#' @title Linear Regression Analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param data a data.frame.
#'
#' @return lm_list
#' @export
#'
#' @examples
mgp.lm <- function(x = x,
                  y = y,
                  covariable = NULL,
                  data = data) {
  # 判断因变量类型
  if (!is.numeric(data[, y])) {
    return(print(paste(y, "因变量不为连续变量")))
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
  lm_reg <- lm(formula, data = data)

  # 结果整理
  lm_result <- c(summary(lm_reg)$coefficients[2, ], confint(lm_reg)[2, ]) %>%
    as.data.frame() %>% t() %>% as.data.frame() %>%
    mutate(xvar = x, yvar = y, n = (data[, c(x, y, covariable)] %>% tidyr::drop_na() %>% dim())[1],
           r.squared = summary(lm_reg)$r.squared, adj.r.squared = summary(lm_reg)$adj.r.squared,
           covars = ifelse(is.null(covariable), NA, paste0(covariable, collapse = "+"))) %>%
    select(xvar, yvar, n, everything())

  # 结果保存
  lm_list <- list(lm_reg = lm_reg,
                  lm_result = lm_result)
  return(lm_list)

}
