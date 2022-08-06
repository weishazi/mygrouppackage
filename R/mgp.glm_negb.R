#' @Title Negative Binomial Regression Analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param data a data.frame.
#'
#' @return glm_list
#' @export
#'
#' @examples
mgp.glm_negb <- function(x = x,
                      y = y,
                      covariable = NULL,
                      data = data) {
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
  # library(MASS) #加载MASS包
  glm_reg <-MASS::glm.nb(formula, data = data)

  # 结果整理
  if(is.factor(data[, x])) {
    k <- levels(data[, x]) %>% length()
    if(k > 2) {
      glm_result <- cbind(summary(glm_reg)$coefficients[2:k, ], confint(glm_reg)[2:k, ]) %>% as.data.frame()
    } else {
      glm_result <- c(summary(glm_reg)$coefficients[k, ], confint(glm_reg)[k, ]) %>%
        as.data.frame() %>% t() %>% as.data.frame()
    }
  } else {
    k <- 2
    glm_result <- c(summary(glm_reg)$coefficients[k, ], confint(glm_reg)[k, ]) %>%
      as.data.frame() %>% t() %>% as.data.frame()
  }
  # 效应值计算
  glm_result <- glm_result %>%
    mutate(xvar = paste0(x, "_", levels(data[, x])[-1]), yvar = rep(y, k-1),
           n = rep((data[, c(x, y, covariable)] %>% tidyr::drop_na() %>% dim())[1], k-1),
           OR = exp(Estimate), lci = exp(`2.5 %`), hci = exp(`97.5 %`),
           covars = ifelse(is.null(covariable), NA, paste0(covariable, collapse = "+")) %>% rep(., k-1)) %>%
    dplyr::select(xvar, yvar, n, everything())

  # 结果保存
  glm_list <- list(glm_reg = glm_reg,
                   glm_result = glm_result)
  return(glm_list)

}
