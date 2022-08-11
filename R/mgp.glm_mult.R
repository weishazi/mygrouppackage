#' @title Multinomial Logistic Regression Analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param ref a character string indicating the y reference level.
#' @param data a data.frame.
#'
#' @return fit_list
#' @export
#'
#' @examples
mgp.glm_mult <- function(x = x,
                        y = y,
                        covariable = NULL,
                        ref = NULL,
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

  # 设置参考项
  if(!is.null(ref)) {
    data[, y] <- relevel(data[, y], ref = ref)
  }

  # 构建公式
  formula <- paste0( y2, "~", paste0(variable_set, collapse = "+")) %>% formula()

  # 回归分析
  # library(nnet)
  fit <- nnet::multinom(formula, data = data)

  # 查看levels
  k_y <- length(levels(data[, y]))
  if(is.factor(data[, x])) {
    k_x <- length(levels(data[, x]))
  } else {
    k_x <- 2
  }

  # 结果整理
  fit_result <- purrr::map_dfr(2:k_x, function(nx) {
    a <- as.data.frame(summary(fit)$coefficients) %>% dplyr::select(nx)
    b <- as.data.frame(summary(fit)$standard.errors) %>% dplyr::select(nx)
    c <- purrr::map_dfr(1:(k_y-1), ~ confint(fit)[nx, , .x])
    df1 <- purrr::reduce(list(a, b, c), cbind)
    colnames(df1) <- c("coef", "se", "2.5 %", "97.5 %")
    df2 <- df1 %>%
      mutate(
        xvar = paste0(x, "_", levels(data[, x])[nx]),
        yvar = paste0(y, "_", levels(data[, y])[-which(levels(data[, y]) == ref)]),
        OR = exp(coef),
        lci = exp(`2.5 %`),
        hci = exp(`97.5 %`),
        p_z = coef / se) %>%
      mutate(
        p = (1 - pnorm(abs(p_z), 0, 1)) * 2) %>%
      dplyr::select(xvar, yvar, everything())

  })

  # 结果保存
  fit_list <- list(fit = fit,
                   fit_result = fit_result)
  return(fit_list)

}
