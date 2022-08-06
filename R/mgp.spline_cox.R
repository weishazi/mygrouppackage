#' @Title Restricted cubic spline for cox
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param spline The location of the knots. defaulted c(0.25,0.5,0.75).
#' @param time_var a character string indicating the time series variable.
#' @param data a data.frame.
#'
#' @return fit
#' @export
#'
#' @examples
mgp.spline_cox <- function(y = y,
                          x = x,
                          covariable = NULL,
                          spline = c(0.25,0.5,0.75),
                          time_var = time_var,
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

  # 设置样条结点
  spline <- data[, x] %>% quantile(spline, na.rm = T) %>% as.numeric() %>% paste(collapse = ",")

  #
  f_Surv <- Surv(data[, time_var], data[, y] == levels(data[, y])[1])

  # 变量名反引号，解决不规范列名
  y <- paste0("`", y, "`")
  x <- paste0("`", x, "`")
  time_var <- paste0("`", time_var, "`")
  covariable <- purrr::map_chr(covariable, ~ paste0("`", .x, "`"))

  # 构建公式
  if (is.null(covariable)) {
    formula <- paste0("f_Surv", "~ rcs(", x, ", knots = c(", spline, "))") %>% formula()
  } else {
    # f_Surv <- Surv(data[, time_var], data[, y] == levels(data[, y])[1])
    formula <- paste0("f_Surv", "~ rcs(", x, ", knots = c(", spline, "))+",
                      paste0(covariable, collapse = "+")) %>% formula()
  }

  #
  fit <- cph(formula, data = data)

  return(fit)

}
