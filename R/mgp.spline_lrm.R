#' @Title Restricted cubic spline for logistic
#'
#' @param x a character string.
#' @param y a character string.
#' @param covariable a character string indicating the covariates.
#' @param spline The location of the knots. defaulted c(0.25,0.5,0.75).
#' @param data a data.frame.
#'
#' @return fit
#' @export
#'
#' @examples ddist<-datadist(df_yili_g);/n options(datadist="ddist")
mgp.spline_lrm <- function(y = y,
                          x = x,
                          covariable = NULL,
                          spline = c(0.25,0.5,0.75),
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
  if(length(spline) == 1) {
    spline <- spline
  } else {
    spline <- data[, x] %>% quantile(spline, na.rm = T) %>% as.numeric() %>% paste(collapse = ",")
  }

  # 变量名反引号，解决不规范列名
  y <- paste0("`", y, "`")
  x <- paste0("`", x, "`")
  covariable <- purrr::map_chr(covariable, ~ paste0("`", .x, "`"))

  # 构建公式
  if (is.null(covariable)) {
    formula <- paste0(y, "~ rcs(", x, ", parms = c(", spline, "))") %>% formula()
  } else {
    formula <- paste0(y, "~ rcs(", x, ", parms = c(", spline, "))+",
                      paste0(covariable, collapse = "+")) %>% formula()
  }

  # 分析
  fit <- rms::lrm(formula, data = data)

  return(fit)

}
