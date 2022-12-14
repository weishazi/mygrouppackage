#' @title Partial / correlation analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param cov a character string indicating the covariates.
#' @param data a data.frame.
#' @param method "pearson", "kendall", or "spearman".
#'
#' @return df_temp
#' @export
#'
#' @examples
mgp.pcor <- function(x = x,
                     y = y,
                     cov = NULL,
                     data = data,
                     method = c("pearson", "kendall", "spearman")) {
  # 加载包
  # library(ppcor)

  if (is.null(cov)) {
    # 相关分析
    # 整理数据
    df_temp <- data %>% dplyr::select(c(x, y)) %>% tidyr::drop_na()
    n <- dim(df_temp)[1]

    # 计算相关性
    df_cor <- cor.test(df_temp[, x],
                       df_temp[, y],
                       method = method)

    # 结果整理
    df_temp <- data.frame(xvar = x, yvar = y, n = n,
                          cor= df_cor$estimate, p.value= df_cor$p.value) %>%
      mutate(Method = method)

  } else {
    # 偏相关分析
    # 整理数据
    df_temp <- data %>% dplyr::select(c(x, y, cov)) %>% tidyr::drop_na(everything())
    n <- dim(df_temp)[1]

    # 计算相关性
    df_temp <- ppcor::pcor.test(df_temp[, x],
                                df_temp[, y],
                                df_temp[, cov],
                                method = method)

    # 结果整理
    df_temp <- df_temp %>% mutate(xvar = x, yvar = y, n = n, covar = paste(cov, collapse = "+")) %>%
      dplyr::select(xvar, yvar, n, everything())

  }

  return(df_temp)

}
