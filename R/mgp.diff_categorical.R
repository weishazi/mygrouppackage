#' @Title Difference Comparison of Categorical Variables
#'
#' @param data a data.frame.
#' @param variables a character string indicating the categorical variables.
#' @param group a character string indicating the grouping variable.
#' @param prop a number 1 or 2.
#' @param correct a logical value. Default is "F". Is the chi-square test corrected?
#'
#' @return a data.frame object df_result
#' @export
#'
#' @examples
mgp.diff_categorical <- function(data = data,
                               variables = variables,
                               group = group,
                               prop = c(1, 2),
                               correct = F) {
  # 判断变量是否是因子型
  if (!isTRUE(purrr::map(data[, c(variables, group)], is.factor) %>% unlist() %>% unique())) {
    data <- data %>% mutate(across(c(variables, group), as.factor))
  }

  purrr::map_dfr(variables, function(x) {
    # 构建交叉表
    ts <- table(data[, x], data[, group])
    ts2 <- prop.table(ts, prop)
    # 交叉表重塑
    df_n <- tidyr::spread(as.data.frame(ts), key = Var2, value = Freq)
    df_p <- tidyr::spread(as.data.frame(ts2), key = Var2, value = Freq)
    # 合并交叉表
    colnames(df_n) <- c("vars", paste0(group, "_", levels(data[, group]), "_n"))
    colnames(df_p) <- c("vars", paste0(group, "_", levels(data[, group]), "_p"))
    df_c <- left_join(df_n, df_p)
    df_c <- df_c %>% mutate(vars = paste0(x, "_", levels(data[, x])))

    # 假设检验
    chisq <- chisq.test(ts, correct = correct)$p.value

    df_result <- cbind(df_c, chisq)

    return(df_result)

  })

}
