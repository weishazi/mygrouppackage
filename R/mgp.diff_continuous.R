#' @Title Difference Comparison of Continuous Variables
#'
#' @param data a data.frame.
#' @param variables a character string indicating the categorical variables.
#' @param group a character string indicating the grouping variable.
#'
#' @return a data.frame object df_resul
#' @export
#'
#' @examples mgp.diff_continuous(data = iris,
#'                               variables = colnames(iris)[1:4],
#'                              group = colnames(iris)[5])
#'
mgp.diff_continuous <- function(data = data,
                              variables = variables,
                              group = group) {
  # 判断分组变量是否是因子型
  if (!is.factor(data[, group])) {
    data[, group] <- as.factor(data[, group])
  }

  # 需要测量的指标
  f_measure <- list(
    n = ~ na.omit(.x) %>% length(),
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    Q25 = ~ quantile(.x,  na.rm = TRUE)[2],
    Q75 = ~ quantile(.x,  na.rm = TRUE)[4]
  )

  # 计算测量指标
  df_measure <- data %>% group_by(get(group)) %>% summarise(across(.cols = variables, f_measure))
  colnames(df_measure)[1] <- group

  # 测量指标整理
  levels <- levels(data[, group])

  df_measure2 <- purrr::map(variables, function(x) {
    purrr::map(levels, function(y) {
      df_measure %>% slice(which(df_measure[, group] == y)) %>% dplyr::select(contains(x)) %>% as.numeric()

    }) %>% purrr::reduce(., c)

  }) %>% purrr::reduce(., rbind) %>% as.data.frame()

  index <- c("n", "mean", "sd", "median", "Q25", "Q75")
  colnames(df_measure2) <- paste0(rep(index, length(levels)), "_", rep(levels, each = length(index)))
  df_measure2 <- df_measure2 %>% mutate(vars = variables) %>% select(vars, everything())

  # 假设检验
  if(length(levels) == 2) {
    test <- purrr::map(variables, function(x) {
      formula <- formula(paste0("`", x, "`", "~", group))
      c(t.test(formula, data = data)$p.value,
        wilcox.test(formula, data = data)$p.value)

    })
    test <- purrr::reduce(test, rbind) %>% as.data.frame()
    colnames(test) <- c("ttest", "wilcox")
    df_result <- cbind(df_measure2, test)

  } else {
    test <- purrr::map(variables, function(x) {
      formula <- formula(paste0("`", x, "`", "~", group))
      aov <- aov(formula, data = data)
      c(summary(aov)[[1]]$`Pr(>F)`[1],
        kruskal.test(formula, data = data)$p.value)

    })
    test <- purrr::reduce(test, rbind) %>% as.data.frame()
    colnames(test) <- c("aov", "kruskal")
    df_result <- cbind(df_measure2, test)

  }
  return(df_result)

}
