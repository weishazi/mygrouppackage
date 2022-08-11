#' @title mediation analysis
#'
#' @param x a character string.
#' @param y a character string.
#' @param m a character string.
#' @param covariable a character string indicating the covariates.
#' @param boot a logical value. Default is "F".
#' @param sims a number.
#' @param out.model glm or lm
#' @param med.model glm or lm
#' @param data a data.frame.
#'
#' @return med.list
#' @export
#'
#' @examples
#' setRefClass("PS",fields = list(m="character", x="character",med.f="formula",out.f="formula"))
#' para_set <- new("PS")


# setRefClass("PS",fields = list(m="character", x="character",med.f="formula",out.f="formula"))
# para_set <- new("PS")
mgp.mediate <- function(x = x,
                       y = y,
                       m = m,
                       covariable = NULL,
                       boot = F,
                       sims = 100,
                       out.model = c("glm", "lm"), # glm or lm
                       med.model = c("glm", "lm"), # glm or lm
                       data = data
) {
  ##
  data <- data %>% tidyr::drop_na(c(y, x, m, covariable)) %>% as.data.frame()

  ## 变量添加到环境中
  para_set$m <-m
  para_set$x <-x

  ## 构建公式
  # out_f
  out.f <- paste0(y, "~", paste0(c(x, m, covariable), collapse = "+")) %>% formula()
  para_set$out.f <- out.f
  # med_f
  med.f <- paste0(m, "~", paste0(c(x, covariable), collapse = "+")) %>% formula()
  para_set$med.f <- med.f

  ## 结果模型
  # 线性回归
  if(out.model == "lm") {
    out.model2 <- lm(
      para_set$out.f,
      data = data
    )
  }
  # logistics回归
  if(out.model == "glm") {
    out.model2 <- glm(
      para_set$out.f,
      family = "binomial",
      data = data
    )
  }

  ## 中介模型
  # 线性回归
  if(med.model == "lm") {
    med.model2 <- lm(
      para_set$med.f,
      data = data
    )
  }
  # logistics回归
  if(med.model == "glm") {
    med.model2 <- glm(
      para_set$med.f,
      family = "binomial",
      data = data
    )
  }

  # 中介变量m属性检测
  if (!is.double(m)) {
    data[, m] <- as.double(data[, m])
  }

  ## 中介分析
  med.fit <- mediation::mediate(model.m = med.model2,
                     model.y = out.model2,
                     treat = para_set$x,
                     mediator =  para_set$m,
                     boot = boot,
                     sims = sims)

  ## 结果整理
  med_sum <- summary(med.fit)
  ACME <- med_sum$d.ave
  ACME_P <- med_sum$d.ave.p
  ADE <- med_sum$z.ave
  ADE_P <- med_sum$z.ave.p
  TE <- (ACME+ADE)
  Prop_Mediated <- ACME/TE
  med.result <- data.frame('xvar'= x, 'yvar'= y, 'mvar'= m,
                           'ACME' = ACME, 'ACME_P' = ACME_P,
                           'ADE' = ADE, 'ADE_P' = ADE_P,
                           'Total_Effect' = TE, 'Prop_Mediated' = Prop_Mediated)

  ##
  med.list <- list(med.fit = med.fit,
                   med.result = med.result)
  return(med.list)

}
