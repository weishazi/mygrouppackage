
# 加载相关包 -------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(DT)
library(forestmodel)

library(rms)
# library(mediation)

# devtools::install_github("weishazi/mygrouppackage", force = TRUE)
library(mygrouppackage)


# 查看mygrouppackage包 -------------------------------------------------------------------------
help(package = "mygrouppackage")


# 加载测试数据集 -------------------------------------------------------------------------
load(file = "test_df.RData")

datatable(head(test_df1, 20))
datatable(head(test_df2, 20))

# 连续型变量差异分析：mgp.diff_continuous -------------------------------------------------------------------------
help("mgp.diff_continuous")

# Examples
datatable(iris)
summary(iris$Species)
re_df <- mgp.diff_continuous(data = iris,
                    variables = colnames(iris)[1:4],
                    group = colnames(iris)[5])


# test_df1 示例
colnames(test_df1)
variables <- colnames(test_df1)[14:21]
re_df <- mgp.diff_continuous(data = test_df1,
                             variables = variables,
                             group = "MetS_P")

## 分组循环
groups <- colnames(test_df1)[c(7, 22:24)]
re_ls <- purrr::map(groups, ~ mgp.diff_continuous(data = test_df1,
                                         variables = variables,
                                         group = .x))
names(re_ls) <- groups

re_df <- re_ls %>% purrr::reduce(rbind)
re_df <- re_ls %>% 
  purrr::reduce(rbind) %>% 
  mutate(group = rep(groups, each = length(variables)), .before = "vars")
datatable(re_df)

# test_df2 示例
colnames(test_df2)
variables <- colnames(test_df2)[7:14]
re_df <- mgp.diff_continuous(data = test_df2,
                             variables = variables,
                             group = "数字分组")

re_df2 <- mgp.diff_continuous(data = test_df2,
                             variables = variables,
                             group = "分组")


# 分类变量差异分析：mgp.diff_categorical -------------------------------------------------------------------------
# test_df1 示例
variables <- colnames(test_df1)[c(104, 105, 107, 109, 112, 113)]
re_df <- mgp.diff_categorical(data = test_df1,
                             variables = variables,
                             group = "MetS_P",
                             prop = 1,
                             correct = F)

re_df2 <- mgp.diff_categorical(data = test_df1,
                              variables = variables,
                              group = "MetS_P",
                              prop = 2,
                              correct = F)
datatable(re_df2)

# （偏）相关分析：mgp.pcor -------------------------------------------------------------------------
# test_df1 示例
mgp.pcor(
  x = "WC",
  y = "BMI",
  cov = NULL,
  data = test_df1,
  method = "pearson"
)

## 循环示例
### 自变量（x）进行循环
df1_variables.x <- colnames(test_df1)[14:21]
re_df <- purrr::map_dfr(df1_variables.x, ~ mgp.pcor(
  x = .x,
  y = "BMI",
  cov = NULL,
  data = test_df1,
  method = "pearson"
))
datatable(re_df)
### 自变量（x）和因变量（y）同时进行循环
df1_variables.y <- colnames(test_df1)[14:21]
re_df1 <- purrr::map_dfr(df1_variables.x, function(x) {
  purrr::map_dfr(df1_variables.y, ~ mgp.pcor(
    x = x,
    y = .x,
    cov = NULL,
    data = test_df1,
    method = "pearson"
  ))
} )
datatable(re_df1)
### 偏相关分析（存在协变量）
co.variables <- colnames(test_df1)[c(104, 105, 107, 109, 112, 113)]
re_df2 <- purrr::map_dfr(df1_variables.x, function(x) {
  purrr::map_dfr(df1_variables.y, ~ mgp.pcor(
    x = x,
    y = .x,
    cov = co.variables,
    data = test_df1,
    method = "pearson"
  ))
} )
datatable(re_df2)

# test_df2 示例
colnames(test_df2)
df2_variables.x <- colnames(test_df2)[7:14]
df2_variables.y <- colnames(test_df2)[7:14]
re_df <- purrr::map_dfr(df2_variables.x, function(x) {
  purrr::map_dfr(df2_variables.y, ~ mgp.pcor(
    x = x,
    y = .x,
    cov = NULL,
    data = test_df2,
    method = "pearson"
  ))
} )
datatable(re_df)

# 线性回归：mgp.lm -------------------------------------------------------------------------
# test_df1 示例
re_ls <- mgp.lm(x = "WC", y = "BMI", covariable = NULL, data = test_df1)

## 自变量（x）循环
# variables.x <- colnames(test_df1)[14:21]
df1_variables.x
re_ls <- purrr::map(df1_variables.x, ~ mgp.lm(x = .x, 
                                          y = "BMI", 
                                          covariable = NULL, 
                                          data = test_df1))
names(re_ls) <- df1_variables.x
re_df <- purrr::map_dfr(re_ls, ~ .x$lm_result)

## 增加协变量
re_ls <- purrr::map(df1_variables.x, ~ mgp.lm(x = .x, 
                                           y = "BMI", 
                                           covariable = co.variables, 
                                           data = test_df1)) 
names(re_ls) <- df1_variables.x
re_mod <- purrr::map(re_ls, ~ .x$lm_reg)
re_df <- purrr::map_dfr(re_ls, ~ .x$lm_result)

## 模型可视化
forest_model(re_mod$WC)

## test_df1.1 (因子转换)
test_df1.1 <- test_df1 %>% mutate(across(.cols = co.variables, ~ as.factor(.x)))
str(test_df1[, co.variables])
str(test_df1.1[, co.variables])

# test_df2
# variables.x <- colnames(test_df2)[7:14]
df2_variables.x
re_df <- purrr::map(df2_variables.x, ~ mgp.lm(x = .x, 
                                          y = "年龄", 
                                          covariable = NULL, 
                                          data = test_df2)) %>% 
  purrr::map_dfr(., ~ .x$lm_result)


# 二分类logistic回归分析:mgp.glm_bi -------------------------------------------------------------------------
# test_df1.1 示例
re_ls <- mgp.glm_bi(x = "WC", 
           y = "MetS_P", 
           covariable = co.variables, 
           data = test_df1.1)
re_ls$glm_reg %>% summary()
forest_model(re_ls$glm_reg)

# 循环
df1.1_variables.x <- c(df1_variables.x, co.variables)
re_df2 <- purrr::map(df1.1_variables.x, ~ mgp.glm_bi(x = .x, 
                                           y = "MetS_P", 
                                           # covariable = co.variables, 
                                           data = test_df1.1)) %>% 
  purrr::map_dfr(., ~ .x$glm_result)
datatable(re_df2)

# 类似分析
# 负二项回归分析:Negative Binomial Regression Analysis
mgp.glm_negb(x = x, y = y, covariable = NULL, data = data)
# 泊松回归分析:Poisson Regression Analysis 
mgp.glm_poisson(x = x, y = y, covariable = NULL, data = data)
# 有序logistic回归分析:Ordinal Logistic Regression Analysis
mgp.glm_ord(x = x, y = y, covariable = NULL, data = data)


# 无序多分类logistic回归分析:mgp.glm_mult -------------------------------------------------------------------------
# test_df2
# variables.x <- colnames(test_df2)[7:14]
re_ls <- mgp.glm_mult(x = "左前臂_BMD平均值_差值", 
             y = "分组", 
             covariable = NULL, 
             ref = "对照组", 
             data = test_df2)
re_ls$fit %>% summary()
re_ls$fit_result

test_df2$"左前臂_BMD平均值_差值" %>% summary()
test_df2$"左前臂_BMD平均值_差值" <- test_df2$"左前臂_BMD平均值_差值" %>% 
  cut_number(4) %>% fct_anon()


# Cox比例风险回归模型:mgp.cox -------------------------------------------------------------------------
# test_df1.1
re_ls <- mgp.cox(x = "WC", 
                    y = "MetS_P1", 
                    time_var = "FY1",
                    covariable = co.variables, 
                    data = test_df1.1)
re_ls$fit %>% summary()
forest_model(re_ls$fit)
re_ls$fit_result

# 循环
re_df <- purrr::map(df1.1_variables.x, ~ mgp.cox(x = .x, 
                                               y = "MetS_P1", 
                                               time_var = "FY1",
                                               # covariable = co.variables, 
                                               data = test_df1.1)) %>% 
  purrr::map_dfr(., ~ .x$fit_result)
datatable(re_df)

# 加速失效时间模型:Accelerated Failure Time Model
# mgp.AFT(x = x, y = y, time_var = time_var, covariable = NULL, data = data)


# 立方样条分析（二分类logistic回归）：mgp.spline_lrm -------------------------------------------------------------------------
# 设置环境
ddist <- datadist(test_df1.1)
options(datadist = "ddist")

# 设置参数
xvar <- "BMI"
yvar <- "MetS_P"
# spline <- c(0.25, 0.5, 0.75)
spline1 <- c(0.1, 0.2, 0.5, 0.8, 0.9)
spline2 <- 5

# 运行模型
re_fit1 <- mgp.spline_lrm(
  y = yvar,
  x = xvar,
  covariable = co.variables,
  # spline = spline1,
  data = test_df1.1
)

re_fit2 <- mgp.spline_lrm(
  y = yvar,
  x = xvar,
  covariable = co.variables,
  spline = spline1,
  data = test_df1.1
)

re_fit3 <- mgp.spline_lrm(
  y = yvar,
  x = xvar,
  covariable = co.variables,
  spline = spline2,
  data = test_df1.1
)

re_fit1 %>% anova()
re_fit2 %>% anova()
re_fit3 %>% anova()

# 预测 ?
pred_OR <- Predict(re_fit3, "BMI", ref.zero=TRUE, fun=exp)

# 绘图
ggplot()+
  geom_line(data=pred_OR, aes(get(xvar), yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=pred_OR, aes(get(xvar), ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+
  geom_hline(yintercept=1, linetype=2, size=1)+
  labs(x = xvar, y="OR (95% CI)") 

# 批量
df1_variables.x
re_ls <- purrr::map(df1_variables.x, ~ mgp.spline_lrm(
  y = yvar,
  x = .x,
  covariable = co.variables,
  spline = spline1,
  data = test_df1.1
))
names(re_ls) <- df1_variables.x
re_ls$WC %>% anova()

## 预测 
pred_OR <- Predict(re_ls$SBP, "SBP", ref.zero=TRUE, fun=exp)
xvar <- "SBP"

## 绘图
ggplot()+
  geom_line(data=pred_OR, aes(get(xvar), yhat),linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=pred_OR, aes(get(xvar), ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+
  geom_hline(yintercept=1, linetype=2, size=1)+
  labs(x = xvar, y="OR (95% CI)") 


# 立方样条分析（Cox比例风险回归）：Restricted cubic spline for cox
mgp.spline_cox(
  y = y,
  x = x,
  covariable = NULL,
  spline = c(0.25, 0.5, 0.75),
  time_var = time_var,
  data = data
)

# 中介分析：mgp.mediate -------------------------------------------------------------------------
# 函数
mgp.mediate(
  x = x,
  y = y,
  m = m,
  covariable = NULL,
  boot = F,
  sims = 100,
  out.model = c("glm", "lm"),
  med.model = c("glm", "lm"),
  data = data
)

# 设置环境
setRefClass("PS",
            fields = list(
              m = "character",
              x = "character",
              med.f = "formula",
              out.f = "formula"
            ))
para_set <- new("PS")

# 运行
set.seed(123)
re_ls <- mgp.mediate(
  x = "WC",
  y = "MetS_P",
  m = "diabetes",
  covariable = co.variables,
  boot = T,
  sims = 100,
  out.model = "glm",
  med.model = "glm",
  data = test_df1.1
)

re_ls$med.fit %>% summary()
re_ls$med.result

# 循环
re_ls <- purrr::map(df1_variables.x, ~ mgp.mediate(
  x = "BMI",
  y = "MetS_P1",
  m = .x,
  covariable = co.variables,
  boot = F,
  sims = 100,
  out.model = "glm",
  med.model = "lm",
  data = test_df1.1
))
names(re_ls) <- paste0("m_", df1_variables.x)

re_ls$m_SBP$med.fit %>% summary()
re_ls$m_SBP$med.result

re_df <- purrr::map_dfr(re_ls, ~ .x$med.result)
datatable(re_df)
