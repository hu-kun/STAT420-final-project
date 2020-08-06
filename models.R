library(lmtest)
bp_test_decision = function(model, alpha){
  decide = unname(bptest(model)$p.value < alpha)
  ifelse(decide, "Reject", "Fail to Reject")
}
calc_loocv_rmse = function(model){
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

is.factor(suicide$generation)
suicide$generation = as.factor(suicide$generation)
is.factor(suicide$generation)

model1 = lm(suicides.100k.pop ~ suicides_no + hdi_year + gdp_year + generation + age + sex + population, data = suicide)
selected1 = step(model1, direction = "backward", trace = FALSE)
summary(selected1)$adj.r.squared
calc_loocv_rmse(selected1)
bp_test_decision(model1, 0.1)

model2 = lm(suicides.100k.pop ~ suicides_no * hdi_year * gdp_year * generation * age * sex * population, data = suicide)
selected2 = step(model2, direction = "backward", trace = FALSE)
summary(selected2)$adj.r.squared #so far it is the best one
calc_loocv_rmse(selected2)
bp_test_decision(model2, 0.1)

model3 = lm(suicides.100k.pop ~ gdp_per_capita * age * sex, data = suicide )
selected3 = step(model3, direction = "backward", trace = FALSE)
summary(selected3)$adj.r.squared
calc_loocv_rmse(selected3)
bp_test_decision(model3, 0.1)

model4 = lm(suicides.100k.pop ~ sex * age * gdp_per_capita * generation, data = suicide)
selected4 = step(model4, direction = "backward", trace = FALSE)
summary(selected4)$adj.r.squared
calc_loocv_rmse(selected4)
bp_test_decision(model4, 0.1)