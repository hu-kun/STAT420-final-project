#For generation,1 = Boomers, 2 = G.I. Generation,3 = Generation X,4 = Generation Z,5 = Millenials,6 = Silent
#For sex, 1 = female, 2 = male

par(mfrow = c(2,3))
hist(suicide$suicides_no, breaks = 20, main = "Number of suicide",border="darkorange", col="dodgerblue")
hist(suicide$population, breaks = 20, main = "Population",border="darkorange", col="dodgerblue")
hist(suicide$suicides.100k.pop, breaks = 20, main = "Suicides per 100k people",border="darkorange", col="dodgerblue")
hist(suicide$age, breaks = 20, main = "Age group",border="darkorange", col="dodgerblue")
hist(as.numeric(suicide$generation), breaks = 20, main = "Generation",border="darkorange", col="dodgerblue")
hist(as.numeric(suicide$sex), breaks = 10, main = "Sex",border="darkorange", col="dodgerblue")