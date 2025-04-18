library(ggplot2)
library(ggpubr)

ggviolin <- ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white")+
  stat_compare_means(method = "anova")




