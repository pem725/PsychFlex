## A closer look at the PF scale and subscales

mod1.dat <- read.csv("./Model1Dat.csv",header = T)

library(ggplot2)

ggplotRegression <- function(fit){
  require(ggplot2)
  require(ggExtra)
  p <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    geom_smooth() +
    labs(title = paste("Corr = ",signif(sqrt(summary(fit)$r.squared),2),
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ),
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)))
  ggExtra::ggMarginal(p, type="histogram", xparams = list(col="blue", fill="orange"), yparams = list(col="orange", fill="blue"))
}

## within subscales between goals
ggplotRegression(lm(av2~av1,data=mod1.dat))
ggplotRegression(lm(ac2~ac1,data=mod1.dat))
ggplotRegression(lm(h2~h1,data=mod1.dat))

## within goals between subscales
ggplotRegression(lm(av1~ac1,data=mod1.dat))
ggplotRegression(lm(av2~ac2,data=mod1.dat))
ggplotRegression(lm(av1~h1,data=mod1.dat))
ggplotRegression(lm(av2~h2,data=mod1.dat))
ggplotRegression(lm(ac1~h1,data=mod1.dat))
ggplotRegression(lm(ac2~h2,data=mod1.dat))

