setwd("~/dropbox/globdev/")

library("data.table")
library("ggplot2")

source("src/globdev_functions.R")
# vrbs = c("gdppc2011", "lifexp", "polity2", "avedu", "stature", "gini", "real_wage", "homicide_rate", "so2emis_pc")

clio = data.table::fread("dat/clioannual.csv")
clio5 = data.table::fread("dat/clio5y.csv")

clio5 = unique(clio[, list(ccode, iso3c)])[clio5, on = 'ccode']
vrbs = c("lifexp", "polity2", "avedu", "stature", "gini", 
        "real_wage", "homicide_rate", "so2emis_pc")

clio5[!is.na(gdppc2011), lapply(.SD, function(x) sum(!is.na(x))), .SDcols = vrbs, by = list(y5)]
cor.test(clio5$gdppc2011, clio5$lifexp)

locors = clio5[, lapply(.SD, function(x) as.numeric(try(cor.test(x, log(gdppc2011))$conf.int[1]))), .SDcols = vrbs, by = list(y5)]
upcors = clio5[, lapply(.SD, function(x) as.numeric(try(cor.test(x, log(gdppc2011))$conf.int[2]))), .SDcols = vrbs, by = list(y5)]
pointcor = clio5[, lapply(.SD, function(x) as.numeric(try(cor.test(x, log(gdppc2011))$estimate))), .SDcols = vrbs, by = list(y5)]
locors[, ci := 'lo']
upcors[, ci := 'up']
pointcor[, ci := 'point']

toplot = melt(rbindlist(list(locors, upcors, pointcor)), id.vars = c("y5", "ci"))

pdf("out/correlations_overtime.pdf", width = 11, height = 6)
ggplot(na.omit(toplot[!(variable == "avedu" & y5 < 1870)])) + 
    geom_line(aes(y5, value, size = ci), col = blue) + 
    facet_wrap( ~ variable, nrow = 2, labeller = labeller(variable = lngvrbs)) + 
    theme_bw() + 
    scale_x_continuous(name = "year") + 
    scale_y_continuous(name = "pearson cor. coef.") + 
    scale_size_manual(values = c(0.5, 1, 0.5)) + 
    geom_hline(yintercept = 0, col = 'gray') + 
    theme(legend.position = "none", 
      strip.background = element_rect(fill = NA),
      legend.title = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
dev.off()
