setwd("~/dropbox/globdev/")

library("data.table")
library("ggplot2")
library("writexl")
library("stringi")

source("src/globdev_functions.R")

vrbs = c("lifexp", "polity2", "avedu", "stature",
    "gini", "real_wage", "homicide_rate", "so2emis_pc") #, "co2emis_pc", "biodiv")

clio = data.table::fread("dat/clioannual.csv")
clio5 = data.table::fread("dat/clio5y.csv")

# clio5[, polity2 := polity2 + 11]
# clio[, polity2 := polity2 + 11]

# ensure all country-year combinations are included
clio5 = unique(clio[, list(ccode, iso3c)])[clio5, on = 'ccode']

# ideally subset the dataset you do the predictions on...
clio5[, paste0(vrbs, '_predicted_loglin') := lapply(.SD, function(x) predict(lm(x ~ log(gdppc2011)), newdata = data.frame(x = x))), .SDcols = vrbs]
# clio5[, paste0(vrbs, '_predicted_loglog') := lapply(.SD, function(x) exp(predict(lm(log1p(x) ~ log(gdppc2011)), newdata = data.frame(x = x)))), .SDcols = vrbs]

# country groups for plots
early_indus = c("GBR", "USA", "DEU", "FRA")
late_indus = c("SWE", "ITA")
latam = c("ARG", "CHL", "BRA", "VEN", "PER", "MEX")
africa = c("ZAF", "KEN", "NGA", "GHA", "UGA", "BFA") # , "BWA", "AGO", "TZA", "BFA")
asia = c("CHN", "IND", "IDN", "PHL", "VNM", "THA")
easteu = c("RUS", "POL", "HUN", "ROU", "BGR", "EST")
regionlist = list(latam = latam, africa = africa, asia = asia, easteu = easteu) #, early = early_indus, late = late_indus)

fullnames = paste0(lngvrbs, '\n(', msrunits, ")")
names(fullnames) = names(lngvrbs)

outlist = list()

pdf("out/predpanels_current_indus.pdf", height = 10, width = 9)
vrbs_currentindus = c("lifexp", "polity2", "avedu", "gini",
                      "real_wage", "homicide_rate")
for (rgn in names(regionlist)){
    x = melt(clio5[iso3c %in% regionlist[[rgn]] & y5 >= 1950], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs_currentindus, paste0(vrbs_currentindus, '_predicted_loglin')))
    x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
    x[, variable := gsub("_predicted_loglin", "", variable)]

    plt = ggplot(na.omit(x)) +
        geom_line(aes(y5, value, col = predicted)) +
        geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) +
        facet_grid(variable ~ iso3c, scales = 'free_y',
            labeller = labeller(.rows = fullnames), switch = 'y') + # only works in this orientation, facet_wrap otherwise
        scale_x_continuous(breaks = c(1950, 2000)) +
        theme_bw() +
        labs(y = "", x = 'year') +
        scale_colour_manual(values = c(1, blue)) +
        scale_alpha_manual(values=c(1, 0)) +
        theme(legend.position = "bottom",
            strip.background = element_blank(),
            strip.placement = "outside",
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    print(plt)
    outlist[[rgn]] = data.table::dcast(x, iso3c + y5~ ...)
}
dev.off()

writexl::write_xlsx(outlist, path = 'out/gdppreds_current.xlsx')

writexl::write_xlsx(clio5[iso3c %in% c(early_indus, late_indus) & y5 >= 1820],
    path = "out/gdppred_earlyindus.xlsx")

x = melt(clio5[iso3c %in% c(early_indus, late_indus) & y5 >= 1820],
    id.vars = c("y5", "iso3c"),
    measure.vars = c(vrbs, paste0(vrbs, '_predicted_loglin')))
x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
x[, variable := gsub("_predicted_loglin", "", variable)]

pdf("out/predpanels_early_indus.pdf", height = 11, width = 8.27)
ggplot(na.omit(x)) +
    geom_line(aes(y5, value, col = predicted)) +
    geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) +
    facet_grid(variable ~ iso3c, scales = 'free_y',
        labeller = labeller(.rows = fullnames), switch = 'y') +
    scale_x_continuous(breaks = c(1820, 2000)) +
    theme_bw() +
    labs(y = "", x = 'year') +
    scale_colour_manual(values = c(1, blue)) +
    scale_alpha_manual(values=c(1, 0)) +
    theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()


x = data.table::melt(clio5[y5 >= 1950], id.vars = c("y5", "iso3c", "region"), measure.vars = vrbs)
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = iso3c == "ZAF", group = iso3c)) + 
    facet_wrap( ~ variable, scales = 'free')

# life expectancy reductions
clio5[!is.na(lifexp), list(y5, reduction = lifexp - shift(lifexp)), by = iso3c][reduction <= -2][order(reduction)]

# regional means
x = data.table::melt(
        clio5[y5 >= 1950, lapply(.SD, mean, na.rm = T), .SDcols = vrbs, by = list(region, y5)]
        , id.vars = c("y5", "region"), measure.vars = vrbs)
ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = region, group = region)) + 
    facet_wrap( ~ variable, scales = 'free')

ctrs = c("GBR", "NLD", "ITA", "JPN", "CHN", "IND", "BRA")
x = melt(clio5[iso3c %in% ctrs], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs[c(2:6, 8)], paste0(vrbs[c(2:6, 8)], '_predicted_loglin')))
x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
x[, variable := gsub("_predicted_loglin", "", variable)]

pdf("out/predpanels_presentation.pdf", width = 11.69, height = 8.27)
plt = ggplot(na.omit(x)) + 
    geom_line(aes(y5, value, col = predicted)) + 
    geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
    facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
    scale_x_continuous(breaks = c(1800, 2000)) + 
    theme_bw() +
    scale_colour_manual(values = c(1, blue)) + 
    scale_alpha_manual(values=c(1, 0)) + 
    theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
print(plt)
dev.off()

pdf("out/predpanels_all_sketch.pdf", height = 11.69, width = 8.27)
for (ctrs in list(latam, asia, africa, easteu, c(early_indus, late_indus))){
    x = melt(clio5[iso3c %in% ctrs], id.vars = c("y5", "iso3c"), measure.vars = c(vrbs, paste0(vrbs, '_predicted_loglin')))
    x[, predicted := ifelse(grepl("_predicted_loglin", variable), "predicted", "actual")]
    x[, variable := gsub("_predicted_loglin", "", variable)]

    plt = ggplot(na.omit(x)) + 
        geom_line(aes(y5, value, col = predicted)) + 
        geom_point(aes(y5, value, col = predicted, alpha = predicted), size = 0.9) + 
        facet_grid(variable ~ iso3c, scales = 'free_y') + # only works in this orientation, facet_wrap otherwise
        scale_x_continuous(breaks = c(1800, 2000)) + 
        theme_bw() +
        scale_colour_manual(values = c(1, blue)) + 
        scale_alpha_manual(values=c(1, 0)) + 
        theme(legend.position = "bottom", 
            legend.title = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(plt)
}
dev.off()
