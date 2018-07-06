mar = c(4.5, 4, 2.5, 0.5)

devctrs = c(chile = 152, argentina = 32, brazil = 76, bolivia = 68, 
    equador = 218, south_africa = 710, botswana = 72, namibia = 516, 
    china = 156,  india = 356, indonesia = 360, thailand = 764,
    malaysia = 458, vietnam = 704, sri_lanka = 144,
    russia = 643, soviet_union = 810, poland = 616, ukraine = 804, 
    latvia = 428, estonia = 233, lithuania = 440)

cliocodes = c(GBR = 826, NLD = 528, FRA = 250, DEU = 276, DDR = 278,
              ITA = 380, ESP = 724, SWE = 752,
              POL = 616, RUS = 643, SUN = 810,
              AUS = 36, CAN = 124, USA = 840, 
              MEX = 484, BRA = 76, ARG = 32,
              EGY = 818, TUR = 792,
              KEN = 404, NGA = 566, ZAF = 710,
              CHN = 156, JPN = 392, IND = 356, IDN = 360, THA = 764)

grey <- rgb(204, 204, 204, maxColorValue=256)
blue <- rgb(red=79, green=129, blue=189, maxColorValue=256)
alphas <- seq(from=50, to=256, length.out=8)
blues <- NULL
for (i in 1:length(alphas)){
  blues[i] <- rgb(79, 129, 189, alpha=alphas[i], maxColorValue=256)
}
lightblue <- rgb(red=167, green=185, blue=227, maxColorValue=256)

vrbs = c("gdppc2011", "lifexp", "polity2", "avedu", "stature", "gini", "real_wage", "homicide_rate", "so2emis_pc", "co2emis_pc", "biodiv")

lngvrbs = c(gdppc2011   = "GDP/cap", 
    lifexp              = "Life expect.", 
    polity2             = "Polity 2", 
    avedu               = "Av. education", 
    stature             = "Stature", 
    gini                = "Income ineq.", 
    real_wage           = "Lab. real wage", 
    homicide_rate       = "Homicide rate", 
    so2emis_pc          = "SO2 emis./cap",
    co2emis_pc          = "CO2 emis./cap",
    population          = "population",
    biodiv              = "biodiversity")

msrunits = c(gdppc2011  = "2011 PPP$", 
    lifexp              = "years", 
    polity2             = "index", 
    avedu               = "years", 
    stature             = "cm", 
    gini                = "Gini coef.", 
    real_wage           = "subsist. baskets", 
    homicide_rate       = "homic./100k inh.", 
    co2emis_pc          = "ton C/capita", # 
    so2emis_pc          = "kg SO2/capita", # ton / capita originally
                                          # but everywhere *1000, so kg
                                         # but the PG is harder to reconstruct
    population          = "1000s",
    biodiv              = "mean species abund.")

MakeInits <- function(y, vrs){
  y$init <- NA
  for (i in 3:length(vrs)){ # 3=min for fantanal
    combinations <- combn(vrs, i)
    for (j in 1:ncol(combinations)){
      variables <- combinations[, j]
      dfa <- y[complete.cases(y[, variables]), variables]
      try(fa <- factanal(dfa, 1, scores='Bartlet'))
      y$init[match(rownames(fa$scores), rownames(y))] <- fa$scores
    }
  }
  initxi <- y$init
  initgamma <- t(apply(y[, variables], 2, function(x) lm(x ~ y$init)$coef))
  initomega <- apply(y[, variables], 2, function(x) summary(lm(x ~ y$init))$sigma)
  return(list(xi=initxi, gamma=initgamma, omega=initomega))
}

extract <- function(coda, varname){
  keep <- grep(varname, colnames(coda[[1]]))
  # out <- coda[[1]][, keep]
  out <- coda[, keep, drop=FALSE]
  out <- as.matrix(out)
  return(out)
}


# to improve
# http://akhilsbehl.github.io/blog/2014/08/20/r-converting-a-data-dot-table-to-a-multi-way-array-cube/
# dt2array = function (x, facts, dims) {
#   stopifnot(is.data.table(x))
#   setkeyv(x, rev(dims))
#   stopifnot(!any(duplicated(x)))
#   dimensions = lapply(x[ , rev(dims), with=FALSE],
#                       function (x) sort(unique(x)))
#   xFull = data.table(expand.grid(dimensions, stringsAsFactors=FALSE))
#   setkeyv(xFull, rev(dims))
#   x = data.table:::merge.data.table(xFull, x, by=dims, all=TRUE)
#   factsVec = unlist(x[ , facts, with=FALSE], recursive=FALSE, use.names=FALSE)
#   nFacts = length(facts)
#   nDims = length(dims)
#   if (nFacts > 1) {
#     dim(factsVec) = c(sapply(dimensions, length), nFacts)
#     dimnames(factsVec) = c(dimensions, "facts"=list(facts))
#     return(aperm(factsVec, perm=c(nDims:1, nDims + 1)))
#   } else {
#     dim(factsVec) = sapply(dimensions, length)
#     dimnames(factsVec) = dimensions
#     return(aperm(factsVec))
#   }
# }
