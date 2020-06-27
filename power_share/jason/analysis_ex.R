### snow depth + irradiance at surface as cov, in year 2005
fc.cov3_5 <- readRDS("output/fc.cov3_5.rds")
forecast::autoplot(fc.cov3_5[[2]])
