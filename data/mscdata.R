mscdata <- local({
	load("mscdata.rda")
	if(capabilities("iconv")) {
		deg <- "\xb0C"
		deg <- iconv(deg, "latin1", "")
		if(is.na(deg)) deg <- "degC"
	}else{
		deg <- "degC"
	}
	attr(mscdata$t_max, "units") <- deg
	attr(mscdata$t_min, "units") <- deg
	attr(mscdata$t_mean, "units") <- deg
	mscdata
})

