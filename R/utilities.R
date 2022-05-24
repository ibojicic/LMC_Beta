library(glue)
library(NISTunits)


latex_format_nmbr <- function(nmbr) {
	if (nmbr < 1000) {
		res <- nmbr
	} else {
		p1 <- nmbr %/% 1000
		p2 <- nmbr %% 1000
		if (p2 < 100) {
			p2 <- paste("0",p2,sep = "",collapse = "")
		}
		res <- glue("{p1}\\,{p2}")
	}
	res
}

latex_format_ngtv <- function(nmbr) {
	if (nmbr >= 0) {
		nres <- mbr
	} else {
		res <- glue("${nmbr}$")
	}
	res
}


scaleFUN <- function(x) sprintf("%.1f", x)

bin_to_same_Ns <- function(main_data, limited_data, flux_column, 
													 numberofbins = 10, inp_value = 'lin_slope') {
	
	data.forbinning <- rbind(main_data,limited_data) %>% 
		select_(inp_value,Flux = flux_column) %>%
		filter(!is.na(Flux)) %>%
		mutate(logFlux = log10(Flux)) %>% 
		mutate(bin = cut_number(logFlux,numberofbins))
	
	binned <- data.forbinning %>% 
		group_by(bin) %>% 
		summarise_each(funs('median','mean','sd'),all_of(inp_value))
	binstartsends <- read.table(text = gsub("[^.0-9-]", " ", levels(data.forbinning$bin)), col.names = c("lower", "upper"))
	binned <- cbind(binned, 10**binstartsends)
	lastrow <- tail(binned,1) %>% mutate(lower = upper)
	binned <- rbind(binned, lastrow)
	
	binned
	
}

trim_string <- function (x) {
	gsub("^\\s+|\\s+$", "", x)
}


parse_errors_to_csv <- function(inp_vector, fl_clmn, err_clmn, empty_char) {

	result_fluxes <- trim_string(gsub("\\s+±\\s+.*$", "", inp_vector))
	result_fluxes <- gsub("NA",empty_char, result_fluxes)
	result_errors <- trim_string(gsub("^.*±\\s+", "", inp_vector))
	result_errors <- gsub("NA",empty_char, result_errors)
	
	result <- data.frame(result_fluxes, result_errors)
	colnames(result) <- c(fl_clmn, err_clmn)
	result
}

squared_err <- function(noise_err, flux, gain_perc) {
	sqrt(noise_err**2 + (gain_perc/100 * flux)**2)
}

radec_distance <- function(data) {
	# ramid = NISTdegTOradian(mean(c(ra1,ra2)))
	decmid = NISTdegTOradian(mean(c(data$dec1,data$dec2)))
	print(decmid)
	ra1 <- NISTdegTOradian(data$ra1)
	ra2 <- NISTdegTOradian(data$ra2)
	dec1 <- NISTdegTOradian(data$dec1)
	dec2 <- NISTdegTOradian(data$dec2)
	
	signsRA = (ra1 - ra2)/abs((ra1-ra2))
	signsDEC = (dec1 - dec2)/abs((dec1-dec2))
	
	resRA = signsRA * acos(sin(dec1) * sin(dec1) + cos(dec1) * cos(dec1) * cos(ra1 - ra2))
	resDEC = signsDEC * acos(sin(dec1) * sin(dec2) + cos(dec1) * cos(dec2))
	
	data.frame('dRA' = NISTradianTOsec(resRA), 'dDEC' = NISTradianTOsec(resDEC))
}
# radec_distance <- function(ra1,dec1,ra2,dec2) {
# 	# ramid = NISTdegTOradian(mean(c(ra1,ra2)))
# 	decmid = NISTdegTOradian(mean(c(dec1,dec2)))
# 	ra1 <- NISTdegTOradian(ra1)
# 	ra2 <- NISTdegTOradian(ra2)
# 	dec1 <- NISTdegTOradian(dec1)
# 	dec2 <- NISTdegTOradian(dec2)
# 	
# 	resRA = acos(sin(decmid) * sin(decmid) + cos(decmid) * cos(decmid) * cos(ra1 - ra2))
# 	resDEC = acos(sin(dec1) * sin(dec2) + cos(dec1) * cos(dec2))
# 	
# 	list('dRA' = NISTradianTOdeg(resRA), 'dDEC' = NISTradianTOdeg(resDEC))
# }




