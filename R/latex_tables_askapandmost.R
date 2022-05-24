# library(Hmisc)
library(errors)
library(xtable)

source('R/utilities.R')
source('R/combineRadioData.R')
source('R/utilities.R')

askapmost.data <- data.askap_most %>%
	filter(idPNMain %in% data.radio.combined$idPNMain) %>%
	filter(!is.na(askapFlux) | !is.na(mostFlux)) %>%
	select(Name,
				 askapRA,
				 askapDEC,
				 askapPeak,
				 askapPeak_err,
				 askapFlux,
				 askapFlux_err,
				 askapRMS,
				 mostRA,
				 mostDEC,
				 mostPeak,
				 mostPeak_err,
				 mostFlux,
				 mostFlux_err,
				 mostRMS
				 ) %>% 
	mutate(
		askapPeak_err = ifelse(askapPeak_err == 0, askapPeak/10, askapPeak_err), 
		askapFlux_err = ifelse(askapFlux_err == 0, askapFlux/10, askapFlux_err),
		mostPeak_err = ifelse(mostPeak_err == 0, mostPeak/10, mostPeak_err), 
		mostFlux_err = ifelse(mostFlux_err == 0, mostFlux/10, mostFlux_err),
		askapPeak_err = ifelse(askapPeak_err < 0, NA, askapPeak_err), 
		askapFlux_err = ifelse(askapFlux_err < 0, NA, askapFlux_err),
		mostPeak_err = ifelse(mostPeak_err < 0, NA, mostPeak_err), 
		mostFlux_err = ifelse(mostFlux_err < 0, NA, mostFlux_err),
		askapPeak = ifelse(is.na(askapPeak_err), as.integer(askapPeak), askapPeak), 
		askapFlux = ifelse(is.na(askapFlux_err), as.integer(askapFlux), askapFlux),
		mostPeak = ifelse(is.na(mostPeak_err), as.integer(mostPeak), mostPeak), 
		mostFlux = ifelse(is.na(mostFlux_err), as.integer(mostFlux), mostFlux),
		askapRMS = round(askapRMS, 2),
		mostRMS = round(mostRMS, 2)
		)


errors(askapmost.data$askapPeak) <- askapmost.data$askapPeak_err
errors(askapmost.data$askapFlux) <- askapmost.data$askapFlux_err
errors(askapmost.data$mostPeak) <- askapmost.data$mostPeak_err
errors(askapmost.data$mostFlux) <- askapmost.data$mostFlux_err


askapmost.data <- askapmost.data %>%
	select(-c(askapPeak_err, askapFlux_err, mostPeak_err, mostFlux_err))

askapmost.data <- format(askapmost.data, 1, notation="plus-minus")

###########################################################
########## PARSE TO CSV ###################################
###########################################################
# empty_char = ""
empty_char = "..."


most_parsed.Peak <- parse_errors_to_csv(askapmost.data$mostPeak, 
																				'Peak_most', 'Peak_err_most', empty_char)
most_parsed.Flux <- parse_errors_to_csv(askapmost.data$mostFlux, 
																				'Flux_most', 'Flux_err_most', empty_char)
askap_parsed.Peak <- parse_errors_to_csv(askapmost.data$askapPeak, 
																				 'Peak_askap', 'Peak_err_askap', empty_char)
askap_parsed.Flux <- parse_errors_to_csv(askapmost.data$askapFlux, 
																				 'Flux_askap', 'Flux_err_askap', empty_char)


askapmost.data.csv <- cbind(askapmost.data, most_parsed.Peak)
askapmost.data.csv <- cbind(askapmost.data.csv, most_parsed.Flux)
askapmost.data.csv <- cbind(askapmost.data.csv, askap_parsed.Peak)
askapmost.data.csv <- cbind(askapmost.data.csv, askap_parsed.Flux)

askapmost.data.csv <- askapmost.data.csv %>%
	mutate(
		mostRA = ifelse(mostRA == 'NA', empty_char, mostRA),
		mostDEC = ifelse(mostDEC == 'NA', empty_char, mostDEC),
		mostRMS = ifelse(mostRMS == 'NA', empty_char, mostRMS),
		
		Peak_most = ifelse(Peak_most == 'NA', empty_char, Peak_most),
		Peak_err_most = ifelse(Peak_err_most == 'NA', empty_char, Peak_err_most),
		Flux_most = ifelse(Flux_most == 'NA', empty_char, Flux_most),
		Flux_err_most = ifelse(Flux_err_most == 'NA', empty_char, Flux_err_most),

		askapRA = ifelse(askapRA == 'NA', empty_char, askapRA),
		askapDEC = ifelse(askapDEC == 'NA', empty_char, askapDEC),
		askapRMS = ifelse(askapRMS == 'NA', empty_char, askapRMS),
		
		Peak_askap = ifelse(Peak_askap == 'NA', empty_char, Peak_askap),
		Peak_err_askap = ifelse(Peak_err_askap == 'NA', empty_char, Peak_err_askap),
		Flux_askap = ifelse(Flux_askap == 'NA', empty_char, Flux_askap),
		Flux_err_askap = ifelse(Flux_err_askap == 'NA', empty_char, Flux_err_askap)
		)

askapmost.data.csv <- askapmost.data.csv %>%
	select(-c(mostPeak,mostFlux,askapPeak,askapFlux))


askapcsv <- askapmost.data.csv %>%
	filter(askapRA != '...') %>%
	select(Name, askapRA, askapDEC, Peak_askap, Peak_err_askap, Flux_askap, Flux_err_askap, askapRMS)

mostcsv <- askapmost.data.csv %>%
	filter(mostRA != '...') %>%
	select(Name, mostRA, mostDEC, Peak_most, Peak_err_most, Flux_most, Flux_err_most, mostRMS)


write.csv(x = askapcsv,file = 'files/askap_detections.csv',quote = T)
write.csv(x = mostcsv,file = 'files/most_detections.csv',quote = T)

###########################################################
########## PARSE TO LATEX #################################
###########################################################

## askap 

askap.data.latex <- askapmost.data %>%
	filter(askapRA != 'NA') %>%
	select(Name, askapRA, askapDEC, askapPeak, askapFlux, askapRMS) %>%
	rename(RA = askapRA,DEC = askapDEC,Peak = askapPeak,Flux = askapFlux, rms = askapRMS) %>%
	mutate(Peak = gsub("NA ± NA","...", as.character(Peak), fixed = T),
				 Flux = gsub("NA ± NA","...", as.character(Flux), fixed = T),
				 Peak = gsub("±","$\\pm$", as.character(Peak), fixed = T),
				 Flux = gsub("±","$\\pm$", as.character(Flux), fixed = T)
	)

askap.res <- xtable(x = askap.data.latex[1:5,],
			 caption = "CAPTION",
			 label = "fig:askaptable",
			 align = c('l','l','l','l','l','l','l')
			 	)

print(askap.res,file = 'files/askap_fluxes.tex',
			include.rownames=FALSE,
			sanitize.text.function = identity)

## most

most.data.latex <- askapmost.data %>%
	filter(mostRA != 'NA') %>%
	select(Name, mostRA, mostDEC, mostPeak, mostFlux, mostRMS) %>%
	rename(RA = mostRA,DEC = mostDEC,Peak = mostPeak,Flux = mostFlux, rms = mostRMS) %>%
	mutate(Peak = gsub("NA ± NA","...", as.character(Peak), fixed = T),
				 Flux = gsub("NA ± NA","...", as.character(Flux), fixed = T),
				 Peak = gsub("±","$\\pm$", as.character(Peak), fixed = T),
				 Flux = gsub("±","$\\pm$", as.character(Flux), fixed = T)
	)

most.res <- xtable(x = most.data.latex[1:5,],
			 caption = "CAPTION",
			 label = "fig:mosttable",
			 align = c('l','l','l','l','l','l','l')
			 	)

print(most.res,file = 'files/most_fluxes.tex',
			include.rownames=FALSE,
			sanitize.text.function = identity)

