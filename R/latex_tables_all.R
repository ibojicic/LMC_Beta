# library(Hmisc)
library(errors)
library(xtable)

source('R/utilities.R')
source('R/combineRadioData.R')
source('R/utilities.R')

empty_char = "..."

# gain calibration error in percents for each  
gain_errors.askap <- 5 
gain_errors.atca <- 10 
gain_errors.most <- 10 

all.data.latex <- data.radio.combined %>%
	mutate(
		# `072-103_1_dS` = ifelse(`072-103_1_dS` == 0 | is.na(`072-103_1_dS`), 
		# 												`072-103_1_S`/10, `072-103_1_dS`),
		# `103-134_1_dS` = ifelse(`103-134_1_dS` == 0 | is.na(`103-134_1_dS`), 
		# 												`103-134_1_S`/10, `103-134_1_dS`),
		# `139-170_1_dS` = ifelse(`139-170_1_dS` == 0 | is.na(`139-170_1_dS`), 
		# 												`139-170_1_S`/10, `139-170_1_dS`),
		`170-231_10_dS` = ifelse(`170-231_10_dS` == 0 | is.na(`170-231_10_dS`), 
														`170-231_10_S`/10, `170-231_10_dS`),
		`70cm_2_dS` = ifelse(`70cm_2_dS` == 0 | is.na(`70cm_2_dS`), 
												 `70cm_2_S`/10, `70cm_2_dS`),
		# `40cm_6_dS` = ifelse(`40cm_6_dS` == 0 | is.na(`40cm_6_dS`), 
		# 										 `40cm_6_S`/10, `40cm_6_dS`),
		`20cm_8_dS` = ifelse(`20cm_8_dS` == 0 | is.na(`20cm_8_dS`), 
												 `20cm_8_S`/10, squared_err(`20cm_8_dS`, `20cm_8_S`, gain_errors.atca)),
		# `6cm_4_dS` = ifelse(`6cm_4_dS` == 0 | is.na(`6cm_4_dS`), 
		# 										`6cm_4_S`/10, `6cm_4_dS`), # pmn
		`6cm_7_dS` = ifelse(`6cm_7_dS` == 0 | is.na(`6cm_7_dS`), 
												`6cm_7_S`/10, squared_err(`6cm_7_dS`, `6cm_7_S`, gain_errors.atca)), # atca
		# `6cm_9_dS` = ifelse(`6cm_9_dS` == 0 | is.na(`6cm_9_dS`), 
		# 										`6cm_9_S`/10, `6cm_9_dS`), # at20g
		`3cm_7_dS` = ifelse(`3cm_7_dS` == 0 | is.na(`3cm_7_dS`), 
												`3cm_7_S`/10, squared_err(`3cm_7_dS`, `3cm_7_S`, gain_errors.atca)), # atca
		# `3cm_9_dS` = ifelse(`3cm_9_dS` == 0 | is.na(`3cm_9_dS`), 
		# 										`3cm_9_S`/10, `3cm_9_dS`), # at20g
		# `2cm_9_dS` = ifelse(`2cm_9_dS` == 0 | is.na(`2cm_9_dS`), 
		# 										`2cm_9_S`/10, `2cm_9_dS`) # at20g
		
	)

errors(all.data.latex$`072-103_1_S`) <- all.data.latex$`072-103_1_dS`
errors(all.data.latex$`103-134_1_S`) <- all.data.latex$`103-134_1_dS`
errors(all.data.latex$`139-170_1_S`) <- all.data.latex$`139-170_1_dS`
errors(all.data.latex$`170-231_1_S`) <- all.data.latex$`170-231_1_dS`

errors(all.data.latex$`70cm_2_S`) <- all.data.latex$`70cm_2_dS`
errors(all.data.latex$`40cm_6_S`) <- all.data.latex$`40cm_6_dS`
errors(all.data.latex$`20cm_8_S`) <- all.data.latex$`20cm_8_dS`
errors(all.data.latex$`6cm_4_S`) <- all.data.latex$`6cm_4_dS`
errors(all.data.latex$`6cm_7_S`) <- all.data.latex$`6cm_7_dS`
errors(all.data.latex$`6cm_9_S`) <- all.data.latex$`6cm_9_dS`
errors(all.data.latex$`3cm_7_S`) <- all.data.latex$`3cm_7_dS`
errors(all.data.latex$`3cm_9_S`) <- all.data.latex$`3cm_9_dS`
errors(all.data.latex$`2cm_9_S`) <- all.data.latex$`2cm_9_dS`

errors(all.data.latex$lin_slope_all) <- all.data.latex$lin_slope_err_all

###########################################################
########## PARSE TO CSV ###################################
###########################################################
# empty_char = ""

all.data.latex.csv <- format(all.data.latex, 2, notation="plus-minus")

all.data.latex.csv <- all.data.latex.csv[,c(2,3,4,5,7,9,11,13,15,21,23,25,27,29,31,33,35,37,41:50)]

fluxes.csv <- all.data.latex.csv[,c(1,2,3)]

fluxes.names <- names(all.data.latex.csv)

for (i in c(4:16)) {
	fluxesAnderrors <-  parse_errors_to_csv(all.data.latex.csv[,i], 
																					fluxes.names[i], 
																					sub("_S","_dS",fluxes.names[i]), 
																					empty_char)
	fluxes.csv <- cbind(fluxes.csv,fluxesAnderrors) 
}



fluxesAnderrors <-  parse_errors_to_csv(all.data.latex.csv[,17], 
																				'alpha', 
																				'alpha_err', 
																				empty_char)
fluxes.csv <- cbind(fluxes.csv,fluxesAnderrors) 

fluxes.csv <- cbind(fluxes.csv,sub('NA', empty_char,all.data.latex.csv[,c(18)]))

flags.csv <- all.data.latex.csv[,1]

flags.csv <- cbind(flags.csv,all.data.latex.csv[,19:28])

for (i in c(2:11)) {
	flags.csv[,i] = sub('false','f',flags.csv[,i])
	flags.csv[,i] = sub('true','t',flags.csv[,i])
}


fluxes.names <- c('Name','RA2000','DEC2000','MWA_072-103_S','MWA_072-103_dS', 
									'MWA_103-134_S','MWA_103-134_dS','MWA_139-170_S','MWA_139-170_dS', 
									'MWA_170-231_S','MWA_170-231_dS','MOST_70cm_S','MOST_70cm_dS', 
									'SUMSS_40cm_S','SUMSS_40cm_dS','ATCA_20cm_S','ATCA_20cm_dS', 
									'PMN_6cm_S','PMN_6cm_dS','ATCA_6cm_S','ATCA_6cm_dS','AT20G_6cm_S', 
									'AT20G_6cm_dS','ATCA_3cm_S','ATCA_3cm_dS','AT20G_3cm_S','AT20G_3cm_dS', 
									'AT20G_2cm_S','AT20G_2cm_dS','alpha','alpha_err','alpha_N')

names(fluxes.csv) <- fluxes.names

write.csv(x = fluxes.csv,file = 'files/main_fluxes.csv',quote = T)
write.csv(x = flags.csv,file = 'files/main_flags.csv',quote = T)


###########################################################
########## PARSE TO LATEX #################################
###########################################################

all.data.latex.latex <- format(all.data.latex, 1, notation="plus-minus")

all.data.latex.latex <- all.data.latex.latex[,c(2,3,4,5,7,9,11,13,15,21,23,25,27,29,31,33,35,37,41:50)]

# fluxes.latex <- all.data.latex.latex[,c(1,2,3,7,8,9,10,11,12,14,16,17,18)]
fluxes.latex <- all.data.latex.latex[,c(1,2,3,7,9,10,12,14,17)]

fluxes.latex <- apply(fluxes.latex, 2, function(y) gsub("NA ± NA", empty_char, y))
fluxes.latex <- apply(fluxes.latex, 2, function(y) gsub("±","$\\\\pm$", y))

fluxes.latex.res <- xtable(x = fluxes.latex[5726:5736,],
										caption = "CAPTION",
										label = "fig:maintable",
										align = c('l','l','l','l','l','l','l','l','l','l')
										# align = c('l','l','l','l','l','l','l','l','l','l','l','l','l','l')
)

print(fluxes.latex.res,file = 'files/main_fluxes_raw.tex',
			include.rownames=FALSE,
			sanitize.text.function = identity)

