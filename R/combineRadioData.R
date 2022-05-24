library(reshape2)
library(stringr)

source('R/loadData.R')
source('R/utilities.R')
# survey parameters

rms.3cm_7 <- 0.5 
rms.6cm_7 <- 0.28
rms.20cm_8 <- 0.5


# rename bands so column name doesn't start with number
# data.radio <- data.radio %>%
# 	mutate(band = gsub('^', 'b', band))

# convert errors ==0 to NAs
# data.radio <- data.radio %>%
# 	mutate(Flux_err = ifelse(Flux_err == 0, NA, Flux_err))
# 
# # apply gain callibration error to all ATCA errors  
# data.radio <- data.radio %>%
# 	mutate(Flux_err = ifelse(instrument == 'askap', 
# 													 squared_err(Flux_err, Flux, gain_errors.askap), 
# 													 Flux_err)) %>%
# 	mutate(Flux_err = ifelse(refKey %in% c(7,8), 
# 													 squared_err(Flux_err, Flux, gain_errors.atca), 
# 													 Flux_err)) %>%
# 	mutate(Flux_err = ifelse(refKey == 3, 
# 													 squared_err(Flux_err, Flux, gain_errors.most), 
# 													 Flux_err)) 

# select used radio fluxes
data.radio.used <- data.radio %>%
	filter(InUse %in% c('y','m','b'))

# cast radio fluxes
data.radio.fluxes <- dcast(data.radio.used, idPNMain ~ band+refKey, value.var = 'Flux')
data.radio.errfluxes <- dcast(data.radio.used, idPNMain ~ band+refKey, value.var = 'Flux_err')

# merge radio fluxes and errors
data.radio.combined <- merge(data.radio.fluxes, data.radio.errfluxes, by = 'idPNMain', all.x = T)
#rename radio columns
data.radio.combined <- data.radio.combined %>% rename_at(vars(ends_with(".x")), funs(str_replace(., ".x", "_S")))
data.radio.combined <- data.radio.combined %>% rename_at(vars(ends_with(".y")), funs(str_replace(., ".y", "_dS")))

# merge radio data and distances to the LMC center
data.radio.fluxes.combined <- merge(data.radio.combined, data.distances_to_centerLMC, by = 'idPNMain', all.x = T)

# filter spindex 
# data.radio.alphas.yesmwa <- data.spindex %>% filter(flag == 'a', !is.na(lin_slope)) %>% select(-flag)
# data.radio.alphas.nomwa <- data.spindex %>% filter(flag == 'b', !is.na(lin_slope)) %>% select(-flag)
# data.radio.alphas.twolowest <- data.spindex %>% filter(flag == 'c', !is.na(lin_slope)) %>% select(-flag)

data.radio.alphas.yesmwa <- data.spindex %>% filter(flag == 'l', !is.na(lin_slope)) %>% select(-flag)
data.radio.alphas.nomwa <- data.spindex %>% filter(flag == 'm', !is.na(lin_slope)) %>% select(-flag)
data.radio.alphas.twolowest <- data.spindex %>% filter(flag == 'n', !is.na(lin_slope)) %>% select(-flag)


data.radio.alphas.at20g <- data.spindex %>% filter(flag == 'd', !is.na(lin_slope)) %>% select(-flag)

data.radio.numberdensity.twolowest <- merge(data.radio.fluxes.combined, data.radio.alphas.twolowest, by ='idPNMain')

# filter optical data
data.optical.detection <- data.optical.flags %>% filter(compact_MCELS == 'true' | 
																													Koz1_det == 'true' |
																													Koz3_det == 'true' |
																													SixDf_det == 'true')


data.optical.detection.all <- with(data.optical.detection, 
																	 sum(compact_MCELS == 'true') + 
																	 	sum(Koz1_det == 'true') + 
																	 	sum(Koz3_det == 'true') + 
																	 	sum(SixDf_det == 'true'))

data.optical.detection.unique = length(data.optical.detection$idPNMain)

data.optical.detection.z <- data.optical.flags %>% filter(Koz1_z == 'true' |
																													Koz3_z == 'true' |
																													SixDf_z == 'true')

data.optical.detection.z.all <- with(data.optical.detection.z, 
																	 	sum(Koz1_z == 'true') + 
																	 	sum(Koz3_z == 'true') + 
																	 	sum(SixDf_z == 'true'))

data.optical.detection.z.unique = length(data.optical.detection.z$idPNMain)

data.radio.alphas.opticaldet <- data.radio.alphas.yesmwa %>% filter(idPNMain %in% data.optical.detection$idPNMain)

# alphas for optical detections
data.optical.detection.alphas <- merge(data.optical.detection,data.radio.alphas.yesmwa, by='idPNMain')

# filter optical data
data.ir.detection <- data.ir.flags %>% filter(MACHO_recno == 'true' | 
																							 	ALLWISE_Name == 'true')

data.ir.detection.alphas <- merge(data.ir.detection, data.radio.alphas.yesmwa, by='idPNMain')

data.ir.nodetection.alphas <- data.radio.alphas.yesmwa[!(data.radio.alphas.yesmwa$idPNMain %in% data.ir.detection$idPNMain),]

# filter sumss < 100 mJy

data.radio.under <- data.radio.fluxes %>% filter(data.radio.fluxes$`40cm_6` < 100) %>% select(idPNMain)
data.radio.under.alphas <- data.radio.alphas.yesmwa[!(data.radio.alphas.yesmwa$idPNMain %in% data.radio.under$idPNMain),]


k = names(data.radio.combined)
#rearange columns
data.radio.combined <- data.radio.combined[,c(1,
																							2,14,  # 170-231
																							13,25, # 70cm_2
																							9,21, # 40cm_6
																							8,20, # 40cm_3
																							7,19, # 40cm_1
																							3,15,  # 20cm_8
																							10,22, # 6cm_4
																							11,23, # 6cm_7
																							12,24, # 6cm_9
																							5,17,  # 3cm_7
																							6,18,  # 3cm_9
																							4,16   # 2cm_9
																							)
																					 ]


# merge with spindex
data.radio.combined <- merge(data.radio.combined, data.radio.alphas.yesmwa, by = 'idPNMain', all.x = T )
data.radio.combined <- merge(data.radio.combined, data.radio.alphas.nomwa, by = 'idPNMain', all.x = T)

#rename spindex columns
data.radio.combined <- data.radio.combined %>% rename_at(vars(ends_with(".x")), funs(str_replace(., ".x", "_all")))
data.radio.combined <- data.radio.combined %>% rename_at(vars(ends_with(".y")), funs(str_replace(., ".y", "_nomwa")))

# positions and names
data.radio.combined <- merge(data.radio.combined, data.meta, by = 'idPNMain', all.x = T)
k = names(data.radio.combined)

data.radio.combined <- data.radio.combined[, c(1,33,34,35,2:31)]

# all flags flags
data.radio.combined <- merge(data.radio.combined, data.allflags, by = 'idPNMain', all.x = T)

# already ordered by RA in sql query
# data.radio.combined <- data.radio.combined %>% arrange(RA)

# flux csv table
write.table(data.radio.combined, 
						"flux_table.csv", 
						na = "", 
						quote = F, 
						col.names = T, 
						sep = ",", 
						row.names = F)

totals <- sapply(data.radio.combined, function(x) sum(!is.na(x)))

# mwa local rms medians
mwa.medrms <- list(
	'170-231' = median(data.mwa_localrms$a, na.rm = T)
	)

# positional offsets

data.positional_offset <- data.positional_offset %>%
	filter(idPNMain %in% data.radio.combined$idPNMain)

inpdata.sumssVSaskap <- data.positional_offset %>%
	mutate(ra1 = sumssRA, dec1 = sumssDEC, ra2 = askapRA, dec2 = askapDEC)
offset.sumssVSaskap <- radec_distance(inpdata.sumssVSaskap)

inpdata.atca20cmVSaskap <- data.positional_offset %>%
	mutate(ra1 = atca20cmRA, dec1 = atca20cmDEC, ra2 = askapRA, dec2 = askapDEC)
offset.atca20cmVSaskap <- radec_distance(inpdata.atca20cmVSaskap)

inpdata.mostVSaskap <- data.positional_offset %>%
	mutate(ra1 = mostRA, dec1 = mostDEC, ra2 = askapRA, dec2 = askapDEC)
offset.mostVSaskap <- radec_distance(inpdata.mostVSaskap)

inpdata.mostVSsumss <- data.positional_offset %>%
	mutate(ra1 = mostRA, dec1 = mostDEC, ra2 = sumssRA, dec2 = sumssDEC)
offset.mostVSsumss <- radec_distance(inpdata.mostVSsumss)




