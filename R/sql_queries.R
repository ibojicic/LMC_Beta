#radio data
query.radiodata <- "SELECT 
										idPNMain, 
										freq, 
										band, 
										flagFlux, 
										Flux, 
										Flux_err, 
										bibtex, 
										refKey, 
										instrument,
										InUse
										FROM LMC_Beta_Paper.radioData
										WHERE `reference` NOT IN ('marx1997');"

# fit results
query.results <- "SELECT `idPNMain`,`lin_slope`, `lin_slope_err`, `n_points`, `flag`
									FROM LMC_Beta_Paper.pne_fit_results;"

# object flags
query.flags <- "SELECT * FROM LMC_Beta_Paper.object_flags;"

# object names and positions
query.meta <- "SELECT `idPNMain`,
											`idPNMain` as 'id',
											`Name`, 
											`RAJ2000` as 'RA',
											`DECJ2000` as 'DEC'
											FROM LMC_Beta_Paper.Original_only_fluxes 
											ORDER BY `Best_Match_RA_Degrees` ASC;"

# `Best_Match_RA_Degrees` as 'RA',
# `Best_Match_DEC_Degrees` as 'DEC'


# oject classification
query.class <- "SELECT * FROM LMC_Beta_Paper.Kevin_flags;"

# optical detection
query.optical <- "SELECT * FROM LMC_Beta_Paper.flags_optical;"

# optical detection
query.ir <- "SELECT * FROM LMC_Beta_Paper.flags_ir;"

# compare askap, most and sumss
query.new40cm <- "SELECT idPNMain,
												`36cm_Integ` as 'most', 
												ASKAP_Integ as 'askap', 
												SUMSS_Integ as 'sumss',
												`36cm_Integ_Err` as 'most_err', 
												ASKAP_Integ_Err as 'askap_err', 
												SUMSS_Integ_Err as 'sumss_err' 

									FROM LMC_Beta_Paper.Original_only_fluxes;"

query.allflags <- "SELECT 
									o.`idPNMain`,
									IF(b.`inside` IS NULL, 'false', b.`inside`)  as '20cm_box',
									IF(f.`blended` IS NULL, 'false', f.`blended`)  as '20cm_blended',
									IF(f.`double_extended` IS NULL, 'false', f.`double_extended`) as '20cm_dbl_ext',
									IF(f.`confused` IS NULL, 'false', f.`confused`) as '20cm_confused',
									IF(i.`MACHO_recno` IS NULL, 'false', i.`MACHO_recno`) as 'MACHO_det',
									IF(i.`ALLWISE_Name` IS NULL, 'false', i.`ALLWISE_Name`) as 'WISE_det',
									IF(p.`compact_MCELS` IS NULL, 'false', p.`compact_MCELS`) as 'MCELS_det',
									IF(p.`Koz1_det` IS NULL, 'false', p.`Koz1_det`) as 'Koz1_det',
									IF(p.`Koz3_det` IS NULL, 'false', p.`Koz3_det`) as 'Koz3_det',
									IF(p.`SixDf_det` IS NULL, 'false', p.`SixDf_det`) as 'SixDf_det'
									FROM LMC_Beta_Paper.Original_only_fluxes o 
									LEFT JOIN LMC_Beta_Paper.box_20cm b ON o.`idPNMain` = b.`idPNMain`
									LEFT JOIN LMC_Beta_Paper.flags_20cm f ON o.`idPNMain` = f.`idPNMain`
									LEFT JOIN LMC_Beta_Paper.flags_ir i ON o.`idPNMain` = i.`idPNMain`
									LEFT JOIN LMC_Beta_Paper.flags_optical p ON o.`idPNMain` = p.`idPNMain`;"


# MWA local rms
# a 170-231
# b 139-170
# c 103-134
# d 072-103

query.mwa_rms <- "SELECT
									a.`idPNMain`,
									a.`local_rms` as 'a' ,
									b.`local_rms` as 'b' ,
									c.`local_rms` as 'c' ,
									d.`local_rms` as 'd' 
									FROM LMC_Beta_Paper.mwa_170_231 a
									LEFT JOIN LMC_Beta_Paper.mwa_139_170 b ON 
									a.`idPNMain` = b.`idPNMain`
									LEFT JOIN LMC_Beta_Paper.mwa_103_134 c ON 
									a.`idPNMain` = c.`idPNMain`
									LEFT JOIN LMC_Beta_Paper.mwa_072_103 d ON 
									a.`idPNMain` = d.`idPNMain`;"


query.radiodata.20cmframe <- "SELECT 
										idPNMain, 
										freq, 
										band, 
										flagFlux, 
										Flux, 
										Flux_err, 
										bibtex, 
										refKey, 
										instrument
										FROM LMC_Beta_Paper.radioData
										WHERE `band` = '20cm'
										AND `refKey` = 8
										AND `idPNMain` IN 
										(SELECT `idPNmain` FROM LMC_Beta_Paper.object_flags
										WHERE `box_20cm` = 'false');"

query.distances_to_centerfield = "SELECT * FROM LMC_Beta_Paper.distance_to_center;"

query.askap_most <- "SELECT 
						    f.`idPNMain`,
						    f.`Name`,
						    a.`RA` as 'askapRA',
						    a.`DEC` as 'askapDEC',
						    a.`Peak` as 'askapPeak',
						    a.`Peak_err` as 'askapPeak_err',
						    a.`Flux` as 'askapFlux',
						    a.`Flux_err` as 'askapFlux_err',
						    a.`RMS` as 'askapRMS',
						    b.`RA` as 'mostRA',
						    b.`DEC` as 'mostDEC',
						    b.`Peak` as 'mostPeak',
						    b.`Peak_err` as 'mostPeak_err',
						    b.`Flux` as 'mostFlux',
						    b.`Flux_err` as 'mostFlux_err',
						    b.`RMS` as 'mostRMS'
						FROM LMC_Beta_Paper.Original_only_fluxes f
								LEFT JOIN `LMC_Beta_Paper`.`askap_aegean` a 
								ON f.`idPNMain` = a.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`most_aegean` b
								ON f.`idPNMain` = b.`idPNMain`
						ORDER BY f.`Best_Match_RA_Degrees`;"


query.positional_offset <- "SELECT 
						    f.`idPNMain`,
						    a.`RAdeg` as 'askapRA',
						    a.`DECdeg` as 'askapDEC',
						    b.`RAdeg` as 'mostRA',
						    b.`DECdeg` as 'mostDEC',
						    c.`RAdeg` as 'atca20cmRA',
						    c.`DECdeg` as 'atca20cmDEC',
						    d.`RAdeg` as 'atca6cmRA',
						    d.`DECdeg` as 'atca6cmDEC',
						    e.`RAdeg` as 'atca3cmRA',
						    e.`DECdeg` as 'atca3cmDEC',
						    g.`RAdeg` as 'sumssRA',
						    g.`DECdeg` as 'sumssDEC'
						FROM LMC_Beta_Paper.Original_only_fluxes f
								LEFT JOIN `LMC_Beta_Paper`.`askap_aegean` a 
								ON f.`idPNMain` = a.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`most_aegean` b
								ON f.`idPNMain` = b.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`atca20cm_aegean` c
								ON f.`idPNMain` = c.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`atca6cm_aegean` d
								ON f.`idPNMain` = d.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`atca3cm_aegean` e
								ON f.`idPNMain` = e.`idPNMain`
								LEFT JOIN `LMC_Beta_Paper`.`sumss_cat` g
								ON f.`idPNMain` = g.`idPNMain`

;"






