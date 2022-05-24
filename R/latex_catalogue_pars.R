library(glue)

source('R/utilities.R')
source('R/loadData.R')
source('R/combineRadioData.R')

src_total <- length(!is.na(data.radio$idPNMain))
SourcesTotal <- latex_format_nmbr(src_total)
src_total_unique <- length(data.radio.combined$idPNMain)
SourcesTotalUnique <- latex_format_nmbr(src_total_unique)

SourcesWithAtLeastTwoPoints <- latex_format_nmbr(length(data.radio.alphas.yesmwa[data.radio.alphas.yesmwa$n_points>1,]$idPNMain))
SourcesWithMoreThanTwoPoints <- latex_format_nmbr(length(data.radio.alphas.yesmwa[data.radio.alphas.yesmwa$n_points>2,]$idPNMain))

SpecIndex <- latex_format_ngtv(round(median(data.radio.alphas.yesmwa$lin_slope,na.rm = T),2))
SpecIndexSD <- round(sd(data.radio.alphas.yesmwa$lin_slope,na.rm = T),2)
SpecIndexSourceCount <- latex_format_nmbr(length(data.radio.alphas.yesmwa[data.radio.alphas.yesmwa$n_points>1,]$idPNMain))
SourcesCSS <- latex_format_nmbr(length(data.radio.alphas.yesmwa[data.radio.alphas.yesmwa$lin_slope < -0.8,]$idPNMain))

SpecIndexLowestTwoFreq <- latex_format_ngtv(round(median(data.radio.alphas.twolowest$lin_slope, na.rm = T),2))
SpecIndexLowestTwoFreqSD <- round(sd(data.radio.alphas.twolowest$lin_slope, na.rm = T),2)
SpecIndexLowestTwoFreqSourceCount <- latex_format_nmbr(length(data.radio.alphas.twolowest$lin_slope))

SpecIndexLowestTwoFreqLimited <- "NOT CLEAR" # ??
SpecIndexLowestTwoFreqLimitedSD <- "NOT CLEAR" # ??
SpecIndexLowestTwoFreqLimitedSourceCount <- "NOT CLEAR" # ??

SpecIndexATG <- latex_format_ngtv(round(median(data.radio.alphas.at20g$lin_slope, na.rm = T),2))
SpecIndexATGSD <- round(sd(data.radio.alphas.at20g$lin_slope, na.rm = T),2)
SpecIndexATGSourceCount <- length(data.radio.alphas.at20g$lin_slope)

SpecIndexOptical <- latex_format_ngtv(round(median(data.optical.detection.alphas$lin_slope, na.rm = T),2))
SpecIndexOpticalSD <- round(sd(data.optical.detection.alphas$lin_slope, na.rm = T),2)
SpecIndexOpticalSourceCount <- length(data.optical.detection.alphas$idPNMain)

SpecIndexRadioToIR <- latex_format_ngtv(round(median(data.ir.detection.alphas$lin_slope, na.rm = T),2))
SpecIndexRadioToIRSD <- round(sd(data.ir.detection.alphas$lin_slope, na.rm = T),2)
SpecIndexRadioToIRSourceCount <- length(data.ir.detection.alphas$idPNMain)

SpecIndexNoIR <- latex_format_ngtv(round(median(data.ir.nodetection.alphas$lin_slope, na.rm = T),2))
SpecIndexNoIRSD <- round(sd(data.ir.nodetection.alphas$lin_slope, na.rm = T),2)
SpecIndexNoIRSourceCount <- latex_format_nmbr(length(data.ir.nodetection.alphas$idPNMain))

SpecIndexUnder <- latex_format_ngtv(round(median(data.radio.under.alphas$lin_slope, na.rm = T),2))
SpecIndexUnderSD <- round(sd(data.radio.under.alphas$lin_slope, na.rm = T),2)
SpecIndexUnderSourceCount <- latex_format_nmbr(length(data.radio.under.alphas$idPNMain))

SourcesMOST <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`40cm_3`)))
SourcesASKAPBETA <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`40cm_1`)))

SourcesTwentyCm <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`20cm_8`)))
SourcesSixCm <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`6cm_7`)))
SourcesThreeCm <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`3cm_7`)))
SourcesAT <-  latex_format_nmbr(sum(!is.na(data.radio.fluxes$`2cm_9`)))
SourcesPMN <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`6cm_4`)))
SourcesSUMSS <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`40cm_6`)))
SourcesMRC <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`70cm_2`)))

SourcesMWAone <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`072-103_1`)))
SourcesMWAtwo <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`103-134_1`)))
SourcesMWAthree <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`139-170_1`)))
SourcesMWAfour <- latex_format_nmbr(sum(!is.na(data.radio.fluxes$`170-231_1`)))

tlf_count <- sum(!(is.na(data.radio.fluxes$`3cm_9`) & is.na(data.radio.fluxes$`20cm_8`)))
TwoLowestFreqCount <- latex_format_nmbr(tlf_count)
TwoLowestFreqCountPerc <- round(100 * tlf_count / src_total_unique,1)


OpticalSourcesTotal <- latex_format_nmbr(8358) # fixwd
OpticalSourcesTotalUnique <- latex_format_nmbr(7883) # fixed

OpticalSourcesTotalWithZ <- latex_format_nmbr(2392) # fixed
OpticalSourcesTotalWithZUnique <- latex_format_nmbr(1792) # fixed

OpticalMatches <- data.optical.detection.all
OpticalMatchesUnique <- data.optical.detection.unique

OpticalMatchesWithZ <- data.optical.detection.z.all
OpticalMatchesUniqueWithZ <- data.optical.detection.z.unique

IRMatches <- "NOT CLEAR"
IRMatchesWithOptical <- length(data.optical.detection[data.optical.detection$idPNMain %in% data.ir.detection$idPNMain,]$idPNMain)
IRMatchesWithRadio <- latex_format_nmbr(length(data.ir.detection$idPNMain))

SourceVariable <- 'FIX'
SourceVariablePerc <- 'FIX'
SourcesCSS <- "COUNT"
SourcesGPS <- "COUNT"
SourcesHFP <- "COUNT"
SourcesHFPpossible <- "COUNT"
SourcesInverted <- "COUNT"

Blended <- 'FIX'
XraySoft <- 'FIX'
XrayMedium <- 'FIX'
XrayHard <- 'FIX'
OpticalExtended <- 'FIX'
OpticalCompact <- 'FIX'

Offset20cmVSaskapRA <- round(median(offset.atca20cmVSaskap$dRA, na.rm = T),2)
Offset20cmVSaskapDEC <- round(median(offset.atca20cmVSaskap$dDEC, na.rm = T),2)
OffsetmostVSaskapRA <- round(median(offset.mostVSaskap$dRA, na.rm = T),2)
OffsetmostVSaskapDEC <- round(median(offset.mostVSaskap$dDEC, na.rm = T),2)

OffsetsumssVSaskapRA <- round(median(offset.sumssVSaskap$dRA, na.rm = T),2)
OffsetsumssVSaskapDEC <- round(median(offset.sumssVSaskap$dDEC, na.rm = T),2)
OffsetmostVSsumssRA <- round(median(offset.mostVSsumss$dRA, na.rm = T),2)
OffsetmostVSsumssDEC <- round(median(offset.mostVSsumss$dDEC, na.rm = T),2)
						
CommonMostVsSumss <- length(data.radio.fluxes[!is.na(data.radio.fluxes$`40cm_3`) & 
																			 	!is.na(data.radio.fluxes$`40cm_6`),]$idPNMain)
CommonAskapVsSumss <- length(data.radio.fluxes[!is.na(data.radio.fluxes$`40cm_1`) & 
																			 	!is.na(data.radio.fluxes$`40cm_6`),]$idPNMain)
catalogue_pars <-
"
%Defining the catalogue parameters so that they can be quickly updated. 
\\newcommand{{\\SourcesTotal}{{{SourcesTotal}}
\\newcommand{{\\SourcesTotalUnique}{{{SourcesTotalUnique}}

%Spec Index
\\newcommand{{\\SourcesWithAtLeastTwoPoints}{{{SourcesWithAtLeastTwoPoints}}
\\newcommand{{\\SourcesWithMoreThanTwoPoints}{{{SourcesWithMoreThanTwoPoints}}

\\newcommand{{\\SpecIndex}{{{SpecIndex}}  
\\newcommand{{\\SpecIndexSD}{{{SpecIndexSD}}
\\newcommand{{\\SpecIndexSourceCount}{{{SpecIndexSourceCount}}

\\newcommand{{\\SpecIndexLowestTwoFreq}{{{SpecIndexLowestTwoFreq}}
\\newcommand{{\\SpecIndexLowestTwoFreqSD}{{{SpecIndexLowestTwoFreqSD}}
\\newcommand{{\\SpecIndexLowestTwoFreqSourceCount}{{{SpecIndexLowestTwoFreqSourceCount}}

\\newcommand{{\\SpecIndexLowestTwoFreqLimited}{{{SpecIndexLowestTwoFreqLimited}}
\\newcommand{{\\SpecIndexLowestTwoFreqLimitedSD}{{{SpecIndexLowestTwoFreqLimitedSD}}
\\newcommand{{\\SpecIndexLowestTwoFreqLimitedSourceCount}{{{SpecIndexLowestTwoFreqLimitedSourceCount}}

\\newcommand{{\\SpecIndexATG}{{{SpecIndexATG}}
\\newcommand{{\\SpecIndexATGSD}{{{SpecIndexATGSD}}
\\newcommand{{\\SpecIndexATGSourceCount}{{{SpecIndexATGSourceCount}}

\\newcommand{{\\SpecIndexOptical}{{{SpecIndexOptical}}
\\newcommand{{\\SpecIndexOpticalSD}{{{SpecIndexOpticalSD}}
\\newcommand{{\\SpecIndexOpticalSourceCount}{{{SpecIndexOpticalSourceCount}}

\\newcommand{{\\SpecIndexRadioToIR}{{{SpecIndexRadioToIR}}
\\newcommand{{\\SpecIndexRadioToIRSD}{{{SpecIndexRadioToIRSD}}
\\newcommand{{\\SpecIndexRadioToIRSourceCount}{{{SpecIndexRadioToIRSourceCount}}

\\newcommand{{\\SpecIndexNoIR}{{{SpecIndexNoIR}}
\\newcommand{{\\SpecIndexNoIRSD}{{{SpecIndexNoIRSD}}  
\\newcommand{{\\SpecIndexNoIRSourceCount}{{{SpecIndexNoIRSourceCount}}

\\newcommand{{\\SpecIndexUnder}{{{SpecIndexUnder}}
\\newcommand{{\\SpecIndexUnderSD}{{{SpecIndexUnderSD}}
\\newcommand{{\\SpecIndexUnderSourceCount}{{{SpecIndexUnderSourceCount}}

%Core Four Frequencies. 
\\newcommand{{\\SourcesMOST}{{{SourcesMOST}} %MOST
\\newcommand{{\\SourcesTwentyCm}{{{SourcesTwentyCm}}   %ATCA
\\newcommand{{\\SourcesSixCm}{{{SourcesSixCm}}         %ATCA
\\newcommand{{\\SourcesThreeCm}{{{SourcesThreeCm}}    %ATCA
\\newcommand{{\\TwoLowestFreqCount}{{{TwoLowestFreqCount}}
\\newcommand{{\\TwoLowestFreqCountPerc}{{{TwoLowestFreqCountPerc}}

%MWA
\\newcommand{{\\SourcesMWAone}{{{SourcesMWAone}}
\\newcommand{{\\SourcesMWAtwo}{{{SourcesMWAtwo}}
\\newcommand{{\\SourcesMWAthree}{{{SourcesMWAthree}}
\\newcommand{{\\SourcesMWAfour}{{{SourcesMWAfour}}

%Extra Radio Catalogues
\\newcommand{{\\SourcesAT}{{{SourcesAT}} %ATCA
\\newcommand{{\\SourcesPMN}{{{SourcesPMN}}         %Parkes
\\newcommand{{\\SourcesSUMSS}{{{SourcesSUMSS}}    %MOST
\\newcommand{{\\SourcesMRC}{{{SourcesMRC}}    %MOST
\\newcommand{{\\SourcesASKAPBETA}{{{SourcesASKAPBETA}}    %ASKAP

%Optical Numbers
\\newcommand{{\\OpticalSourcesTotal}{{{OpticalSourcesTotal}}
\\newcommand{{\\OpticalSourcesTotalUnique}{{{OpticalSourcesTotalUnique}}

\\newcommand{{\\OpticalSourcesTotalWithZ}{{{OpticalSourcesTotalWithZ}}
\\newcommand{{\\OpticalSourcesTotalWithZUnique}{{{OpticalSourcesTotalWithZUnique}}

\\newcommand{{\\OpticalMatches}{{{OpticalMatches}}
\\newcommand{{\\OpticalMatchesUnique}{{{OpticalMatchesUnique}}

\\newcommand{{\\OpticalMatchesWithZ}{{{OpticalMatchesWithZ}}
\\newcommand{{\\OpticalMatchesUniqueWithZ}{{{OpticalMatchesUniqueWithZ}}

%IR Numbers
\\newcommand{{\\IRMatches}{{{IRMatches}}
\\newcommand{{\\IRMatchesWithOptical}{{{IRMatchesWithOptical}}
\\newcommand{{\\IRMatchesWithRadio}{{{IRMatchesWithRadio}}

%Calc
\\newcommand{{\\SourceVariable}{{{SourceVariable}}    %MOST
\\newcommand{{\\SourceVariablePerc}{{{SourceVariablePerc}}    %ASKAP
\\newcommand{{\\SourcesCSS}{{{SourcesCSS}}
\\newcommand{{\\SourcesGPS}{{{SourcesGPS}}
\\newcommand{{\\SourcesHFP}{{{SourcesHFP}}
\\newcommand{{\\SourcesHFPpossible}{{{SourcesHFPpossible}}
\\newcommand{{\\SourcesInverted}{{{SourcesInverted}}

%Catalogue Flags
\\newcommand{{\\Blended}{{{Blended}} 
\\newcommand{{\\XraySoft}{{{XraySoft}}   
\\newcommand{{\\XrayMedium}{{{XrayMedium}}         
\\newcommand{{\\XrayHard}{{{XrayHard}}     
\\newcommand{{\\OpticalExtended}{{{OpticalExtended}}   
\\newcommand{{\\OpticalCompact}{{{OpticalCompact}} 

%Offsets
\\newcommand{{\\OffsetAtcaTwcmVSaskapRA}{{{Offset20cmVSaskapRA}} 
\\newcommand{{\\OffsetAtcaTwcmVSaskapDEC}{{{Offset20cmVSaskapDEC}} 
\\newcommand{{\\OffsetMostVSaskapRA}{{{OffsetmostVSaskapRA}} 
\\newcommand{{\\OffsetMostVSaskapDEC}{{{OffsetmostVSaskapDEC}} 

\\newcommand{{\\OffsetSumssVSaskapRA}{{{OffsetsumssVSaskapRA}} 
\\newcommand{{\\OffsetSumssVSaskapDEC}{{{OffsetsumssVSaskapDEC}} 
\\newcommand{{\\OffsetMostVSsumssRA}{{{OffsetmostVSsumssRA}} 
\\newcommand{{\\OffsetMostVSsumssDEC}{{{OffsetmostVSsumssDEC}} 

%Commons
\\newcommand{{\\CommonMostVsSumss}{{{CommonMostVsSumss}} 
\\newcommand{{\\CommonAskapVsSumss}{{{CommonAskapVsSumss}} 

"
catalogue_pars_tex <- glue(catalogue_pars)

write(catalogue_pars_tex, 'files/catalogue_pars.tex')
