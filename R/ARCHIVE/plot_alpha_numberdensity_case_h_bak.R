library(ggplot2)
library(dplyr)
library(gridExtra)

source('R/combineRadioData.R')
source('R/plot_fncts.R')
source('R/utilities.R')

axisFonts = list(size = 20)
dotsMargins <- c(0.5, .5, 0.67, 0.3)
mediansMargins <- c(0, .5, 0, 0.3)

numberofbins = 10

limit.sumss <- 6
limit.20cm <- 2.5


## flag g: 20cm limited to 2.5, 40cm limited to 6 
data.radio.alphas.twolowest.limits_h <- data.spindex %>% filter(flag == 'h', !is.na(lin_slope)) %>% select(-flag)


# filter fluxes and spindex for alphas number density
data.radio.numberdensity.limits_h <- merge(data.radio.fluxes.combined, data.radio.alphas.twolowest.limits_h, by ='idPNMain')


data0 <- data.radio.numberdensity.twolowest

data1 <- data0 %>% filter(`40cm_6_S` >= limit.sumss & `20cm_8_S` >= limit.20cm)

data1 <- data1 %>% filter(distance < 13000)
	
data2 <- data0 %>% filter(!(idPNMain %in% data1$idPNMain))

data3 <- data.radio.numberdensity.limits_h

data4 <- data3 %>% filter(`40cm_6_S` >= limit.sumss & `20cm_8_S` >= limit.20cm)

data4 <- data4 %>% filter(distance < 13000)



## fixed sumss setup
flux_column <- '`20cm_8_S`'
mainfrequency <- 1.384
secondfrequency <- 0.834
x_title <- '20cm Flux Density (mJy)'
y_title <- 'Spectral Index'
xmain_lims <- c(1,2000)
ymain_lims <- c(-4.5,2.5)
ymedian_lims <- c(-1.6,-0.6)
pl_flimits <- F
pl_blimits <- T
mainTitle <- paste("S20cm >",limit.20cm,'mJy; S40cm>',limit.sumss,'mJy',sep = "",collapse = "")
mainLimit <- limit.20cm
secondLimit <- limit.sumss
filename = paste("fixed_20cm_sumss_",limit.sumss,"mJy_20cm_",limit.20cm,"mJy.eps",sep = "",collapse = "")
legendSetup = list(
	position = c(0.82,0.98),
	textsize = 14,
	justif = c("left", "top"),
	guidesize = 4,
	barwidth = 0.5,
	barheight = 5,
	bartitle = 'dens.[%]'
)
median.breaks = seq(ymedian_lims[1],ymedian_lims[2],length.out = 3)

source('R/plot_alpha_numberdensity_script.R')

## fixed 20cm setup
flux_column <- '`40cm_6_S`'
mainfrequency <- 0.834
secondfrequency <- 1.384
x_title <- 'SUMSS Flux Density (mJy)'
y_title <- 'Spectral Index'
xmain_lims <- c(4,3000)
ymain_lims <- c(-4.5,2.5)
ymedian_lims <- c(-1.2,-0.6)
pl_flimits <- T
pl_blimits <- F
mainTitle <- paste("S20cm >",limit.20cm,'mJy; S40cm>',limit.sumss,'mJy',sep = "",collapse = "")
mainLimit <- limit.sumss
secondLimit <- limit.20cm
filename = paste("fixed_sumss_sumss_",limit.sumss,"mJy_20cm_",limit.20cm,"mJy.eps",sep = "",collapse = "")
legendSetup = list(
	position = c(0.82,0.98),
	textsize = 14,
	justif = c("left", "top"),
	guidesize = 4,
	barwidth = 0.5,
	barheight = 5,
	bartitle = 'dens.[%]'
)
median.breaks = seq(ymedian_lims[1],ymedian_lims[2],length.out = 3)

source('R/plot_alpha_numberdensity_script.R')
