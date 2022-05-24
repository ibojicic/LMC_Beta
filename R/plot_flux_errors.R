library(ggplot2)
library(dplyr)
library(doBy)
library(grid)

source('R/combineRadioData.R')
source('R/plot_fncts.R')

grob_3cm <- grobTree(textGrob("a)", x=0.05,  y=0.93, hjust=0,
																			gp=gpar(col="black", fontsize=24)))
grob_6cm <- grobTree(textGrob("b)", x=0.05,  y=0.93, hjust=0,
																			gp=gpar(col="black", fontsize=24)))
grob_20cm <- grobTree(textGrob("c)", x=0.05,  y=0.93, hjust=0,
																			gp=gpar(col="black", fontsize=24)))
grob_mwa <- grobTree(textGrob("d)", x=0.05,  y=0.93, hjust=0,
																			gp=gpar(col="black", fontsize=24)))



# 3cm 
	
data.3cm <- data.radio.combined %>% 
	select(Flux = `3cm_7_S`, errFlux = `3cm_7_dS`) %>% 
	filter(!is.na(Flux) & !is.na(errFlux) & errFlux > 0) %>%
	mutate(errFlux_perc = 100 * errFlux / Flux)


errorplot.3cm <- error_plot(data.3cm,
														"3cm Flux Density", 
														"3cm Flux Density Error (%)",
														c(1,400), 
														c(0,25), 
														rms.3cm_7) +
	xlab( expression(paste(S['8.64GHz'],'[mJy]'))) +
	ylab( expression(paste(sigma['n'] ,'[%]'))) +
	ggtitle("ATCA 8.64GHz") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_3cm) +
	geom_rect(mapping=aes(xmin=1.2, 
												xmax=4.6, 
												ymin=0.5, 
												ymax=3.5), 
						fill = NA, 
						color="magenta", 
						linetype = '22')
	


	

plot_hardopy("flux_err_3cm_7.eps",errorplot.3cm)

print(errorplot.3cm)

# 6cm

data.6cm <- data.radio.combined %>% 
	select(Flux = `6cm_7_S`, errFlux = `6cm_7_dS`) %>% 
	filter(!is.na(Flux) & !is.na(errFlux) & errFlux > 0) %>%
	mutate(errFlux_perc = 100 * errFlux / Flux)


errorplot.6cm <- error_plot(data.6cm,
														"6cm Flux Density", 
														"6cm Flux Density Error (%)",
														c(8e-1,600), 
														c(0,40), 
														rms.6cm_7) +
	xlab( expression(paste(S['4.80GHz'],'[mJy]'))) +
	ylab( expression(paste(sigma['n'] ,'[%]'))) +
	ggtitle("ATCA 4.80GHz") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_6cm)+
	geom_rect(mapping=aes(xmin=1.2, 
												xmax=4.6, 
												ymin=0.5, 
												ymax=3.5), 
						fill = NA, 
						color="magenta", 
						linetype = '22')




plot_hardopy("flux_err_6cm_7.eps", errorplot.6cm)

print(errorplot.6cm)

# 20cm

data.20cm <- data.radio.combined %>% 
	select(Flux = `20cm_8_S`, errFlux = `20cm_8_dS`) %>% 
	filter(!is.na(Flux) & !is.na(errFlux) & errFlux > 0) %>%
	mutate(errFlux_perc = 100 * errFlux / Flux)


errorplot.20cm <- error_plot(data.20cm,
														"20cm Flux Density", 
														"20cm Flux Density Error (%)",
														c(8e-1,1500), 
														c(0,20), 
														rms.20cm_8) +
	xlab( expression(paste(S['1.38GHz'],'[mJy]'))) +
	ylab( expression(paste(sigma['n'] ,'[%]'))) +
	ggtitle("ATCA 1.384GHz") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_20cm) +
	geom_rect(mapping=aes(xmin=1.2, 
												xmax=4.6, 
												ymin=0.5, 
												ymax=3.5), 
						fill = NA, 
						color="magenta", 
						linetype = '22')





plot_hardopy("flux_err_20cm_8.eps", errorplot.20cm)

print(errorplot.20cm)


# MWA 170-231 band

data.mwa <- data.radio.combined %>% 
	select(Flux = `170-231_10_S`, errFlux = `170-231_10_dS`) %>% 
	filter(!is.na(Flux) & !is.na(errFlux) & errFlux > 0) %>%
	mutate(errFlux_perc = 100 * errFlux / Flux)


errorplot.mwa <- error_plot(data.mwa,
														 "170-231 Flux Density", 
														 "170-231 Density Error (%)",
														 c(30,6500), 
														 c(0,30), 
														 mwa.medrms$`170-231` * 1E3) +
	xlab( expression(paste(S['0.2GHz'],'[mJy]'))) +
	ylab( expression(paste(sigma['n'] ,'[%]'))) +
	ggtitle("MWA 0.2GHz") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_mwa)+
	geom_rect(mapping=aes(xmin=1.2, 
												xmax=4.6, 
												ymin=0.5, 
												ymax=3.5), 
						fill = NA, 
						color="magenta", 
						linetype = '22')


plot_hardopy("flux_err_mwa.eps", errorplot.mwa)

print(errorplot.mwa)