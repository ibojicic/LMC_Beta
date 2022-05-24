library(ggplot2)
library(dplyr)
library(doBy)
library(NISTunits)
library(grid)

source('R/combineRadioData.R')
source('R/loadData.R')
source('R/plot_fncts.R')


grob_sumssVSaskap <- grobTree(textGrob("a)", x=0.05,  y=0.93, hjust=0,
															 gp=gpar(col="black", fontsize=24)))

grob_atca20cmVSaskap <- grobTree(textGrob("b)", x=0.05,  y=0.93, hjust=0,
																gp=gpar(col="black", fontsize=24)))

grob_mostVSaskap <- grobTree(textGrob("c)", x=0.05,  y=0.93, hjust=0,
															 gp=gpar(col="black", fontsize=24)))

grob_mostVSsumss <- grobTree(textGrob("d)", x=0.05,  y=0.93, hjust=0,
																gp=gpar(col="black", fontsize=24)))

# sumss - askap

plot.sumssVSaskap <- offset_plot(offset.sumssVSaskap,
														"", 
														"",
														c(-40,40), 
														c(-40,40)
														) +
	xlab( expression(paste(Delta,'RA [arcsec]'))) +
	ylab( expression(paste(Delta,'DEC [arcsec]'))) +
	ggtitle("Separation SUMSS - ASKAP") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_sumssVSaskap)



plot_hardopy("offset_sumss_askap.eps",plot.sumssVSaskap,pl_height = 16, pl_width = 16)

# 20cm - askap

plot.atca20cmVSaskap <- offset_plot(offset.atca20cmVSaskap,
														"", 
														"",
														c(-40,40), 
														c(-40,40)
														) +
	xlab( expression(paste(Delta,'RA [arcsec]'))) +
	ylab( expression(paste(Delta,'DEC [arcsec]'))) +
	ggtitle("Separation ATCA 1.384GHz - ASKAP") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_atca20cmVSaskap)



plot_hardopy("offset_atca20cm_askap.eps",plot.atca20cmVSaskap,pl_height = 16, pl_width = 16)

# most - askap


plot.mostVSaskap <- offset_plot(offset.mostVSaskap,
																		"", 
																		"",
																		c(-40,40), 
																		c(-40,40)
) +
	xlab( expression(paste(Delta,'RA [arcsec]'))) +
	ylab( expression(paste(Delta,'DEC [arcsec]'))) +
	ggtitle("Separation MOST - ASKAP") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_mostVSaskap)



plot_hardopy("offset_most_askap.eps",plot.mostVSaskap,pl_height = 16, pl_width = 16)

# most - sumss

plot.mostVSsumss <- offset_plot(offset.mostVSsumss,
																"", 
																"",
																c(-40,40), 
																c(-40,40)
) +
	xlab( expression(paste(Delta,'RA [arcsec]'))) +
	ylab( expression(paste(Delta,'DEC [arcsec]'))) +
	ggtitle("Separation MOST - SUMSS") +
	theme(plot.title = element_text(size = 20)) +
	annotation_custom(grob_mostVSsumss)


plot_hardopy("offset_most_sumss.eps",plot.mostVSsumss,pl_height = 16, pl_width = 16)

print(plot.mostVSsumss)

