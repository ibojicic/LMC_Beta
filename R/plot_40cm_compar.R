library(ibrlib)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(colorRamps)
library(grDevices)
library(colorspace)
library(wesanderson)
library(grid)
library(cowplot)

source('R/sql_queries.R')
source('R/plot_fncts.R')
source('R/utilities.R')
source('R/combineRadioData.R')



mydb <- connect_local('r_user', 'ja.sam.r.user',)

# optical data
tofactors <- c('idPNMain')
data.new40cm <- load_sql(mydb, query.new40cm, tofactors)

kill_local()


# flux density difference as in Gregory and Condon 1991
sumss_vs_most <- data.new40cm %>% 
	mutate(dS = (sumss-most) / sqrt(sumss_err^2 + most_err^2),
				 # tS = sqrt(sumss_err^2 + (most * 0.1)^2 + most_err^2),
				 qS = (sumss-most)/sumss,
				 S = sumss) %>%
	# select(dS) %>%
	filter(!is.na(dS))

sumss_vs_askap <- data.new40cm %>% 
	mutate(dS = (sumss-askap) / sqrt(sumss_err^2 + askap_err^2),
				 # tS = sqrt(sumss_err^2 + (askap * 0.1)^2 + askap_err^2),
				 qS = (sumss-askap)/sumss,
				 S = sumss) %>%
	# select(dS) %>%
	filter(!is.na(dS))

# error borders
numberofbins = 10

data.forbinning <- sumss_vs_most %>% 
	select(qS, S) %>%
	filter(!is.na(S)) %>%
	mutate(logFlux = log10(S)) %>% 
	# mutate(bin = cut_number(logFlux,numberofbins))
	mutate(bin = cut_interval(logFlux,numberofbins))

binned <- data.forbinning %>% 
	group_by(bin) %>% 
	summarise_each(funs('median','mean','sd'),all_of("qS"))
binstartsends <- read.table(text = gsub("[^.0-9-]", " ", 
																				levels(data.forbinning$bin)), 
														col.names = c("lower", "upper"))
binned <- cbind(binned, 10**binstartsends)
lastrow <- tail(binned,1) %>% mutate(lower = upper)
binned <- rbind(binned, lastrow) 





# SUMSS VS MOST

grob_most_points <- grobTree(textGrob("b)", x=0.05,  y=0.93, hjust=0,
													gp=gpar(col="black", fontsize=24)))

grob_askap_points <- grobTree(textGrob("a)", x=0.05,  y=0.93, hjust=0,
													gp=gpar(col="black", fontsize=24)))

grob_most_histo <- grobTree(textGrob("b)", x=0.05,  y=0.93, hjust=0,
																			gp=gpar(col="black", fontsize=24)))

grob_askap_histo <- grobTree(textGrob("a)", x=0.05,  y=0.93, hjust=0,
																			 gp=gpar(col="black", fontsize=24)))


plot.sumssVSmost <-
	ggplot(data = data.new40cm, aes(x = sumss, y = most)) +
	geom_point(
		size = 1,
		na.rm = T,
		show.legend = T,
		color = 'navyblue',
		shape = 1
	) +
	geom_smooth(
		method = 'lm',
		fullrange = TRUE,
		size = 1,
		se = F,
		color = 'red'
	) +
	geom_abline(
		slope = 1,
		intercept = 0,
		color = 'black',
		size = 1,
		linetype = 'dashed'
	) +
	# geom_line(data = binned, aes(x = lower, y = up),inherit.aes = F) +
	annotation_custom(grob_most_points)


axis = list(size = 20)

plot.sumssVSmost <- log_ggplot(plot.sumssVSmost,
															 xlab = "SUMSS Integrated Flux (Jy)",
															 ylab = "MOST Integrated Flux (Jy)",
															 axis = axis) +
	scale_x_log10(limits = c(1e-3, 5)) +
	scale_y_log10(limits = c(1e-3, 5)) +
	coord_cartesian(xlim = c(4e-3, 1.5), ylim = c(4e-3, 1.5))


plot_hardopy(file_name = "sumssVSmost.eps",pl_name = plot.sumssVSmost)
print(plot.sumssVSmost)


offsets <-
	data.new40cm %>% mutate(sumss_most_offset = 100 * abs(sumss - most) / sumss)
offsets.binned <-
	aggregate(offsets, by = list(cut(log10(offsets$sumss),
																	 seq(-4, 0.3, 0.2))),
						mean , na.rm = T)
# SUMSS VS ASKAP
plot.sumssVSaskap <-
	ggplot(data = data.new40cm, aes(x = sumss, y = askap)) +
	geom_point(
		size = 1,
		na.rm = T,
		show.legend = T,
		color = 'navyblue',
		shape = 1
	) +
	geom_smooth(
		method = 'lm',
		fullrange = TRUE,
		size = 1,
		se = F,
		color = 'red'
	) +
	geom_abline(
		slope = 1,
		intercept = 0,
		color = 'black',
		size = 1,
		linetype = 'dashed'
	) +
	annotation_custom(grob_askap_points)



axis = list(size = 20)

plot.sumssVSaskap <- log_ggplot(plot.sumssVSaskap,
																xlab = "SUMSS Integrated Flux (Jy)",
																ylab = "ASKAP Beta Integrated Flux (Jy)",
																axis = axis) +
	scale_x_log10(limits = c(1e-3, 5)) +
	scale_y_log10(limits = c(1e-3, 5)) +
	coord_cartesian(xlim = c(4e-3, 1.5), ylim = c(4e-3, 1.5))




plot_hardopy(file_name = "sumssVSaskap.eps",pl_name = plot.sumssVSaskap)
print(plot.sumssVSaskap)



# # flux density difference as in Gregory and Condon 1991
# sumss_vs_most <- data.new40cm %>% 
# 	mutate(dS = (sumss-most) / sqrt(sumss_err^2 + most_err^2),
# 				 tS = sqrt(sumss_err^2 + most_err^2),
# 				 S = sumss) %>%
# 	# select(dS) %>%
# 	filter(!is.na(dS))
# 
# sumss_vs_askap <- data.new40cm %>% 
# 	mutate(dS = (sumss-askap) / sqrt(sumss_err^2 + askap_err^2),
# 				 tS = sqrt(sumss_err^2 + askap_err^2),
# 				 S = sumss) %>%
# 	# select(dS) %>%
# 	filter(!is.na(dS))


dickel_vs_pmn <- data.radio.combined %>% 
	mutate(dS = (`6cm_7_S`-`6cm_4_S`) / sqrt(`6cm_7_S`^2 + `6cm_4_S`^2)) %>%
	# select(dS) %>%
	filter(!is.na(dS))



# binned_data <- bin_to_same_Ns(sumss_vs_most, NA, 'S', 10, inp_value = 'dS')
# 
# limited.medians <-
# 	plot_medians(
# 		main_data = sumss_vs_most,
# 		binned_data = binned_data,
# 		x_title = "test",
# 		xmain_lims = c(1,1000),
# 		ymedian_lims = c(-4,4), 
# 		breaks = seq(-4,4,length.out = 3),
# 		inp_value = 'mean'
# 	)


filename <- "sumss_vs_others_both.eps"
use_bin <- 0.7

hist_compar <- ggplot() +
	stat_bin(
		data = sumss_vs_askap,
		aes(x = dS, y = ..count..),# / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'red',
		size = 1
	) +
	geom_vline(xintercept = median(sumss_vs_askap$dS), colour = 'red', linetype = '12') + 
	stat_bin(
		data = sumss_vs_most,
		aes(x = dS, y = ..count..),# / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'blue',
		linetype = "3111",
		size = 1
	) + 
	geom_vline(xintercept = median(sumss_vs_most$dS), colour = 'blue', linetype = '12') +
	geom_vline(xintercept = 0, colour = 'black', linetype = '32') 
# +
# 	stat_bin(
# 		data = dickel_vs_pmn,
# 		aes(x = dS, y = ..count..),# / max(..count..)),
# 		geom = "step",
# 		binwidth = use_bin,
# 		color = 'black',
# 		# linetype = "11",
# 		size = 1
# 	)


axis = list(size = 20)

hist_compar <-
	hist_plot_set_linear(hist_compar,
								'',
								'Normalised Count',
								legendOff = T,
								axis = axis) +
	# geom_vline(xintercept = median(alphas$lin_slope), colour = 'red', linetype = 'dotdash') +
	xlim(-10, 10) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15)))) +
	xlab( expression(paste(Delta, S ,"/", sigma[s]))) + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot_hardopy(filename, hist_compar)

print(hist_compar)


# separate histograms
use_bin <- 1

filename <- "sumss_vs_askap.eps"
hist_compar <- ggplot(sumss_vs_askap, aes(x = dS)) + 
	geom_histogram(color="navyblue", fill="white",binwidth = use_bin) +
	geom_histogram(data = dickel_vs_pmn, aes(x = dS), 
								 color="black", fill="red",
								 binwidth = use_bin) +
	geom_vline(xintercept = mean(sumss_vs_askap$dS), colour = 'red', 
					 linetype = '31', size = 1) + 
	stat_function(fun = function(x)
		dnorm(x, mean = mean(sumss_vs_askap$dS),
					sd = sd(sumss_vs_askap$dS)) * use_bin * length(sumss_vs_askap$dS),
		linetype = "13") + 
	annotation_custom(grob_askap_histo)

axis = list(size = 20)

hist_compar <-
	hist_plot_set_linear(hist_compar,
											 '',
											 'Count',
											 legendOff = T,
											 axis = axis) +
	xlim(-10, 10) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15)))) +
	xlab( expression(paste(Delta, S ,"/", sigma[s]))) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	

plot_hardopy(filename, hist_compar)

print(hist_compar)


filename <- "sumss_vs_most.eps"
hist_compar <- ggplot(sumss_vs_most, aes(x = dS)) + 
	geom_histogram(color="navyblue", fill="white",binwidth = use_bin) +
	geom_histogram(data = dickel_vs_pmn, aes(x = dS), 
								 color="black", fill="red",
								 binwidth = use_bin) +
	geom_vline(xintercept = mean(sumss_vs_most$dS), colour = 'red', 
						 linetype = '31', size = 1) +
	stat_function(fun = function(x)
		dnorm(x, mean = mean(sumss_vs_most$dS),
					sd = sd(sumss_vs_most$dS)) * use_bin * length(sumss_vs_most$dS),
		linetype = "13") + 
	annotation_custom(grob_most_histo)


	

axis = list(size = 20)

hist_compar <-
	hist_plot_set_linear(hist_compar,
											 '',
											 'Count',
											 legendOff = T,
											 axis = axis) +
	xlim(-10, 10) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15)))) +
	xlab( expression(paste(Delta, S ,"/", sigma[s]))) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	

plot_hardopy(filename, hist_compar)

print(hist_compar)

