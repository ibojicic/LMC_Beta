library(ggplot2)
library(dplyr)
library(doBy)

source('R/combineRadioData.R')
source('R/plot_fncts.R')
limit.sumss <- 6
limit.20cm <- 2.5

data0 <- data.radio.numberdensity.twolowest

data1 <- data0 %>% filter(`40cm_6` >= limit.sumss & `20cm_8` >= limit.20cm)

data2 <- data0 %>% filter(!(idPNMain %in% data1$idPNMain))

data3 <- data.radio.numberdensity.limits 

data4 <- data3 %>% filter(`40cm_6` >= limit.sumss & `20cm_8` >= limit.20cm)


numberofbins = 10

## fixed sumss
filename = paste("fixed_sumss_sumss_",limit.sumss,"mJy_20cm_",limit.20cm,"mJy.eps",sep = "",collapse = "")

# data calculated just by limits

numberofbins = 10

data.forbinning <- rbind(data1,data4) %>% select(lin_slope,`20cm_8`) %>%
	mutate(logFlux = log10(`20cm_8`)) %>% 
	mutate(bin = cut_number(logFlux,numberofbins))

binned <- data.forbinning %>% group_by(bin) %>% summarise_each(funs(median),lin_slope)
binstartsends <- read.table(text = gsub("[^.0-9]", " ", levels(data.forbinning$bin)), col.names = c("lower", "upper"))
# binstarts <- seq(min(data.forbinning$logFlux), max(data.forbinning$logFlux),length.out = (numberofbins + 1))
binned <- cbind(binned, 10**binstartsends)
lastrow <- tail(binned,1) %>% mutate(lower = upper)
binned <- rbind(binned, lastrow)

sumsslimited.dots <- ggplot(data = data1, aes(x=`20cm_8`, y=lin_slope)) +
	geom_point(size = 1, shape = 1, colour = 'navyblue') +
	geom_point(data = data2, aes(x=`20cm_8`, y=lin_slope), size = 2,colour = 'grey80', shape = 0 ) +
	# geom_point(data = data3, aes(x=`20cm_8`, y=lin_slope), size = 1,colour = 'slateblue2', shape = 4 ) +
	geom_hline(yintercept = median(data1$lin_slope), size=0.8, linetype = '3111', colour='green') +
	geom_step(data = binned, aes(lower,lin_slope), colour = 'red', direction = 'hv', size = 1) +
	geom_bin2d(data = data3,  aes(fill = ..density.. * 100), bins=90, drop = T) + #binwidth = c(0.04, 0.08)) +
	scale_fill_viridis_c(option = "plasma", trans = 'log10') + 
	geom_vline(xintercept = limit.20cm, size=0.8, linetype = '33', colour='blue') +
	stat_function(fun = ~log(.x/limit.sumss)/log(1.384/0.834), 
								inherit.aes = F, linetype = '33', fullrange = T, size = 0.8, colour='blue')


axis = list(size = 20)

legend = list(
	position = c(0.82,0.98),
	textsize = 14,
	justif = c("left", "top"),
	guidesize = 4
)

sumsslimited.dots <- hist_plot_set(gplot = sumsslimited.dots,
																	 xlab = '20cm Flux Density (mJy)',
																	 ylab = 'Spectral Index',
																		legendOff = F,
																		axis = axis,
																	 legend = legend) +
	guides(fill = guide_colourbar(barwidth = 0.5, barheight = 5, title = 'dens.[%]')) +
	theme(legend.background = element_rect(colour = 'black')) +
	# ylim(-4, 2) +
	# coord_cartesian(ylim = c(-4,2), xlim = c(1e-3,100)) +
	annotation_logticks(sides = 'tb') +
	scale_x_log10(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0))),
										 limits = c(1,2000)) + 
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0))),
										 limits = c(-4,2.5)) + 

	ggtitle(paste("S20cm >",limit.20cm,'mJy; S40cm>',limit.sumss,'mJy',sep = "",collapse = ""))


ggsave(
	filename,
	sumsslimited.dots,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)

	
print(sumsslimited.dots)


## fixed 20cm
filename = paste("fixed_20cm_sumss_",limit.sumss,"mJy_20cm_",limit.20cm,"mJy.eps",sep = "",collapse = "")

# data calculated just by limits

data.forbinning <- rbind(data1,data4) %>% select(lin_slope,`40cm_6`) %>%
	mutate(logFlux = log10(`40cm_6`)) %>% 
	mutate(bin = cut_number(logFlux,numberofbins))

binned <- data.forbinning %>% group_by(bin) %>% summarise_each(funs(median),lin_slope)
binstartsends <- read.table(text = gsub("[^.0-9]", " ", levels(data.forbinning$bin)), col.names = c("lower", "upper"))
# binstarts <- seq(min(data.forbinning$logFlux), max(data.forbinning$logFlux),length.out = (numberofbins + 1))
binned <- cbind(binned, 10**binstartsends)
lastrow <- tail(binned,1) %>% mutate(lower = upper)
binned <- rbind(binned, lastrow)

limited20cm.dots <- ggplot(data = data1, aes(x=`40cm_6`, y=lin_slope)) +
	geom_point(size = 1, shape = 1, colour = 'navyblue') +
	geom_point(data = data2, aes(x=`40cm_6`, y=lin_slope), size = 2,colour = 'grey80', shape = 0 ) +
	geom_point(data = data3, aes(x=`40cm_6`, y=lin_slope), size = 3,colour = 'magenta', shape = 25 ) +
	geom_hline(yintercept = median(data1$lin_slope), size=0.8, linetype = '3111', colour='green') +
	geom_step(data = binned, aes(lower,lin_slope), colour = 'red', direction = 'hv', size = 1) +
	# geom_bin2d(data = data3,  aes(fill = ..density.. * 100), bins=90, drop = T) + #binwidth = c(0.04, 0.08)) +
	# scale_fill_viridis_c(option = "plasma", trans = 'log10') + 
	geom_vline(xintercept = limit.sumss, size=0.8, linetype = '33', colour='blue') +
	stat_function(fun = ~log(.x/limit.20cm)/log(0.834/1.384), 
								inherit.aes = F, linetype = '33', fullrange = T, size = 0.8, colour='blue')


axis = list(size = 20)

legend = list(
	position = c(0.82,0.98),
	textsize = 14,
	justif = c("left", "top"),
	guidesize = 4
)

limited20cm.dots <- hist_plot_set(gplot = limited20cm.dots,
																	 xlab = 'SUMSS Flux Density (mJy)',
																	 ylab = 'Spectral Index',
																	 legendOff = F,
																	 axis = axis,
																	 legend = legend) +
	# guides(fill = guide_colourbar(barwidth = 0.5, barheight = 5, title = 'dens.[%]')) +
	# theme(legend.background = element_rect(colour = 'black')) +
	annotation_logticks(sides = 'tb') +
	scale_x_log10(sec.axis = dup_axis(labels = NULL, name = NULL),
								expand = expand_scale((mult = c(0, 0))),
								limits = c(4,3000)) + 
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0))),
										 limits = c(-4,2.5)) + 
	
	ggtitle(paste("S20cm >",limit.20cm,'mJy; S40cm>',limit.sumss,'mJy',sep = "",collapse = ""))


ggsave(
	filename,
	limited20cm.dots,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)


print(limited20cm.dots)



# ## fixed sumss
# filename = "fixed_20cm.eps"
# 
# data3 <- data.radio.numberdensity.limits 
# 
# data4 <- data3 %>% filter(`40cm_6` >= limit.sumss & `20cm_8` >= limit.20cm)
# 
# numberofbins = 10
# 
# data.forbinning <- rbind(data1,data4) %>% select(lin_slope,`40cm_6`) %>%
# 	mutate(logFlux = log10(`40cm_6`)) %>%
# 	mutate(bin = cut_number(logFlux,numberofbins))
# 
# binned <- data.forbinning %>% group_by(bin) %>% summarise_each(funs(median),lin_slope)
# binstartsends <- read.table(text = gsub("[^.0-9]", " ", levels(data.forbinning$bin)), col.names = c("lower", "upper"))
# # binstarts <- seq(min(data.forbinning$logFlux), max(data.forbinning$logFlux),length.out = (numberofbins + 1))
# binned <- cbind(binned, 10**binstartsends)
# lastrow <- tail(binned,1) %>% mutate(lower = upper)
# binned <- rbind(binned, lastrow)
# 
# 
# x <- c(1:1000)
# y <- log(x/limit.20cm)/log(0.834/1.384)
# 
# dl <- data_frame(x,y)
# 
# limited20cm.dots <- ggplot(data = data1, aes(x=`40cm_6`, y=lin_slope)) +
# 	geom_point(size = 1, shape = 1, colour = 'black') +
# 	geom_point(data = data2, aes(x=`40cm_6`, y=lin_slope), size = 1,colour = 'grey80', shape = 0 ) +
# 	geom_point(data = data3, aes(x=`40cm_6`, y=lin_slope), size = 1,colour = 'slateblue2', shape = 4 ) +
# 	geom_line(data = dl, aes(x,y), size=0.5) +
# 	geom_step(data = binned, aes(lower,lin_slope), colour = 'red', direction = 'hv', size = 1) +
# 	# geom_density_2d(colour = 'green') +
# 	geom_vline(xintercept = limit.sumss, size=0.5)
# 
# 
# axis = list(size = 20)
# 
# limited20cm.dots <- hist_plot_set(limited20cm.dots,
# 																	 'SUMSS Flux Density (mJy)',
# 																	 'Spectral Index',
# 																	 legendOff = T,
# 																	 axis = axis) +
# 	ylim(-4, 2) +
# 	scale_x_log10() +
# 	annotation_logticks(sides = 'b') +
# 	ggtitle(paste("S20cm >",limit.20cm,'mJy; S40cm>',limit.sumss,'mJy',sep = "",collapse = ""))
# 
# 
# 
# ggsave(
# 	filename,
# 	limited20cm.dots,
# 	device = 'eps',
# 	family = 'Courier',
# 	height = 16,
# 	width = 20,
# 	units = 'cm'
# )
# 
# # print(limited20cm.dots)
# 
