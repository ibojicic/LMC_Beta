library(ggplot2)
source('R/combineRadioData.R')
source('R/plot_fncts.R')



# all points
alphas <- data.radio.alphas.yesmwa
filename <- "alphas_fullcat.eps"
use_bin <- 0.2

hist_fullCat <- ggplot() +
	stat_bin(
		data = alphas,
		aes(x = lin_slope, y = ..count.. / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'red',
		size = 1
	) +
	stat_bin(
		data = alphas[alphas$n_points > 2, ],
		aes(x = lin_slope, y = ..count.. / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'blue',
		linetype = "3111",
		size = 1
	) +
	stat_bin(
		data = alphas[alphas$n_points == 2, ],
		aes(x = lin_slope, y = ..count.. / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'lightgreen',
		linetype = "31",
		size = 1
	)

# + ggtitle('Full Catalogue')


axis = list(size = 20)

hist_fullCat <-
	hist_plot_set(hist_fullCat,
								'Spectral Index',
								'Normalised Count',
								legendOff = T,
								axis = axis) +
	geom_vline(xintercept = median(alphas$lin_slope), colour = 'red', linetype = 'dotdash') +
	# geom_vline(xintercept = median(alphas[alphas$n_points > 2, ]$lin_slope), colour = 'blue', linetype = '3111')
	# geom_vline(xintercept = median(alphas[alphas$n_points == 2, ]$lin_slope), colour = 'grey', linetype = '31')
	xlim(-3, 1.5) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15))),
										 breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

ggsave(
	filename,
	hist_fullCat,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)
print(hist_fullCat)

# all points & lowest freq

alphas_all <- data.radio.alphas.yesmwa
alphas_low <- data.radio.alphas.twolowest

filename <- "alphas_lowest.eps"
use_bin <- 0.2

hist_fullCat <- ggplot() +
	stat_bin(
		data = alphas_all,
		aes(x = lin_slope, y = ..count.. / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'red',
		size = 1
	) +
	stat_bin(
		data = alphas_low,
		aes(x = lin_slope, y = ..count.. / max(..count..)),
		geom = "step",
		binwidth = use_bin,
		color = 'blue',
		linetype = "3111",
		size = 1
	) 

# + ggtitle('Full Catalogue')


axis = list(size = 20)

hist_fullCat <-
	hist_plot_set(hist_fullCat,
								'Spectral Index',
								'Normalised Count',
								legendOff = T,
								axis = axis) +
	geom_vline(xintercept = median(alphas_all$lin_slope), colour = 'red', linetype = 'dotdash') +
	geom_vline(xintercept = median(alphas_low$lin_slope), colour = 'blue', linetype = '11') +
	# geom_vline(xintercept = median(alphas[alphas$n_points > 2, ]$lin_slope), colour = 'blue', linetype = '3111')
	# geom_vline(xintercept = median(alphas[alphas$n_points == 2, ]$lin_slope), colour = 'grey', linetype = '31')
	xlim(-3, 1.5) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15))),
										 breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

ggsave(
	filename,
	hist_fullCat,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)
print(hist_fullCat)

# AT20G

alphas_at20g <- data.radio.alphas.at20g

filename <- "alphas_at20g.eps"
use_bin <- 0.3

hist_fullCat <- ggplot() +
	stat_bin(
		data = alphas_at20g,
		aes(x = lin_slope, y = ..count..),
		geom = "bar",
		fill = NA,
		binwidth = use_bin,
		color = 'blue',
		size = 1
	) 

# + ggtitle('Full Catalogue')


axis = list(size = 20)

hist_fullCat <-
	hist_plot_set(hist_fullCat,
								'Spectral Index',
								'Count',
								legendOff = T,
								axis = axis) +
	geom_vline(xintercept = median(alphas_at20g$lin_slope), colour = 'red', linetype = 'dotdash') +
	# geom_vline(xintercept = median(alphas[alphas$n_points > 2, ]$lin_slope), colour = 'blue', linetype = '3111')
	# geom_vline(xintercept = median(alphas[alphas$n_points == 2, ]$lin_slope), colour = 'grey', linetype = '31')
	xlim(-2, 2) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15))))
										 # breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

ggsave(
	filename,
	hist_fullCat,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)
print(hist_fullCat)

# optical detections

alphas_optical <- data.radio.alphas.opticaldet

filename <- "alphas_optical_detections.eps"
use_bin <- 0.3

hist_fullCat <- ggplot() +
	stat_bin(
		data = alphas_optical,
		aes(x = lin_slope, y = ..count..),
		geom = "bar",
		fill = NA,
		binwidth = use_bin,
		color = 'blue',
		size = 1
	) 

# + ggtitle('Full Catalogue')


axis = list(size = 20)

hist_fullCat <-
	hist_plot_set(hist_fullCat,
								'Spectral Index',
								'Count',
								legendOff = T,
								axis = axis) +
	geom_vline(xintercept = median(alphas_optical$lin_slope), colour = 'red', linetype = 'dotdash') +
	# geom_vline(xintercept = median(alphas[alphas$n_points > 2, ]$lin_slope), colour = 'blue', linetype = '3111')
	# geom_vline(xintercept = median(alphas[alphas$n_points == 2, ]$lin_slope), colour = 'grey', linetype = '31')
	xlim(-2, 2) +
	scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL),
										 expand = expand_scale((mult = c(0, 0.15))))
# breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

ggsave(
	filename,
	hist_fullCat,
	device = 'eps',
	family = 'Courier',
	height = 16,
	width = 20,
	units = 'cm'
)
print(hist_fullCat)





