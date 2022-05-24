library(ggplot2)

plot_points <- function(inpdata, x_axis, y_axis, point_size) {
	gplot_out <- ggplot(data = inpdata,
											aes_string(x = x_axis,
																 y = y_axis)) +
		geom_point(size = point_size,
							 na.rm = T,
							 show.legend = T)
	
	
	gplot_out <- log_ggplot(gplot_out, xlab = x_axis, ylab = y_axis)
	gplot_out
}

## define plot limits
def_plot_limits <-
	function(min_data,
					 max_data,
					 min_fac = 0.5,
					 max_fac = 2.5) {
		plotmin <- min_data * min_fac
		plotmax <- max_data * max_fac
		if (floor(log10(plotmax)) > floor(log10(plotmin))) {
			return(c(plotmin, plotmax))
		}
		
		plotmin <- 10 ^ floor(log10(plotmin))
		if (floor(log10(plotmax)) > floor(log10(plotmin))) {
			return(c(plotmin, plotmax))
		}
		
		plotmax <- 10 ^ ceiling(log10(plotmax))
		c(plotmin, plotmax)
		
	}



plotAddArrows <- function(inputData) {
	inputData.limits <- subset(inputData, flagFlux %in% c("<", ">")) %>%
		mutate(arrfact = ifelse(flagFlux == ">", 1.2, 0.8))
	
	if (length(inputData.limits) == 0) {
		return(NA)
	}
	arrows <- geom_segment(
		data = inputData.limits,
		aes(
			x = freq,
			xend = freq,
			y = Flux,
			yend = Flux * arrfact
		),
		arrow = arrow(length = unit(0.3, "cm"))
	)
	arrows
}


hist_plot_set <- function(gplot,
													xlab = "X",
													ylab = "Y",
													x_limits  = NA,
													y_limits = NA,
													fontFamily = "Courier",
													axis = list(size = 12),
													main_title = NA,
													legend = list(
														position = "right",
														textsize = 8,
														justif = c("left", "top"),
														guidesize = 4,
														barwidth = 0.5,
														barheight = 5,
														bartitle = NA
													),
													breaks = waiver(),
													legendOff = F) {
	setup <- gplot +
		theme_bw(base_family = fontFamily) +
		theme(
			panel.grid.minor = element_line(linetype = 'dotted', colour = 'black'),
			panel.grid.major = element_line(linetype = 'dotted', colour = 'black'),
			axis.text = element_text(size = axis$size),
			axis.title = element_text(size = axis$size + 2),
			legend.position = legend$position,
			legend.justification = legend$justif,
			legend.text = element_text(size = round(legend$textsize)),
			legend.title = element_text(size = legend$textsize),
			legend.background = element_rect(colour = 'black'),
			# strip.background = element_rect(colour = "white", fill = "white"),
			panel.spacing = unit(0, "lines"),
			axis.ticks.length = unit(-5, "pt"),
			axis.text.x = element_text(margin = margin(10, 5, 10, 5, "pt")),
			axis.text.y = element_text(margin = margin(5, 10, 10, 5, "pt")) ,
			panel.ontop = T,
			panel.background = element_rect(fill = NA),
			plot.title = element_text(hjust = 0.5)
		) +
		annotation_logticks(sides = 'tb') +
		guides(
			shape = guide_legend(override.aes = list(size = legend$guidesize)),
			fill = guide_colourbar(
				barwidth = legend$barwidth,
				barheight = legend$barheight,
				title = legend$bartitle
			)
		) +
		
		
		labs(x = xlab, y = ylab) +
		scale_x_log10(
			sec.axis = dup_axis(labels = NULL, name = NULL),
			expand = expand_scale((mult = c(0, 0))),
			limits = x_limits
		) +
		scale_y_continuous(
			sec.axis = dup_axis(labels = NULL, name = NULL),
			expand = expand_scale((mult = c(0, 0))),
			limits = y_limits,
			breaks = breaks,
			labels = scaleFUN
		)
	
	if (!is.na(main_title)) {
		setup <- setup + ggtitle(main_title)
	}
	
	if (legendOff) {
		setup <- setup + theme(legend.position = "none")
	}
	
	setup
}


hist_plot_set_linear <- function(gplot,
													xlab = "X",
													ylab = "Y",
													x_limits  = NA,
													y_limits = NA,
													fontFamily = "Courier",
													axis = list(size = 12),
													main_title = NA,
													legend = list(
														position = "right",
														textsize = 8,
														justif = c("left", "top"),
														guidesize = 4,
														barwidth = 0.5,
														barheight = 5,
														bartitle = NA
													),
													breaks = waiver(),
													legendOff = F) {
	setup <- gplot +
		theme_bw(base_family = fontFamily) +
		theme(
			panel.grid.minor = element_line(linetype = 'dotted', colour = 'black'),
			panel.grid.major = element_line(linetype = 'dotted', colour = 'black'),
			axis.text = element_text(size = axis$size),
			axis.title = element_text(size = axis$size + 2),
			legend.position = legend$position,
			legend.justification = legend$justif,
			legend.text = element_text(size = round(legend$textsize)),
			legend.title = element_text(size = legend$textsize),
			legend.background = element_rect(colour = 'black'),
			# strip.background = element_rect(colour = "white", fill = "white"),
			panel.spacing = unit(0, "lines"),
			axis.ticks.length = unit(-5, "pt"),
			axis.text.x = element_text(margin = margin(10, 5, 10, 5, "pt")),
			axis.text.y = element_text(margin = margin(5, 10, 10, 5, "pt")) ,
			panel.ontop = T,
			panel.background = element_rect(fill = NA),
			plot.title = element_text(hjust = 0.5)
		) +
		# annotation_logticks(sides = 'tb') +
		guides(
			shape = guide_legend(override.aes = list(size = legend$guidesize)),
			fill = guide_colourbar(
				barwidth = legend$barwidth,
				barheight = legend$barheight,
				title = legend$bartitle
			)
		) +
		
		
		labs(x = xlab, y = ylab) +
		# scale_x_log10(
		# 	sec.axis = dup_axis(labels = NULL, name = NULL),
		# 	expand = expand_scale((mult = c(0, 0))),
		# 	limits = x_limits
		# ) +
		scale_y_continuous(
			sec.axis = dup_axis(labels = NULL, name = NULL),
			expand = expand_scale((mult = c(0, 0))),
			limits = y_limits,
			breaks = breaks,
			labels = scaleFUN
		)
	
	if (!is.na(main_title)) {
		setup <- setup + ggtitle(main_title)
	}
	
	if (legendOff) {
		setup <- setup + theme(legend.position = "none")
	}
	
	setup
}



error_plot <-
	function(indata,
					 xtitle,
					 ytitle,
					 xlimits,
					 ylimits,
					 rmsnoise) {
		errorplot <- ggplot(data = indata, aes(Flux, errFlux_perc)) +
			geom_point(size = 1,
								 shape = 1,
								 colour = 'navyblue')
		
		axis = list(size = 20)
		errorplot <- hist_plot_set(errorplot,
															 xtitle,
															 ytitle,
															 legendOff = T,
															 axis = axis) +
			scale_x_log10(limits = xlimits) +
			scale_y_continuous(limits = ylimits) +
			annotation_logticks(sides = 'b') +
			geom_vline(
				xintercept = 3 * rmsnoise,
				linetype = '3111',
				colour = 'blue'
			) +
			geom_vline(
				xintercept = 5 * rmsnoise,
				linetype = '31',
				colour = 'red'
			)
		
		errorplot
		
	}


offset_plot <-
	function(indata,
					 xtitle,
					 ytitle,
					 xlimits,
					 ylimits) {
		offsetplot <- ggplot(data = indata, aes(dRA, dDEC)) +
			geom_point(size = 1,
								 shape = 1,
								 colour = 'navyblue')
		
		axis = list(size = 20)
		offsetplot <- hist_plot_set_linear(offsetplot,
															 xtitle,
															 ytitle,
															 legendOff = T,
															 axis = axis) +
			scale_x_continuous(limits = xlimits) +
			scale_y_continuous(limits = ylimits) + 
			geom_vline(
				xintercept = median(indata$dRA, na.rm = T),
				linetype = '3111',
				colour = 'red'
			) +
			geom_hline(
				yintercept = median(indata$dDEC, na.rm = T),
				linetype = '3111',
				colour = 'red'
			)
		

		offsetplot
		
	}
plot_hardopy <- function(file_name,
												 pl_name,
												 pl_path = "plots/",
												 pl_height = 16,
												 pl_width = 20) {
	pl_target <- paste(pl_path, file_name, sep = "", collapse = "")
	
	ggsave(
		pl_target,
		pl_name,
		device = 'eps',
		family = 'Courier',
		height = pl_height,
		width = pl_width,
		units = 'cm'
	)
}

plot_fluxVSalpha <- function(main_data,
														 excluded_data,
														 limited_data,
														 main_x,
														 main_y,
														 main_limit,
														 main_freq,
														 second_limit,
														 second_freq,
														 plot_forced_limits = F,
														 plot_binforced_limits = F) {
	plot_out <-
		ggplot(data = main_data, aes_string(x = main_x, y = main_y)) +
		geom_point(
			data = excluded_data,
			aes_string(x = main_x, y = main_y),
			size = 2,
			colour = 'grey80',
			shape = 0
		) +
		geom_point(size = 1,
									 shape = 1,
									 colour = 'navyblue')
	
	# plot forced limits points
	if (plot_forced_limits) {
		plot_out <- plot_out +
			geom_point(
				data = limited_data,
				aes_string(x = main_x, y = main_y),
				size = 3,
				colour = 'magenta',
				shape = 25
			)
		
	}
	
	# plot binned forced limits
	if (plot_binforced_limits) {
		plot_out <- plot_out +
			geom_bin2d(
				data = limited_data,
				aes(fill = ..density.. * 100),
				bins = 90,
				drop = T
			) +
			scale_fill_viridis_c(option = "plasma", trans = 'log10') +
			guides(fill = guide_colourbar(
				barwidth = 0.5,
				barheight = 5,
				title = 'dens.[%]'
			)) +
			theme(legend.background = element_rect(colour = 'black'))
	}
	
	# plot median of the full sample and limits
	plot_out <- plot_out +
		geom_hline(
			yintercept = median(main_data$lin_slope),
			size = 0.8,
			linetype = '3111',
			colour = 'red'
		) +
		geom_vline(
			xintercept = main_limit,
			size = 0.8,
			linetype = '33',
			colour = 'grey50'
		) +
		stat_function(
			fun = ~ log(.x / second_limit) / log(main_freq / second_freq),
			inherit.aes = F,
			linetype = '33',
			fullrange = T,
			size = 0.8,
			colour = 'grey50'
		)
	
	plot_out
	
}


plot_medians <- function(main_data,
												 binned_data,
												 x_title,
												 xmain_lims,
												 ymedian_lims,
												 breaks,
												 inp_value = 'median'
												 ) {
	plot_out <- ggplot() +
		geom_hline(
			yintercept = median(main_data$lin_slope),
			size = 0.8,
			linetype = '3111',
			colour = 'red'
		) +
		geom_step(
			data = binned_data,
			aes_string('lower', inp_value),
			colour = 'navyblue',
			direction = 'hv',
			size = 1
		)
	
	plot_out <- hist_plot_set(
		gplot = plot_out,
		xlab = x_title,
		ylab = 'med',
		x_limits = xmain_lims,
		y_limits = ymedian_lims,
		legendOff = T,
		axis = axis,
		breaks = breaks
	)
	
	
	plot_out
}

plot_combine <-
	function(plot_up,
					 plot_down,
					 margins_up,
					 margins_down) {
		plot_up <- plot_up +
			theme(
				axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				plot.margin = unit(margins_up, "lines")
			)
		
		
		plot_down <- plot_down +
			theme(
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				plot.margin = unit(margins_down, "lines")
			)
		
		plot_out <-
			grid.arrange(plot_up,
									 plot_down,
									 ncol = 1,
									 heights = c(2, 1))
		
		plot_out
		
	}
