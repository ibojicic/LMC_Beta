limited.dots <- plot_fluxVSalpha(
	main_data = data1,
	excluded_data = data2,
	limited_data = data4,
	main_x = flux_column,
	main_y = 'lin_slope',
	main_limit = mainLimit,
	main_freq = mainfrequency,
	second_limit = secondLimit,
	second_freq = secondfrequency,
	plot_forced_limits = pl_flimits,
	plot_binforced_limits = pl_blimits
)

limited.dots <- hist_plot_set(
	gplot = limited.dots,
	xlab = x_title,
	ylab = y_title,
	x_limits = xmain_lims,
	y_limits = ymain_lims,
	main_title = mainTitle,
	legendOff = F,
	axis = axisFonts,
	legend = legendSetup
)

binned_data <- bin_to_same_Ns(data1, data4, flux_column, numberofbins)

limited.medians <-
	plot_medians(
		main_data = data1,
		binned_data = binned_data,
		x_title = x_title,
		xmain_lims = xmain_lims,
		ymedian_lims = ymedian_lims, 
		breaks = median.breaks,
		inp_value = 'median'
	)

limited.medians <- hist_plot_set(
	gplot = limited.medians,
	xlab = x_title,
	ylab = "med",
	x_limits = xmain_lims,
	y_limits = ymedian_lims,
	main_title = NA,
	legendOff = F,
	axis = axisFonts,
	legend = legendSetup,
	breaks = median.breaks
)

limited.full <- plot_combine(
	plot_up = limited.dots,
	plot_down = limited.medians,
	margins_up = dotsMargins,
	margins_down = mediansMargins
)

plot_hardopy(filename,
						 limited.full,
						 pl_height = 14,
						 pl_width = 20)

print(limited.full)
