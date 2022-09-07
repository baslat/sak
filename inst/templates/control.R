capsule::whinge()

library(targets) # nolint

source("_targets.R") # nolint

tar_outdated()
tar_visnetwork()
tar_make(
	# callr_function = callr::r_bg # run in background
)
