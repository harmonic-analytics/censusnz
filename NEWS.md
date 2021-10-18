# censusnz 0.4.0

* Reconfigured to include dwelling and household variables
* Updated `get_data()` to handled multiple years and to warn if variables are not available for certain geography-year combinations

# censusnz 0.3.0.9000

* Removed `plot_relative`, functionality replaced by `position = 'fill'` in `plot_data`

# censusnz 0.2.1.9000

* Updated colours for plots
* Changed `plot_data` scale to scientific by default
* Created a short vignette example for further manipulation of plots with ggplot

# censusnz 0.2.0.9000

* Added `plot_relative()` to plot proportions for easier comparison between regions
* Added a vignette demonstrating the features of the package

# censusnz 0.1.0.9000

* Added a `plot_data()` function which takes drop-in arguments from `get_data` and some extra arguments
* Tweaked how `get_data()` deals with land_type labels

# censusnz 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Initial release using 2018 census data, and functions get_data() and get_variables() 
