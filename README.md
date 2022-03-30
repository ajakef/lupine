# lupine

### Examples:
Before running anything, first read in these functions using
```source('read_lupine.R')```

For this to work, you must be running R from the same folder that `read_lupine.R` is saved in. Or, you can add the path to `read_lupine.R` before the filename, separated by forward slashes `/`.

You can find the folder you're running R from using `getwd()` and change it using `setwd('new_folder_name')`.

##### Look at stats for a set of files
If you want to look at all files in folder `data`, first define `filenames` as
```
filenames = list.files('data', full.names = TRUE)
```

If you only want to look at files in folder `data` that end in `003`, instead define `filenames` as
```
filenames = list.files('data', pattern = '003$', full.names = TRUE)
```

Then, run `lupine_file_stats(filenames)` to see some basic statistics about the files.

##### Read single file as a data frame
Read single file `FILE0001.003` in folder `data/`
```
data = lupine_read_single('data/FILE0001.003')
```
The result is a data frame with UTC time estimated for all samples via a regression line with GPS times. GPS lines are still recognizable because they have non-NA locations, HDOP, and satellite counts.

##### Plot data
Once you've read in some data, you can plot it using these functions. You can also plot the columns in the data frame directly, but these following plots are routine and can save time.

```lupine_plot_health(data) # plot temperature and battery voltage```

```lupine_plot_intervals(data) # plot the sample interval...should be about 0.5```
