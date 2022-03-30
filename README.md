# lupine

Functions to read in and analyze lupine data files. This is still a new and immature body of code and bugs should be expected.

### Examples:
Before running anything below, you need to read in the functions. The easiest way to do this is by reading from the github site directly:
```source('https://raw.githubusercontent.com/ajakef/lupine/main/read_lupine.R')```

If you want to download the code file and work with it locally instead, you can do that too.

##### Look at stats for a set of files
If you want to look at all files in folder `data`, first define `filenames` as
```
filenames = list.files('data', full.names = TRUE)
```

If you only want to look at files in folder `data` that end in `003`, instead define `filenames` as
```
filenames = list.files('data', pattern = '003$', full.names = TRUE)
```

Then, run 
```lupine_file_stats(filenames)``` 
to see some basic statistics about the files. Columns are start time/end time (UTC), minimum and maximum battery voltage, 1st and 99th percentile sample time intervals, length (in samples), and file name.

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
