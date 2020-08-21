# cowlogdata
<i>cowlogdata, an R package to visualize obersrvations generated by the event logging software CowLog</i><br>

cowlogdata takes a list of individual observation csv files generated by CowLog (though any csv files can be used if adjusted for formatting), compiles a summary dataset, and then generates initial data exploration graphs such as boxplots, pie charts, and time series graphs. <br>

Kelly Wallace (kwallace@utexas.edu)<br>
CowLog: http://cowlog.org/cowlog3/<br>

## install
copy and paste everything that looks `like this` into R

`if (!"devtools" %in% installed.packages()) install.packages("devtools"); library(devtools)`

`install_github("kellyjwallace/cowlogdata")`

`library(cowlogdata)`

try `?clseries` to make sure everything has loaded correctly

## functions
<b>clflag</b><br>
clflag is designed to identify any cowlog csv observation sheets that may hoave scorer input errors. It flags csv files that do not include an END entry, that have fewer than three entries, and that report negative time differences between entries.

<b>cldata</b><br>
cldata generates a summary dataframe based on individual cowlog csv observations. Additionaly it generates a list of zones dientified in the observations.

<b>clseries</b><br>
to visualize average movement in the arena over time, clseries splits the observations into ten even segments of the same length and visualizes an aveage of the relative time in zones per time segment.

![clseries!](https://github.com/kellyjwallace/cowlogdata/raw/master/examples/clseries.png)

<b>clpie</b><br>
to visualize where individuals are spending time in the observations, clpie generates a pie graph of proportion time in each zone, and can generate pie graphs per category.

![clpie!](https://github.com/kellyjwallace/cowlogdata/raw/master/examples/clpie.png)


<b>clboxplot</b><br>
to visualize and compare the distribution of values, clboxplot generates bosplots of values and additionally can compares distributions across categories via t-tests and anovas.

![clboxplot!](https://github.com/kellyjwallace/cowlogdata/raw/master/examples/clboxplot.png)

<b>clreg</b><br>
to determine which metrics correlate with each other and/or show interaction effects of factors, clreg runs linear regression models and generates summary scatterplots.

![clreg!](https://github.com/kellyjwallace/cowlogdata/raw/master/examples/clreg.png)



## notes

NOTE: This package is designed to use csv sheets from just base cowlog data, not data that has used modifiers<br>
NOTE: This package is designed to be used for locational data, not event logs. See below for the individual csv sheet requirements. <br>
NOTE: Many of the functions included in this package likely only work on Windows due to path character specifications (forwardslash vs backslash). <br>

<b>example filename</b> <br>
"arthur_male_round1.csv"<br>
Note: the filename should be separated by underscores, and the index of relevant categocial data should be included. 


<b>example csv sheet</b><br>
The function of the csv sheets (and the way CowLog should be intended for use in relation to this package) is to track the location data of a focal individual across different areas of an apparatus/enclosure. As seen below, a time stamp is recorded of when an individual transitioned to a new zone (e.g. 264 seconds into the observation, the focal individual moved from upper left corner to left interaction)<br>
Note: column names should always be "time" and "code" (optional to include class but that is what CowLog generates)<br>
Note: when calculating time, time always begins at the first entry (shown here as 0.015 seconds into the observation)<br>
Note: every spreadsheet must end with a last observation coded END<br>

![sheet!](https://github.com/kellyjwallace/cowlogdata/raw/master/examples/sheet.png)

## necessary packages

dplyr<br>
ggplot2<br>
viridis<br>
stringr<br>
broom<br>

