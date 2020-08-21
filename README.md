# cowlogdata
cowlogdata, an R package to visualize obersrvations generated by the event logging software CowLog<br>

cowlogdata takes a list of individual observation csv files generated by cowlog (though theoretically any csv files can be used if adjusted for formatting), compiles a summary dataset, and then can generate initial data analysis graphs on subsets of data such as boxplots, pie charts, and time series graphs. <br>
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
clflag is designed to identify any cowlog csv observation sheets that do may host scorer input errors. It flags csv files that do not include an END entry, that have fewer than entries, and that report negative time differences between entries.

<b>cldata</b><br>
cldata generates a summary dataframe based on individual cowlog csv observations. Additionaly it generates a list of zones dientified in the observations.

<b>clpie</b><br>
to visualize where individuals are spending time in the observations, clpie generates a pie graph of proportion time in each zone, and can generate pie graphs per category.

<b>clboxplot</b><br>
to visualize and compare the distribution of values, clboxplot generates bosplots of values and additionally can compares distributions across categories via t-tests and anovas.

<b>clreg</b><br>
to determine which metrics correlate with each other and/or show interaction effects of factors, clreg runs linear regression models and generates summary scatterplots.

<b>clseries</b><br>
to visualize average movement in the arena over time, clseries splits each observation into ten even segments of the same length and visualizes relative time in zones per segment.


## notes

NOTE: This package is designed to use csv sheets from just base cowlog data, not data that has used modifiers<br>
NOTE: This package is designed to be used for locational data, not event logs. See below for the individual csv sheet requirements. <br>
NOTE: Many of the functions included in this package likely only work on Windows due to path character specifications (forwardslash vs backslash). <br>

<b>example filename</b> <br>
"arthur_male_round1.csv"<br>
Note: the filename should be separated by underscores, and the index of relevant categocial data should be included. 


<b>example csv sheet</b><br>
The csv sheets, and the way CowLog shoudl be intended for use in relation to this package, is tracking location data of a focal individual across different areas of an apparatus/enclosure. As seen below, a time stamp is recorded of when an individual transitioned to a new zone (e.g. 6.3 seconds into the observation, the focal individual moved from Zone B to Zone C)<br>
Note: column names should always be time and code (optional to include class but that is what CowLog generates)<br>
Note: when calculating time, time always begins at the first entry (shown here as 5 seconds into the observation)<br>
Note: every spreadsheet must end with a last observation coded END<br>

time___           code___                  class

5.1___            zone_B___                  1

6.3___             zone_C___                  1

12.4___             zone_B___                  1

14.8___             zone_A___                  1

22.3___             zone_D___                  1

35.6___             zone_E___                  1

35.8___             zone_D___                  1

36.0___             zone_E___                  1

39.1___             zone_A___                  1

45.2___             zone_C___                  1

70.4___             END___                     0

## necessary packages

dplyr<br>
ggplot2<br>
viridis<br>
stringr<br>
broom<br>






#adding more TBD
