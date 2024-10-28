# reproduce this chart:
# https://www.nytimes.com/interactive/2024/10/26/upshot/census-relative-income.html
# but add white men w/ degrees and asian-americans

# response to these threads:
# https://bsky.app/profile/swordsjew.bsky.social/post/3l7hvtq6ars2j

# To calculate the relative economic standing of various demographic groups and workers over time,
# The Upshot analyzed income reported in microdata from the decennial census and American Community Survey
# for the years 1980, 1990, 2000, and the 2010 and the 2022 five-year sample.
#
# The average full-time worker benchmark shown is the average personal income of full-time workers in
# America who worked at least 50 weeks and 35 hours per week in the previous year.
# Relative incomes for each demographic group or occupation are calculated by comparing the
# average income of full-time workers in that group to the national benchmark.
# A result of 1 represents a group with an average income comparable to the average full-time worker nationwide.
# A higher figure represents a group earning more on average than the typical full-time American worker;
# a lower number is a group earning less.

# maybe instead of comparing to the average, start all groups at 0 in 1980 and do relative percent + or -
# interactive chart? rollover to highlight a group? or facet? both?

# use ACS via ipums?

library(ipumsr)

ipums_data_collections()

# to get variable names
# data for ACS https://usa.ipums.org/usa-action/variables/group



## analysis - use survey and/or srvry packages
# https://federicovegetti.github.io/teaching/heidelberg_2018/lab/sst_lab_day2.html
## Use PERWT for person-specific variables, HHWT for household variables
library(srvyr)
