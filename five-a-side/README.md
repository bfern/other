# Five-a-side

This is some code that I've used for my 5 a side team to predict results in our league. The model to generate predictions is a simpler version of the (Dixon-Coles model)[http://web.math.ku.dk/~rolf/teaching/thesis/DixonColes.pdf] where goals are assumed to be distributed poissonly and each team is given an attack and defence coefficient.

If you run the file "script.R" from start to finish then a plot of all team abilities will be created. There are also functions to estimate both the expected goals of a fixture (`get_expected_goals(team1, team1)`) and to estimate the probabilities of different results (`get_result_probabilities(team1, team2)`).

This could be used for any amateur football leagues if you replace the `data/results.csv` with the results of that league.

Code was written in R 4.0, and the following packages are required: readr, dplyr, tidyr, stringr, ggplot2.