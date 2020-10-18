# Code for article "Identification of technical analysis patterns with smoothing splines for Bitcoin prices"

This repository is the code used in the following paper: **Miller, N.**, Yang, Y., Bruce, S., Zhang, G. (2019) "Identification of technical analysis patterns with smoothing splines for Bitcoin prices." Journal of Applied Statistics, Vol. 46, Issue 12, 2289-2297. DOI: 10.1080/02664763.2019.1580251

ABSTRACT:
This research studies automatic price pattern search procedure for bitcoin cryptocurrency based on 1-min price data. To achieve this, search algorithm is proposed based on nonparametric regression method of smoothing splines. We investigate some well-known technical analysis patterns and construct algorithmic trading strategy to evaluate the effectiveness of the patterns. We found that method of smoothing splines for identifying the technical analysis patterns and that strategies based on certain technical analysis patterns yield returns that significantly exceed results of unconditional trading strategies.

This repository supplements the paper by providing the code that was used in the paper for pattern identification. The dataset that was used in the paper is also provided. Therefore, usage of these materials will let you to replicate the results in the paper. Various scripts refer to the different patterns considered in the paper. Please refer to the paper for further details. If needed, the access to the paper can be requested directly from the first author (me) by e-mailing nmil@protonmail.com

If you prefer to download your own dataset from Coinbase Pro, you can use the script written by Yiming Yang to do so: https://github.com/yihming/gdax-data . Note that GDAX has changed name since that script was written, so package name and commands might need to be changed.

Note that the pattern recognition scripts, as uploaded to this repo, are assuming you're running a MySQL server with dataset loaded using Yiming's script. If that is not the case, or you want to simply use the csv file provided, the pattern recognition scripts will need to be rewritten slightly in order to load the csv files instead.
