## About
This repo contains the [Shiny](http://shiny.rstudio.com/) user interface and server structure to produce an interactive application for analysis of [Supports Intensity Scale](https://aaidd.org/publications/supports-intensity-scale#.VsU2kObNUrc) (SIS) data.  

## This repo is for UI and reactive data scripts
Please feel free to fork this repo if you want to make changes to the reactive data structure or add new visualizations or interactivity.

If you want to clean and manage your own SIS data using this interface, you should fork and clone the [exploreSIS_node](https://github.com/j-hagedorn/exploreSIS_node) repo and change the local variables in those scripts to grab and clean your own data.  The 'node' repo grabs updated copies of the `ui.R` and `server.R` scripts from this repo so new functionality should be updated (*and potentially new bugs as well, so test thoroughly!*)

## License
We believe in freely sharing ideas and software rather than charging non-profits and governments over and over for something we create.  But we also have to guard against some taking advantage of our goodwill and to keep development of this tool in the public sphere. 
 
So we decided to publish this under the [Creative Commons Attribution – Non-Commercial – Share Alike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/). Here’s why:
* We want to promote free software for non-profits and governments.
* We do not want our work to contribute to money-making activities without some return on our investment.

If you wish to use this work to augment any endeavor that generates revenues – regardless of whether you are for-profit, non-profit or governmental – please contact [info@tbdsolutions.com](info@tbdsolutions.com) or the author of this repository for information on licensing.

Otherwise, please use what’s here – and contribute your own modifications or improvements. 

## Credits
Like most efforts, this one was built on the backs of giants.  Special thanks and shout-outs to the following:

* [hadley](https://github.com/hadley), for: [dplyr](https://github.com/hadley/dplyr), [ggplot2](https://github.com/hadley/ggplot2), and [tidyr](https://github.com/hadley/tidyr)
* The [rstudio](https://github.com/rstudio) team, for: [shiny](https://github.com/rstudio/shiny), [shinydashboard](https://github.com/rstudio/shinydashboard), [dygraphs](https://github.com/rstudio/dygraphs), [d3heatmap](https://github.com/rstudio/d3heatmap), [DT](https://github.com/rstudio/DT)
* [timelyportfolio](https://github.com/timelyportfolio), for [parsetR](https://github.com/timelyportfolio/parsetR)
* [ropensci](https://github.com/ropensci), for open-sourcing the beautiful [plotly](https://github.com/ropensci/plotly) JS library
* [joshuaulrich](https://github.com/joshuaulrich), for the [xts](https://github.com/joshuaulrich/xts) package


