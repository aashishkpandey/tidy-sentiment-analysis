# update.packages(ask = FALSE)
try(require("devtools")||install.packages("devtools"))
library("devtools")
try(require("tidytext")||install_github("juliasilge/tidytext"))

try(require("shiny")||install.packages("shiny"))
try(require("tidyr")||install.packages("tidyr"))
try(require("dplyr")||install.packages("dplyr"))
try(require("ggplot2")||install.packages("ggplot2"))
try(require("DT")||install.packages("DT"))

try(require("reshape2")||install.packages("reshape2"))
try(require("wordcloud")||install.packages("wordcloud"))

library("reshape2")
library("wordcloud")

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")