# cfx



> `cfx` is able to easily load csv files exported from the Bio-Rad CFX Manager Software.

## Installation

```
devtools::install_git('https://github.com/koncina/cfx.git')
```

## Usage


```r
# Load amplification curves
c(example = "./example") %>%
  map(list.cfx) %>%
  map_df(load.amplification, .id = "experiment")

# Load melt curves
c(example = "./example") %>%
  map(list.cfx) %>%
  map_df(load.meltcurve, .id = "experiment") 

# Load ct values
c(example = "./example") %>%
  map(list.cfx) %>%
  map_df(load.ct, .id = "experiment") 

# Load detected melt curve peaks
c(example = "./example") %>%
  map(list.cfx) %>%
  map_df(load.meltpeak, .id = "experiment")
```

