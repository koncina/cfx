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
  map(list_cfx) %>%
  map_df(read_amplification, .id = "experiment")

# Load melt curves
c(example = "./example") %>%
  map(list_cfx) %>%
  map_df(read_meltcurve, .id = "experiment") 

# Load Cq values
c(example = "./example") %>%
  map(list_cfx) %>%
  map_df(read_cq, .id = "experiment") 

# Load detected melt curve peaks
c(example = "./example") %>%
  map(list_cfx) %>%
  map_df(read_meltpeak, .id = "experiment")
```

