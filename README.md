# cfx



> `cfx` is able to easily load csv files exported from the Bio-Rad CFX Manager Software.

## Installation

```
devtools::install_git('https://github.com/koncina/cfx.git')
```

## Usage


```r
# Provide the path to the Run information file or the folder name containing a single CFX dataset
f <- "/path/to/cfx_test_run_Run Information.csv"

# Load amplification curves
read_amplification(f)

# Load melt curves
read_meltcurve(f)

# Load Cq values
read_cq(f)

# Load detected melt curve peaks
read_meltpeak(f)
```

