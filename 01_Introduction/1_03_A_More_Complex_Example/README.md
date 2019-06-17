These files are used to create [Figure 1.7](https://bookdown.org/max/FES/intro-intro.html#fig:intro-chicago-summary).

Note that each model file begins with the commands:

```r
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)
```

If you go to run these files, please investigate whether parallel processing can be used on your system. For some of the larger feature sets, each worker process will require about 1GB in memory. 

To re-run them, consider using `make -i` at the command line. 