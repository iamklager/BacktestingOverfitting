# BacktestingOverfitting

An R package providing tools enabling users to avoid overfitting their backtests.  
The package contains implementations the works of Bailey and Lopez (2012, 2014), Bailey et al. (2014, 2016), Harvey and Liu (2022), a function to estimate Sharpe ratios, as well as four different datasets acting as examples for Backtesting Overfitting and testing grounds for the package's contents.

## Dependencies

The ''BacktestingOverfitting'' package includes the ''pbo'' package (see [pbo on CRAN](https://cran.r-project.org/web/packages/pbo/index.html)) as it already provides an implementation of the Probability of Backtesting Overfitting (PBO) proposed by Bailey et al. (2016).


## How to install

The package can be installed using the ''devtools'' package:
```r
# Installing devtools
install.packages("devtools")
```

```r
# Installing BacktestingOverfitting
devtools::install_github("iamklager/BacktestingOverfitting")
```
