MVA_Example
================
Seungyeon Oh

# Example for MVA

``` r
rm(list=ls())
library(mixsqp)
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
source("~/MVA_code_for_export/MVA.R")
```

<br> <br>

``` r
# set the parameters
p=10000
Train_n1=25 ; Train_n2=25
Test_n1=100 ; Test_n2=100
Beta = 3.5
```

<br> <br>

``` r
# Generate the data set 
set.seed(1)
Param = generate_param_beta(p,Beta)
Data = generate_data(Param, Train_n1,Train_n2,Test_n1,Test_n2)
```

<br> <br>

``` r
# MVA 
X_jV_j = get_Xj_Vj(Data,Train_n1, Train_n2)
Est = get_est(p, X_jV_j, ngrid_var = 300, ngrid = 300, Train_n1, Train_n2)
```

    ## Running mix-SQP algorithm 0.3-54 on 10000 x 300 matrix
    ## convergence tol. (SQP):     1.0e-08
    ## conv. tol. (active-set):    1.0e-10
    ## zero threshold (solution):  1.0e-08
    ## zero thresh. (search dir.): 1.0e-14
    ## l.s. sufficient decrease:   1.0e-02
    ## step size reduction factor: 7.5e-01
    ## minimum step size:          1.0e-08
    ## max. iter (SQP):            1000
    ## max. iter (active-set):     20
    ## number of EM iterations:    10
    ## Computing SVD of 10000 x 300 matrix.
    ## SVD computation took 1.14 seconds.
    ## Rank of matrix is estimated to be 35.
    ## iter        objective max(rdual) nnz stepsize max.diff nqp nls
    ##    1 +1.479091351e+00  -- EM --  300 1.00e+00 7.03e-03  --  --
    ##    2 +1.435316213e+00  -- EM --  300 1.00e+00 2.35e-03  --  --
    ##    3 +1.425243218e+00  -- EM --  300 1.00e+00 1.19e-03  --  --
    ##    4 +1.421734120e+00  -- EM --  300 1.00e+00 7.09e-04  --  --
    ##    5 +1.420193670e+00  -- EM --  300 1.00e+00 4.69e-04  --  --
    ##    6 +1.419414713e+00  -- EM --  300 1.00e+00 3.32e-04  --  --
    ##    7 +1.418980368e+00  -- EM --  300 1.00e+00 2.48e-04  --  --
    ##    8 +1.418719378e+00  -- EM --  300 1.00e+00 1.92e-04  --  --
    ##    9 +1.418552757e+00  -- EM --  299 1.00e+00 1.54e-04  --  --
    ##   10 +1.418440853e+00  -- EM --  297 1.00e+00 1.26e-04  --  --
    ##    1 +1.418362397e+00 +4.362e-02 295  ------   ------   --  --
    ##    2 +1.418305276e+00 +3.842e-02 275 1.00e+00 1.04e-04  20   1
    ##    3 +1.418262412e+00 +3.440e-02 255 1.00e+00 8.83e-05  20   1
    ##    4 +1.418229353e+00 +3.129e-02 235 1.00e+00 7.47e-05  20   1
    ##    5 +1.418203206e+00 +2.671e-02 215 1.00e+00 1.35e-04  20   1
    ##    6 +1.418182162e+00 +2.064e-02 195 1.00e+00 3.60e-04  20   1
    ##    7 +1.418164912e+00 +1.853e-02 175 1.00e+00 1.73e-03  20   1
    ##    8 +1.418150592e+00 +1.675e-02 155 1.00e+00 2.63e-03  20   1
    ##    9 +1.418138532e+00 +1.525e-02 135 1.00e+00 2.94e-03  20   1
    ##   10 +1.418128075e+00 +1.337e-02 115 1.00e+00 5.63e-03  20   1
    ##   11 +1.418119003e+00 +1.164e-02  95 1.00e+00 7.99e-03  20   1
    ##   12 +1.418111383e+00 +1.038e-02  75 1.00e+00 9.65e-03  20   1
    ##   13 +1.418104674e+00 +9.358e-03  55 1.00e+00 1.35e-02  20   1
    ##   14 +1.418098554e+00 +8.633e-03  35 1.00e+00 6.55e-02  20   1
    ##   15 +1.418087311e+00 +9.457e-03  15 1.00e+00 2.49e-01  20   1
    ##   16 +1.418008753e+00 +3.191e-03  12 1.00e+00 2.85e-02  20   1
    ##   17 +1.418001352e+00 +3.345e-04  13 1.00e+00 9.40e-02  20   1
    ##   18 +1.417996593e+00 +1.458e-05  13 1.00e+00 8.31e-02  20   1
    ##   19 +1.417996593e+00 -9.434e-08  13 1.00e+00 5.34e-04  20   1
    ## Optimization took 3.76 seconds.
    ## Convergence criteria met---optimal solution found.
    ## Running mix-SQP algorithm 0.3-54 on 10000 x 300 matrix
    ## convergence tol. (SQP):     1.0e-08
    ## conv. tol. (active-set):    1.0e-10
    ## zero threshold (solution):  1.0e-08
    ## zero thresh. (search dir.): 1.0e-14
    ## l.s. sufficient decrease:   1.0e-02
    ## step size reduction factor: 7.5e-01
    ## minimum step size:          1.0e-08
    ## max. iter (SQP):            1000
    ## max. iter (active-set):     20
    ## number of EM iterations:    10
    ## Computing SVD of 10000 x 300 matrix.
    ## SVD computation took 1.12 seconds.
    ## Rank of matrix is estimated to be 34.
    ## iter        objective max(rdual) nnz stepsize max.diff nqp nls
    ##    1 +2.330886230e+00  -- EM --  300 1.00e+00 4.82e-03  --  --
    ##    2 +2.225871959e+00  -- EM --  300 1.00e+00 2.86e-03  --  --
    ##    3 +2.187690469e+00  -- EM --  300 1.00e+00 1.98e-03  --  --
    ##    4 +2.168789621e+00  -- EM --  300 1.00e+00 1.53e-03  --  --
    ##    5 +2.157731532e+00  -- EM --  298 1.00e+00 1.26e-03  --  --
    ##    6 +2.150562108e+00  -- EM --  281 1.00e+00 1.07e-03  --  --
    ##    7 +2.145579509e+00  -- EM --  267 1.00e+00 9.41e-04  --  --
    ##    8 +2.141938571e+00  -- EM --  257 1.00e+00 8.39e-04  --  --
    ##    9 +2.139175103e+00  -- EM --  248 1.00e+00 7.58e-04  --  --
    ##   10 +2.137014335e+00  -- EM --  241 1.00e+00 6.93e-04  --  --
    ##    1 +2.135283956e+00 +2.859e-02 235  ------   ------   --  --
    ##    2 +2.133870582e+00 +2.596e-02 215 1.00e+00 6.38e-04  20   1
    ##    3 +2.132697122e+00 +2.374e-02 195 1.00e+00 5.92e-04  20   1
    ##    4 +2.131709090e+00 +2.186e-02 175 1.00e+00 5.55e-04  20   1
    ##    5 +2.130867135e+00 +2.024e-02 155 1.00e+00 5.37e-04  20   1
    ##    6 +2.130142128e+00 +1.883e-02 135 1.00e+00 4.92e-04  20   1
    ##    7 +2.129512084e+00 +1.759e-02 115 1.00e+00 8.85e-04  20   1
    ##    8 +2.128960091e+00 +1.650e-02  95 1.00e+00 5.07e-03  20   1
    ##    9 +2.128470833e+00 +1.553e-02  75 1.00e+00 6.41e-03  20   1
    ##   10 +2.128035986e+00 +1.465e-02  55 1.00e+00 1.95e-02  20   1
    ##   11 +2.127649812e+00 +1.373e-02  35 1.00e+00 2.60e-02  20   1
    ##   12 +2.127300484e+00 +1.302e-02  15 1.00e+00 1.75e-01  20   1
    ##   13 +2.121371017e+00 +1.416e-01   4 1.00e+00 6.78e-01  20   1
    ##   14 +2.121251872e+00 +1.725e-02   5 1.00e+00 1.91e-01  20   1
    ##   15 +2.121248164e+00 +5.420e-04   6 1.00e+00 5.81e-03  20   1
    ##   16 +2.121247993e+00 +3.173e-06   6 1.00e+00 1.08e-02  20   1
    ##   17 +2.121242941e+00 +2.123e-05   6 1.00e+00 4.08e-01  20   1
    ##   18 +2.121242906e+00 -9.108e-08   6 1.00e+00 4.31e-03  20   1
    ## Optimization took 2.00 seconds.
    ## Convergence criteria met---optimal solution found.

``` r
Eval = evaluation(Data, X_jV_j, Est, test_group = c(rep(1,Test_n1),rep(2,Test_n2)))
cat("\n");cat("Misclassification Rate of MVA :",Eval)
```

    ## Misclassification Rate of MVA : 0.145

<br><br>

``` r
# compare the estimated parameters with true parameters
par(mfrow = c(1, 2))
true_mean_diff = Param$mu1 - Param$mu2
true_sigma_2 = Param$sigma_2
plot(true_mean_diff, Est$mu, main = "Mean difference", xlab="True mean differences", ylab = "Estimated mean differneces by MVA")
abline(0,1,col="skyblue")
text(0.7,0.5, "y=x",col="skyblue")

plot(true_sigma_2, Est$sigma_2, main = "Mean difference", xlab="True variances", ylab = "Estimated variances by MVA")
abline(0,1,col="skyblue")
text(3.5,3, "y=x",col="skyblue")
```

![](Example_for_MVA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
