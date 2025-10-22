#### R codes used to generate results 

Standard Normal distribution density: 

``` r 
dnorm(x, mean = 0, sd = 1)
```

`x` are quantiles, `mean` and `sd` are mean value and standard deviation respectively. 

Simulate `n` random samples from the standard Normal distribution: 

``` r 
rnorm(n, mean = 0, sd = 1)
``` 

Calculate critical values for the standard Normal distribution: 
``` r
qnorm(p, mean = 0, sd = 1)
```

`p` are probabilities. Replace `norm` with `binom`, `t`, `chisq` or `F` to get results for binomial, t, chi-squared and F distributions respectively.  

Generate histogram: 
``` r
hist(x)
```
`x` is a numeric vector. 
