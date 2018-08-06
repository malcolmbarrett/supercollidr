---
title: 'When interaction is not interaction: confounding and measurement error'
author: 'Malcolm Barrett'
date: '2018-08-04'
slug: interaction-confounding
categories:
  - r
tags:
  - causalinference
  - methods
  - r
header:
  caption: 'test test test'
  image: ''
---

Last week, I presented [ggdag](https://ggdag.netlify.com) at JSM in Vancouver. As you can imagine, I had a lot of conversations with people about DAGs, confounding, colliders, and all the types of bias that can arise in research. One strange type of bias came up a couple of times that I don't see discussed very often: measuring either the effect you are studying (`x`) or a variable along a confounding pathway (`z`) incorrectly can make it appear as if there is an interaction between `x` and `z`, even if there isn't one.

Let's consider a simple example: there's an association between `x` and `y`, which is what we're interested in, and this association is confounded by `z`. This is a classic example of confounding.


```r
library(broom)
library(tidyverse)
library(kableExtra) # to add headers on kable() tables
library(knitr)
library(ggdag)
library(patchwork) # to combine the plots
options(knitr.kable.NA = "--") # don't show NA values in kable()

set.seed(293951)
```


```r
confounder_triangle(x_y_associated = TRUE) %>% 
  ggdag() +
  theme_dag()
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/dag1-1.png" width="672" />

Let's simulate some data. `x` and `y` are both continuous, and `z` is binary (0 or 1, with only about 10% of the population with `z=1`). We'll simulate 10,000 participants so random error is not a big issue.


```r
z <- sample(0:1, 10000, replace = TRUE, prob = c(.9, .1))
x <- rnorm(10000) + 2*z
y <- 1 + 2*x + 1.5*z + rnorm(10000)

df <- data_frame(y, x, z)

df
```

```
## # A tibble: 10,000 x 3
##          y       x     z
##      <dbl>   <dbl> <int>
##  1 -0.494  -0.463      0
##  2 -0.649   0.0242     0
##  3  2.50   -0.146      0
##  4  1.36    0.0434     1
##  5  3.41    1.00       0
##  6 -2.17   -1.94       0
##  7  0.989   0.209      0
##  8  0.0667 -0.831      0
##  9  1.98    0.0196     0
## 10  1.45    0.0405     0
## # ... with 9,990 more rows
```

We'll consider what happens when we mismeasure `x` or `z`. For each scenario, we'll compare models with and without an interaction term (`x*z`). To fit, tidy, and compare the models, let's write a few functions:

* `tidy_lm()` tidies the regression models and formats the numbers a little
* `models_kable()` joins two tables and renders them with `kable()` using the `kableExtra` package to add a couple of headers
* `compare_models()` combines these two functions and accepts the variables we want to change as arguments so we don't need to rewrite formulas every time
* `plot_models()` plots the relationship between `x` and `y` by levels of `z` with hex bins (for the distribution) and regression lines (to assess interaction), combining them with the `patchwork` and `cowplot` packages (plus a little tidy eval magic!)


```r
tidy_lm <- function(model) {
  model %>% 
    tidy() %>% 
    mutate(p.value = ifelse(p.value < .001, "<.001", round(p.value, 3))) %>% 
    mutate_if(is.numeric, round, 2)
}

models_kable <- function(no_int, int, ...) {
  no_int <- select(no_int, term, estimate, p.value)
  int <- select(int, term, estimate, p.value)
  full_join(no_int, int, by = "term") %>% 
    mutate(term = c("(Intercept)", "x", "z", "x * z")) %>% 
    kable(col.names = c("Term", rep(c("Estimate", "P-Value"), 2)), ...) %>%
    add_header_above(c(" ", "No Interaction" = 2, "Interaction" = 2))
}

compare_models <- function(exposure = "x", confounder = "z", ...) {
  fmla <- as.formula(paste("y ~ ", exposure, " + ", confounder)) # create a formula
  no_int <- lm(fmla, data = df) %>% 
    tidy_lm()
  
  fmla_int <-   fmla <- as.formula(paste("y ~ ", exposure, " * ", confounder))
  int <- lm(fmla_int, data = df) %>% 
    tidy_lm()
  
  models_kable(no_int, int, ...)
}

plot_models <- function(x = x,
                        z = z,
                        x_label = "x (Measured well)", 
                        z_label = "Confounder (Measured well)",
                        crude = FALSE) {
  x <- rlang::enquo(x)
  z <- rlang::enquo(z)
  
  p1 <- df %>% 
    ggplot(aes(x = !!x, y = y, col = factor(!!z))) +
      geom_hex(aes(fill = factor(z)), col = "white", alpha = .7) +
      scale_color_manual(name = z_label, values = c("#56B4E9", "#EFA722", "#E36A25")) + 
      scale_fill_manual(name = z_label, values = c("#56B4E9", "#EFA722", "#E36A25")) + 
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            axis.title.x = element_blank()) +
      labs(y = "y (Measured well)") + 
      ylim(c(-10, 16))
  
  legend <- cowplot::get_legend(p1)
  
  p1a <- p1 + theme(legend.position = "none")
  
  p2 <- df %>% 
    ggplot(aes(x = !!x, y = y, col = factor(!!z))) +
      geom_hex(fill = "grey92", col = "white", alpha = .8) +
      geom_smooth(method = "lm", se = FALSE, size = .9) + 
      scale_color_manual(name = z_label, values = c("#56B4E9", "#EFA722", "#E36A25")) + 
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
              axis.title = element_blank()) +
      ylim(c(-10, 16))
  
  if (crude) {
    p2 <- p2 + 
            geom_smooth(aes(group = 1, col = "Crude Estimate"), 
                                    method = "lm", se = FALSE, size = .9) +
            theme(legend.position = "bottom")
    legend <- cowplot::get_legend(p2)
    p2 <- p2 + theme(legend.position = "none")
    }
  
  patchworked <- p1a + p2
  pl1 <- cowplot::plot_grid(patchworked, align = "h")
  pl2 <- cowplot::add_sub(pl1, x_label, 
                          vpadding = grid::unit(0,"lines"), 
                          y = 4.5, x = .5, vjust = 4.5)
  pl3 <- cowplot::plot_grid(NULL, legend, NULL, nrow = 1)
  cowplot::plot_grid(pl2, pl3, ncol = 1, rel_heights = c(1.5, .2))
}
```

If all three variables are measured well, there's no problem. The effect estimates are about right, and there's no false interaction. For the plot, notice how 1) the crude estimate (which ignores `z`) is an mix of the other two lines and 2) the slopes of the lines for `z=0` and `z=1` are parallel. 


```r
compare_models()
```

<table>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">No Interaction</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Interaction</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.99 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.99 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x </td>
   <td style="text-align:right;"> 2.01 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 2.02 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z </td>
   <td style="text-align:right;"> 1.46 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 1.47 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x * z </td>
   <td style="text-align:right;"> -- </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:left;"> 0.912 </td>
  </tr>
</tbody>
</table>

```r
plot_models(crude = TRUE)
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/correct_model-1.png" width="672" />

## Measurement error for the outcome (`x`)

Let's say that, in practice, we are measuring `x` incorrectly (measurement error). The only device around to measure `x` is an old 1980 version of the measureator, which by complete coincidence is the same year Sander Greenland [first talked about this issue](https://academic.oup.com/aje/article-abstract/112/4/564/59323?redirectedFrom=fulltext). The 1980 version of this device has a problem: it measures `x` better in people who have values of `y` above 0 than those below 0. The 2018 version of the device still measures `x` with error, but the manufacturer fixed the problem with `y`, and now it has nothing to do with that.

### Mismeasured `x`, dependent on `y`

When we use `x_1980` as a proxy for `x`, there now appears to be an interaction with `z`. It's present in both the model estimates and the plot. The slopes are no longer parallel. The effect of `x` on `y` seems like it's heterogeneous for levels of `z`: for people with `z=1`, the effect of `x` on `y` is stronger.


```r
measureator2018 <- function(x) x + rnorm(10000)
measureator1980 <- function(x) ifelse(y > 2, x + rnorm(10000, sd = 1.5), ifelse(y > 0, x + rnorm(10000, sd = 2), x + rnorm(10000, sd = 3)))

df <- df %>% 
  mutate(
    x_2018 = measureator2018(x),
    x_1980 = measureator1980(x)
  )

compare_models("x_1980")
```

<table>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">No Interaction</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Interaction</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.32 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z </td>
   <td style="text-align:right;"> 4.80 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 4.28 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x * z </td>
   <td style="text-align:right;"> -- </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
</tbody>
</table>

```r
plot_models(x = x_1980, x_label = "Mismeasured x (1980 device)")
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/x_1980-1.png" width="672" />

What's going on here? The plot helps demonstrate the distortion in x (it's much less stable when `y` is below 0 and most people with `z=1` are above 0). We can put this together in a DAG. In the case of the 1980 device, we have differential measurement error, so called because the degree of mismeasurement is affected by `y`. We want to estimate the effect of `x`, but what we really have is an approximation, `x_m`, dependent on both `error` (the amount of mismeasurement) and `x`. `error` is dependent on `y`. 


```r
dme <- dagify(y ~ x + z, 
       x ~ z, 
       x_m ~ x + error,
       error ~ y) 

ggdag(dme) +
  theme_dag()
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/dag2-1.png" width="672" />

Because two arrows are leading into it, `x_m` is a [collider](https://ggdag.netlify.com/articles/intro-to-dags.html#colliders-and-collider-stratification-bias). Including a collider in the regression model will induce an association between its parents, creating bias. Collider bias can also travel upstream (see the previous link), here affecting `z` and `y`, as well. Look at the web of associations it creates!


```r
ggdag_adjust(dme, c("x_m", "z")) +
  theme_dag() +
    scale_color_manual(values = c("#2C7FBF", "#E69F00")) + 
    ggraph::scale_edge_color_manual(values = c("#E7E7E7", "#E69F00"))
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/dag3-1.png" width="672" />

### Mismeasured `x`, independent of `y`

Things are a little different if we use the 2018 device, which measures `x` incorrectly but has nothing to do with `y`. In this case, there's no apparent interaction.


```r
compare_models("x_2018")
```

<table>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">No Interaction</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Interaction</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.97 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.97 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x </td>
   <td style="text-align:right;"> 0.99 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.99 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z </td>
   <td style="text-align:right;"> 3.48 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 3.40 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x * z </td>
   <td style="text-align:right;"> -- </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:left;"> 0.27 </td>
  </tr>
</tbody>
</table>

```r
plot_models(x = x_2018, x_label = "Mismeasured x (1980 device)")
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/x_2018-1.png" width="672" />

This is a case of non-differential measurement error; the error is not dependent on the outcome, `y`. `x_m` is still a collider but it ends up being less important. Instead, the model underestimate the effect of `x` on `y` (otherwise known as bias towards the null, which is what happens under most cases of non-differential error) and attributes too much of the effect on `y` to `z` (`x` is a mediator of `z`'s impact on `y`). 


```r
ndme <- dagify(y ~ x + z, 
       x ~ z, 
       x_m ~ x + error) 

ggdag(ndme) +
  theme_dag()
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/dag4-1.png" width="672" />

## Measurement error for covariates (`z`)

What about if we measure `x` well but mismeasure the confounder, `z`? Let's say we have a similar situation: one device that occasionally misclassifies `z` but isn't effected by `y` and another, broken device that measures `z` in people who have higher values of `y` better.


```r
swap <- function(x) ifelse(x == 1, 0, 1)

z_ometer <- function(z) {
  mismeasured <- sample(c(TRUE, FALSE), size = 10000, replace = TRUE, prob = c(.3, .7))
  ifelse(mismeasured, swap(z), z)
}

z_ometer_broken <- function(z) {
  mismeasured <- ifelse(
    df$y > 0,
    sample(c(TRUE, FALSE), size = 10000, replace = TRUE, prob = c(.5, .5)),
    sample(c(TRUE, FALSE), size = 10000, replace = TRUE, prob = c(.1, .9))
  )
  ifelse(mismeasured, swap(z), z)
}

df <- df %>% 
  mutate(
    z_results = z_ometer(z),
    z_results_broken = z_ometer_broken(z)
  )
```

The DAGs for `z_m` look pretty similar to `x_m`.


```r
ndme_z <- dagify(y ~ x + z, 
       x ~ z, 
       z_m ~ z + error) 

dme_z <- dagify(y ~ x + z, 
       x ~ z, 
       z_m ~ z + error,
       error ~ y) 

dag1 <- ggdag(ndme_z) +
  theme_dag() +
  ggtitle("Wrongly measure z, independent of y")

dag2 <- ggdag(dme_z) +
  theme_dag() +
  ggtitle("Wrongly measure z, dependent on y")

dag1 + dag2
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/dag5-1.png" width="672" />

For both devices, there appears to be a little interaction (probably only detectable because of our large sample size). While that's often the case, the larger issue is that our proxies for `z` are no longer blocking the back-door path between `x` and `y`. In other words, there's residual confounding. In this example, what ends up happening is a little bit of false interaction for both differential and non-differential misclassification of `z` and a lot of confounding bias. The estimate for `x` is a little bit off (about 10% too high), and `z` is much too low (it's biased towards the null).

### Mismeasured `z`, dependent on `y`


```r
compare_models(confounder = "z_results_broken")
```

<table>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">No Interaction</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Interaction</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x </td>
   <td style="text-align:right;"> 2.19 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 2.22 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x * z </td>
   <td style="text-align:right;"> -- </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:right;"> -0.08 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
</tbody>
</table>

```r
plot_models(z = z_results_broken, z_label = "Confounder (Z-ometer, broken)")
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/z_results-1.png" width="672" />

### Mismeasured `z`, independent of `y`


```r
compare_models(confounder = "z_results")
```

<table>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">No Interaction</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Interaction</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:left;"> P-Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 1.04 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x </td>
   <td style="text-align:right;"> 2.21 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 2.13 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> z </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:left;"> &lt;.001 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> x * z </td>
   <td style="text-align:right;"> -- </td>
   <td style="text-align:left;"> -- </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
</tbody>
</table>

```r
plot_models(z = z_results, z_label = "Confounder (Z-ometer, broken)")
```

<img src="/post/2018-08-04-when-interaction-is-not-interaction-confounding-and-measurement-error_files/figure-html/z_results_broken-1.png" width="672" />

So, mismeasurement of both `x` and `z` can cause problems. Under most circumstances, of course, we are mismeasuring more than one variable (including `y`!). Moreover, the errors in the way those variables are measured may themselves be dependent. We need to, then, be very mindful of the structures of these bias and, if necessary, try to address them with bias analysis approaches. 

If you want to learn about more about these methods, you may be interested in this great-looking resource from Maarten van Smeden:

{{< tweet 1025990407757422592 >}}

Thanks to him for providing it!
