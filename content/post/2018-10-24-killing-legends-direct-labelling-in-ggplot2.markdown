---
title: 'Killing Legends: Direct Labelling in ggplot2'
author: ''
date: '2018-10-24'
draft: true
slug: killing-legends-direct-labelling-in-ggplot2
categories: [r, dataviz]
tags:
  - r
header:
  caption: ''
  image: ''
---




```r
label_axes_directly <- function(.data, p = last_plot(), .labels, .x = x, .y = y, axis = "y", ...) {
  .x <- enquo(.x)
  .y <- enquo(.y)
  .labels <- enquo(.labels)
  direct_labels_axis <- axis_canvas(p, axis = "y") +
    geom_text(data = .data, aes(!!.x, !!.y, label = !!.labels), ...)

  p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)

  ggdraw(p_direct_labels)
}


compute_density <- ggplot2:::compute_density

locate_density_labels <- function(.data, x, ...) {
  group_vars <- rlang::enquos(...)
  x <- rlang::enquo(x)
  .data %>% 
    group_by(!!!group_vars) %>%
    nest() %>% 
    mutate(data = map(data, ~compute_density(.x[[quo_name(x)]], w = NULL)),
           data = map(data, ~filter(.x, density == max(density)))) %>% 
    unnest() %>% 
    mutate(density = density + .01)
}
# 
# ggplot(iris, aes(Sepal.Length, col = Species)) + geom_density() + ggrepel::geom_text_repel(data = locate_density_labels(iris, Sepal.Length, Species), aes(x, density, label = Species), inherit.aes = FALSE, nudge_y = .01)
```



```r
library(tidyverse)

scatterplot_extras <- function(legend.position = "none") { 
  list(
    theme_minimal(base_size = 14), 
    theme(
      legend.position = legend.position,
      panel.grid.minor.x = element_blank(), 
      panel.grid.minor.y = element_blank() 
     ),
    labs(
      x = "log(GDP per capita)",
      y = "life expectancy"
     ),
    scale_color_manual(values = country_colors)
  )
}
```



```r
library(gapminder)

gapminder_2007 <- gapminder %>%
  filter(year == 2007)

gapminder_2007 %>% 
  ggplot(aes(log(gdpPercap), lifeExp, col = country)) +
    geom_point(size = 3.5, alpha = .9) +
    scatterplot_extras("right")
```

<img src="/post/2018-10-24-killing-legends-direct-labelling-in-ggplot2_files/figure-html/unnamed-chunk-3-1.png" width="2240" style="display: block; margin: auto;" />



```r
library(ggrepel)
gapminder_2007 %>% 
  ggplot(aes(log(gdpPercap), lifeExp)) +
    geom_point(
       size = 3.5, 
       alpha = .9, 
       shape = 21, 
       col = "white", 
       fill = "#0162B2"
     ) +
    geom_text_repel(
      aes(label = country), 
      size = 4.5,
      point.padding = .2,
      box.padding = .4, 
      min.segment.length = 0
      ) +
    scatterplot_extras()
```

<img src="/post/2018-10-24-killing-legends-direct-labelling-in-ggplot2_files/figure-html/unnamed-chunk-4-1.png" width="2240" style="display: block; margin: auto;" />

## Sample labels rather than display them all


```r
set.seed(1010)

countries <- gapminder_2007 %>% 
    sample_n(15) %>%  
    pull(country)

countries
```

```
##  [1] Malaysia              Comoros               Colombia             
##  [4] Nigeria               Taiwan                Sierra Leone         
##  [7] Netherlands           Turkey                Guinea               
## [10] Romania               Sudan                 Sao Tome and Principe
## [13] Tanzania              Korea, Rep.           Albania              
## 142 Levels: Afghanistan Albania Algeria Angola Argentina ... Zimbabwe
```


<img src="/post/2018-10-24-killing-legends-direct-labelling-in-ggplot2_files/figure-html/unnamed-chunk-6-1.png" width="2240" style="display: block; margin: auto;" />




```r
continent_data <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp))

line_plot_extras <- function(legend.position = "none", 
                             values = continent_colors) {
  list(
    theme_minimal(base_size = 14),
    theme(
      legend.position = legend.position,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
     ),
    scale_color_manual(values = values),
    labs(y = "life expectancy")
  )
}
```


```r
continent_data %>% 
  ggplot(aes(year, lifeExp, col = continent)) +
    geom_line(size = 1.2) +
    line_plot_extras("right")
```

<img src="/post/2018-10-24-killing-legends-direct-labelling-in-ggplot2_files/figure-html/unnamed-chunk-8-1.png" width="2240" style="display: block; margin: auto;" />

## Change in average life expectancy by continent


```r
direct_labels <- continent_data %>% 
  group_by(continent) %>%
  summarize(
    x = max(year), 
    y = max(lifeExp)
   )

direct_labels
```

```
## # A tibble: 5 x 3
##   continent     x     y
##   <fct>     <dbl> <dbl>
## 1 Africa     2007  54.8
## 2 Americas   2007  73.6
## 3 Asia       2007  70.7
## 4 Europe     2007  77.6
## 5 Oceania    2007  80.7
```



```r
library(cowplot)
```


```r
p <- continent_data %>% 
  ggplot(aes(year, lifeExp, col = continent)) +
    geom_line(size = 1.2) +
    line_plot_extras() + 
    scale_x_continuous(expand = expand_scale(0))

direct_labels_axis <- axis_canvas(p, axis = "y") + 
     geom_text(
       data = direct_labels, 
       aes(y = y, label = continent), 
       x = 0.06, 
       hjust = 0, 
       size = 5, 
       col = continent_colors
      )

p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)

ggdraw(p_direct_labels)
```

<img src="/post/2018-10-24-killing-legends-direct-labelling-in-ggplot2_files/figure-html/unnamed-chunk-11-1.png" width="2240" style="display: block; margin: auto;" />
