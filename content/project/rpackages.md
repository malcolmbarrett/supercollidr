+++
title = "R Packages"
date = 2018-01-04T14:16:29-08:00
draft = false

# Tags: can be used for filtering projects.
# Example: `tags = ["machine-learning", "deep-learning"]`
tags = ["R", "coding"]

# Project summary to display on homepage.
summary = "DAG analysis and visualization, visualization for meta-analyses, tools for causal inference, and more"

# Optional image to display on homepage.
image_preview = "rpackage_proj.png"

# Optional external URL for project (replaces project detail page).
external_link = ""

# Does the project detail page use math formatting?
math = false

# Does the project detail page use source code highlighting?
highlight = true

# Featured image
# Place your image in the `static/img/` folder and reference its filename below, e.g. `image = "example.jpg"`.
[header]
image = "rpackage_proj-wide.png"
caption = ""
+++

The source code for my R Packages can be found on [GitHub](//github.com/malcolmbarrett).

**DAG analysis and visualization**: `ggdag`, a package for working with directed acyclic graphs in `ggplot2` and built on top of `dagitty`. `ggdag` focuses on working with selection bias, confounding, and selecting the right variables to adjust for in an analysis.

**Visualization for meta-analyses**: `tidymeta`, including `ggplot2` functionality for forest plots, funnel plots, and influence plots.

**Creating and visualizing sufficient-component cause models**: `causalpie`, which creates, analyzes, and plots causal pies.

**Estimate Sample Size Based on Precision**: `precisely`, a package to estimate sample size based on precision rather than power. Power calculations are focused on whether or not an estimate will be statistically significant; calculations of precision are based on the same prinicples as power calculation but turn the focus to the width of the confidence interval.

**Other Packages**: `mbmisc`, a package that contains functionsI use often and `ggplot2` themes. `koanr`, package containing data on important Zen texts. 
