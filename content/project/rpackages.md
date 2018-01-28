+++
title = "R Packages"
date = 2018-01-04T14:16:29-08:00
draft = false

# Tags: can be used for filtering projects.
# Example: `tags = ["machine-learning", "deep-learning"]`
tags = ["R", "coding"]

# Project summary to display on homepage.
summary = "DAG analysis and visualization, publication-quality tables, working with confounders, visualization for meta-analyses"

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

My R Packages are currently available on [GitHub](//github.com/malcolmbarrett), and I will be submitting some of them to CRAN soon.

**DAG analysis and visualization**: `ggdag`, a package for working with directed acyclic graphs in `ggplot2` and built on top of `dagitty`. `ggdag` focuses on working with selection bias, confounding, and selecting the right variables to adjust for in an analysis.

**Working with confounders**: `confoundr`, a package for detecting and visualizing confounding in observational studies.

---
# **Publication-quality tables**: `mable`, a package influenced by `ggplot2` and built on top of `knitr::kable()` and `kableExtra` to produce fluid, easy-to-build tables for `RMarkdown`.
# 
# **Visualization for meta-analyses**: `ggmeta`, including `ggplot2` functionality for forest plots, funnel plots, and influence plots.
---