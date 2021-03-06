# Lpa_plotting
Plots for high-dimensional, genetic data for  Lp(a)*

*note: this plotting technique was developed for Lp(a) but can be applied to other phenotypes. Titles and labels are de-identified. 

## PheWas Circle Plots
This plot can be used to demonstrate heterogeneity of results across a vast array of phenotypes. Direction of effect as well as strength of effect can be shown through color and shade mapping. Results are split based on 4 strata (each row the the circle). 

**Relevant Script:** PheWas_circle_plot.R

**Example Plot**:
![](circle_plot_example.png)

*Note: The titles are purely to show labels are possible. They are truncated and not meant to be legible for this example.*

## SNP forest plot
These 2 plots can be used to demonstrate SNP effect sizes and confidence intervals as well as heterogeneity in coded allele frequencies by strata. Each Row corresponds to a single SNP. 

**Relevant Script:** Forest_plot.R

**Examples Plots:**

1. Forest Plot
![](forest_plot_example.png)

2. Coded Allele Frequencies 
![](caf_plot_example.png)


