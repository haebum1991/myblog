
---
title: "GAM & PM2.5 and O3 smoke tool (PMO3smokeTool)"
---
<div style="text-align: left;">
  <a href="https://westar.shinyapps.io/PMO3smokeTool/" target="_blank" style="font-size: 16px;">
    https://westar.shinyapps.io/PMO3smokeTool/
  </a>
</div>

<h2 style="font-weight: bold;">Purpose of tool</h2>

<p>The purpose of this app is to demonstrate the application of a
<b>Generalized Additive Model (GAM)</b> for predicting MDA8 O3 using meteorological data under both smoke and non-smoke conditions.
In this app, a GAM is derived using the "mgcv" package in R. We hope this tool will be useful for state agencies and others 
in understanding the factors that control O3 production.</p>

<img src="/images/rsGAM_page_sample.png" alt="R shiny">

<div style="text-align: center;">
  Example page in the Smoke tool app
</div>

<p><b>The GAM</b> is a statistical or machine learning framework that provides interpretability 
by applying smoothing functions to individual variables. Notably, GAMs can incorporate linear,
nonlinear, and categorical predictors, providing flexibility for modeling complex relationships 
<a href="https://doi.org/10.1201/9781315370279" target="_blank">(Wood, 2017).</a>
In particular, such statistical/machine learning models can provide an efficient tool 
for analyzing air quality, especially in relation to wildfire smoke.
For example, O3 can be used as the response variable, modeled using meteorological data, satellite-derived measurements, and/or backward air trajectories 
<a href="https://doi.org/10.1021/acs.est.4c05870" target="_blank">(Lee and Jaffe, 2024).</a>
This study demonstrated the importance of wildfire smoke as contributors to exceedances of the health-based national air quality standards for PM2.5 and O3.
This app offers options to easily build a GAM using the <b>"mgcv"</b> package 
<a href="https://CRAN.R-project.org/package=mgcv" target="_blank">(Wood, 2023)</a>
and also allows viewing the results of previously studied GAMs.</p>

<p>If you want to run your own GAM applications, go to the <b>[GAM manual]</b> tab.
Here, you can download data and set up the GAM based on your goals and your understanding of ozone in your local region.
This feature is generally intended for expert users who are familiar with statistical models for ozone.</p>
<p>If you want to see the results from our runs, 
which have been published in peer-reviewed journals or are soon to undergo peer review, 
please go to the <b>[GAM previous]</b> tab.
<b style="color: blue;">We also provide various research results on PM2.5 and O3 smoke in this tab.</b>
You can plot and download results in a variety of formats.</p>

<p>This app was created by Dan Jaffe and Haebum Lee, University of Washington (UW).<br>
Please send any feedback or bug reports to: <b>Dan Jaffe</b> 
(<a href="mailto:djaffe@uw.edu">djaffe@uw.edu</a>) and 
<b>Haebum Lee</b> (<a href="mailto:haebum1991@gmail.com">haebum1991@gmail.com</a>).</p>

<p style="font-size: 16px;">Related GAM references:</p>
<ul style="font-size: 16px;">
  <li>Lee, H. and Jaffe, D. A.: Wildfire impacts on O3 in the continental United States using PM2.5 and a generalized additive model (2018–2023), Environ. Sci. Technol., 58, 14764–14774, 2024.
  <a href="https://doi.org/10.1021/acs.est.4c05870" target="_blank">https://doi.org/10.1021/acs.est.4c05870</a></li>
  <li>Lee, H. and Jaffe, D. A.: Impact of wildfire smoke on ozone concentrations using a Generalized Additive model in Salt Lake City, Utah, USA, 2006–2022, J. Air Waste Manag. Assoc., 74, 116–130, 2024.
  <a href="https://doi.org/10.1080/10962247.2023.2291197" target="_blank">https://doi.org/10.1080/10962247.2023.2291197</a></li>
  <li>Jaffe, D. A.,  Ninneman, M., Nguyen, L., Lee, H., Hu, L., Ketcherside, D., Jin L., Cope, E., Lyman, S., Jones, C., ONeli, T., Mansfield, M. L.: Key results from the Salt Lake regional smoke, ozone and Aerosol study (SAMOZA), J. Air Waste Manag. Assoc., 74, 163–180, 2024.
  <a href="https://doi.org/10.1080/10962247.2024.2301956" target="_blank">https://doi.org/10.1080/10962247.2024.2301956</a></li>
  <li>Ninneman, M., et al.: Investigation of the Parameters Influencing Baseline Ozone in the Western United States: A Statistical Modeling Approach, Atmosphere, 13, 1883, 2022.
  <a href="https://doi.org/10.3390/atmos13111883" target="_blank">https://doi.org/10.3390/atmos13111883</a></li>
  <li>McClure, C. D. and Jaffe, D. A.: Investigation of high ozone events due to wildfire smoke in an urban area, Atmos. Environ., 194, 146–157, 2018.
  <a href="https://doi.org/10.1016/j.atmosenv.2018.09.021" target="_blank">https://doi.org/10.1016/j.atmosenv.2018.09.021</a></li>
  <li>Gong, X., et al.: Quantifying O3 Impacts in Urban Areas Due to Wildfires Using a Generalized Additive Model, Environ. Sci. Technol., 51, 13216–13223, 2017.
  <a href="https://doi.org/10.1021/acs.est.7b03130" target="_blank">https://doi.org/10.1021/acs.est.7b03130</a></li>
</ul>

