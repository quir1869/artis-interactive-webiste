# About webiste

[**SeafoodExplorer**](https://quir1869.shinyapps.io/SeafoodExplorer/) is a website tool that explores the Aquatic Resource Trade In Species database as well as data I've created for my first chapter ([see work](https://github.com/Seafood-Globalization-Lab/foreign-dependency-protein)) as an educational tool for members of the public to learn about our seafood consumption and nutritional sourcing.

------------------------------------------------------------------------

# How to deploy website:

In the R console of this opened repository, run the following line of code:

`rsconnect::deployApp(appDir = here(), appPrimaryDoc = "00_Consumption-Website.R", appName = "SeafoodExplorer")`
