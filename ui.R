library(shiny)
library(shinythemes)
require(markdown)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("p { font-size: small; }"))
  ),
  titlePanel("R Packages and Licenses by Task View"),
  fluidRow(
    column(checkboxInput("task", "Filter by Task View", TRUE), width = 2),
    column(
      conditionalPanel(
        "input.task",
        selectizeInput("view", NULL,
                       choices = list(
                         "Bayesian Inference" = "Bayesian",
                         "Chemometrics and Computational Physics" = "ChemPhys",
                         "Clinical Trial Design, Monitoring, and Analysis" = "ClinicalTrials",
                         "Cluster Analysis & Finite Mixture Models" = "Cluster",
                         "Differential Equations" = "DifferentialEquations",
                         "Probability Distributions" = "Distributions",
                         "Econometrics" = "Econometrics",
                         "Analysis of Ecological and Environmental Data" = "Environmetrics",
                         "Design of Experiments (DoE) & Analysis of Experimental Data" = "ExperimentalDesign",
                         "Extreme Value Analysis" = "ExtremeValue",
                         "Empirical Finance" = "Finance",
                         "Statistical Genetics" = "Genetics",
                         "Graphic Displays & Dynamic Graphics & Graphic Devices & Visualization" = "Graphics",
                         "High-Performance and Parallel Computing with R" = "HighPerformanceComputing",
                         "Machine Learning & Statistical Learning" = "MachineLearning",
                         "Medical Image Analysis" = "MedicalImaging",
                         "Meta-Analysis" = "MetaAnalysis",
                         "Multivariate Statistics" = "Multivariate",
                         "Model Deployment with R" = "ModelDeployment",
                         "Natural Language Processing" = "NaturalLanguageProcessing",
                         "Numerical Mathematics" = "NumericalMathematics",
                         "Official Statistics & Survey Methodology" = "OfficialStatistics",
                         "Optimization and Mathematical Programming" = "Optimization",
                         "Analysis of Pharmacokinetic Data" = "Pharmacokinetics",
                         "Phylogenetics, Especially Comparative Methods" = "Phylogenetics",
                         "Psychometric Models and Methods" = "Psychometrics",
                         "Reproducible Research" = "ReproducibleResearch",
                         "Robust Statistical Methods" = "Robust",
                         "Statistics for the Social Sciences" = "SocialSciences",
                         "Analysis of Spatial Data" = "Spatial",
                         "Handling and Analyzing Spatio-Temporal Data" = "SpatioTemporal",
                         "Survival Analysis" = "Survival",
                         "Time Series Analysis" = "TimeSeries",
                         "Web Technologies and Services" = "WebTechnologies",
                         "gRaphical Models in R" = "gR"
                       ),
                       multiple = TRUE, selected = c("MachineLearning", "Cluster"),
                       options = list(plugins = list("remove_button")), width = "100%")
      ),
      width = 6),
    column(checkboxGroupInput(
      "fields", "Show additional fields:",
      choices = c("Description", "URL", "Authors"),
      inline = TRUE
    ), width = 4)
  ),
  DT::dataTableOutput("packages"),
  br(),
  includeMarkdown("README.md")
))
