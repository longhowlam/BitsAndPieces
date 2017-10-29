library(titanic)
library(h2o)
library(purrr)
library(dplyr)

#### Calculate univariate GINI of inputs and a target 

## titanic data set as a test
titan = titanic::titanic_train
titan = titan %>% mutate(Target = as.factor(ifelse(Survived < 0.5,"N","Y")))
titan = titan %>% mutate_if(is_character, as.factor)

## Use h2o random forest for GINI calculations
h2o.init()
titan.h2o = as.h2o(titan) 

univariate = function(x, target, data)
{
  obj = h2o.randomForest(
    x = x,
    y = target,
    training_frame = data
  )
  obj@model$training_metrics@metrics$Gini
}

## consider only inputs and exclude some variables
inputs = setdiff(names(titan),  c("Target", "Survived"))
ginis = map_dbl(inputs, univariate, "Target", titan.h2o)

tibble(inputs, ginis)
