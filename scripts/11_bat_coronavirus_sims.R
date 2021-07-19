devtools::install_github("alrobles/ecointeraction",
                         auth_token = "1d246d7725ebae0cfc417de923096cb4b608a6dc", dependencies = FALSE)

library(ecointeraction)

list.of.packages <- c("caret",
                      "modelgrid",
                      "ranger",
                      "dplyr",
                      "tidymodels")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org/")
sapply(list.of.packages, require, character.only = TRUE)
library(ecointeraction)

# cutting top 10 incidence mammals with dengue incidence
bats_cutoff <- ecointeraction::batscoronavirus  %>%
  ecointeraction::acummulate_incidence(species) %>%
  ecointeraction::cutoff_incidence(accuracy =  4) %>%
  dplyr::select(species)

listBatsData <- replicate(1000, ecointeraction::prep_incidence_data(distance = ecointeraction::batsdistance, incidence = bats_cutoff), simplify = FALSE)

automodel_replication <- function(data, distance)
{
  data_split <-  dplyr::select( data, -species) %>%
    initial_split(prop = 0.7, strata = incidence)

  data_recipe <- data_split %>%
    training() %>%
    recipe(incidence ~.) %>%
    step_interact(terms = ~ (matches("distance$"))^3) %>%
    #step_corr(all_predictors(), -item1, -item2) %>%
    prep()

  data_train <- juice(data_recipe)

  #
  data_test <- data_recipe %>%
    bake(testing(data_split))

  AutomodelGrid <- function(data_train, distance, tunelength = 5){
    mg <- model_grid() %>%
      share_settings(
        y = data_train[["incidence"]],
        x = data_train %>%
          ungroup %>%
          dplyr::select(contains("distance")),
        metric = "ROC",
        trControl = trainControl(
          method = "repeatedcv",
          repeats = 4,
          number = 4,
          summaryFunction = twoClassSummary,
          classProbs = TRUE,
          savePredictions = TRUE
        )
      )

    mg <- mg %>%
      modelgrid::add_model(model_name = "Ranger",
                           method = "ranger",
                           tuneLength = tunelength,
                           importance = 'impurity')


    mg <- caret::train(mg)
    return(mg)
  }
  mg <- AutomodelGrid(data_train, tunelength = 6)

  #variabe importance

  vi_score <- vip::vi(mg$model_fits$Ranger)

  #roc_auc metric
  roc_score <- mg$model_fits$Ranger %>%
    predict(data_test, "prob") %>%
    dplyr::bind_cols(data_test) %>%
    yardstick::roc_auc(incidence, susceptible)

  #model accuracy
  accuracy_score <- mg$model_fits$Ranger %>%
    predict(data_test) %>%
    table(data_test$incidence, .) %>%
    yardstick::accuracy()

  data_test <- data_recipe %>%
    bake(distance)

  predicted <- mg$model_fits$Ranger %>%
    predict(data_test, "prob") %>%
    dplyr::bind_cols(dplyr::select(distance, species), .) %>%
    dplyr::select(-unknown)

  return(list(model = mg$model_fits$Ranger,
              vip = vi_score,
              roc = roc_score,
              accuracy = accuracy_score,
              prediction = predicted))
}

#for parallel running run
# library(furrr); plan(multiprocess);furrr::future_map insead map
# library(furrr)
# plan(multiprocess)
library(tictoc)
{
  tic()
  list_results <- listBatsData %>%
    purrr::map(function(x){
      #furrr::future_map(.progress = TRUE, function(x){
      automodel_replication(data = x, distance = ecointeraction::batsdistance)
    })
  toc()
}

save(list_results, file = "data-raw/bat_coronavirus_1000_sims.rds")
