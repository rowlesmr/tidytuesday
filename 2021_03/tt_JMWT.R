library(tidytuesdayR)
library(tidyverse)
library(gghighlight)
library(ggrepel)
library(glue)
library(scales)
library(grid)

theme_set(theme_bw())

tuesdata <- tidytuesdayR::tt_load(2021, week = 3)

artwork <- tuesdata$artwork


#looking at the artists
artwork  %>% 
  count(artist, sort=T)

artwork  %>% 
  count(artistRole, sort=T)

artwork  %>% 
  count(medium, sort=T)



#get rid of Turner: too prolific, could be skewing the data
artwork <- artwork %>% 
  filter(artist !="Turner, Joseph Mallord William")

glimpse(artwork)


artwork  %>% 
  count(medium, sort=T)

artwork %>% 
  ggplot(aes(year)) + 
  geom_histogram()



#we want to look at predicting when an artwork was created, based on its' description
tate_df <- artwork %>% 
  filter(year >= 1750) %>% 
  select(year,medium, artist) %>% 
  na.omit()


library(tidytext)

tate_df %>% 
  unnest_tokens(word, medium) %>% 
  count(word, sort=T)

library(tidymodels)


#splitting the data into training and testing sets based on year
set.seed(123)
art_split <- initial_split(tate_df, strata = year)
art_train <- training(art_split)
art_test <- testing(art_split)

#resample to allow for model tuning and evaluation
set.seed(234)
art_folds <- vfold_cv(art_train, strata=year)
art_folds



library(textrecipes)


sparse_bp <- hardhat::default_recipe_blueprint(composition = "dgCMatrix")


#we want to explain year by medium
art_rec <- recipe(year ~ medium, data = art_train) %>% 
  step_tokenize(medium) %>% #split the medium column into individual words
  step_stopwords(medium) %>%  #remove "stop words". These are commmon words like "on", "in"...
  step_tokenfilter(medium, max_tokens = 500) %>% # keep only the top 500 tokens
  step_tfidf(medium)# %>% #weight by term frequency
  #step_normalize(all_predictors()) #normalise the counts to be on the same scale


#good model for text is lasso
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

art_wf <- workflow() %>% 
  add_recipe(art_rec, blueprint = sparse_bp) %>% 
  add_model(lasso_spec)
art_wf




#do the model
doParallel::registerDoParallel()

lambda_grid <- grid_regular(penalty(range = c(-3,0)), levels = 20)
lambda_grid

lasso_rs <- #this is the actual model
tune_grid(
  art_wf,
  resamples = art_folds,
  grid = lambda_grid
)


#look at model results
autoplot(lasso_rs)

show_best(lasso_rs, "rsq")


best_rmse <- select_best(lasso_rs, "rmse")


final_lasso <- finalize_workflow(art_wf, best_rmse)
final_lasso


art_final <- 
last_fit(final_lasso, art_split)
art_final

#the rsq is a little dodgy...
collect_metrics(art_final)



#what about variable importance?
library(vip)

art_vip <- 
pull_workflow_fit(art_final$.workflow[[1]]) %>% 
  vi()

art_vip %>% 
  arrange(-Importance)


art_vip %>% 
  group_by(Sign) %>% 
  slice_max(abs(Importance), n = 20) %>% 
  ungroup() %>% 
  mutate(
    Variable = str_to_sentence(str_remove(Variable, "tfidf_medium_")),
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance),
    Sign = if_else(Sign == "POS", "More in later art", "More in earlier art")
    
  ) %>% 
  ggplot(aes(x = Variable, y = Importance, fill = Sign))+
  geom_col(show.legend = F)+
  facet_wrap(~ Sign, scales = "free_y") +
  coord_flip()+
  labs(x="",
       y= "Importance",
       title="Medium of art in The Tate",
       subtitle = "What words are in the description of the medium, given that you aren't J.M.W. Turner?")


#test the prediction
collect_predictions(art_final) %>% 
  ggplot(aes(x= year, y = .pred))+
  geom_abline(lty = 2, colour = "red", size = 1)+
  geom_point(alpha = 0.4) + 
  coord_fixed()+
  labs(x = "Actual year",
       y = "Predicted year",
       title = "Prediction of creation year based on medium description",
       subtitle = "Lasso regression, assuming you aren't J.M.W. Turner.")


misclassified <- 
collect_predictions(art_final) %>% 
  bind_cols(art_test %>% select(medium)) %>% 
  filter(abs(year - .pred) > 100)

misclassified %>% unnest_tokens(word,medium) %>% 
  count(word, sort =T)

preditctions_with_data <- 
  collect_predictions(art_final) %>% 
  bind_cols(art_test %>% select(medium))%>% 
  select(pred = .pred, year, medium)


preditctions_with_data %>% 
  filter(pred > 1903 & pred < 1905) %>% 
  count(medium, sort = T)



#looking at residuals

augment(art_final) %>% 
  ggplot(aes(.pred, .resid))+
  geom_hline(yintercept = 0,lty = 2, colour = "red", size = 2)+
  geom_point(alpha = 0.4) +
  geom_smooth()


augment(art_final) %>% 
  arrange(desc(.resid))













