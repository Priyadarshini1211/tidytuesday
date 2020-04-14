
#Predict horror movie ratings


library(tidyverse)
theme_set(theme_light())


horror_movies_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")


horror_movies_raw %>%  arrange(desc(review_rating)) %>% 
                extract(title,"year", "\\((\\d\\d\\d\\d)\\)$",remove = FALSE,convert = TRUE) %>% 
                filter(year >= 2005) %>% 
                ggplot(aes(year))+
                geom_histogram()

head(horror_movies_raw$plot)


horror_movies <- horror_movies_raw %>%
  arrange(desc(review_rating)) %>%
  extract(title, "year", "\\((\\d\\d\\d\\d)\\)$", remove = FALSE, convert = TRUE) %>%
  mutate(budget = parse_number(budget)) %>%
  separate(plot, c("director", "cast_sentence", "plot"), extra = "merge", sep = "\\. ", fill = "right") %>%
  distinct(title, .keep_all = TRUE)

view(horror_movies)

horror_movies %>%
  ggplot(aes(budget)) +
  geom_histogram() +
  scale_x_log10(labels = scales::dollar)


  horror_movies %>% 
      ggplot(aes(budget,review_rating)) +
    geom_point()+
    scale_x_log10()+
    geom_smooth(method = "lm")

  #No relation between budget and rating.
  #Is there andy relation bw movie rating and review
  
  
  
  horror_movies %>% 
    mutate(movie_rating=fct_lump(movie_rating,5),
    movie_rating = fct_reorder(movie_rating,review_rating,na.rm=TRUE)) %>% 
  ggplot(aes(movie_rating,review_rating))+
    geom_boxplot()+coord_flip()
  
  
  horror_movies %>% 
    filter(!is.na(movie_rating)) %>% 
    mutate(movie_rating=fct_lump(movie_rating,5)) %>% 
    lm(review_rating~movie_rating,data = .) %>% 
    anova()

  
  horror_movies %>% 
    separate_rows(genres,sep = "\\|") %>% 
    mutate(genres= fct_lump(genres,5)) %>% 
    ggplot(aes(genres,review_rating))+
    geom_boxplot()
  
  
 
  
##############----Text Mining---------################
  
  library(tidytext)
  
  horror_movies_unnested <- horror_movies %>%
    unnest_tokens(word, plot) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!is.na(word))
  
  horror_movies_unnested %>%
    filter(!is.na(review_rating)) %>%
    group_by(word) %>%
    summarize(movies = n(),
              avg_rating = mean(review_rating)) %>%
    arrange(desc(movies)) %>%
    filter(movies >= 100) %>%
    mutate(word = fct_reorder(word, avg_rating)) %>%
    ggplot(aes(avg_rating, word)) +
    geom_point()
             
#############################################################################################################
  
  ###lASSO REGRESSION FOR PREDITING REVIEW BY WORDS

  
  library(glmnet)
  library(Matrix)
  
  movie_word_matrix <- horror_movies_unnested %>%
    filter(!is.na(review_rating)) %>%
    add_count(word) %>%
    filter(n >= 20) %>%
    count(title, word) %>%
    cast_sparse(title, word, n)
  
  
  rating <-
  horror_movies$review_rating[match(rownames(movie_word_matrix), horror_movies$title)]
  
  lasso_model <- cv.glmnet(movie_word_matrix, rating)

  
  
  library(broom)
  
  tidy(lasso_model$glmnet.fit) %>%
    filter(term %in% c("quickly", "seek", "army", "teacher", "unexpected", "friends", "evil")) %>%
    ggplot(aes(lambda, estimate, color = term)) +
    geom_line() +
    scale_x_log10() +
    geom_vline(xintercept = lasso_model$lambda.min) +
    geom_hline(yintercept = 0, lty = 2)
  
  
  
  plot(lasso_model)
  tidy(lasso_model$glmnet.fit) %>%
    filter(lambda == lasso_model$lambda.min,
           term != "(Intercept)") %>%
    mutate(term = fct_reorder(term, estimate)) %>%
    ggplot(aes(term, estimate)) +
    geom_col() +
    coord_flip()
  
  
  #Throwing everything into the linear model
  
  features <- horror_movies %>%
    filter(!is.na(review_rating)) %>%
    select(title, genres, director, cast, movie_rating, language, release_country) %>%
    mutate(director = str_remove(director, "Directed by ")) %>%
    gather(type, value, -title) %>%
    filter(!is.na(value)) %>%
    separate_rows(value, sep = "\\| ?") %>%
    unite(feature, type, value, sep = ": ") %>%
    mutate(n = 1)
  
  movie_feature_matrix <- horror_movies_unnested %>%
    filter(!is.na(review_rating)) %>%
    count(title, feature = paste0("word: ", word)) %>%
    bind_rows(features) %>%
    add_count(feature) %>%
    filter(n >= 10) %>%
    cast_sparse(title, feature)
  
  
  rating <- horror_movies$review_rating[match(rownames(movie_feature_matrix), horror_movies$title)]
  feature_lasso_model <- cv.glmnet(movie_feature_matrix, rating)
  
  
  
  
  
  plot(feature_lasso_model)
  tidy(feature_lasso_model$glmnet.fit) %>%
    filter(lambda == feature_lasso_model$lambda.1se,
           term != "(Intercept)") %>%
    mutate(term = fct_reorder(term, estimate)) %>%
    ggplot(aes(term, estimate)) +
    geom_col() +
    coord_flip() +
    labs(x = "",
         y = "Coefficient for predicting movie rating",
         title = "What affects a horror movie rating?",
         subtitle = "Based on a lasso regression to predict IMDb ratings of ~3000 movies")
  
  
  
  
  