rfe_ctrl <-rfeControl(functions = gamFuncs,
           method = "repeatedcv" ,
           verbose = T,
           number = 5)

train__ <- create_modeldata("home_cover") %>% na.omit() %>% mutate(home_cover = as.factor(home_cover))
# train_ <- model.matrix(home_cover ~ . ^2 , train__ %>% select(-game_id)) %>% as.data.frame()
train.x <- train__ %>% select(-game_id , -home_cover)
train.y <- train__[["home_cover"]]
rfe_run <- rfe(x = train.x , y = train.y , rfeControl = rfe_ctrl)


rfe_run
