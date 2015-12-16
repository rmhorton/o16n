library(ggplot2)

c_training <- sim_classification_data(1000)
c_test <- sim_classification_data(1000)

cmodel1 <- glm(grp ~ x + y, c_training, family="binomial")
c_test$pred1 <- predict(cmodel1, newdata=c_test, type="response")
ggplot(c_test, aes(x=x, y=y, col=pred1)) + geom_point()

cmodel2 <- glm(grp ~ I(x^2 + y^2), c_training, family="binomial")
c_test$pred2 <- predict(cmodel2, newdata=c_test, type="response")
ggplot(c_test, aes(x=x, y=y, col=pred2)) + geom_point()


c_test$edge <- ifelse(c_test$pred2 > 0.1 & c_test$pred2 < 0.9, 1, 2)
ggplot(c_test, aes(x=x, y=y, col=edge)) + geom_point()


rf_model <- randomForest(grp ~ x + y, data=c_training, ntree=200)
c_test$pred_rf <- predict(rf_model, newdata=c_test)
ggplot(c_test, aes(x=x, y=y, col=pred_rf)) + geom_point()

wsp <- get_webservice_parameters(rf_model, workspace_id, auth_token)
eval(parse(text=wsp$src))

rf_model_predict(c_test$x, c_test$y)

ws_info <- do.call("publishWebService", wsp[names(formals(publishWebService))])
# saveRDS(ws_info, file="rf_model_ws_info.Rmd")

apiKey <- "I0cbXxim9vv6eeMroh+IV+Kv9SRfOI62vBgBpnhedfX/mgUmPvpjTvaQqvHOSxjFVlaLpsymx9jXRmIWRFzjRQ=="
requestUrl <- "https://ussouthcentral.services.azureml.net/workspaces/7e41241404044674a6909d6ac9cdcd2b/services/cc11b4525fac46b086379b66f95f3aab/execute?api-version=2.0&details=true&format=swagger"
consumeDataframe(apiKey, requestUrl, c_test[1:5, c("x", "y")])
