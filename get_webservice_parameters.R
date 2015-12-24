get_webservice_parameters <- function(my_model, wkID="foo", authToken="bar"){
  library(dplyr)
  
  get_dataClasses <- function(fit) UseMethod("get_dataClasses")
  get_dataClasses.default <- function(fit) attr(fit$terms, "dataClasses")  # nnet, rpart, randomForest.formula
  get_dataClasses.gbm     <- function(fit) attr(fit$Terms, "dataClasses")
  get_dataClasses.lm      <- function(fit) attr(attr(fit$model, "terms"), "dataClasses")
  
  translate_types <- function(t){
    translation <- c(factor="string", character="string", integer="int", numeric="float")
    tt <- translation[t]
    names(tt) <- names(t)
    tt
  }
  
  data_types <- my_model %>% get_dataClasses %>% translate_types %>% as.list
  
  modelName <- as.character(substitute(my_model))
  functionName <- paste0(modelName, "_predict")
  serviceName <- paste0(functionName, "webservice")
  inputSchema <- data_types[-1]
  outputSchema <- data_types[1]
  
  prediction_function_src <- function(){
    prediction_function_template <- paste("%s <- function(%s){",
                                          "  fit <- %s;",
                                          "  predictor <- switch(class(fit)[1],",
                                          "    rpart = function(dataframe) predict(fit, dataframe, type='class'),",
                                          "    function(dataframe) predict(fit, dataframe)",
                                          "  );",
                                          "  res <- predictor(data.frame(%s, stringsAsFactors=F));",
                                          "  if (class(res)=='factor') levels(res)[res] else res}",
                                          collapse="\n")
    
    var_text <- paste(names(inputSchema), collapse=", ")
    
    sprintf(prediction_function_template, 
            functionName, var_text, modelName, var_text)

  }
  
  list(param=list(functionName=functionName, serviceName=serviceName, 
                    inputSchema=inputSchema, outputSchema=outputSchema,
                    wkID=wkID, authToken=authToken), 
      src=prediction_function_src())
  
}


# You must define workspace_id and auth_token for your Azure account.

# faithful_lm_01 <- lm(eruptions ~ waiting, faithful)
# wsp <- get_webservice_parameters(faithful_lm_01, workspace_id, auth_token)
# eval(parse(text=wsp$src))
# ws_info <- do.call("publishWebService", wsp$param)
