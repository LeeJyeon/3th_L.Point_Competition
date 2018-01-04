library(recommenderlab)

# ----------------------------------------------------
# 아이템 기반 협업 필터링 (12월 제외 최근 3개월) (2015-09-01 ~ 2015-11-30)
# ----------------------------------------------------
model_ibcf_valid <- Recommender(real_bin.Freq_valid, method = "IBCF",
                                param = list(method = "Jaccard"))
prediction_ibcf5_valid <- predict(model_ibcf_valid, real_bin.Freq_valid, n = 5)
head(as(prediction_ibcf5_valid, "list"), 10)
prediction_ibcf3_valid <- bestN(prediction_ibcf5_valid, n = 3)
head(as(prediction_ibcf3_valid, "list"), 5)

model_ibcf_rmse_valid <- Recommender(getData(e, "train"), method = "IBCF", 
                                     param=list(method="Jaccard"))
prediction_ibcf_rmse_valid <- predict(model_ibcf_rsme_valid, getData(e, "known"), type="ratings")

rmse_ibcf_valid <- calcPredictionAccuracy(prediction_ibcf_rsme_valid, getData(e, "unknown"))[1]
rmse_ibcf_valid

# ----------------------------------------------------
# 아이템 기반 협업 필터링 (최근 3개월)
# ----------------------------------------------------
model_ibcf <- Recommender(real_bin.Freq, method = "IBCF",
                          param = list(method = "Jaccard"))
prediction_ibcf5 <- predict(model_ibcf, real_bin.Freq, n = 5)
head(as(prediction_ibcf5, "list"), 10)
prediction_ibcf3 <- bestN(prediction_ibcf5, n = 3)
head(as(prediction_ibcf3, "list"), 5)
model_ibcf_rmse <- Recommender(getData(e, "train"), method = "IBCF", 
                               param=list(method="Jaccard", k=350))
prediction_ibcf_rmse <- predict(model_ibcf_rsme, getData(e, "known"), type="ratings")

rmse_ibcf <- calcPredictionAccuracy(prediction_ibcf_rsme, getData(e, "unknown"))[1]
rmse_ibcf