# 검증 최근 1개월 추출 (2015-12)
data_valid_12 <- data.frame(subset(purchased, 구매일자 >= '2015-12-01'
                                   & 구매일자 < '2016-01-01'))
afd <- subset(data_valid_12, data_valid_12$고객번호 == 1848 & data_valid_12$소분류코드 %in% c("B050106", "B140101", "B140102"))
afd

# 검증 최근 3개월 추출 (2015-09-01 ~ 2015-11-30)
data_valid <- data.frame(subset(purchased, 구매일자 >= '2015-09-01'
                                & 구매일자 < '2015-12-01'))
data_valid <- data_valid[, c(6, 5)]
data_valid.table <- table(data_valid)
Freqs_valid <- as.data.frame(data_valid.table, 
                             stringsAsFactors=default.stringsAsFactors())
head(Freqs_valid)
uniq_freq_cus <- unique(Freqs_valid$고객번호)
uniq_freq_pro <- unique(Freqs_valid$소분류코드)
level_pro <- levels(Freqs_valid$소분류코드)

# 변수 유형 변경 (sparse 매트릭스 만들려고.)
Freqs_valid$고객번호 <- as.numeric(factor(Freqs_valid$고객번호))
Freqs_valid$소분류코드 <- as.numeric(factor(Freqs_valid$소분류코드))
Freqs_valid$Freq <- as.numeric(Freqs_valid$Freq)

# sparse 매트릭스 (생성 속도 빨라진다.)
sparse_Freq_valid <- sparseMatrix(i = Freqs_valid$고객번호, j = Freqs_valid$소분류코드, x = Freqs_valid$Freq, 
                                  dims = c(length(unique(Freqs_valid$고객번호)), length(unique(Freqs_valid$소분류코드))),  
                                  dimnames = list(unique(Freqs_valid$고객번호), level_pro))
real_Frequency_valid <- as(sparse_Freq_valid, "realRatingMatrix")
real_bin.Freq_valid <- binarize(real_Frequency_valid, minRating = 1)