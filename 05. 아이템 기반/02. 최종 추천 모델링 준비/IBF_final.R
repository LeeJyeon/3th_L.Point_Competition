# 최근 3개월 추출
data <- data.frame(subset(purchased, 구매일자 >= '2015-10-01'
                          & 구매일자 < '2016-01-01'))
unique(data$고객번호)
unique(data$소분류코드)
data <- data[, c(6, 5)]
data.table <- table(data)
Freqs <- as.data.frame(data.table, stringsAsFactors = default.stringsAsFactors())
unique(Freqs$고객번호)
uniq_Freq_prod <- unique(Freqs$소분류코드)
level_prod <- levels(Freqs$소분류코드)

# 변수 유형 변경 (sparse 매트릭스 만들려고.)
Freqs$고객번호 <- as.numeric(factor(Freqs$고객번호))
Freqs$소분류코드 <- as.numeric(factor(Freqs$소분류코드))
Freqs$Freq <- as.numeric(Freqs$Freq)

# sparse 매트릭스 (생성 속도 빨라진다.)
sparse_Freq <- sparseMatrix(i = Freqs$고객번호, j = Freqs$소분류코드, x = Freqs$Freq, 
                            dims = c(length(unique(Freqs$고객번호)), length(unique(Freqs$소분류코드))),  
                            dimnames = list(unique(Freqs$고객번호), level_prod))
sparse_Freq[1:10, 1:10]

real_Frequency <- as(sparse_Freq, "realRatingMatrix")
real_bin.Freq <- binarize(real_Frequency, minRating = 1)
as(real_Frequency, "matrix")[1:10, 1:10]
as(real_bin.Freq, "matrix")[1:10, 1:10]