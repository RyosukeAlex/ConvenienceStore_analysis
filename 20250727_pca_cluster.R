getwd()

# ①データを"kuobai"という名称のデータフレームに読み込み、概要を把握
koubai <- read.csv("C:/Academic_work/programmingR/koubai_data.csv")
str(koubai)
summary(koubai)
# 性別ごとの人数集計
table(koubai$性別)
# FLGフラグの人数集計
table(koubai$FLG)
# 購買金額の平均を算出
mean(koubai$昨年5月購買金額)
mean(koubai$今年5月購買金額)

### ②
# 年代の棒グラフ
barplot(table(koubai$年代))
# おにぎりMのヒストグラム
hist(koubai$おにぎりM)
# 性別ごとの"おにぎりR"の箱ひげ図のグラフ
boxplot(おにぎりR ~ 性別, 
        data = koubai)

### ③
# t検定（対応のある2群の平均の差の検定）
t.test(koubai$今年5月購買金額, koubai$昨年5月購買金額, paired = TRUE)

### ④ツリー分析
# ツリー分析を実施するパッケージのライブラリ読み込み
library(rpart)
## 前処理を実施する。
koubai2 <- data.frame(koubai)

# 戦闘のID列を削除
koubai2 <- koubai2[,-1]
# "FLG"を因子型に変換する。
koubai2$FLG <- as.factor(koubai2$FLG)
# 分析用データの確認
str(koubai2)

# 分析の開始
result_tree <- rpart(FLG ~ . ,
                     data = koubai2,
                     method = "class",
                     parms = list(split = "gini"),
                     control = rpart.control(minsplit = 20, maxdepth = 30, cp = 0.01))
result_tree

#変数の重要度の表示
result_tree$variable.importance
barplot(result_tree$variable.importance, las=2, cex.names=0.8)
summary(result_tree)

# ツリー分析結果のビジュアル化
library(partykit)
plot(as.party(result_tree), gp = gpar(fontfamily = "sans", fontsize = 8)) 

### ⑤PCAの実施
# PCA用のdfの作成
library(dplyr)
koubai_pca <- koubai2 %>%
  select(キャラクター商品M, 飲料M, パンM, おにぎりM, 今年5月購買金額, 昨年5月購買金額)
str(koubai_pca)

#主成分分析実施
result_pca <- prcomp(koubai_pca, scale=T)
summary(result_pca)

#主成分負荷量のプロット
barplot(sort(result_pca$rotation[,1], decreasing = T ), las=2, main="PC1", horiz=T)
biplot(result_pca, choices=c(1,2))

# 6つの変数を使用した場合だと、"購買金額"と"商品"が並列に登場するため、解釈が難しいと判断。
# そのため購買金額を外した4変数でPCAを実施しました。
koubai_pca4 <- koubai_pca %>%
  select(キャラクター商品M, 飲料M, パンM, おにぎりM)
str(koubai_pca4)
result_pca4 <- prcomp(koubai_pca4, scale=T)
summary(result_pca4)
barplot(sort(result_pca4$rotation[,1], decreasing = T ), las=2, main="PC1", horiz=T)


### "M"のつくカラムを使用してクラスタ＾分析を実施
# dfを作成する
koubai_cluster <- koubai2 %>%
 select(ends_with("M"))
str(koubai_cluster)

#データの標準化
koubai_cluster_scale <- scale(koubai_cluster)
#非階層クラスター分析の実施
set.seed(222)
km_cluster <- kmeans(koubai_cluster_scale,3)
km_cluster
result_cluster <- km_cluster

#結果のグラフ化
plot(koubai_cluster_scale, col=km_cluster$cluster) #クラスター別に色付けて散布図作成
points(km_cluster$centers, pch=8) #クラスターの中央にしるしをつける

plot(koubai_cluster_scale[, c("キャラクター商品M")], 
     koubai_cluster_scale[, c("生活用品M")], 
     col = km_cluster$cluster, 
     xlab = "キャラクター商品M", 
     ylab = "生活用品M", 
     main = "クラスタリング結果")

points(km_cluster$centers[, c("キャラクター商品M")], 
       km_cluster$centers[, c("生活用品M")], 
       pch = 8, cex = 2)

cluster_means <- aggregate(koubai_cluster, by = list(Cluster = km_cluster$cluster), mean)

library(reshape2)
library(ggplot2)

cluster_melt <- melt(cluster_means, id.vars = "Cluster")
ggplot(cluster_melt, aes(x = variable, y = value, fill = factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "商品カテゴリ", y = "平均購買額", fill = "クラスター") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
