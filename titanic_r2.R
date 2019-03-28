## *ライブラリの読み込み----

# tidyverse:
# dplyr, tidyr, purrr などの
# 解析に必要なパッケージが全てまとまったライブラリ
library(tidyverse)

# magrittr:
# パイプ演算子(%>%)の亜種(%<>%, %T>%, %$%)を使用するためのライブラリ
library(magrittr)

# ggplot2:
# グラフ作成用ライブラリ
library(ggplot2)

# infotheo:
# 連続値を離散値に変換するために使用(discretize)
library(infotheo)

# Hmisc:
# リストを見やすく表示するlist.tree()のために使用
library(Hmisc)

# DataExplorer:
# データの概要をレポート形式でまとめてくれるライブラリ
library(DataExplorer)

# rpivotTable:
# ピボットテーブルを作成するライブラリ
library(rpivotTable)

# esquisse:
# ggplotのグラフをGUIで作成するためのライブラリ
library(esquisse)

# mlr:
# 機械学習関連のパッケージが全てまとまったライブラリ
library(mlr)

# titanic:
# 勉強会で使用するデータを読み込むために使用
library(titanic)

## *探索的データ分析(EDA) ----

# データの確認----
titanic_train %>% head(10)
titanic_train %>% View

# クラスの確認----
titanic_train %>% map(class) %>% list.tree()

# データのサマリーの確認----
# ・titanic_trainではcharacter型のNAが""となっているため、""をNAに変換する
#   (NAの数をカウントするため)
# ・character型の列とその他一部の列について、factor型への変換を行う
#   (要素数をカウントするため。)

titanic::titanic_train %>% 
  mutate_if(
    .predicate = is.character,
    .funs = ~ ifelse(.=="",NA,.) %>% factor
  ) %>%
  mutate_at(
    .vars = c('Survived','Pclass','Sex'),
    .funs = factor
  ) %>%
  summary

# 死亡率を計算----
# (常に"死亡した"と予測しても正解率は61.6%程度となる)
Dead_rate = 549/(342+549)
Dead_rate %>% print

# 探索用データの作成----
# 1. character型の空白行をNAへ変換
# 2. 相関を確認したいcharacter型のみfactor型へ変換(create_report関数への入力用)
# 3. dummy変数であるSurvived/Pclassもfactor型へ変換
# 4. PassengerId は解析に使用しないため削除

data_eda <-
  titanic::titanic_train %>% 
  mutate_if(
    .predicate = is.character,
    .funs = ~ ifelse(.=="",NA,.)
  ) %>%
  mutate_at(
    .vars = c('Survived','Pclass','Sex','Embarked'),
    .funs = factor
  ) %>% 
  select(-PassengerId)

# 各列のNAの割合を確認----
data_eda %>% plot_missing()

# Completeing:欠損値補完(後述) ----
# 欠損値の多いCabinはデータから削除し
# Age,EmbarkはMedian,Modeで補完
data_eda %<>% 
  select(-Cabin) %>% 
  mlr::impute(
    cols=list(
      Age = imputeMedian(),
      Embarked = imputeMode()
    )
  ) %>% .$data

# データの概略を知るために便利なパッケージ3種類----

# レポートの作成
data_eda %>% DataExplorer::create_report()

# ピボットテーブルの確認
data_eda %>% rpivotTable::rpivotTable()

# 単純なグラフの確認
data_eda %>% esquisse::esquisser()

# 性別と生存率について----
g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Sex, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position = "fill"
  )
g %>% print

# 年齢と生存率について----

# 年齢別_生存者/死亡者合計の密度曲線
g1 <-
  data_eda %$% 
  ggplot(
    data=.,
    aes(x=Age,fill=Survived)
  ) +
  geom_density(alpha=.3)
g1 %>% print


# 年齢をbiningして生存率を確認する
data_eda %<>%
  mutate(
    AgeBin = Age %>% discretize(disc="equalwidth",nbins=6) %>% .$X %>% factor
  )

g2 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x=AgeBin,fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position ="fill"
  )
g2 %>% print

## Pclassと生存率について----
g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Pclass, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position = "fill"
  )
g %>% print

## Fareと生存率について----

# Fare別_生存者/死亡者合計の密度曲線
g1 <-
  data_eda %$% 
  ggplot(
    data=.,
    aes(x=Fare,fill=Survived)
  ) +
  geom_density(alpha=.3)
g1 %>% print

# Fareをbiningして生存率を確認する
data_eda %<>%
  mutate(
    FareBin = Fare %>% discretize(disc="equalfreq",nbins=5) %>% .$X %>% factor
  )

g2 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x=FareBin,fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position ="fill"
  )
g2 %>% print

## Embarkedと生存率について----
g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Embarked, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position = "fill"
  )
g %>% print

# Pclass別にEmbarked別の生存率を確認
# Pclassが同じでも、出港地がCであれば生存しやすく、
# Sであれば死亡しやすい
g1 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Embarked, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position = "dodge"
  ) +
  facet_grid(. ~ Pclass)
g1 %>% print

g2 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Embarked, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position = "fill"
  ) +
  facet_grid(. ~ Pclass)
g2 %>% print

## SibSp/Parchと生存率について----
g1 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = SibSp, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  )
g1 %>% print

g2 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Parch, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  )
g2 %>% print

# SibSp/Parchの相関を確認
g <-
  data_eda %$%
  ggplot(
    data = .,
    aes(x=as.factor(Parch),y=SibSp)
  ) +
  geom_boxplot()
g %>% print

# FamilySizeと生存確率の確認
data_eda %<>% mutate(FamilySize = SibSp+Parch+1)

g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x=FamilySize,fill=Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  )
g %>% print

# IsAloneフラグ(乗客に家族がいるか)で生存率が変わるか確認
# (独り身は死亡しやすい)
data_eda %<>% mutate(IsAlone = ifelse(SibSp+Parch==0,"Alone","NotAlone") %>% factor)

g1 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x=IsAlone, fill=Survived)
  ) +
  geom_bar(
    stat="count"
    #position="fill"
  )
g1 %>% print   

g2 <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x=IsAlone, fill=Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  )
g2 %>% print

# FamilySize, IsAloneとSurvivedの相関を確認----
# IsAloneはSurvivedと相関が強いため、新たな特徴量として後ほど使用
data_eda %>%
  select(
    Survived,
    SibSp,
    Parch,
    FamilySize,
    IsAlone
  ) %>%
  plot_correlation()

# Nameに含まれるTitleと生存率について----

# NameからTitleを抽出
data_eda %<>% 
  mutate(
    Title = Name %>% str_extract("\\w+\\.") %>% str_remove("\\.")
  )

# Titleの中身を確認
data_title <-
  data_eda %>% 
  group_by(Title) %>% 
  dplyr::summarize(count=n()) %>% 
  arrange(desc(count))

data_title %>% print

# Titleの数が基準値(stat_min)よりも小さければ「Misc」としてまとめる
stat_min <-5
title_names <- data_title$Title[data_title$count < stat_min]
data_eda %<>% 
  mutate(Title=ifelse(is.element(Title,title_names),"Misc",Title) %>% factor)

# Titleごとに生存確率を確認
g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Title, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  )
g %>% print

# 男女別にTitleの効果を確認
# Dr,Master,Miscは生存しやすく、Revは死亡しやすい
g <-
  data_eda %$% 
  ggplot(
    data = .,
    aes(x = Title, fill = Survived)
  ) +
  geom_bar(
    stat="count",
    position="fill"
  ) +
  facet_grid(Sex ~.)
g %>% print

# 今一度データの中身を確認----
data_eda %>% head(10)
data_eda %>% map(class) %>% list.tree(maxcomp=15)

# Converting(データのフォーマット変更)----
# 1.不必要な説明変数(Name,Ticket,FamilySize,Age,Fare)を削除
# 2.mlr::createDummyFeatures()でfactor型をダミー変数に変換
data_ml <-
  data_eda %>%
  select(-Name, -Ticket, -FamilySize, -Age, -Fare) %>% 
  createDummyFeatures(target="Survived")

# *データモデルの作成----

# タスクの定義----
titanic.task = makeClassifTask(id="titanic",data=data_ml,target="Survived")
titanic.task %>% print

# 使用できるアルゴリズムの一覧を出力----
# properties = "prob" : 確率を出力できるアルゴリズムの一覧
listLearners(titanic.task, properties ="prob") %>% head

# 使用するアルゴリズムをリスト化---
titanic.lrns =
  list(
     makeLearner("classif.kknn", id="kknn")
    ,makeLearner("classif.naiveBayes", id="naiveBayes")
    ,makeLearner("classif.nnet", id="nnet")
    ,makeLearner("classif.gausspr", id="gausspr")
    ,makeLearner("classif.svm", id="svm")
    ,makeLearner("classif.rpart", id="rpart")
    ,makeLearner("classif.randomForest", id="randomForest")
    ,makeLearner("classif.xgboost", id="xgboost")
  )

# チューニング前の各アルゴリズムの性能を評価----
titanic.bmr = benchmark(
  learners    = titanic.lrns,
  tasks       = titanic.task,
  resamplings = cv5,
  measures    = list(acc,timetrain)
)

# ベンチマークテストの結果をグラフで確認----
g1<-
  plotBMRBoxplots(
    titanic.bmr,
    measure = acc,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))

g2<-
  plotBMRBoxplots(
    titanic.bmr,
    measure = timetrain,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))
print(g1)
print(g2)

# *データモデルのチューニング ----

# チューニングできるハイパーパラメータを確認----
getParamSet("classif.naiveBayes")
?naiveBayes

# グリッドサーチ用のハイパーパラメータを設定----
grid_n_estimator = c(10, 50, 100, 300)
grid_ratio       = c(.1, .25, .5, .75, 1.0)
grid_learn       = c(.01, .03, .05, .1, .25)
grid_max_depth   = c(2 ,4, 6, 8, 10)

titanic.ps = list(
  
  # kknn
  makeParamSet(
    makeDiscreteParam("k"        ,values = c(1,2,3,4,5,6,7)),
    makeDiscreteParam("distance" ,values = c(1,2)),
    makeDiscreteParam("kernel"   ,values = c("rectangular","inv","gaussian","optimal"))
  ),
  
  # naiveBayes
  makeParamSet(
    makeDiscreteParam("laplace" ,values = grid_ratio)
  ),
  
  # nnet
  makeParamSet(
    makeDiscreteParam("size"  ,values = c(1,3,5,7)),
    makeDiscreteParam("maxit" ,values = grid_n_estimator),
    makeDiscreteParam("decay" ,values = grid_learn)
  ),
  
  # gausspr
  makeParamSet(
    makeDiscreteParam("kernel" ,values = c("rbfdot","polydot","vanilladot"))
  ),
  
  # svm
  makeParamSet(
    makeDiscreteParam("cost"   ,values = c(1,2,3,4,5)),
    makeDiscreteParam("kernel" ,values = c("linear","polynomial","radial","sigmoid")),
    makeDiscreteParam("gamma"  ,values = grid_ratio)
  ),
  
  # rpart
  makeParamSet(
    makeDiscreteParam("cp" ,values = c(.001, .005, .01, .025, .05))
  ),  
  
  # randomForest
  makeParamSet(
    makeDiscreteParam("ntree" ,values = grid_n_estimator),
    makeDiscreteParam("mtry"  ,values = c(1, 2, 3, 4, 5))
  ),  
  
  # xgboost
  makeParamSet(
    makeDiscreteParam("eta"       ,values = grid_learn),
    makeDiscreteParam("max_depth" ,values = grid_max_depth),
    makeDiscreteParam("nrounds"   ,values = grid_n_estimator)
  )
)

# グリッドサーチを実行----
titanic.res =
  map2(
    .x = titanic.lrns,
    .y = titanic.ps,
    .f =
      ~ tuneParams(
        learner  = .x,
        task     = titanic.task,
        resampling = cv5,
        measures = acc,
        par.set  = .y,
        control  = makeTuneControlGrid()
      )
  )

# チューニングされたハイパーパラメータを設定----
titanic.lrn.tuned <-
  map2(
    .x = titanic.lrns,
    .y = titanic.res,
    .f =
      ~ setHyperPars(
        learner  = .x,
        par.vals = .y$x
      )
  )

# チューニング後の性能を確認----
titanic.bmr.tuned <-
  benchmark(
    learners    = titanic.lrn.tuned,
    tasks       = titanic.task,
    resamplings = cv5,
    measures    = list(acc,timetrain)
  )

# ベンチマークテストの結果をプロット----
g1<-
  plotBMRBoxplots(
    titanic.bmr.tuned,
    measure = acc,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))

g2<-
  plotBMRBoxplots(
    titanic.bmr.tuned,
    measure = timetrain,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))
print(g1)
print(g2)

#####






















