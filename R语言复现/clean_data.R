library(tidyverse)

# 读取数据
data_raw <- read.csv("FINAL fluency.csv",
                     stringsAsFactors = FALSE)

# 第一列是ID
id_col <- 1

# 动物response列
response_cols <- 2:ncol(data_raw)

# 小写化
data_raw[response_cols] <- lapply(
  data_raw[response_cols],
  tolower
)

# 拼写修正
spell_map <- c(
  "cats" = "cat",
  "dogs" = "dog",
  "eliphant" = "elephant"
)

# 替换函数
clean_word <- function(x){
  
  if(is.na(x)) return(NA)
  
  x <- trimws(x)
  
  # 拼写替换
  if(x %in% names(spell_map)){
    x <- spell_map[x]
  }
  
  # 手动处理合并词
  if(x == "catefrog"){
    return(c("cat","frog"))
  }
  
  if(x == "birdfish"){
    return(c("bird","fish"))
  }
  
  return(x)
}

# 创建list保存每个参与者动物
participant_animals <- list()

for(i in 1:nrow(data_raw)){
  
  responses <- unlist(data_raw[i, response_cols])
  
  responses <- responses[!is.na(responses)]
  
  cleaned <- c()
  
  for(word in responses){
    
    tmp <- clean_word(word)
    
    cleaned <- c(cleaned, tmp)
  }
  
  # 删除空值
  cleaned <- cleaned[cleaned != ""]
  
  # 删除重复
  cleaned <- unique(cleaned)
  
  participant_animals[[i]] <- cleaned
}

# 所有动物
all_animals <- unique(unlist(participant_animals))

# 初始化矩阵
result <- data.frame(ID = data_raw[[id_col]])

for(animal in all_animals){
  result[[animal]] <- 0
}

# 正确填充
for(i in 1:length(participant_animals)){
  
  animals <- participant_animals[[i]]
  
  result[i, animals] <- 1
}

# 删除只出现一次动物
animal_counts <- colSums(result[,-1])

keep_animals <- names(animal_counts[animal_counts > 1])

result <- result %>%
  select(ID, all_of(keep_animals))

# 删除全0参与者
result <- result[rowSums(result[,-1]) > 0, ]

#手动矫正
#删除99值
result <- result %>%
  select(-`99`)
#删除错误列
result$ca <- NULL
#合并pig
idx1 <- which(result$`ginea pig` == 1)
idx2 <- which(result$`guinnea pig` == 1)

result[idx1, "guinea pig"] <- 1
result[idx2, "guinea pig"] <- 1

# 删除错误列
result$`ginea pig` <- NULL
result$`guinnea pig` <- NULL
#合并bear
idx <- which(result$`polar bears` == 1)

result[idx, "polar bear"] <- 1

result$`polar bears` <- NULL


# 保存
write.csv(result,
          "cleaned_animal_fluency_FIXED.csv",
          row.names = FALSE)