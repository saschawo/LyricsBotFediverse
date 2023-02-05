rm(list=ls())

library(rhymer)
library(dplyr)
library(rtoot)

options("rtoot_token" = "radiocave_token.Rds")
lyr <- readRDS("Data/lyrics_df_ver2.Rds")

lyr %>% group_by(file) %>%
  mutate(last_line_in_file = line_no == max(line_no)) -> lyr

generate <- function () {
  # stage1: get random lines
  sampled_line <- lyr[sample(rownames(lyr[!lyr$last_line_in_file,]), size = 1),]
  second_line <- lyr[lyr$file == sampled_line$file &
                       lyr$line_no == sampled_line$line_no + 1,]
  
  # stage1: get rhymes
  rhymes <- get_rhyme(second_line$last_word)
  lyr_stage2 <- lyr[lyr$last_word %in% rhymes$word &
                      lyr$artist != sampled_line$artist,]
  
  # stage2: get random lines from rhymed lines
  sampled_line2 <- lyr_stage2[sample(rownames(lyr_stage2[!lyr_stage2$last_line_in_file,]), size = 1),]
  second_line2 <- lyr[lyr$file == sampled_line2$file &
                        lyr$line_no == sampled_line2$line_no + 1,]
  
  # stage2: get rhymes
  rhymes_stage2 <- get_rhyme(second_line2$last_word)
  lyr_stage3 <- lyr[lyr$last_word %in% rhymes_stage2$word &
                      lyr$artist != sampled_line2$artist,]
  
  # stage2: get random lines from rhymed lines
  sampled_line3 <- lyr_stage3[sample(rownames(lyr_stage3[!lyr_stage3$last_line_in_file,]), size = 1),]
  second_line3 <- lyr[lyr$file == sampled_line3$file &
                        lyr$line_no == sampled_line3$line_no + 1,]
  
  # stage3: final line
  rhymes_stage3 <- get_rhyme(second_line3$last_word)
  lyr_stage4 <- lyr[lyr$last_word %in% rhymes_stage3$word &
                      lyr$artist != sampled_line3$artist,]
  final_line <- lyr_stage4[sample(rownames(lyr_stage4), 1),]
  
  
  rbind(sampled_line, second_line,
        sampled_line2, second_line2,
        sampled_line3, second_line3,
        final_line)
}

gen <- try(generate())
while (class(gen) == "try-error" || nchar(gen) > 500) gen <- try(generate())
gen

toot <- paste(gen$line, collapse = "\n")
post_toot(toot)