library(tidyverse)
library(magick)
library(tesseract)
library(hunspell)
library(rvest)

setwd("C:/Users/Stephen/Desktop/R/de-codenames")
list.files()


###################################################################################


# get actual words on board
actual_words <- c("TAP", "CROWN", "CENTER", "BOX", "TRAIN",
                  "PART", "COMPOUND", "SERVER", "PIANO", "INDIA",
                  "NOTE", "FILM", "SHOE", "PARACHUTE", "SUB",
                  "BACK", "SOUND", "CLUB", "PLANE", "CHARGE", 
                  "OLYMPUS", "CHAIR", "PLATYPUS", "TELESCOPE", "HORSE")
actual_words


#################################################################################


# create board_image1
board_image1 <- image_read("board_image1.jpg")
board_image1


##############


# create bookshelf_image2
image_read("board_image1.jpg") %>% image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(image = ., path = "board_image2.png", format = 'png', density = '300x300')

board_image2 <- image_read("board_image2.png")
board_image2


##############


# create bookshelf_image3
image_read("board_image1.jpg") %>% image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(image = ., path = "board_image3.png", format = 'png', density = '300x300')

board_image3 <- image_read("board_image3.png")
board_image3


#################################################################################


# get board_image ocr
board_image1_ocr <- board_image1 %>% ocr(.)
board_image2_ocr <- board_image2 %>% ocr(.)
board_image3_ocr <- board_image3 %>% ocr(.)


# inspect
board_image1_ocr
board_image2_ocr
board_image3_ocr


#################################################################################

# get board_image ocr_data
board_image1_ocr_data <- board_image1 %>% ocr_data(.)
board_image2_ocr_data <- board_image2 %>% ocr_data(.)
board_image3_ocr_data <- board_image3 %>% ocr_data(.)


# inspect
board_image1_ocr_data %>% data.frame()
board_image2_ocr_data %>% data.frame()
board_image3_ocr_data %>% data.frame()


#################################################################################


# see how many actual_words were read
board_image1_ocr_data %>% filter(word %in% actual_words) # 21
board_image2_ocr_data %>% filter(word %in% actual_words) # 23
board_image3_ocr_data %>% filter(word %in% actual_words) # 23

# look at confidence for actual_words
board_image2_words %>% filter(word %in% actual_words) %>% 
        summarize(avg_conf = sum(confidence) / n(), max_conf = max(confidence), min_conf = min(confidence))

# get words in all caps
board_image2_words <- board_image2_ocr_data %>% mutate(nchar = nchar(word),
                                 regex_string = str_c("[A-Z]{", nchar, "}")) %>%
        filter(str_detect(string = word, pattern = regex(regex_string)))

# use hunspell to drop spelling errors
board_image2_words_index <- hunspell_check(board_image2_words$word)
board_image2_words <- board_image2_words %>% filter(board_image2_words_index)
board_image2_words %>% data.frame()

# save board_image2_words
# write_csv(board_image2_words, "board_image2_words.csv")

# read board_image2_words
codenames_tbl <- read_csv("board_image2_words.csv")
codenames_tbl


##############################################################################


# methodology: 
# 1) find matches across multiple codenames from thresaurus synonyms for codenames
# clue scores are all 1

# 2) find single-codename wikipedia results containing codename in title 
# clue scores are tf-idf (standardized between 0-1 across all #2 clues)
# (tf-idf is amongst random wikipedia corpus)

# 3) find multi-codename wikipedia search results, get tf-idf (amongst random wikipedia corpus, top 500)
# clue scores are multiplying tfidf * codenames count in article (account for clue's distinctiveness and relevance)
# then standardizing clue scores
# summing 10-choose-1through10 = 1016 search variants, 
# 1016 * top 100 tfidf terms across all results for each variant = 101,600 clues
# 101,600 clues / 10 codenames = 10,160 clues per codename

# then apply bonus multiplier to each codename-specific scores based on how many codename lists the clue is found
# 1) a clue gets multiplied by 3 if it's on a codename's #1 synonym list
# 2) a clue gets multiplied by 2 if it's on a codename's #2 single-codename search list
# 3) a clue gets multiplied by 1 if it's on a codename's #3 multi-codename search list

# then sum the clue scores across each codename list to get aggregate clue scores


##############################################################################


# 1) thesaurus synonyms

# create get_synonyms function
get_synonyms <- function(word) {

        # create thesaurus.com url
        thesaurus_base_url <- "https://www.thesaurus.com/browse/"
        thesaurus_word_url <- str_c(thesaurus_base_url, word)
        
        # get html, but return NA if you get an error (like when word is not in thesaurus and redirects (e.g. "INDIA"))
        thesaurus_word_html <- tryCatch({ read_html(thesaurus_word_url) },
                                        error = function(e) { return(NA) })
        
        # if  and is NA, then just return NA 
        if(is.na(thesaurus_word_html)) {
                return(tibble(codename = NA, synonym = NA, definition = NA))
        }
        
        # if thesaurus_word_html did not get an error then proceed
        if(!is.na(thesaurus_word_html)) { 
                
                # stop(str_c("error accessing thesaurus for codename: ", word))
                
                # get page numbers
                thesaurus_page_numbers <- thesaurus_word_html %>% html_nodes(css = "span[class = 'css-fzwid6 e1gu66k43']") %>% 
                        html_text()
                # since max page can be seperated from first few by ellipses, get 1 to max (e.g. page 1, 2, 3 ... 12)
                if(length(thesaurus_page_numbers) > 0) {
                        thesaurus_page_numbers <- 1:max(as.numeric(thesaurus_page_numbers))
                }
                # also allow for cases with only 1 page, so thesaurus_page_numbers is initally set to character(0)
                if(length(thesaurus_page_numbers) == 0) {
                        thesaurus_page_numbers <- 1
                }
                
                # call loop_through_pages_getting_synonyms function
                map_dfr(.x = thesaurus_page_numbers, 
                        .f = ~ loop_through_pages_getting_synonyms(thesaurus_page_number = .x,
                                                        thesaurus_word_url = thesaurus_word_url, word = word))
        }
        
        

}

# test
word <- "CROWN"
get_synonyms(word)


################


# create loop_through_pages_getting_synonyms function
loop_through_pages_getting_synonyms <- function(thesaurus_page_number, thesaurus_word_url, word) {
        
        tryCatch({
        
                # get current_page_number
                current_page_number <- thesaurus_page_number
                print(str_c("getting page number ", current_page_number))
                
                # get current_page_url
                current_page_url <- thesaurus_word_url
                if(current_page_number > 1) {
                        current_page_url <- str_c(current_page_url, current_page_number, sep = "/")
                }
                
                # get html for current_page_url
                current_page_html <- read_html(current_page_url)
                
                # get synonyms 
                # removing css code combined with first word of some sections
                current_page_synonyms <- current_page_html %>% 
                        # html_nodes(css = "span[class = 'css-133coio etbu2a32']") %>%
                        html_nodes(css = "span[class = 'css-133coio etbu2a32'], div[class = 'css-ytk8xl eqpevqj6'],
                                   span[class = 'css-c3577y eqpevqj3']") %>%
                        html_text() %>% tibble(synonym = .) %>% 
                        mutate(synonym = ifelse(str_sub(string = synonym, start = 1, end = 4) == ".css", 
                                str_replace(string = synonym, pattern = regex("\\.css.*\\}"), replacement = ""), synonym)) 
                
                # get synonym_definition
                current_page_definitions <- current_page_html %>% 
                        html_nodes(css = "div[class = 'css-ytk8xl eqpevqj6'],
                                   span[class = 'css-c3577y eqpevqj3']") %>% html_text() %>% tibble(defintion = .)
                
                # flag synonym definitions in current_page_synonyms
                current_page_synonyms <- current_page_synonyms %>% 
                        mutate(definition_flag = ifelse(synonym %in% current_page_definitions$defintion, 1, 0))
                
                # call assign_synonym_definition function
                definition <- NA
                current_page_synonyms <- pmap_dfr(.l = current_page_synonyms, .f = assign_synonym_definition)
                
                # get antonyms to remove from synonyms (hard to exclude using css selectors)
                current_page_antonyms <- current_page_html %>%
                        html_nodes(css = "section[class ^= 'antonyms'] a") %>% html_text() %>% tibble(antonym = .)
                
                # remove current_page_antonyms from current_page_synonyms
                current_page_synonyms <- current_page_synonyms %>% filter(!(synonym %in% current_page_antonyms$antonym)) %>%
                        distinct(synonym, definition)
                
                # add word to tibble
                current_page_synonyms <- current_page_synonyms %>% mutate(codename = word) %>%
                        select(codename, synonym, definition)
                return(current_page_synonyms)
                
        }, error = function(e) { 
                print("error message: there was an error in loop_through_pages_getting_synonyms function")
                return(tibble(codename = NA, synonym = NA, definition = NA))})
}

# test get_synonyms
word <- "TRAIN"
current_page_number <- 1
output <- get_synonyms(word = word)
output %>% filter(is.na(synonym))


#############


# define defintion outside of function so it can be overwritten within function
definition <- NA

# create assign_synonym_definition
assign_synonym_definition <- function(synonym, definition_flag, ...) {
        
        # set definition
        if(definition_flag == 1) {
                definition <<- synonym
        }
        
        # create synonyms_w_definitions tibble
        synonyms_w_definitions <- tibble(synonym = synonym, definition_flag = definition_flag,
                                         definition = definition)
        
        # filter out rows where definition_flag = 1, since the "synonym" value is really a defintion
        # also drop definition_flag
        synonyms_w_definitions <- synonyms_w_definitions %>% filter(definition_flag == 0) %>%
                select(-definition_flag)
        synonyms_w_definitions
}

# test assign_synonym_definition function
output <- pmap_dfr(.l = current_page_synonyms, .f = assign_synonym_definition)
output %>% head(50) %>% data.frame()

#############


# create get_all_synonyms function
get_all_synonyms <- function(word) {
        
        # get current_word_synonyms
        print(str_c("current word is:", word))
        current_word_synonyms <- get_synonyms(word)
        current_word_synonyms
        
}

# test get_all_synonyms
codename_synonyms <- map_dfr(.x = codenames_tbl$word[9:11], .f = get_all_synonyms)
codename_synonyms

# get codename_synonyms by calling get_all_synonyms
codename_synonyms <- map_dfr(.x = codenames_tbl$word, .f = get_all_synonyms)
codename_synonyms

# write to file
write_csv(codename_synonyms, "codename_synonyms.csv")

# read codename_synonyms
codename_synonyms <- read_csv("codename_synonyms.csv")
codename_synonyms


###############################################################################


# reshape codenames_synonyms to calculate common synonyms
codename_synonyms
glimpse(codename_synonyms)
head(codename_synonyms)

# add count of codename/synonym pairs (after filtering out NAs)
codename_synonyms2 <- codename_synonyms %>% rename(codename = word) %>% distinct(codename, synonym, definition) %>% 
        filter(!is.na(synonym), synonym != "no thesaurus output",
               str_to_lower(codename) != str_to_lower(synonym)) %>% 
        add_count(synonym) %>% rename(synonym_count = n) %>% arrange(desc(synonym_count), synonym) %>%
        group_by(synonym) %>% mutate(codename_count = n_distinct(codename)) %>% arrange(desc(codename_count))
        
codename_synonyms2 

# inspect codenames_synonyms2
codename_synonyms2 %>% head(100) %>% data.frame()


#############################################################################


# assign codename teams, and deconflict list of codename synonyms to filter out synonyms for other team's codenames

# #1 )add scoring function that gives 1 point for each codename above 1 that shares a synonym
# #2 ) and .25 points for every instance of the synonym 
# # 2) is basically a tiebreaker when codename count is tied, but also weighting prevalent association







##############################################################################


# tf-idf based on single word wikipedia searches


##############################################################################


# tf_idf based on multiple word wikipedia searches


