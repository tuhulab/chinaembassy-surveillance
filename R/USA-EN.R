#!/usr/bin/env Rscript

# Surevillance for China Embassy in USA ---------------
library(dplyr)
library(stringr)
library(ggplot2)
library(rebus)
library(purrr)

extract_content <- function(url = ...){
  content <- url %>% xml2::read_html() %>% 
    rvest::html_nodes(css ="#News_Body_Txt_A p") %>% rvest::html_text()  %>% 
    str_subset("") %>% str_c(collapse = TRUE)
  return(content)
}
html <- xml2::read_html("http://www.china-embassy.org/eng/notices/")

page <- tibble(Title = rvest::html_nodes(html, css = "#docMore a") %>% rvest::html_text() %>% 
                 as.character(),
               Time = rvest::html_nodes(html,css = "#docMore li") %>% as.character() %>% 
                 str_extract("\\([:graph:]{1,}\\)") %>% str_remove_all("\\(|\\)") %>% as.Date(),
               url = rvest::html_nodes(html, css = "#docMore a") %>% as.character() %>% 
                 str_extract('(?<=href=")[:graph:]{1,}(?=\")')) %>% 
  mutate(url = url %>% 
           str_replace(START %R% DOT, "http://www.china-embassy.org/eng/notices/")) %>% 
  dplyr::arrange(desc(Time)) 

content <- page$url %>% map_chr(extract_content)

page_content_preview <- 
  page %>% 
  mutate(Content_preview = paste0("[",content %>% str_remove_all("TRUE") %>% str_sub(1, 300), " ... (Read More)]","(", url,")")) %>% 
  select(-url)

header_1 <- 
  paste("## The Table was Updated at", lubridate::with_tz(Sys.time(), tzone = "America/New_York"), "(New York Time) \n ")

header_2 <- 
  paste0("## The Latest Update from the Embassy was on ", page$Time[1], "\n")

table <- knitr::kable(page_content_preview , format = "pipe")

output <- c(header_1, header_2, table)
write(output, "USA/README.md")

