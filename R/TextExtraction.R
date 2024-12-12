# Removes all XML-Tags from a given text.
# @param text String that should be cleaned.
# @return Cleaned text.
.cleanText <- function(text) {
  text <- text %>% paste(collapse = "") %>% str_trim()
  return(gsub("<.*?>", "", text))
}

# Extracts all attributes from a given xml element.
# This function has been originally developed by Dmytro Perepolkin (https://github.com/dmi3kno/hocr).
# @param xml_node Given xml element.
# @return List of attributes.
.parseAttributes <- function(xml_node){
  obj_att <- split(unname(xml2::xml_attrs(xml_node)), names(xml2::xml_attrs(xml_node)))
  obj_class <- obj_att$'class'
  names2keep <- setdiff(names(obj_att), "class")
  result <- setNames(lapply(names2keep,
                            function(x) obj_att[[x]]),
                     paste(obj_class, names2keep, sep = "_"))
  return(result)
}

# Extracts all children of a given list of xml objects and applies a function to them.
# This function has been originally developed by Dmytro Perepolkin (https://github.com/dmi3kno/hocr).
# @param l List of xml objects.
# @param f Function to by applied to the children.
# @return Tibble of parsed children.
.parseObject <- function(l, f){
  obj_attr_list <- .parseAttributes(l)
  obj_df <- bind_rows(lapply(xml2::xml_children(l), f))
  result <- mutate(obj_df, !!!(obj_attr_list))
  return(result)
}

# Parses a word on a HOCR page and returns the word with all of its attributes in a tabular structure.
# This function has been originally developed by Dmytro Perepolkin (https://github.com/dmi3kno/hocr).
# @param(xml_word_node)
.parseWord <- function(xml_word_node){
  word_node_list <- list(text = xml2::xml_text(xml_word_node),
                         .attrs = xml2::xml_attrs(xml_word_node))
  word_node_list <- word_node_list[setdiff(names(word_node_list),
                                           ".attrs")]
  word_node_list <- list(ocrx_word_tag = names(word_node_list),
                         ocrx_word_value = unlist(unname(word_node_list)))
  result <- as_tibble(c(.parseAttributes(xml_word_node), word_node_list))
  return(result)
}

# Parses a text line by parsing all of its words.
.parseLine <- partial(.parseObject, f = .parseWord)

# Parses a paragraph by parsing all of its text lines.
.parsePar <- partial(.parseObject, f = .parseLine)

# Parses a text area by parsing all of its paragraphs.
.parseArea <- partial(.parseObject, f = .parsePar)

# Parses a text page by parsing all of its areas.
.parsePage <- partial(.parseObject, f = .parseArea)

# Extracts a word table (as tibble) from a tex page x that has been converted into text via tesseract.
# This function has been originally developed by Dmytro Perepolkin (https://github.com/dmi3kno/hocr).
# @param x Text page that is converted into a word table.
# @return Tibble representing the extracted word table.
.parseHOCR <- function(x) {
  wordTable <- xml2::read_xml(x) %>% .parsePage()
  return(wordTable)
}

# Parses the text extracted directly from a PDF file.
# @param text Text that has been directly extracted from a PDF file.
# @param data PDF data including font information and information about the words in the PDF file.
# @return List including the extracted text, a data table including the lines, and a data table including the words.
.parseText <- function(text, data) {
  wordsPerPage <- unlist(lapply(data, nrow))
  wordsDT <- bind_rows(data) %>%
    mutate(page = rep(1:length(wordsPerPage), times = wordsPerPage),
           length = str_length(text),
           characterDiff = width/length,
           yBottom = y + height) %>%
    arrange(page, yBottom, x)
  medianCharacterSize = mean(wordsDT$characterDiff, na.rm = T)
  minLineX <- min(wordsDT$x)
  wordsDT <- wordsDT %>%
    group_by(page) %>%
    mutate(lineDiff = yBottom - lag(yBottom) + 2,
           correction = if_else(height < 8 & height > 1, height - 1, height - 2),
           emptyLines = if_else(is.na(lineDiff), 0, floor(lineDiff / correction)),
           line = cumsum(emptyLines) + 1)
  if (min(wordsDT$line) < 1) {
    wordsDT$line = wordsDT$line + 1 - min(wordsDT$line)
  }
  wordsDT <- wordsDT %>%
    arrange(page, line, x) %>%
    mutate(startTemp = round((x - minLineX) / medianCharacterSize),
           endTemp = startTemp + length - 1) %>%
    select(-c(lineDiff, emptyLines))
  wordsDT <- wordsDT %>%
    group_by(page, line) %>%
    mutate(shift = if_else(lag(endTemp) < startTemp, 0, lag(endTemp) + 1 - startTemp),
           newLine = if_else(is.na(shift), 1, 0),
           puffer = if_else(is.na(shift) | lag(endTemp) >= startTemp, 0, startTemp - lag(endTemp) - 1),
           shift = if_else(is.na(shift), 0, shift),
           cumShift = cumsum(shift),
           pufferDiff = cumShift - cumsum(pmin(puffer, cumShift)),
           cumDiff = pmax(cumsum(pufferDiff), 0),
           corShift = if_else(shift > 0, 0, -pmin(cumDiff, puffer)),
           tempShift = cumsum(shift + corShift),
           diff = if_else(tempShift < 0, -lag(tempShift), tempShift - lag(tempShift)),
           diff = if_else(is.na(diff), tempShift, diff),
           newShift = cumsum(diff),
           start = startTemp + newShift,
           end = start + length - 1,
           estPuffer = ceiling((x - lag(x) - lag(width)) / medianCharacterSize),
           estPuffer = if_else(is.na(estPuffer), 0, estPuffer),
           estPuffer = if_else(estPuffer < 0, 1, estPuffer),
           shift2 = if_else(puffer >= estPuffer & !newLine, estPuffer - puffer - 1, shift),
           puffer2 = if_else(puffer > estPuffer & !newLine, 1, puffer),
           corShift2 = if_else(shift2 != 0, 0, -pmin(cumDiff, puffer)),
           tempShift2 = cumsum(shift2 + corShift2),
           minDiff = if_else(is.na(lag(endTemp)), estPuffer, pmin(estPuffer, startTemp - lag(endTemp))),
           diff2 = if_else(startTemp + tempShift2 - (lag(startTemp) + lag(tempShift2) + lag(length)) < (minDiff - 1),
                           1 + minDiff + tempShift2 + (lag(startTemp) + lag(tempShift2) + lag(length)) - (startTemp + tempShift2) - lag(tempShift2),
                           tempShift2 - lag(tempShift2)),
           diff2 = if_else(is.na(diff2), tempShift2, diff2),
           newShift2 = cumsum(diff2),
           start2 = startTemp + newShift2,
           end2 = start2 + length - 1,
           shift3 = if_else(lag(endTemp) < startTemp - 1, 0, lag(endTemp) + 2 - startTemp),
           newLine3 = if_else(is.na(shift3), 1, 0),
           puffer3 = if_else(is.na(shift3) | lag(endTemp) >= startTemp - 1, 0, startTemp - lag(endTemp) - 2),
           shift3 = if_else(is.na(shift3), 0, shift3),
           cumShift3 = cumsum(shift3),
           offeredPuffer3 = if_else(cumShift3 == 0, 0, pmin(puffer3, cumShift3)),
           pufferDiff3 = cumsum(shift3 - offeredPuffer3),
           usedPuffer3 = pmax(0, offeredPuffer3 + pmin(0, pufferDiff3)),
           corShift3 = shift3 - usedPuffer3,
           newShift3 = cumsum(corShift3),
           start3 = startTemp + newShift3,
           end3 = start3 + length - 1)
  wordShift <- 1 - min(wordsDT$start)
  wordsDT <- wordsDT %>%
    mutate(start = start + wordShift,
           end = end + wordShift,
           start2 = start2 + wordShift,
           end2 = end2 + wordShift) %>%
    rename(value = text) %>%
    select(-c(space, length, characterDiff, shift, cumShift, pufferDiff, cumDiff, corShift, newShift, puffer,
              tempShift, diff, diff2, tempShift2, minDiff,
              newLine, estPuffer, shift2, puffer2, corShift2, newShift2, startTemp, endTemp, correction, yBottom,
              shift3, newLine3, puffer3, cumShift3, offeredPuffer3, pufferDiff3, usedPuffer3, corShift3, newShift3)) %>%
    relocate(c(page, line, start, end, start2, end2, start3, end3, value, x, y, width, height))
  numLines <- unlist(wordsDT %>% group_by(page) %>% summarize(lines = max(line)) %>% select(lines))
  numLines <- cumsum(c(0, numLines[-length(numLines)]))
  wordsDT$absLine <- as.numeric(numLines[wordsDT$page] + wordsDT$line)
  wordsDT <- wordsDT %>% arrange(absLine, start)
  linesDT <- wordsDT %>%
    group_by(page, line) %>%
    summarize(value = paste(value, collapse = " "),
              start = min(start),
              end = max(end),
              start2 = min(start2),
              end2 = max(end2),
              start3 = min(start3),
              end3 = max(end3),
              width = last(x) - first(x) + last(width),
              height = max(height),
              x = min(x),
              y = min(y),
              absLine = first(absLine), .groups = "drop_last") %>%
    relocate(page, line, start, end, start2, end2, start3, end3, value, x, y, width, height)
  textStructure <- list(text = .cleanText(text), lines = linesDT, words = wordsDT)
  return(textStructure)
}

# Parses the text extracted from an image included in a PDF file.
# @param text Text that has been extracted from an image included in a PDF file.
# @return List including the extracted text, a data table including the lines, and a data table including the words.
.parseOCRText <- function(text) {
  pages <- length(text)
  boxDF <- vector()
  for (page in 1:pages) {
    pageDT <- .parseHOCR(text[page])
    pageDT$page = page
    boxDF <- rbind(boxDF, pageDT)
  }
  boxDF <- boxDF %>% filter(ocrx_word_tag == "text") %>%
    mutate(linePos = as.numeric(str_split_fixed(ocr_line_id, "_", 3)[,3]),
           wordLength = str_length(ocrx_word_value),
           lineBox = str_split_fixed(ocr_line_title, ";", 5)[,1],
           wordBox = str_split_fixed(ocrx_word_title, ";", 2)[,1]) %>%
    rename(word = ocrx_word_value) %>%
    separate(lineBox, c("lineBox", "lineX1", "lineY1", "lineX2", "lineY2"), convert = T) %>%
    separate(wordBox, c("wordBox", "wordX1", "wordY1", "wordX2", "wordY2"), convert = T) %>%
    mutate(width = wordX2 - wordX1,
           height = wordY2 - wordY1,
           characterDiff = width/wordLength) %>%
    select(-c(ocrx_word_id, ocr_page_id, ocr_page_title, ocr_par_id, ocr_par_lang, ocr_par_title, ocr_carea_id, ocr_carea_title,
              ocrx_word_tag, ocrx_word_title, ocr_line_title, lineBox, wordBox))
  minLineX <- min(boxDF$lineX1)
  linesDT <- boxDF %>% group_by(page, linePos) %>%
    summarize(line = paste(word, collapse = " "),
              lineY1 = min(lineY1),
              lineY2 = max(lineY2),
              .groups = "drop_last") %>%
    mutate(height = lineY2 - lineY1,
           lineDiff = lineY1 - lag(lineY2) + 2,
           emptyLines = if_else(is.na(lineDiff), 1, pmax(0, floor(lineDiff / (height - 2)) + 1)),
           lineNum = cumsum(emptyLines)) %>%
    rename(value = line, line = lineNum) %>%
    select(-c(lineY1, lineY2, height, lineDiff, emptyLines)) %>%
    as.data.table()
  boxDF <- boxDF %>%
    left_join(linesDT, by = c("page", "linePos")) %>%
    select(-value)
  medianCharacterSize = mean(boxDF$characterDiff, na.rm = T)
  minLineX <- min(boxDF$lineX1)
  wordsDT <- boxDF %>%
    mutate(startTemp = round((wordX1 - minLineX) / medianCharacterSize),
           endTemp = startTemp + wordLength - 1) %>%
    rename(value = word, x = wordX1, y = wordY1)
  wordsDT <- wordsDT %>%
    group_by(page, line) %>%
    mutate(shift = if_else(lag(endTemp) < startTemp, 0, lag(endTemp) + 1 - startTemp),
           newLine = if_else(is.na(shift), 1, 0),
           puffer = if_else(is.na(shift) | lag(endTemp) >= startTemp, 0, startTemp - lag(endTemp) - 1),
           shift = if_else(is.na(shift), 0, shift),
           cumShift = cumsum(shift),
           pufferDiff = cumShift - cumsum(pmin(puffer, cumShift)),
           cumDiff = pmax(cumsum(pufferDiff), 0),
           corShift = if_else(shift > 0, 0, -pmin(cumDiff, puffer)),
           tempShift = cumsum(shift + corShift),
           diff = if_else(tempShift < 0, -lag(tempShift), tempShift - lag(tempShift)),
           diff = if_else(is.na(diff), tempShift, diff),
           newShift = cumsum(diff),
           start = startTemp + newShift,
           end = start + wordLength - 1,
           estPuffer = ceiling((x - lag(x) - lag(width)) / medianCharacterSize),
           estPuffer = if_else(is.na(estPuffer), 0, estPuffer),
           estPuffer = if_else(estPuffer < 0, 1, estPuffer),
           shift2 = if_else(puffer >= estPuffer & !newLine, estPuffer - puffer - 1, shift),
           puffer2 = if_else(puffer > estPuffer & !newLine, 1, puffer),
           corShift2 = if_else(shift2 != 0, 0, -pmin(cumDiff, puffer)),
           tempShift2 = cumsum(shift2 + corShift2),
           minDiff = if_else(is.na(lag(endTemp)), estPuffer, pmin(estPuffer, startTemp - lag(endTemp))),
           diff2 = if_else(startTemp + tempShift2 - (lag(startTemp) + lag(tempShift2) + lag(wordLength)) < (minDiff - 1),
                           1 + minDiff + tempShift2 + (lag(startTemp) + lag(tempShift2) + lag(wordLength)) - (startTemp + tempShift2) - lag(tempShift2),
                           tempShift2 - lag(tempShift2)),
           diff2 = if_else(is.na(diff2), tempShift2, diff2),
           newShift2 = cumsum(diff2),
           start2 = startTemp + newShift2,
           end2 = start2 + wordLength - 1,
           shift3 = if_else(lag(endTemp) < startTemp - 1, 0, lag(endTemp) + 2 - startTemp),
           newLine3 = if_else(is.na(shift3), 1, 0),
           puffer3 = if_else(is.na(shift3) | lag(endTemp) >= startTemp - 1, 0, startTemp - lag(endTemp) - 2),
           shift3 = if_else(is.na(shift3), 0, shift3),
           cumShift3 = cumsum(shift3),
           offeredPuffer3 = if_else(cumShift3 == 0, 0, pmin(puffer3, cumShift3)),
           pufferDiff3 = cumsum(shift3 - offeredPuffer3),
           usedPuffer3 = pmax(0, offeredPuffer3 + pmin(0, pufferDiff3)),
           corShift3 = shift3 - usedPuffer3,
           newShift3 = cumsum(corShift3),
           start3 = startTemp + newShift3,
           end3 = start3 + wordLength - 1) %>%
    select(-c(wordLength, linePos, ocr_line_id, lineX1, lineY1, lineX2, lineY2, wordX2, wordY2, tempShift2, minDiff, diff2,
              characterDiff, shift, cumShift, pufferDiff, cumDiff, corShift, newShift, puffer, tempShift, diff, newLine,
              estPuffer, shift2, puffer2, corShift2, newShift2, startTemp, endTemp,
              shift3, newLine3, puffer3, cumShift3, offeredPuffer3, pufferDiff3, usedPuffer3, corShift3, newShift3)) %>%
    relocate(c(page, line, start, end, start2, end2, start3, end3, value))
  wordShift <- 1 - min(wordsDT$start)
  wordsDT <- wordsDT %>%
    mutate(start = start + wordShift,
           end = end + wordShift,
           start2 = start2 + wordShift,
           end2 = end2 + wordShift,
           start3 = start3 + wordShift,
           end3 = end3 + wordShift)
  numLines <- unlist(wordsDT %>% group_by(page) %>% summarize(lines = max(line)) %>% select(lines))
  numLines <- cumsum(c(0, numLines[-length(numLines)]))
  wordsDT$absLine <- as.numeric(numLines[wordsDT$page] + wordsDT$line)
  wordsDT <- wordsDT %>% arrange(absLine, start)
  wordsDT$x <- round(wordsDT$x / 8.35)
  wordsDT$y <- round(wordsDT$y / 8.35)
  wordsDT$width <- round(wordsDT$width / 8.35)
  wordsDT$height <- round(wordsDT$height / 8.35)
  linesDT <- wordsDT %>%
    group_by(page, line) %>%
    summarize(value = paste(value, collapse = " "),
              start = min(start),
              end = max(end),
              start2 = min(start2),
              end2 = max(end2),
              start3 = min(start3),
              end3 = max(end3),
              width = last(x) - first(x) + last(width),
              height = max(height),
              x = min(x),
              y = min(y),
              absLine = first(absLine), .groups = "drop_last") %>%
    relocate(page, line, start, end, start2, end2, start3, end3, value, x, y, width, height)
  textStructure <- list(text = .cleanText(text), lines = linesDT, words = wordsDT)
  return(textStructure)
}

#' Extracts the text from a PDF file
#' @description
#' This function extracts text from PDF documents and returns the text as a string,
#' as a list of lines and as a list of words. It uses 'pdftools' to extract the
#' content from textual PDF files and 'tesseract' to extract the content from
#' image-based PDF-files.
#'
#' @param file Path to the PDF file
#' @return List including the extracted text, a data table including the lines, a data table including the words, the type and language of the document.
#' @examples
#' file <- system.file("extdata", "OrderDocument_en.pdf", package = "orderanalyzer")
#' text <- extractText(file)
#' text$words
#'
#' @export
extractText <- function(file) {
  text <- pdftools::pdf_text(pdf = file)
  if (text[1] != "") { # check if pdf files includes text
    data <- pdftools::pdf_data(file)
    text <- str_remove_all(text, "@@.*?@@")
    textStructure <- .parseText(text, data)
    textStructure$language <- identifyLanguage(text)
    textStructure$type <- "text"
  } else { # extract text via tesseract
    images <- pdftools::pdf_convert(pdf = file, dpi = 600, verbose = FALSE)
    fullText <- tesseract::ocr(image = images[1])
    languages <- tesseract::tesseract_info()$available
    language <- identifyLanguage(fullText)
    if (language == "german") {
      if (!"deu" %in% languages) {
        tesseract::tesseract_download(lang = "deu") # German
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("deu"), HOCR = T)
    } else if (language == "french") {
      if (!"fra" %in% languages) {
        tesseract::tesseract_download(lang = "fra") # French
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("fra"), HOCR = T)
    } else if (language == "spanish") {
      if (!"spa" %in% languages) {
        tesseract::tesseract_download(lang = "spa") # Spanish
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("spa"), HOCR = T)
    } else if (language == "czech") {
      if (!"ces" %in% languages) {
        tesseract::tesseract_download(lang = "ces") # Czech
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("ces"), HOCR = T)
    } else if (language == "dutch") {
      if (!"nld" %in% languages) {
        tesseract::tesseract_download(lang = "nld") # Dutch and Flemish
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("nld"), HOCR = T)
    } else if (language == "latvian") {
      if (!"lav" %in% languages) {
        tesseract::tesseract_download(lang = "lav") # Latvian
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("lav"), HOCR = T)
    } else if (language == "lithuanian") {
      if (!"lit" %in% languages) {
        tesseract::tesseract_download(lang = "lit") # Lithuanian
      }
      text <- tesseract::ocr(image = images, engine = tesseract::tesseract("lit"), HOCR = T)
    } else {
      text <- tesseract::ocr(image = images, HOCR = T)
    }
    unlink(images)
    text <- str_remove_all(text, "@@.*?@@")
    textStructure <- .parseOCRText(text)
    textStructure$language <- language
    textStructure$type <- "image"
  }
  textStructure$filename <- file
  return(textStructure)
}
