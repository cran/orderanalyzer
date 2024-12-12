# Generates a binary matrix in which each position with a text is marked as 1.
# @param words Data table or data frame including the tokenized words.
# @param fill Integer specifying how many spaces surrounded with text will be interpreted as text.
# @param type Integer indicating which start and end type to be used.
# @return Binary matrix in which each row corresponds to a text line and each cell to a letter.
.getBinaryMatrix <- function(words, fill = 0, type = 1) {
  if (type == 1) {
    wordSummary <- words %>%
      group_by(page) %>%
      summarize(lines = max(line), lineLength = max(end))
    rows <- sum(wordSummary$lines)
    cols <- max(wordSummary$lineLength)
    occurrences <- matrix(rep(0, rows * cols), nrow = rows)
    for (i in 1:nrow(words)) {
      occurrences[words$absLine[i], words$start[i]:words$end[i]] <- 1
    }
    if (fill > 0) {
      for (j in fill:1) {
        rightShift <- shift.right(occurrences, cols = j)
        leftShift <- shift.left(occurrences, cols = j)
        occurrences <- 1 * (occurrences | (rightShift & leftShift & !occurrences))
      }
    }
  } else {
    wordSummary <- words %>%
      group_by(page) %>%
      summarize(lines = max(line), lineLength = max(end3))
    rows <- sum(wordSummary$lines)
    cols <- max(wordSummary$lineLength)
    occurrences <- matrix(rep(0, rows * cols), nrow = rows)
    for (i in 1:nrow(words)) {
      occurrences[words$absLine[i], words$start3[i]:words$end3[i]] <- 1
    }
    if (fill > 0) {
      for (j in fill:1) {
        rightShift <- shift.right(occurrences, cols = j)
        leftShift <- shift.left(occurrences, cols = j)
        occurrences <- 1 * (occurrences | (rightShift & leftShift & !occurrences))
      }
    }
  }
  return(occurrences)
}

# Compare two start patterns. A start pattern is a logical vector that represents a text line and includes a TRUE whenever
# a new word (i.e., possible column) starts.
# @param pattern1 A vector representing the start pattern of a text line.
# @param pattern2 A vector representing the start pattern of another text line.
# @param similarity A double representing the ratio of corresponding TRUEs in the two patterns.
# @return A logical value indicating whether the two start patterns are similar to each other.
.comparePatterns <- function(pattern1, pattern2, similarity) {
  pattern1 <- as.logical(pattern1)
  pattern2 <- as.logical(pattern2)
  pattern <- pattern1 | pattern2
  right1 <- c(FALSE, pattern1[-length(pattern1)])
  startPattern1 <- xor(pattern1, right1) & pattern1
  right2 <- c(FALSE, pattern2[-length(pattern2)])
  startPattern2 <- xor(pattern2, right2) & pattern2
  right <- c(FALSE, pattern[-length(pattern)])
  startPattern <- xor(pattern, right) & pattern
  col1 <- sum(startPattern1, na.rm = T)
  col2 <- sum(startPattern2, na.rm = T)
  col <- sum(startPattern, na.rm = T)
  col2in1 <- sum(pattern1 & pattern2) # Is pattern1 included in pattern2
  comparison <- col2in1 >= (similarity*col2) || col2in1 >= (similarity*col1)
  if (col == col1) {
    comparison <- comparison || (sum(pattern & pattern2) >= (similarity*col))
  }
  return(comparison)
}


# Compares a pair of start patterns and optionally also a pair of reversed patterns (i.e., end pattern). A start pattern is a
# logical vector that represents a text line and includes a TRUE whenever a new word (i.e., possible column) starts.
# @param pattern1 A vector representing the start pattern of a text line.
# @param pattern2 A vector representing the start pattern of another text line.
# @param pattern3 A vector representing the end pattern of a text line. Default is NA.
# @param pattern4 A vector representing the end pattern of another text line. Default is NA.
# @param similarity A double representing the ratio of corresponding TRUEs in the two patterns.
# @return A logical value indicating if pattern1 and pattern2 and/or pattern3 and pattern4 match.
.matchPatterns <- function(pattern1, pattern2, pattern3 = NA, pattern4 = NA, similarity = 1) {
  pattern1 <- as.logical(pattern1)
  pattern2 <- as.logical(pattern2)
  right1 <- c(FALSE, pattern1[-length(pattern1)])
  startPattern1 <- xor(pattern1, right1) & pattern1
  right2 <- c(FALSE, pattern2[-length(pattern2)])
  startPattern2 <- xor(pattern2, right2) & pattern2
  p1 <- which(startPattern1)
  p2 <- which(startPattern2)
  startDistances <- abs(outer(p1, p2, FUN = "-"))
  dist1 <- apply(startDistances, MARGIN = 2, min)
  ratio1 <- length(which(dist1 <= 3)) / length(dist1)
  ratio3 <- sum(startPattern1) / sum(startPattern2)
  if (!any(is.na(pattern3))) {
    pattern3 <- as.logical(pattern3)
    pattern4 <- as.logical(pattern4)
    left1 <- c(pattern3[-1], FALSE)
    endPattern1 <- xor(pattern3, left1) & pattern3
    left2 <- c(pattern4[-1], FALSE)
    endPattern2 <- xor(pattern4, left2) & pattern4
    p3 <- which(endPattern1)
    p4 <- which(endPattern2)
    endDistances <- abs(outer(p3, p4, FUN = "-"))
    dist2 <- apply(endDistances, MARGIN = 2, min)
    ratio2 <- length(which(dist2 <= 3)) / length(dist2)
  } else {
    dist2 <- 999
    ratio2 <- 1
  }
  comparison1 <- ratio1 >= similarity && ratio2 >= 0.5 && ratio3 >= 0.5 && ratio3 <= 2 && mean(dist2) < 1.5
  comparison2 <- ratio1 >= similarity && ratio2 >= similarity && ratio3 >= 0.8 && ratio3 <= 1.5
  comparison <- comparison1 || comparison2
  return(comparison)
}

# Checks if two ranges are overlapping. The two ranges are described by the starting and ending point.
# @param start1 Starting point of the first range.
# @param end1 Ending point of the first range.
# @param start2 Starting point of the second range.
# @param end2 Ending point of the second range.
# @return Logical value.
.isOverlapping <- function(start1, end1, start2, end2) {
  overlapping <- F
  if (!is.na(start1) && length(start1) > 0) {
    if (start1 <= start2 && end1 > start2) {
      overlapping <- T
    } else if (start1 > start2 && start1 < end2) {
      overlapping <- T
    }
  }
  return(overlapping)
}

# Computes start patterns for each line and group these pattern.
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @param m Binary matrix describing the text of a PDF file.
# @param minRows Minimal number of rows a table must consist of.
# @param minCols Number of columns a valid table must at least consist of.
# @return List of row numbers per unique pattern and number of occurrences per unique pattern.
.extractTablePatterns <- function(words, m, minRows, minCols) {
  sizes <- apply(m, MARGIN = 1, FUN = function(x) max(c(0, which(x == 1))))
  sizes <- sort(sizes, decreasing = T)
  size <- 999
  for (i in 1:(length(sizes) - 1)) {
    if (sizes[i] <= sizes[i + 1] * 1.1) {
      size = sizes[i]
      break
    }
  }
  right <- shift.right(m)
  left <- shift.left(m)
  patterns <- xor(m, right) & m # Matrix of start patterns
  patterns2 <- xor(m, left) & m # Matrix of end patterns
  startPatterns <- as.data.frame(matrix(NA, ncol = ncol(m))) # includes all unique start patterns
  endPatterns <- as.data.frame(matrix(NA, ncol = ncol(m))) # includes all unique end patterns
  freqStartPatterns <- vector() # includes the number of occurrences per unique start pattern
  rows <- list() # includes the row numbers per unique start pattern
  for (i in 1:nrow(m)) {
    startPattern <- patterns[i,]
    endPattern <- patterns2[i,]
    starts <- which(startPattern)
    ends <- which(endPattern)
    rowSize <- ends[length(ends)] + 1
    if (sum(startPattern) >= minCols && rowSize >= 0.5*size && starts[1] <= 0.2*size) {
      # Check if the line includes numbers (i.e., to exclude headers)
      numDT <- words %>%
        filter(absLine == i) %>%
        mutate(numeric = str_count(value, pattern = "[0-9]")) %>%
        summarize(sum = sum(numeric), .groups = "drop")
      if (numDT$sum > 0) {
        if (sum(freqStartPatterns) == 0) { # initialization
          startPatterns <- as.data.frame(t(startPattern))
          endPatterns <- as.data.frame(t(endPattern))
          freqStartPatterns <- 1
          rows <- list(c(i))
        } else {
          found <- F
          for (j in 1:nrow(startPatterns)) { # check if the current start and end patterns match with already parsed patterns
            if (.matchPatterns(startPatterns[j,], startPattern, endPatterns[j,], endPattern, similarity = 0.8)) {
              # update the matched pattern
              freqStartPatterns[j] <- freqStartPatterns[j] + 1
              rows[[j]] <- c(rows[[j]], i)
              startPatterns[j,] <- startPatterns[j,] | startPattern
              endPatterns[j,] <- endPatterns[j,] | endPattern
              found <- T
              break
            }
          }
          if (!found) {
            # include the new pattern
            startPatterns <- rbind(startPatterns, startPattern)
            endPatterns <- rbind(endPatterns, endPattern)
            freqStartPatterns <- c(freqStartPatterns, 1)
            rows <- c(rows, i)
          }
        }
      }
    }
  }
  result <- list(rows = rows, frequencies = freqStartPatterns)
  return(result)
}

# Merges table patterns and returns the most significant line numbers (row positions) for the merged table patterns.
# @param tablePatterns Table patterns as generated by the function .extractTablePatterns.
# @param minRows Minimal number of rows a table must consist of.
# @param candidateLines Vector including lines being a candidate for a table
# @return A list containing two components. The first component (positions) includes the start and end line number of each potential table.
# The second component (rows) represents the most significant line numbers per potential table.
.extractTablePositions <- function(tablePatterns, minRows, candidateLines) {
  tablePos <- which(tablePatterns$frequencies >= minRows)
  if (length(tablePos) == 0) {
    tablePos <- which(unlist(lapply(tablePatterns$rows, function(x) any(x %in% candidateLines$line))))
  }
  positions <- data.frame(start = vector(), end = vector())
  positionRows <- list()
  if (length(tablePos) > 0) {
    for (i in 1:length(tablePos)) {
      tableRows <- tablePatterns$rows[[tablePos[i]]]
      start <- min(tableRows)
      end <- max(tableRows)
      n <- nrow(positions)
      found <- F
      if (n > 0) {
        for (j in 1:n) {
          # check if the current table pattern is overlapping with an already handled pattern
          if (.isOverlapping(positions$start[j], positions$end[j], start, end)) {
            if (length(tableRows) > length(positionRows[[j]])) { # update the already handled pattern only if the current pattern includes more rows
              positions$start[j] <- start
              positions$end[j] <- end
              positionRows[[j]] <- tableRows
            }
            found <- T
            break
          }
        }
      }
      if (!found) {
        difference <- 0
        if (length(tableRows) > 1) {
          difference <- mean(diff(tableRows), na.rm = T)
        }
        if (length(tableRows) > 2 || difference < 40) {
          positions <- rbind(positions, data.frame(start = start, end = end))
          positionRows[[length(positionRows) + 1]] <- tableRows
        }
      }
    }
  }
  result = list(positions = positions, rows = positionRows, tableStarts = NA, type = "columnPattern")
  return(result)
}

# Identifies lines that might not be part of a table.
# @param lines Dateframe including the lines of a PDF file and their positions. The lines-dataframe is generated by the function extractText.
# @return Vector of lines numbers.
.getNonTableLines <- function(lines) {
  if (length(pkg.conf$noTableNames) == 0) {
    noTableLines <- NA
  } else {
    lines$value <- str_trim(str_to_lower(lines$value))
    noTableLines <- lines %>%
      filter(str_length(value) >= 5) %>%
      filter(str_detect(value, paste0(pkg.conf$noTableNames, collapse = "|")) | str_count(value, "[:punct:]") == str_length(value)) %>%
      ungroup() %>%
      select(absLine) %>%
      unlist() %>%
      as.numeric()
  }
  return(noTableLines)
}

# Identifies lines that might be a significant line of a table row (i.e., first line of a table row) and lines that might include table headers.
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @param lines Dateframe including the lines of a PDF file and their positions. The lines-dataframe is generated by the function extractText.
# @return List of two numeric vectors. Vector startLines includes the line numbers of those lines that might be a significant line.
# Vector tableStarts includes the line numbers of those lines that might include the table header.
.getStartLines <- function(words, lines) {
  lines$value <- tolower(lines$value)
  words$value <- tolower(words$value)

  hfk <- lines %>%
    mutate(hfk = str_count(value, paste0("\\b", pkg.conf$headerNames, "\\b", collapse = "|"))) %>%
    ungroup() %>%
    select(hfk) %>%
    unlist() %>%
    as.numeric()

  tableline <- lines$absLine[which(hfk == max(hfk))]
  line_words <- words %>%
    filter(absLine == tableline[1])
  if (nrow(line_words) > 1) {
    second_word <- line_words[2,]
  } else {
    second_word <- line_words[1,]
  }

  general_infos_word <- words %>%
    filter(absLine < tableline[1])

  ### Just lines below the table header of each page
  lines_shrink <- c()
  for (k in 1:length(tableline)) {
    lines_help <- lines %>%
      filter(absLine > tableline[k] & page == .$page[which(.$absLine == tableline[k])[1]])
    lines_shrink <- rbind(lines_shrink, lines_help)
  }
  words_shrink <- words %>%
    filter(absLine %in% lines_shrink$absLine)

  ### Assumption: Order positions always start with a number (the position number)
  startlines <- words_shrink %>%
    filter(str_detect(value, "^[0-9]+[.,]?[0-9]*$") & start < second_word$start)
  startlines <- startlines %>%
    mutate(val = .prepareNumerics(value),
           val = if_else(val == 0, 1, val),
           power = floor(log(val) / log(10)),
           candidate = if_else(val / 10^power == 1, T, F))
  candidatelines <- startlines
  firstStart <- which(startlines$candidate == T)[1]
  if (!is.na(firstStart)) {
    if (firstStart/nrow(startlines) <= 0.333 || nrow(startlines) <= 5) {
      startlines <- startlines[firstStart:nrow(startlines),]
    } else {
      startlines <- data.frame()
    }
  } else {
    startlines <- data.frame()
  }
  if (nrow(startlines) > 0) {
    startlines <- startlines %>%
      filter(((start >= first(.$start) - 1 & start <= first(.$start) + 1) | (end >= first(.$end) - 1 & end <= first(.$end) + 1)) &
               nchar(value) %in% ((nchar(first(.$value)) - 1):(nchar(first(.$value)) + 1)))
  }
  result <- list(startLines = startlines, tableLines = tableline, candidateLines = candidatelines)
}

# Returns the mode of a given vector (i.e., the value that appears most often).
# @param v Vector of values.
# @return Value that appears most often.
.getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Extracts the table positions based on the most significant lines per table.
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @param lines Dateframe including the lines of a PDF file and their positions. The lines-dataframe is generated by the function extractText.
# @param m Binary matrix describing the text of a PDF file.
# @param minRows Minimal number of rows a table must consist of.
# @param minCols Number of columns a valid table must at least consist of.
# @return A list containing two components. The first component (positions) includes the start and end line number of each potential table.
# The second component (rows) represents the most significant line numbers per potential table.
.extractSignificantLines <- function(words, lines, m, minRows, minCols) {
  linePos <- .getStartLines(words, lines)
  tableStarts <- linePos$tableLines
  startlines <- linePos$startLines
  startAbsLines <- startlines$absLine
  noTableLines <- .getNonTableLines(lines)

  if (!any(is.na(startAbsLines)) && length(startAbsLines) > 0) {
    patterns <- m[startAbsLines,]
    if (length(startAbsLines) > 1) {
      right <- shift.right(patterns)
      startPatterns <- xor(patterns, right) & patterns # Matrix of start patterns
      # left <- shift.left(patterns)
      # endPatterns <- xor(patterns, left) & patterns
      # blockPatterns <- startPatterns | endPatterns
      cols <- rowSums(startPatterns)
      candidates <- which(cols > 1)
      startAbsLines <- startAbsLines[candidates]
      cols <- cols[candidates]
      # equalCols <- sum(colSums(startPatterns) == length(cols))
      # colTest1 <- equalCols > 4 && equalCols >= 0.5 * .getMode(cols)
      # colTest2 <- sum(colSums(blockPatterns) == length(cols))
      if (sum(diff(cols)) == 0) {
        pos <- seq(1, length(startAbsLines))
      } else {
        # accept all rows having between mode of cols +/- 2
        pos <- which(cols >= .getMode(cols) - 2)
      }
    } else {
      right <- c(F, patterns[-length(patterns)])
      startPatterns <- xor(patterns, right) & patterns # Matrix of start patterns
      cols <- sum(startPatterns)
      pos <- 1
    }
    if (length(pos) > 0) {
      significantRows <- startAbsLines[pos]
      posDF <- data.frame(start = min(significantRows), end = max(significantRows))
    } else {
      significantRows <- list()
      posDF <- data.frame(start = vector(), end = vector())
    }
    positions = list(positions = posDF, rows = significantRows, tableStarts = tableStarts, noTableLines = noTableLines, type = "positionPattern")
  } else {
    tablePatterns <- .extractTablePatterns(words, m, minRows, minCols)
    positions <- .extractTablePositions(tablePatterns, minRows, linePos$candidateLines)
    positions$tableStarts = tableStarts
    positions$noTableLines = noTableLines
  }
  return(positions)
}

# Computes the .cosine similarity between two numeric vectors.
# @param a Numeric vector
# @param b Numeric vector
# @return Numeric value in [0, 1].
.cosine <- function(a, b) {
  value <- 0
  if (length(a) == length(b)) {
    value <- a %*% b / (sqrt(a %*% a) * sqrt(b %*% b))
  }
  return(value)
}

# Identify the line numbers between the most significant line numbers that also belong to the table.
# @param m Binary matrix describing the text of a PDF file.
# @param skeleton A table skeleton describing the most significant line numbers of a table.
# @param maxDistance Number of text lines that can maximally exist between the start of two table rows.
# @param pageBreaks Number of text lines after which a new page begins.
# @param headerPos Vector of absolute line numbers that have been identified as table headers.
# @return List (skeleton of a table) including the pattern and start pattern and the line numbers that belong to a table.
.fillRows <- function(m, skeleton, maxDistance, pageBreaks, headerPos) {
  rows <- skeleton$rows
  noTableLines <- skeleton$noTableLines
  pages <- unlist(lapply(rows, FUN = function(x) which(x < pageBreaks)[1]))
  tableStarts <- data.frame(line = skeleton$tableStarts,
                            page = unlist(lapply(skeleton$tableStarts, FUN = function(x) which(x < pageBreaks)[1])))
  if (length(tableStarts$line) > 0) {
    headerRows <- sort(unique(c(tableStarts$line, rows)))
  } else {
    headerRows <- rows
  }
  breaks <- c(diff(pages))
  rowSizes <- rep(0, length(rows))
  nonBreaks = which(breaks == 0)
  rowSizes[nonBreaks] <- diff(rows)[nonBreaks]
  ranges <- data.frame(start = rows[nonBreaks], end = rows[nonBreaks] + rowSizes[nonBreaks] - 1)
  rowLines <- unlist(apply(ranges, MARGIN = 1, FUN = function(x) x[1]:x[2]))
  if (length(rowLines) == 0) {
    rowLines <- headerRows
  }
  occurrences <- m[rowLines,]
  if (length(rowLines) > 1) {
    rightShift <- shift.right(occurrences)
    leftShift <- shift.left(occurrences)
  } else {
    rightShift <- c(F, occurrences[-length(occurrences)])
    leftShift <- c(occurrences[-1], F)
  }
  occurrences <- 1 * (occurrences | (rightShift & leftShift & !occurrences))
  if (length(rowLines) > 1) {
    rightShift <- shift.right(occurrences)
  } else {
    rightShift <- c(F, occurrences[-length(occurrences)])
  }
  starts <- xor(occurrences, rightShift) & occurrences
  if (length(rowLines) > 1) {
    startPos <- apply(starts, MARGIN = 1, FUN = function(x) which(x))
  } else {
    startPos <- list(which(starts))
  }
  startPos <- startPos[which(lengths(startPos) > 0)]
  detectedPatterns <- startPos[which(!duplicated(startPos))]
  filledRows <- vector()
  for (i in 1:length(rows)) {
    start <- rows[i]
    fills <- vector()
    end <- NA
    startPage <- 1
    endPage <- 1
    if (i < length(rows)) {
      end <- rows[i + 1] - 1
      startPage <- which(start < pageBreaks)[1]
      endPage <- which(end < pageBreaks)[1]
    }
    if (i < length(rows) && startPage == endPage) {
      if (end > start) {
        lines <- start:end
        fills <- lines[which(rowSums(m[start:end,]) > 0)]
      } else if (sum(m[start,]) > 0) {
        fills <- start
      }
    } else {
      if (is.na(end)) {
        if (max(rowSizes) > 0) {
          end <- start + max(rowSizes) - 1
          if (end > max(pageBreaks)) {
            end <- max(pageBreaks)
          }
        } else {
          page <- sum((start) > pageBreaks) + 1
          end <- pageBreaks[page]
        }
      }
      patterns <- m[start:end,]
      fills <- start
      if (end > start) {
        right <- shift.right(patterns)
        left <- shift.left(patterns)
        filledPatterns <- 1 * (patterns | (right & left & !patterns))
        right <- shift.right(filledPatterns)
        startPatterns <- xor(filledPatterns, right) & filledPatterns
        lastFill <- 1
        blankLines <- 0
        seqBlankLines <- 0
        lastPage <- sum(start > pageBreaks) + 1
        endPage <- sum(end > pageBreaks) + 1
        for (j in 2:nrow(startPatterns)) {
          startPattern <- as.numeric(which(startPatterns[j,]))
          page <- sum((start + j - 1) > pageBreaks) + 1
          tableStart <- tableStarts$line[which(tableStarts$page == page)]
          if (lastPage == endPage && (start + j - 1) %in% noTableLines) {
            break
          }
          if (length(startPattern) == 0) {
            blankLines <- blankLines + 1
            seqBlankLines <- seqBlankLines + 1
            if (seqBlankLines == 4 && lastPage == endPage) {
              break
            }
          } else {
            seqBlankLines <- 0
            aligned <- startPattern[1] %in% unlist(detectedPatterns) && startPattern[1] > min(unlist(detectedPatterns))
            comparisons <- lapply(detectedPatterns, FUN = function(x) sum(startPattern %in% unique(c(x - 2, x - 1, x, x + 1, x + 2))) >= 0.75*length(startPattern))
            similarities <- lapply(detectedPatterns, FUN = function(x) .cosine(startPattern, x) > 0.99 && sum(startPattern %in% x) > 0.25*length(startPattern))
            similar <- sum(comparisons == T) > 0 || sum(similarities == T) > 0 || aligned
            isHeader <- (start + j - 1) %in% headerPos
            if (length(tableStart) == 1) {
              afterTableStart <- ifelse(start + j - 1 > tableStart, T, F)
            } else {
              afterTableStart <- T
            }
            if (length(startPattern) == 1 && sum(patterns[j,])/length(patterns[j,]) > 0.5 && startPattern == min(unlist(detectedPatterns))) {
              startPattern = NULL
            }
            if (similar &&
                afterTableStart &&
                !isHeader &&
                ((j - lastFill) <= 4 || page > lastPage) &&
                length(startPattern) > 0 &&
                !(start + j - 1) %in% noTableLines) {
              fills <- c(fills, start + j - 1)
              lastFill <- j
              lastPage <- page
            }
          }
        }
      }
      filledPages <- data.frame(page = unlist(lapply(fills, FUN = function(x) sum(x > pageBreaks))) + 1, row = fills)
      filledPages <- filledPages %>% group_by(page) %>% summarize(start = min(row), end = max(row), .groups = "keep")
      lines <- unlist(apply(filledPages, MARGIN = 1, FUN = function(x) x[2]:x[3]))
      if (length(lines) > 1) {
        fills <- unique(c(fills, lines[which(rowSums(m[lines,]) > 0)]))
      } else {
        if (lines[which(sum(m[lines,]) > 0)]) {
          fills <- unique(c(fills, lines))
        }
      }
    }
    filledRows <- c(filledRows, fills)
    if (rowSizes[i] == 0) {
      rowSizes[i] <- max(fills) - start + 1
    }
  }
  filledRows <- sort(unique(filledRows))

  if (length(headerRows) > 1) {
    pattern <- colSums(m[headerRows,])
    pattern <- ifelse(pattern > 0, 1, 0)
  } else {
    pattern <- m[headerRows,]
  }
  right <- c(FALSE, pattern[-length(pattern)])
  startPattern <- xor(pattern, right) & pattern
  colStart <- which(startPattern)
  colEnd <- c(colStart[-1] - 1, ncol(m))
  removeRows <- vector()
  for (j in 1:length(colStart)) { # Identify lines that include columns that are not included in the majority of the other lines.
    if (colEnd[j] > colStart[j]) {
      if (length(filledRows) > 1) {
        colRows <- which(rowSums(m[filledRows, colStart[j]:colEnd[j]]) > 0)
      } else {
        colRows <- which(sum(m[filledRows, colStart[j]:colEnd[j]]) > 0)
      }
    } else {
      colRows <- which(m[filledRows, colStart[j]:colEnd[j]] > 0)
    }
    fillScore <- length(colRows) / length(filledRows)
    if (fillScore < 0.1) {
      removeRows <- c(removeRows, colRows)
    }
  }
  keepRows <- which(filledRows %in% rows)
  removeRows <- setdiff(removeRows, keepRows)
  if (length(removeRows) > 0) { # Remove the identified lines.
    filledRows <- filledRows[-removeRows]
  }
  skeleton$significantRows <- rows
  skeleton$rows <- filledRows
  skeleton$pattern <- pattern
  skeleton$startPattern <- startPattern
  return(skeleton)
}

# Extract table skeletons (i.e., table descriptions).
# @param positions List of table positions. This is the output of the function .extractTablePositions.
# @param m Binary matrix describing the text of a PDF file.
# @param minCols Number of columns a table must minimal consist of.
# @param maxDistance Number of text lines that can maximally exist between the start of two table rows.
# @param pageBreaks Number of text lines after which a new page begins.
# @param headerPos Vector of absolute line numbers that have been identified as table headers.
# @return A list of table skeletons. Each skeleton include the binary pattern of the most significant lines, the start pattern of the most
# significant lines and the lines that belong to a table.
.extractTableSkeletons <- function(positions, m, minCols, maxDistance, pageBreaks, headerPos) {
  skeletons <- list()
  if (length(positions$rows) == 1) {
    minCols = 0
  }
  positionType <- positions$type
  if (length(positions$rows) > 0) {
    for (i in 1:length(positions$rows)) { # extract table position information and merge table positions if they have similar start patterns
      rows <- positions$rows[[i]]
      # remove rows that are too far away from the other rows
      pageStartDiff <- sapply(rows, FUN = function(x) {
        y = x - c(1, pageBreaks)
        return(min(y[which(y >= 0)]))
      })
      pages <- sapply(rows, FUN = function(x) sum(x >= pageBreaks) + 1)
      rowStructure <- data.frame(row = rows, pageStart = pageStartDiff, pageBreak = c(0, diff(pages)), lastOnPage = c(diff(pages), 1), lag = c(0, diff(rows)))
      rowStructure <- rowStructure %>%
        mutate(pageStartPatternDiff = abs(pageStartDiff - pageStartDiff[1]) * pageBreak) %>%
        as.data.table()
      removeCandidates <- rowStructure[pageBreak == 1 & lastOnPage == 1 & pageStartDiff > 25 & lag > 25 & pageStartPatternDiff > 1, row]
      if (length(removeCandidates) > 0) {
        letters1 <- colSums(m[rows,])
        pattern1 <- ifelse(letters1 > 0, 1, 0)
        right1 <- c(F, pattern1[-length(pattern1)])
        startPattern1 <- xor(pattern1, right1) & pattern1
        for (j in 1:length(removeCandidates)) {
          pattern2 <- m[removeCandidates[j],]
          right2 <- c(F, pattern2[-length(pattern2)])
          startPattern2 <- xor(pattern2, right2) & pattern2
          if (.comparePatterns(startPattern1, startPattern2, similarity = 0.8)) {
            removeCandidates <- removeCandidates[-j]
          }
        }
      }
      if (length(removeCandidates) > 0) {
        rows <- rows[-which(rows %in% removeCandidates)]
      }

      if (length(rows) > 1) {
        letters <- colSums(m[rows,])
      } else {
        letters <- m[rows,]
      }
      pattern <- ifelse(letters > 0, 1, 0)
      right <- c(F, pattern[-length(pattern)])
      startPattern <- xor(pattern, right) & pattern
      found = F
      if (length(skeletons) > 0) {
        for (j in 1:length(skeletons)) {
          refPattern <- skeletons[[j]]$pattern
          if (positionType == "columnPattern") {
            refStartPattern <- skeletons[[j]]$startPattern
            if (.comparePatterns(refStartPattern, startPattern, similarity = 0.75)) {
              pattern <- refPattern | pattern
              right <- c(F, pattern[-length(pattern)])
              startPattern <- xor(pattern, right) & pattern
              skeletons[[j]] <- list(startPattern = startPattern,
                                     pattern = pattern,
                                     rows = c(skeletons[[j]]$rows, rows),
                                     tableStarts = skeletons[[j]]$tableStarts,
                                     noTableLines = positions$noTableLines)
              found = T
              break
            }
          } else {
            pattern <- refPattern | pattern
            right <- c(F, pattern[-length(pattern)])
            startPattern <- xor(pattern, right) & pattern
            skeletons[[j]] <- list(startPattern = startPattern,
                                   pattern = pattern,
                                   rows = c(skeletons[[j]]$rows, rows),
                                   tableStarts = skeletons[[j]]$tableStarts,
                                   noTableLines = positions$noTableLines)
          }
        }
      } else {
        found = T
        skeletons[[length(skeletons) + 1]] <- list(startPattern = startPattern,
                                                   pattern = pattern,
                                                   rows = rows,
                                                   tableStarts = positions$tableStarts,
                                                   noTableLines = positions$noTableLines)
      }
      if (positionType == "columnPattern" && !found) {
        skeletons[[length(skeletons) + 1]] <- list(startPattern = startPattern,
                                                   pattern = pattern,
                                                   rows = rows,
                                                   tableStarts = positions$tableStarts,
                                                   noTableLines = positions$noTableLines)
      }
    }
  }
  result <- list()
  if (length(skeletons) > 0) {
    for (k in 1:length(skeletons)) { # fill table rows
      skeleton <- skeletons[[k]]
      if (sum(skeleton$startPattern) >= minCols || (length(skeletons) == 1 && sum(skeleton$startPattern) >= 2)) {
        result[[length(result) + 1]] <- .fillRows(m, skeleton, maxDistance, pageBreaks, headerPos)
      }
    }
  }
  return(result)
}

# Extracts date values from a given vector of characters.
# @param column A vector of characters (i.e., a column in a table).
# @return A vector of date values.
.prepareDates <- function(column) {
  datePatterns <- c("^([0-9]{2}\\.[0-9]{2}\\.[0-9]{2})$",
                    "^([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})$",
                    "^([0-9]{2}-[0-9]{2}-[0-9]{2})$",
                    "^([0-9]{2}-[0-9]{2}-[0-9]{4})$",
                    "^([0-9]{2}\\/[0-9]{2}\\/[0-9]{2})$",
                    "^([0-9]{2}\\/[0-9]{2}\\/[0-9]{4})$",
                    "^([0-9]{2}\\/[0-9]{2})$")
  datePattern <- paste(datePatterns, collapse = "|")
  dates <- str_trim(column[str_detect(column, datePattern)])
  return(dates)
}

# Extracts potential date values from a given vector of characters.
# @param column A vector of characters (i.e., a column in a table).
# @return A vector of date values.
.prepareDateGuesses <- function(column) {
  datePatterns <- c("^(\\/)$",
                    "^(\\/[0-9]{2})$")
  datePattern <- paste(datePatterns, collapse = "|")
  dates <- str_trim(column[str_detect(column, datePattern)])
  return(dates)
}

# Extracts numeric values from a given vector of characters.
# @param column A vector of characters (i.e., a column in a table).
# @param asNumeric Logical indicating if the result is a numeric vector (default) or a character vector.
# @return A vector of numeric values.
.prepareNumerics <- function(column, asNumeric = TRUE) {
  values <- column %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all(pattern = "'|/|\u20AC|(eur)|(euro)|(stck)|(stk\\.)|(stk)|(tk)|(st\\.)|(st)", replacement = "") %>%
    str_replace_all(pattern = "\\s", replacement = "")
  points <- str_detect(values, "\\.")
  commas <- str_detect(values, ",")
  if (sum(points, na.rm = T) == 0 && sum(commas, na.rm = T) > 0) {
    values <- values %>%
      str_replace_all(pattern = ",", replacement = "\\.")
  } else if (sum(points, na.rm = T) > 0 && sum(commas, na.rm = T) > 0) {
    commaSel <- values[which(commas)]
    pointSel <- values[which(points)]
    posCommas <- str_locate(commaSel, pattern = ",")[,1]
    posPoints <- str_locate(pointSel, pattern = "\\.")[,1]
    if (mean(str_length(commaSel) - posCommas) == 2) {
      values <- values %>%
        str_replace_all(pattern = "\\.", replacement = "") %>%
        str_replace_all(pattern = ",", replacement = "\\.")
    } else if (mean(str_length(pointSel) - posPoints) == 2) {
      values <- values %>%
        str_replace_all(pattern = ",", replacement = "")
    }
  } else if (sum(points, na.rm = T) > 0 && sum(commas, na.rm = T) == 0) {
    pointSel <- values[which(points)]
    posPoints <- str_locate(pointSel, pattern = "\\.")[,1]
    if (mean(str_length(pointSel) - posPoints) > 2) {
      values <- values %>%
        str_replace_all(pattern = "\\.", replacement = "")
    }
  }
  if (asNumeric) {
    suppressWarnings(values <- as.numeric(values))
  }
  return(values)
}

# Builds a table for each extracted skeleton. Each table is represented by a dataframe.
# @param skeletons A list of table skeletons (i.e., table pattern and text lines representing the table).
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @param m Binary matrix describing the text of a PDF file.
# @return A list of tables.
.buildTables <- function(skeletons, words, m) {
  numLines <- unlist(words %>% group_by(page) %>% summarize(lines = max(line)) %>% select(lines))
  numLines <- cumsum(c(0, numLines[-length(numLines)]))
  words$absLine <- as.numeric(numLines[words$page] + words$line)
  w <- matrix(rep(" ", nrow(m) * ncol(m)), ncol = ncol(m))
  tables <- list()
  for (i in 1:length(skeletons)) {
    rows <- skeletons[[i]]$rows
    significantRows <- skeletons[[i]]$significantRows
    if (length(significantRows) > 1) {
      pattern <- ifelse(colSums(m[significantRows,]) > 0, 1, 0)
    } else {
      pattern <- ifelse(m[significantRows,] > 0, 1, 0)
    }
    right <- c(F, pattern[-length(pattern)])
    startPattern <- xor(pattern, right) & pattern
    colStart <- which(startPattern)
    colEnd <- c(colStart[-1] - 1, ncol(m))
    tableMatrix <- matrix(rep(" ", length(rows) * length(colStart)), ncol = length(colStart))
    removeRows <- vector()
    keepCols <- vector()
    for (j in 1:length(colStart)) { # Identify sparsely filled columns and remove these columns as well as corresponding rows.
      selection <- words %>%
        filter(absLine %in% rows & start >= colStart[j] & start <= colEnd[j]) %>%
        group_by(absLine) %>%
        summarize(value = paste(value, collapse = " "))
      if (colEnd[j] > colStart[j]) {
        if (length(rows) > 1) {
          filledRows <- which(rowSums(m[rows, colStart[j]:colEnd[j]]) > 0)
        } else {
          filledRows <- which(sum(m[rows, colStart[j]:colEnd[j]]) > 0)
        }
      } else {
        filledRows <- which(m[rows, colStart[j]:colEnd[j]] > 0)
      }
      fillScore <- length(filledRows) / length(rows)
      if (fillScore >= 0.1 || length(significantRows) == length(filledRows)) {
        tableMatrix[which(rows %in% selection$absLine),j] <- selection$value
        nonNumerics <- which(is.na(.prepareNumerics(tableMatrix[,j])))
        numericsLen <- length(rows) - length(nonNumerics)
        empties <- which(str_trim(tableMatrix[,j]) == "")
        nonNumericsLen <- length(rows) - length(intersect(nonNumerics, empties)) - numericsLen
        numericsRatio <- numericsLen / (length(rows) - length(empties))
        if (numericsRatio > 0.8 && nonNumericsLen > 0) {
          charPos <- setdiff(nonNumerics, empties)
          chars <- tableMatrix[charPos, j]
          chars <- str_replace_all(chars, "A|L|O|/", "")
          if (any(str_length(chars) - str_count(chars, pattern = "[0-9]") > 1)) {
            removeCandidates <- setdiff(nonNumerics, empties)
            removeRows <- c(removeRows, removeCandidates)
            if (length(filledRows) > length(removeCandidates)) {
              keepCols <- c(keepCols, j)
            }
          } else {
            keepCols <- c(keepCols, j)
          }
        } else {
          keepCols <- c(keepCols, j)
        }
      } else {
        removeRows <- c(removeRows, filledRows)
      }
    }
    keepRows <- which(rows %in% significantRows)
    removeRows <- setdiff(removeRows, keepRows)
    if (length(removeRows) == 0) {
      if (nrow(tableMatrix) == 1) {
        data <- as.data.frame(t(tableMatrix[, keepCols]))
      } else {
        data <- as.data.frame(tableMatrix[, keepCols])
      }
    } else {
      data <- as.data.frame(tableMatrix[-removeRows, keepCols])
      rows <- rows[-removeRows]
    }
    tables[[length(tables) + 1]] <- list(data = data,
                                         significantLines = significantRows,
                                         lines = rows)
  }
  return(tables)
}

# Identifies the type of a column by its values.
# @param values A vector including the values of a column.
# @return The type name.
.getColumnType <- function(values) {
  content <- values[which(values != "")]
  length <- length(content)
  dateLength <- length(.prepareDates(content))
  numerics <- .prepareNumerics(content)
  numLength <- sum(!is.na(numerics))
  possibleNumerics <- numeric(0)
  if (numLength < length) {
    possibleNumerics <- .prepareNumerics(str_replace_all(content, "A|L|O|/", "0"))
  }
  if (length == 0) {
    return("null")
  } else if (dateLength == length) {
    return("date")
  } else if (numLength == length || (numLength > 0.5*length && length(possibleNumerics) == length)) {
    if (numLength < length) {
      numerics <- numerics[which(!is.na(numerics))]
    }
    numerics <- ifelse(numerics == 0, 1, numerics)
    power <- floor(mean(floor(log10(abs(numerics)) + 1), na.rm = T))
    if (length > 2 && sum(diff(numerics) == 1) == (numLength - 1)) {
      return(paste0("pos_", power))
    } else if (sum(round(numerics) == numerics) < numLength) {
      return(paste0("double_", power))
    } else {
      return(paste0("integer_", power))
    }
  } else if (sum(str_detect(content, "[0-9]")) == length &&
             sum(str_detect(content, " ")) == 0 &&
             sum(str_detect(content, "[\\p{P}\\p{S}&&[^.-]]")) == 0 &&
             sum(str_length(content) > 3) == length) {
    return("id")
  } else {
    freq = table(content)
    if (sum(str_replace_all(content, "[:punct:]", "") == "") == length(content)) {
      return("punct")
    } else if (sum(str_to_lower(content) %in% pkg.conf$currencyUnits) == length ||
               sum(str_to_lower(content) %in% pkg.conf$quantityUnits) == length ||
               (length(freq) == 1 && length > 2 && sum(str_detect(content, " ")) == 0)) {
      return(paste0("unit_", content[1]))
    } else {
      return("char")
    }
  }
}

# Compares two column types. A column type is identified by the function getType.
# @param type1 Name of the type of one column.
# @param type2 Name of the type of another column.
# @return Logical value indicating if the two types are equal (or at least similar) to each other.
.compareTypes <- function(type1, type2) {
  if (type1 == type2) {
    return(T)
  } else if (str_starts(type1, "integer|double") && str_starts(type2, "integer|double")) {
    power1 = as.numeric(unlist(str_split(type1, "_"))[2])
    power2 = as.numeric(unlist(str_split(type2, "_"))[2])
    if (abs(power1 - power2) <= 1) {
      return(T)
    } else {
      return(F)
    }
  } else {
    return(F)
  }
}

# Identifies and rearranges table cells whose content is spanned over multiple columns. This function also removes duplicated columns.
# @param tables A list of extracted tables.
# @return A list of extracted and cleared tables.
.clearTables <- function(tables) {
  for (i in 1:length(tables)) {
    lastCol <- NA
    table <- tables[[i]]$data
    lines <- tables[[i]]$lines
    sigLines <- tables[[i]]$significantLines
    sigRows <- which(lines %in% sigLines)
    otherRows <- which(!lines %in% sigLines)
    newTable <- vector()
    colTypes <- vector()
    for (j in 1:ncol(table)) {
      table[,j] <- str_trim(table[,j])
      sigValues <- table[sigRows, j]
      values <- table[otherRows, j]
      sigValueType <- .getColumnType(sigValues)
      valueType <- .getColumnType(values)
      if (sum(values == "") < length(values)) {
        if (!.compareTypes(sigValueType, valueType)) {
          column1 <- rep("", length(lines))
          column2 <- column1
          column1[sigRows] <- sigValues
          column2[otherRows] <- values
          if (is.na(lastCol)) {
            if (sigValueType != "punct") {
              newTable <- cbind(newTable, column1)
              colTypes <- c(colTypes, sigValueType)
            }
            if (valueType != "punct") {
              newTable <- cbind(newTable, column2)
              colTypes <- c(colTypes, valueType)
            }
            if (valueType == "char") {
              lastCol <- ncol(newTable)
            }
          } else {
            newTable[,lastCol] <- paste(newTable[,lastCol], column2, sep = " ")
            if (sigValueType != "punct") {
              newTable <- cbind(newTable, column1)
              colTypes <- c(colTypes, sigValueType)
            }
          }
        } else {
          if (sigValueType != "punct") {
            newTable <- cbind(newTable, table[,j])
            colTypes <- c(colTypes, sigValueType)
            if (valueType == "null" || valueType == "char") {
              lastCol <- ncol(newTable)
            } else {
              lastCol <- NA
            }
          }
        }
      } else {
        if (sigValueType != "punct") {
          newTable <- cbind(newTable, table[,j])
          colTypes <- c(colTypes, sigValueType)
          if (sigValueType == "char") {
            lastCol <- ncol(newTable)
          } else {
            lastCol <- NA
          }
        }
      }
    }
    nonDuplicates <- which(!duplicated(as.list(as.data.frame(newTable))))
    doubles <- which(str_starts(colTypes, "double"))
    newColumns <- sort(unique(c(nonDuplicates, doubles)))
    newTable <- newTable[, newColumns] # remove duplicate columns
    if (nrow(table) == 1) {
      tables[[i]]$data <- as.data.frame(t(newTable))
    } else {
      tables[[i]]$data <- as.data.frame(newTable)
    }
  }
  return(tables)
}

# Checks all elements in a vector (i.e. column) for being a candidate for an ID.
# @param column The vector of elements (i.e. a column of a table).
# @param tableRows Numeric vector with a counter variable for each table row.
# @return A vector of positions of those elements that are a candidate for an ID.
.getIDCandidates <- function(column, tableRows = NA) {
  vowels <- str_count(column, "[A|E|I|O|U]")
  consonants <- str_count(column, "[B|C|D|F|G|H|J|K|L|M|N|P|Q|R|S|T|V|W|X|Y|Z]")
  consonantRatio <- consonants / vowels
  consonantRatio <- ifelse(is.na(consonantRatio), 0, ifelse(consonantRatio == Inf, 0, consonantRatio))
  idCandidates <- which((consonantRatio > 3 | str_count(column, "[0-9]") / str_length(column) > 0.2) & column == str_to_upper(column) &
                          str_length(column) > 3 & !str_detect(column, "[\\p{P}\\p{S}&&[^.-]]") & str_count(column, "\\s") == 0)
  if (!any(is.na(tableRows))) {
    candidateDF <- data.frame(candidate = idCandidates, value = column[idCandidates], row = tableRows[idCandidates])
    idCandidates <- candidateDF %>%
      group_by(row) %>%
      mutate(dup = duplicated(value)) %>%
      filter(dup == FALSE) %>%
      ungroup() %>%
      select(candidate) %>%
      unlist() %>%
      as.numeric()
  }
  return(idCandidates)
}

# Parses a table column and returns a suitable column header.
# @param column A vector including the values of the corresponding column.
# @param numerics A vector including all numeric values of the column.
# @param quantityNum The number of so far identified columns that might be an order's quantity.
# @param priceNum The number of so far identified columns that might be the price.
# @param isLast Boolean flag indicating the last column.
# @return The header of this column as string value.
.annotateColumn <- function(column, numerics, quantityNum, priceNum, isLast) {
  len <- length(which(str_trim(column) != ""))
  toleranceLen <- round(len - 0.25 * len)
  dates <- .prepareDates(column)
  dateGuesses <- .prepareDateGuesses(column)
  numerics <- ifelse(numerics == 0, 1, numerics)
  power <- floor(mean(floor(log10(abs(numerics))), na.rm = T))
  chars <- str_trim(column)
  chars <- chars[which(chars != "")]
  leadingZeros <- chars %>% str_starts("0")
  includedNumParts <- str_count(chars, pattern = "([0-9])+")
  idCandidates <- .getIDCandidates(column)
  name <- NA
  if (length(dates) == len) {
    name <- "date"
  } else if (length(dateGuesses) == len) {
    name <- "date_guess"
  } else if (length(numerics) == len && max(includedNumParts) <= 3) {
    if (sum(diff(numerics) == 10^power) == (len - 1) &&
        sum(numerics %% 1 == 0) == length(numerics) &&
        mean(numerics) < 99999 &&
        min(numerics) < 9999 &&
        sum(((numerics / 10^power) %% 1) == 0) == length(numerics)) {
      name <- "pos"
    } else if (sum(diff(numerics / 10^power) == 1) == (len - 1) && len > 3) {
      name <- "pos_guess"
    } else if (sum(round(numerics) == numerics) < len || (mean(numerics) > 100 && quantityNum > 1) || (priceNum == 1 && isLast)) {
      name <- "price"
    } else if (mean(numerics) < 99999 &&
               (min(numerics) < 9999 || (quantityNum == 0 && min(numerics) < 99999)) &&
               (mean(numerics) < 9999 || !any(leadingZeros))) {
      if (sum(diff(numerics) == 0) == (len - 1)) {
        name <- "quantity_guess"
      } else {
        name <- "quantity"
      }
    } else {
      name <- "id"
    }
  } else if (length(numerics) >= toleranceLen && max(includedNumParts) <= 3) {
    if (sum(diff(numerics) == 1) == (len - 1)) {
      name <- "pos_guess"
    } else if (sum(round(numerics) == numerics) < length(numerics)) {
      name <- "price_guess"
    } else if (mean(numerics) < 99999) {
      name <- "quantity_guess"
    }
  } else if (length(idCandidates) == len) {
    name <- "id"
  }else if (str_to_lower(chars[1]) %in% pkg.conf$currencyUnits) {
    name <- "currency"
  } else if (str_to_lower(chars[1]) %in% pkg.conf$quantityUnits) {
    name <- "quantityUnit"
  } else if (sum(str_detect(chars, "[0-9]")) == len &&
             sum(str_detect(chars, " ") == 0) &&
             sum(str_detect(chars, "[\\p{P}\\p{S}&&[^.-]]")) == 0 &&
             sum(str_length(chars) > 3) == len) {
    name <- "id"
  }
  return(name)
}

# Corrects the vector of significant lines if there are more positions, quantities or prices than significant lines so far.
# @param column The column that included more values than significant lines.
# @param sigLines Numeric vector of so far identified significant lines.
# @param lines Numeric vector of identified lines.
# @param maxLength Maximal length (in lines) of a table entry.
# @return Numeric vector of corrected significant lines.
.correctSignificantLines <- function(column, sigLines, lines, maxLength) {
  filledLines <- lines[which(column != "")]
  diff <- sapply(sigLines, FUN = function(x) {
    y = x - filledLines
    return(y[which(abs(y) == min(abs(y)))[1]])
  })
  if (sum(diff == diff[1]) == length(diff)) {
    correction <- diff[1]
  } else {
    correction <- 0
  }
  candidates <- filledLines + correction
  newCandidates <- which(!filledLines %in% sigLines)
  removeCandidates <- setdiff(which((candidates[newCandidates] - 1) %in% sigLines), which((candidates[newCandidates] + 1) %in% sigLines))
  if (length(removeCandidates) > 0) {
    candidates <- candidates[-newCandidates[removeCandidates]]
  }
  newSigLines <- which(!candidates %in% sigLines)
  if (length(newSigLines) == length(filledLines) - length(sigLines) && length(filledLines) <= maxLength) {
    sigLines <- candidates
  }
  return(sigLines)
}

# Corrects the vector of significant lines if there are more positions, quantities or prices than significant lines and if the maximal position
# number is larger than the number of significant lines.
# @param column The column that included more values than significant lines.
# @param sigLines Numeric vector of so far identified significant lines.
# @param lines Numeric vector of identified lines.
# @param maxLength Maximal length (in lines) of a table entry.
# @param positions Data frame of identified position columns.
# @return Numeric vector of corrected significant lines.
.checkSignificantLines <- function(column, sigLines, lines, maxLength, positions) {
  if (nrow(positions) > 0 && "hard" %in% positions$type) {
    maxPos <- positions %>%
      filter(type == "hard") %>%
      select(max) %>%
      first() %>%
      unlist() %>%
      as.numeric()
    if (all(length(sigLines) != maxPos)) {
      sigLines <- .correctSignificantLines(column, sigLines, lines, maxLength)
    }
  } else {
    sigLines <- .correctSignificantLines(column, sigLines, lines, maxLength)
  }
  return(sigLines)
}

# Tests if there exists a pair of columns that represents the same data type can be joined to one column without loosing information.
# @param info Dataframe describing the information type (including all columns that belong to the type).
# @param table Extracted order table.
# @return A list of the updated order table, the updated information type data frame and the indices of the table columns that can be removed.
.joinInfoColumns <- function(info, table) {
  filledPositions <- lapply(table[,info$col], FUN = function(x) which(nchar(x) > 0))
  removableColumns = vector()
  if (length(filledPositions) > 1) {
    names(filledPositions) <- info$col
    pairs <- data.frame(col1 = vector(), col2 = vector())
    for (i in 1:(length(filledPositions) - 1)) {
      for (j in (i + 1):length(filledPositions)) {
        if (!any(filledPositions[[i]] %in% filledPositions[[j]])) {
          pairs <- rbind(pairs, data.frame(col1 = i, col2 = j))
        }
      }
    }
    if (nrow(pairs) > 0) {
      frequencies <- table(unlist(pairs))
      for (i in 1:nrow(pairs)) {
        if (frequencies[[as.character(pairs$col1[i])]] == 1 && frequencies[[as.character(pairs$col2[i])]] == 1) {
          table[filledPositions[[pairs$col2[i]]], info$col[pairs$col1[i]]] <- table[filledPositions[[pairs$col2[i]]], info$col[pairs$col2[i]]]
          removableColumns <- c(removableColumns, info$col[pairs$col2[i]])
        }
      }
      if (length(removableColumns) > 0) {
        info <- info[-which(info$col %in% removableColumns),]
      }
    }
  }
  return(list(table = table, info = info, removableColumns = removableColumns))
}

# Generates a table header based on the format of the content of each column.
# @param tables A list of extracted tables.
# @return A list of extracted tables with enhanced header.
.annotateTables <- function(tables) {
  removeTables <- vector()
  for (i in 1:length(tables)) {
    table <- tables[[i]]$data
    sigLines <- tables[[i]]$significantLines
    lines <- tables[[i]]$lines
    names <- vector()
    prices <- data.frame(col = vector(), mean = vector(), type = vector())
    quantities <- data.frame(col = vector(), type = vector())
    positions <- data.frame(col = vector(), type = vector())
    dates <- data.frame(col = vector(), type = vector())
    addedColumns <- vector()
    columnLengths <- apply(table, MARGIN = 2, FUN = function(x) length(which(str_length(x) > 0)))
    for (j in 1:ncol(table)) {
      if (length(addedColumns) > 0) {
        j <- j + length(which(addedColumns <= j))
      }
      column <- table[,j]
      numerics <- as.numeric(na.omit(.prepareNumerics(column)))
      name <- .annotateColumn(column, numerics, nrow(quantities), nrow(prices), j == ncol(table))
      sigLength <- length(sigLines)
      if (!is.na(name)) {
        names[j] <- name
        if (length(which(column != "")) > sigLength && name %in% c("pos", "price", "quantity")) {
          maxColumnLength = max(columnLengths[-j])
          sigLines <- .checkSignificantLines(column, sigLines, lines, maxColumnLength, positions)
          if (length(sigLines) == sigLength) {
            newColumn <- rep("", length(column))
            newColumn[which(!lines %in% sigLines)] <- column[which(!lines %in% sigLines)]
            newName <- "X"
            table <- add_column(.data = table, !!(newName) := newColumn, .after = j, .name_repair = "minimal")
            table[which(!lines %in% sigLines),j] <- ""
            column <- table[,j]
            numerics <- as.numeric(na.omit(.prepareNumerics(column)))
            name <- .annotateColumn(column, numerics, nrow(quantities), nrow(prices), j == ncol(table))
            names[j] <- name
            names[j + 1] <- paste("X", j + 1, sep = "")
            addedColumns <- c(addedColumns, j + 1)
          }
        }
        if (name == "pos") {
          positions <- rbind(positions, data.frame(col = j, type = "hard", min = min(numerics), max = length(numerics)))
        } else if (name == "pos_guess") {
          positions <- rbind(positions, data.frame(col = j, type = "soft", min = min(numerics), max = length(numerics)))
        } else if (name == "price") {
          prices <- rbind(prices, data.frame(col = j, mean = mean(numerics), type = "hard"))
        } else if (name == "price_guess") {
          prices <- rbind(prices, data.frame(col = j, mean = mean(numerics), type = "soft"))
        } else if (name == "quantity") {
          quantities <- rbind(quantities, data.frame(col = j, type = "hard"))
        } else if (name == "quantity_guess") {
          quantities <- rbind(quantities, data.frame(col = j, type = "soft"))
        } else if (name == "date") {
          dates <- rbind(dates, data.frame(col = j, type = "hard"))
        } else if (name == "date_guess") {
          dates <- rbind(dates, data.frame(col = j, type = "soft"))
        }
      } else {
        names[j] <- paste("X", j, sep = "")
      }
    }

    ### Define price columns
    removableColumns <- vector()
    joinedPriceColumns <- .joinInfoColumns(info = prices, table = table)
    if (length(joinedPriceColumns$removableColumns) > 0) {
      table <- joinedPriceColumns$table
      prices <- joinedPriceColumns$info
      removableColumns <- c(removableColumns, joinedPriceColumns$removableColumns)
    }
    if (nrow(prices) == 1) {
      if (nrow(quantities) == 2) {
        quantityPricePos <- which(quantities$col > prices$col)
        quantityPos <- quantities$col[-quantityPricePos]
        totalPricePos <- quantities$col[quantityPricePos]
        if (length(totalPricePos) == 1) {
          quantityGuess <- .prepareNumerics(table[, quantityPos])
          unitPriceGuess <- .prepareNumerics(table[, prices$col[1]])
          totalPriceGuess <- .prepareNumerics(table[, totalPricePos])
          if (sum(quantityGuess * unitPriceGuess == totalPriceGuess, na.rm = T) == sum(is.na(totalPriceGuess))) {
            names[prices$col[1]] <- "unitPrice"
            names[totalPricePos] <- "totalPrice"
            quantities <- quantities[-quantityPricePos,]
          } else if (sum(quantityGuess * totalPriceGuess == unitPriceGuess, na.rm = T) == sum(is.na(totalPriceGuess))) {
            names[prices$col[1]] <- "totalPrice"
            names[totalPricePos] <- "unitPrice"
            quantities <- quantities[-quantityPricePos,]
          } else {
            names[prices$col[1]] <- "price"
          }
        } else {
          names[prices$col[1]] <- "price"
        }
      } else {
        names[prices$col[1]] <- "price"
      }
    } else if (nrow(prices) == 2) {
      lowPos <- which(prices$mean == min(prices$mean))[1]
      highPos <- 3 - lowPos
      names[prices$col[lowPos]] <- "unitPrice"
      names[prices$col[highPos]] <- "totalPrice"
    } else if (nrow(prices) > 2) {
      if (sum(prices$type == "hard") == 2) {
        posX <- prices$col[which(prices$type == "soft")]
        names[posX] <- paste0("X", posX)
      } else {
        priceNumerics <- lapply(table[,prices$col], .prepareNumerics)
        for (k in 1:(nrow(prices) - 1)) {
          for (l in (k + 1):nrow(prices)) {
            if (sum((priceNumerics[[k]] / priceNumerics[[l]]) %% 1 == 0, na.rm = T) == sum(!is.na(priceNumerics[[k]]))) {
              names[prices$col[k]] <- "totalPrice"
              names[prices$col[l]] <- "unitPrice"
              prices <- prices[-c(k, l), ]
              break
            } else if (sum((priceNumerics[[l]] / priceNumerics[[k]]) %% 1 == 0, na.rm = T) == sum(!is.na(priceNumerics[[l]]))) {
              names[prices$col[l]] <- "totalPrice"
              names[prices$col[k]] <- "unitPrice"
              prices <- prices[-c(k, l), ]
              break
            }
          }
        }
        if (nrow(prices) > 0 && !"pos" %in% names && prices$col[1] == 1) {
          names[1] <- "pos"
        }
      }
    }

    ### Define quantity columns
    joinedQuantityColumns <- .joinInfoColumns(info = quantities, table = table)
    if (!any(is.na(joinedQuantityColumns$removableColumns)) && length(joinedQuantityColumns$removableColumns) > 0) {
      table <- joinedQuantityColumns$table
      quantities <- joinedQuantityColumns$info
      removableColumns <- c(removableColumns, joinedQuantityColumns$removableColumns)
    }
    if (nrow(quantities) == 1) {
      names[quantities$col[1]] <- "quantity"
    } else if (nrow(quantities) > 1) {
      if (sum(quantities$type == "hard") == 1) {
        posX <- quantities$col[which(quantities$type == "soft")]
        names[posX] <- paste0("X", posX)
      } else if ("quantityUnit" %in% names) {
        unitPos <- which(names == "quantityUnit")
        unitDiff <- abs(quantities$col - unitPos)
        quantityPos <- which(unitDiff == min(unitDiff))
        if (length(quantityPos) == 1) {
          names[quantities$col[quantityPos]] <- "quantity"
          names[quantities$col[-quantityPos]] <- paste0("X", quantities$col[-quantityPos])
        }
      } else if (nrow(prices) == 1 && nrow(quantities) == 2) {
        pricePre <- which(prices$col[1] == quantities$col + 1)
        pricePost <- which(prices$col[1] == quantities$col - 1)
        if (length(pricePre) == 1 && length(pricePost) == 0) {
          names[quantities$col[-pricePre]] <- "quantity"
          names[quantities$col[pricePre]] <- "price"
          prices <- rbind(prices, data.frame(col = quantities$col[pricePre],
                                             mean = mean(.prepareNumerics(table[,quantities$col[pricePre]]), na.rm = T),
                                             type = "hard"))
          quantities <- quantities[-pricePre,]
        }
        if (length(pricePre) == 0 && length(pricePost == 0)) {
          names[quantities$col[-pricePost]] <- "quantity"
          names[quantities$col[pricePost]] <- "price"
          prices <- rbind(prices, data.frame(col = quantities$col[pricePost],
                                             mean = mean(.prepareNumerics(table[,quantities$col[pricePost]]), na.rm = T),
                                             type = "hard"))
          quantities <- quantities[-pricePost,]
        }
      }
    }

    ### Define position columns
    if (nrow(positions) == 1) {
      unitPos <- which(names == "quantityUnit")
      if (length(unitPos) == 0) {
        unitPos <- 0
      }
      if (nrow(quantities) == 0 && positions$col == unitPos - 1) {
        names[positions$col[1]] <- "quantity"
      } else if (positions$type[1] == "soft" && nrow(quantities) > 0) {
        names[positions$col[1]] <- "pos"
      }
    } else if (nrow(positions) > 1) {
      if (sum(positions$type == "hard") == 1) {
        posX <- positions$col[which(positions$type == "soft")]
        if (nrow(quantities) == 0) {
          names[posX] <- "quantity"
        } else {
          names[posX] <- paste0("X", posX)
        }
      } else if (nrow(positions) > 1 && min(positions$min) == 0 && nrow(prices) == 0) {
        pricePos <- which(positions$min == 0)
        if (length(pricePos) == 1) {
          names[positions$col[pricePos]] <- "unitPrice"
          positions <- positions[-pricePos,]
        } else if (length(pricePos) == 2) {
          names[positions$col[pricePos[1]]] <- "unitPrice"
          names[positions$col[pricePos[2]]] <- "totalPrice"
          positions <- positions[-pricePos,]
        }
      }
      if (nrow(positions == 2) && nrow(quantities) == 0) {
        posIndex <- which(positions$min == min(positions$min))[1]
        quaIndex <- 3 - posIndex
        names[positions$col[quaIndex]] <- "quantity"
      } else {
        posIndex <- which(positions$min == min(positions$min))[1]
        names[positions$col[-posIndex]] <- paste0("X", positions$col[-posIndex])
      }
    }

    ### Define date columns
    if (nrow(dates) == 1) {
      names[dates$col[1]] <- "date"
    }

    ### Update table
    if (!any(is.na(removableColumns)) && length(removableColumns) > 0) {
      table <- table[,-unique(removableColumns)]
      names <- names[-unique(removableColumns)]
    }
    tables[[i]]$data <- table
    tables[[i]]$significantLines <- sigLines


    descPos <- which(str_starts(names, "X"))
    if (length(descPos) < (ncol(table)) && ("unitPrice" %in% names || "totalPrice" %in% names || "quantity" %in% names || (length(tables) == 1 && "pos" %in% names) || (length(tables) == 1 && "id" %in% names))) {
      names(tables[[i]]$data) <- names
      descNames <- names[descPos]
      if (length(descNames) > 1) {
        content <- tables[[i]]$data[,descPos] %>% unite(descNames, sep = " ")
        names(content) <- NULL
        tables[[i]]$data[,descPos[1]] <- content
        tables[[i]]$data[,descPos[-1]] <- NULL
        names(tables[[i]]$data)[descPos[1]] <- "description"
      } else if (length(descNames) == 1) {
        names(tables[[i]]$data)[descPos] <- "description"
      }
      tables[[i]]$data <- tables[[i]]$data %>% repair_names()
      tables[[i]]$nonDescCols <- ncol(table) - length(descPos)
    } else {
      removeTables <- c(removeTables, i)
    }
  }
  if (length(removeTables) > 0) {
    tables <- tables[-removeTables]
  }
  tables <- tables %>% list.sort((nonDescCols))
  return(tables)
}

# Joins two tables that have the same structure.
# @param tables A list of extracted tables.
# @return A list of joined tables.
.joinTables <- function(tables) {
  newTables <- list()
  for (i in 1:(length(tables) - 1)) {
    table1 <- tables[[i]]$data
    names1 <- names(table1)
    tableJoin <- c(i)
    for (j in (i + 1):length(tables)) {
      table2 <- tables[[j]]$data
      names2 <- names(table2)
      if (length(names1) == length(names2) && sum(names1 != names2) == 0) {
        tableJoin <- c(tableJoin, j)
      }
    }
    if (length(tableJoin) == 1) {
      newTable <- tables[[tableJoin]]
    } else {
      data <- NA
      significantLines <- c()
      lines <- c()
      nonDescCols <- c()
      for (k in 1:length(tableJoin)) {
        tab <- tableJoin[k]
        if (k == 1) {
          data <- tables[[tab]]$data
          significantLines <- tables[[tab]]$significantLines
          lines <- tables[[tab]]$lines
          nonDescCols <- tables[[tab]]$nonDescCols
        } else {
          data <- rbind(data, tables[[tab]]$data)
          significantLines <- unique(c(significantLines, tables[[tab]]$significantLines))
          lines <- unique(c(lines, tables[[tab]]$tab))
          nonDescCols <- unique(c(nonDescCols, tables[[tab]]$nonDescCols))
        }
      }
      newTable <- list(data = data, significantLines = significantLines, lines = lines, nonDescCols = nonDescCols)
    }
    newTables[[length(newTables) + 1]] <- newTable
  }
  return(newTables)
}

# Concatenates vectors after removing duplicated entries.
# @param x Vector to be converted to one value.
# @param sep A character string to separate the terms.
# @param collapse An optional character string to separate the results.
# @return Concatenated value.
.pasteNoDuplicates <- function(x, sep = " ", collapse = NULL) {
  x <- x[!duplicated(x)]
  return(paste(x, sep = sep, collapse = collapse))
}

# Split columns that include multiple information units.
# @param tables A list of extracted tables.
# @return A list of enriched tables.
.enrichTables <- function(tables) {
  for (i in 1:length(tables)) {
    table <- tables[[i]]$data
    significantLines <- tables[[i]]$significantLines
    lines <- tables[[i]]$lines
    numRows = length(significantLines)
    tableRows <- sapply(lines, FUN = function(x) sum(x >= significantLines))
    descriptionPos <- which(names(table) == "description")
    columnLengths <- apply(table, MARGIN = 2, FUN = function(x) length(which(str_length(x) > 0)))
    if (length(descriptionPos) > 0) {
      maxColumnLength <- max(columnLengths[-descriptionPos])
      description <- str_trim(table[,descriptionPos])
      candidates <- data.frame(value = str_trim(str_extract(description, "^(.*?)(?=:)")), row = tableRows, pos = 1:length(lines), line = lines)
      idCandidates <- .getIDCandidates(description, tableRows)
      if (length(idCandidates) > 0 && sum(is.na(candidates$value[idCandidates])) == length(idCandidates)) {
        tolerance <- min(2, length(significantLines) - 1)
        if (length(idCandidates) > length(significantLines) + tolerance) {
          centers <- ceiling(length(idCandidates) / length(significantLines))
          idStructure <- data.frame(line = 1:length(tableRows), rows = tableRows)
          idStructure <- idStructure %>%
            group_by(rows) %>%
            mutate(pos = sequence(n()))
          idStructure <- idStructure %>%
            mutate(numerics = str_count(description[line], "[0-9]"),
                   chars = str_count(description[line], "[A-Z][a-z]"),
                   punct = str_count(description[line], "[:punct:]"))
          if (centers == length(idCandidates)) {
            clusterResult <- list(cluster = seq(1, centers))
          } else {
            clusterResult <- kmeans(idStructure[idCandidates,3:6], centers = centers)
          }
          for (c in 1:centers) {
            candidatePos <- which(clusterResult$cluster == c)
            candidates$value[idCandidates[candidatePos]] <- paste0("<<!ID?>>", c)
          }
        } else {
          candidates$value[idCandidates] <- "<<!ID?>>"
        }
      }
      candidates <- candidates %>% filter(!is.na(value))
      minRows <- pmin(numRows - 1, round(numRows * 0.8))
      columnCandidates <- candidates %>%
        group_by(value) %>%
        summarize(count = n(), rows = length(unique(row))) %>%
        filter(count >= minRows & rows >= minRows)
      if (nrow(columnCandidates) > 0) {
        for (j in nrow(columnCandidates):1) {
          columnName <- columnCandidates$value[j]
          columnLines <- candidates$pos[which(candidates$value == columnName)]
          columnRows <- candidates$row[which(candidates$value == columnName)]
          columnInfo <- candidates[which(candidates$value == columnName),]
          if (!is.na(columnName) && startsWith(columnName, "<<!ID?>>")) {
            columnValue <- description[columnInfo$pos]
            name <- "id"
          } else {
            columnValue <- str_trim(str_extract(description[columnInfo$pos], "(?<=:).*"))
            columnNumerics <- as.numeric(na.omit(.prepareNumerics(columnValue)))
            name <- .annotateColumn(columnValue, columnNumerics, 0, 0, F)
          }
          if (sum(str_length(columnValue)) > 0) {
            if (!is.na(name)) {
              newColumn <- rep("", length(lines))
              if (length(which(lines %in% significantLines[columnRows])) < length(columnValue)) {
                significantLines <- .correctSignificantLines(columnValue, significantLines, columnInfo$line, max(length(columnValue), maxColumnLength))
                tables[[i]]$significantLines <- significantLines
                lines <- sort(unique(c(lines, significantLines)))
                tables[[i]]$lines <- lines
                candidates$row <- sapply(candidates$line, FUN = function(x) sum(x >= significantLines))
                columnLines <- candidates$pos[which(candidates$value == columnName)]
                columnRows <- candidates$row[which(candidates$value == columnName)]
                columnInfo <- candidates[which(candidates$value == columnName),]
              }
              duplicates <- which(duplicated(columnRows))
              if (length(duplicates) > 0) {
                columnRows <- columnRows[-duplicates]
                columnValue <- columnValue[-duplicates]
              }
              newColumn[which(lines %in% significantLines[columnRows])] <- columnValue
              if (sum(apply(table, MARGIN = 2, FUN = function(x) sum(x == newColumn)) == length(lines)) == 0) {
                table <- add_column(.data = table, !!(name) := newColumn, .after = "description", .name_repair = "minimal")
              }
              table[columnLines, descriptionPos] <- ""
            }
          }
          # table[columnLines, descriptionPos] <- ""
        }
      }
    }
    if ("quantity" %in% names(table)) {
      quantityPos <- which(names(table) == "quantity")
      quantityUnitPos <- which(names(table) == "quantityUnit")
      quantitySplit <- str_split(table[, quantityPos], " ", simplify = T)
      if (ncol(quantitySplit) == 2) {
        num1 <- sum(!is.na(.prepareNumerics(quantitySplit[,1])))
        num2 <- sum(!is.na(.prepareNumerics(quantitySplit[,2])))
        found <- F
        if (num2 == 0 && num1 > 0) {
          quantity <- .prepareNumerics(quantitySplit[,1], asNumeric = F)
          quantityUnit <- quantitySplit[,2]
          found <- T
        } else if (num1 == 0 && num2 > 0) {
          quantity <- .prepareNumerics(quantitySplit[,2], asNumeric = F)
          quantityUnit <- quantitySplit[,1]
          found <- T
        }
        if (found) {
          table[,quantityPos] <- quantity
          if (length(quantityUnitPos) == 0 ||
              sum(sapply(quantityUnitPos, FUN = function(x) sum(quantityUnit == table[,x]) == length(quantityUnit))) == 0) {
            table <- table %>% add_column("quantityUnit" = quantityUnit, .after = quantityPos, .name_repair = "minimal")
          }
        }
      } else {
        table[,quantityPos] <- .prepareNumerics(table[,quantityPos], asNumeric = F)
      }
    }
    if (sum(c("price", "unitPrice", "totalPrice") %in% names(table)) > 0) {
      pricePositions <- which(names(table) %in% c("price", "unitPrice", "totalPrice"))
      priceUnitPos <- which(names(table) == "currency")
      for (k in 1:length(pricePositions)) {
        pricePos <- pricePositions[k]
        priceColumn <- str_squish(str_replace_all(table[,pricePos], "\u20AC", " \u20AC"))
        priceSplit <- str_split(priceColumn, " ", simplify = T)
        if (ncol(priceSplit) == 2) {
          num1 <- sum(!is.na(.prepareNumerics(priceSplit[,1])))
          num2 <- sum(!is.na(.prepareNumerics(priceSplit[,2])))
          found <- F
          if (num2 == 0 && num1 > 0) {
            price <- .prepareNumerics(priceSplit[,1], asNumeric = F)
            priceUnit <- priceSplit[,2]
            found <- T
          } else if (num1 == 0 && num2 > 0) {
            price <- .prepareNumerics(priceSplit[,2], asNumeric = F)
            priceUnit <- priceSplit[,1]
            found <- T
          }
          if (found) {
            table[,pricePos] <- price
            if (length(priceUnitPos) == 0 ||
                sum(sapply(priceUnitPos, FUN = function(x) sum(priceUnit == table[,x]) == length(priceUnit))) == 0) {
              table <- table %>% add_column("currency" = priceUnit, .after = pricePos, .name_repair = "minimal")
              pricePositions <- which(names(table) %in% c("price", "unitPrice", "totalPrice"))
              priceUnitPos <- which(names(table) == "currency")
            }
          }
        } else {
          table[,pricePos] <- .prepareNumerics(table[,pricePos], asNumeric = F)
        }
      }
    }
    lineLength <- apply(table, MARGIN = 1, FUN = function(x) sum(str_length(str_trim(x))))
    removePos <- which(lineLength == 0)
    if (length(removePos) > 0) {
      table <- table[-removePos,]
      tables[[i]]$lines <- tables[[i]]$lines[-removePos]
    }
    sigRows <- which(tables[[i]]$lines %in% significantLines)
    otherRows <- which(!tables[[i]]$lines %in% significantLines)
    table <- table %>% mutate(across(where(is.character), str_trim))
    shiftCandidates <- which(apply(table[sigRows,], MARGIN = 2, FUN = function(x) sum(str_length(x))) == 0)
    if (length(shiftCandidates) > 0) {
      for (j in 1:length(shiftCandidates)) {
        candidate <- shiftCandidates[j]
        values <- str_trim(table[otherRows, candidate])
        table[,candidate] <- ""
        table[otherRows - 1, candidate] <- values
      }
    }
    tableLines <- sort(unique(c(sigRows, otherRows)))
    lineStructure <- data.frame(line = tableLines, row = sapply(tableLines, FUN = function(x) sum(x >= sigRows)))
    newTable <- table[FALSE,]
    for (k in 1:max(lineStructure$row)) {
      lines <- lineStructure %>% filter(row == k) %>% select(line) %>% unlist()
      newTable <- rbind(newTable, str_trim(apply(table[lines,], 2, .pasteNoDuplicates, collapse = " ")))
    }
    names(newTable) <- names(table)
    tables[[i]]$data <- newTable
  }
  return(tables)
}

# Removes words being used as table row separators.
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @return Dataframe including the cleaned words of a PDF file and their positions.
.removeSeparators <- function(words) {
  wordLength <- str_length(words$value)
  separatorLength <- str_count(words$value, pattern = "-|_")
  removePos <- which(wordLength - separatorLength == 0)
  if (length(removePos) > 0) {
    words <- words[-removePos,]
  }
  return(words)
}

# Identifies the absolute line numbers of lines that might be used as table headers.
# @param text List including several representations of text extracted from a PDF file. This list is generated by the function extractText.
# @return A vector of absolute line numbers.
.getHeaderPos <- function(text) {
  lines <- text$lines
  words <- text$words
  wordsPerLine <- words %>%
    group_by(absLine) %>%
    summarize(len = n(), .groups = "drop") %>%
    select(len) %>%
    unlist()
  headerOccurrences <- str_count(str_to_lower(lines$value), paste(pkg.conf$headerNames, collapse = "|"))
  numericOccurrences <- str_count(lines$value, "[0-9]")
  candidates <- which(headerOccurrences / wordsPerLine >= 0.75)
  pos <- numeric()
  if (length(candidates) > 0) {
    candidates <- as.numeric(candidates[which(numericOccurrences[candidates] == 0)])
    pos <- lines$absLine[candidates]
  }
  return(pos)
}

# Identify missing information.
# @param tableNames Character vector including the column names of a table.
# @return Character vector of missing information.
.missingInformation <- function(tableNames) {
  missing <- setdiff(c("unitPrice", "totalPrice", "quantity"), tableNames)
  return(missing)
}

# Adds a missing column to a given table.
# @param table A dataframe representing an order table.
# @param candidates List of value candidates.
# @param columnName Character describing the name of the column that will be added.
# @return Table with added column.
.addColumn <- function(table, candidates, columnName) {
  table[[columnName]] = sapply(candidates, FUN = function(x) min(x, na.rm = T))
  removePos = sapply(candidates, FUN = function(x) which(x == min(x, na.rm = T)))
  descriptionWords <- str_count(table$description, "\\S+")
  for (j in 1:nrow(table)) {
    desc <- ""
    if (removePos[j] > 1) {
      desc <- word(table$description[j], start = 1, end = removePos[j] - 1)
    }
    if (removePos[j] < descriptionWords[j]) {
      desc <- paste(desc, word(table$description[j], start = removePos[j] + 1, end = descriptionWords[j]), sep = " ")
    }
    table$description[j] <- desc
  }
  return(table)
}

# Extract missing information from description column.
# @param tables A list of extracted tables.
# @return A list of expanded tables.
.expandTableDescription <- function(tables) {
  for (i in 1:length(tables)) {
    table <- tables[[i]]$data
    description <- table$description
    numerics <- lapply(str_split(description, " "), .prepareNumerics)
    missing <- .missingInformation(names(table))
    if (length(missing) > 0 && length(numerics) > 0) {
      if ("unitPrice" %in% missing) {
        unitPriceCandidates <- lapply(numerics, function(x) ifelse(x %% 1 != 0, x, NA))
        if (!"totalPrice" %in% missing) {
          for (j in 1:nrow(table)) {
            ratio <- table$totalPrice[j] / unitPriceCandidates[[j]]
            unitPriceCandidates[[j]][which(ratio %% 1 != 0)] <- NA
          }
          unitPriceCount <- sapply(unitPriceCandidates, FUN = function(x) length(which(!is.na(x))))
          if (sum(unitPriceCount == rep(1, nrow(table))) == nrow(table)) {
            table <- .addColumn(table, unitPriceCandidates, "unitPrice")
            numerics <- lapply(str_split(table$description, " "), .prepareNumerics)
          }
        }
      }
      if ("totalPrice" %in% missing) {
        totalPriceCandidates = list()
        for (j in 1:nrow(table)) {
          totalPrice <- numerics[[j]]
          if ("unitPrice" %in% missing) {
            unitPricePos <- which(!is.na(unitPriceCandidates[[j]]))
            unitPrice <- unitPriceCandidates[[j]][unitPricePos]
          } else {
            unitPrice <- as.numeric(table$unitPrice[j])
          }
          if (length(unitPrice) > 0) {
            checks <- sapply(unitPrice, FUN = function(x) (totalPrice %/% x) == (totalPrice / x))
            if (!any(is.na(checks))) {
              candidatePos <- which(colSums(checks, na.rm = T) > 1)
              removePos <- which(colSums(checks, na.rm = T) == 0)
            } else {
              candidatePos = NULL
              removePos = NULL
            }
          } else {
            candidatePos = NULL
            removePos = NULL
          }
          if (length(removePos) > 0 && "unitPrice" %in% missing) {
            unitPriceCandidates[[j]][unitPricePos[removePos]] <- NA
          }
          if (length(candidatePos) > 0) {
            if ("unitPrice" %in% missing) {
              totalPricePos <- setdiff(which(checks[,candidatePos]), unitPricePos)
            } else {
              totalPricePos <- which(checks[,candidatePos])
            }
            if (length(totalPricePos) > 0) {
              totalPrice[-totalPricePos] <- NA
            } else {
              totalPrice <- rep(NA, length(totalPrice))
            }
          } else {
            totalPrice <- rep(NA, length(totalPrice))
          }
          totalPriceCandidates[[j]] <- totalPrice
        }
        if ("unitPrice" %in% missing) {
          unitPriceCount <- sapply(unitPriceCandidates, FUN = function(x) length(which(!is.na(x))))
          if (sum(unitPriceCount == rep(1, nrow(table))) == nrow(table)) {
            table <- .addColumn(table, unitPriceCandidates, "unitPrice")
            removePos <- sapply(unitPriceCandidates, FUN = function(x) which(x == min(x, na.rm = T)))
            for (j in 1:nrow(table)) {
              totalPriceCandidates[[j]] <- totalPriceCandidates[[j]][-removePos[j]]
            }
            numerics <- lapply(str_split(table$description, " "), .prepareNumerics)
          }
        }
        totalPriceCount <- sapply(totalPriceCandidates, FUN = function(x) length(which(!is.na(x))))
        if (sum(totalPriceCount == rep(1, nrow(table))) == nrow(table)) {
          table <- .addColumn(table, totalPriceCandidates, "totalPrice")
          numerics <- lapply(str_split(table$description, " "), .prepareNumerics)
        }
      }
      if ("quantity" %in% missing) {
        quantityCandidates <- lapply(numerics, function(x) ifelse(x %% 1 == 0, x, NA))
        if ("unitPrice" %in% names(table)) {
          for (j in 1:nrow(table)) {
            quantityCandidates[[j]][which(quantityCandidates[[j]] == table$unitPrice[j])] <- NA
          }
        }
        if ("totalPrice" %in% names(table)) {
          for (j in 1:nrow(table)) {
            quantityCandidates[[j]][which(quantityCandidates[[j]] == table$totalPrice[j])] <- NA
          }
        }
        quantityCount <- sapply(quantityCandidates, FUN = function(x) length(which(!is.na(x))))
        if (sum(quantityCount == rep(1, nrow(table))) == nrow(table)) {
          table <- .addColumn(table, quantityCandidates, "quantity")
          numerics <- lapply(str_split(table$description, " "), .prepareNumerics)
        }
      }
    }
    tables[[i]]$data <- table
  }
  return(tables)
}

# Annotate x and y coordinates of the top-left and bottom-right corner of each table on each page.
# @param tables A list of extracted tables.
# @param words Dataframe including the words of a PDF file and their positions. The words-dataframe is generated by the function extractText.
# @return A list of annotated tables.
.annotateAbsolutePositions <- function(tables, words) {
  for (i in 1:length(tables)) {
    table <- tables[[i]]
    tablePositions <- words %>%
      filter(absLine %in% table$lines) %>%
      group_by(page) %>%
      summarize(xStart = min(x), xEnd = max(x), yStart = min(y), yEnd = max(y))
    tables[[i]]$tablePositions <- tablePositions
  }
  return(tables)
}

# Standardizes an order table so that each table includes the following columns: pos, id, description, quantity, quantityUnit, unitPrice, totalPrice, currency, date.
# @param tables A list of extracted tables.
# @return A standardized list of tables.
.standardizeTables <- function(tables) {
  for (i in 1:length(tables)) {
    table <- tables[[i]]$data
    columnNames <- names(table)
    if (!"pos" %in% columnNames) {
      table$pos <- seq(1, nrow(table), 1)
      columnNames <- names(table)
    }
    if (!"id" %in% columnNames) {
      table$id <- rep("", nrow(table))
      columnNames <- names(table)
    }
    if (!"quantity" %in% columnNames) {
      if (sum(c("unitPrice", "totalPrice") %in% columnNames) == 2) {
        table$quantity <- round(.prepareNumerics(table$totalPrice) / .prepareNumerics(table$unitPrice))
      } else {
        table$quantity <- rep("", nrow(table))
      }
      columnNames <- names(table)
    }
    if (!"quantityUnit" %in% columnNames) {
      table$quantityUnit <- rep("", nrow(table))
      columnNames <- names(table)
    } else {
      table <- table %>%
        mutate(quantityUnit = replace(quantityUnit, str_to_lower(quantityUnit) %in% c("st", "st.", "stk", "stk.", "stck", "st\u00FCck", "stueck"), "ST"))
      columnNames <- names(table)
    }
    if (!"unitPrice" %in% columnNames) {
      if ("price" %in% columnNames) {
        names(table)[which(columnNames == "price")[1]] <- "unitPrice"
      } else {
        table$unitPrice <- rep("", nrow(table))
      }
      columnNames <- names(table)
    }
    if (!"totalPrice" %in% columnNames) {
      if ("price" %in% columnNames) {
        names(table)[which(columnNames == "price")[1]] <- "totalPrice"
      } else {
        quantity <- .prepareNumerics(table$quantity)
        unitPrice <- .prepareNumerics(table$unitPrice)
        if (!any(is.na(quantity)) && !any(is.na(unitPrice))) {
          table$totalPrice <- quantity * unitPrice
        } else {
          table$totalPrice <- rep("", nrow(table))
        }
      }
      columnNames <- names(table)
    }
    if (!"currency" %in% columnNames) {
      table$currency <- rep("Euro", nrow(table))
      columnNames <- names(table)
    } else {
      table <- table %>%
        mutate(currency = replace(currency, str_to_lower(currency) %in% c("\u20AC", "eur", "eur.", "euro"), "Euro"))
      columnNames <- names(table)
    }
    if (!"date" %in% columnNames) {
      table$date <- rep("", nrow(table))
    }
    if (!"description" %in% columnNames) {
      table$description <- rep("", nrow(table))
    }
    table$quantity <- .prepareNumerics(table$quantity)
    table$unitPrice <- .prepareNumerics(table$unitPrice)
    table$totalPrice <- .prepareNumerics(table$totalPrice)
    if (all(!is.na(table$totalPrice)) &&
        all(!is.na(table$unitPrice)) &&
        all(!is.na(table$quantity)) &&
        all(table$unitPrice > table$totalPrice) &&
        any(table$unitPrice > 1000)) {
      table$unitPrice <- table$totalPrice / table$quantity
    }
    table <- table %>% select(c(pos, id, description, quantity, quantityUnit, unitPrice, totalPrice, currency, date, everything()))
    tables[[i]]$data <- table
  }
  return(tables)
}

# Compares two double values with some tolerance.
# @param x First double value.
# @param y Second double value.
# @param tol Tolerance value.
# @return Boolean value.
.double_equal <- function(x, y, tol = sqrt(.Machine$double.eps)){
  abs(x - y) < tol
}

# Identifies tribbles of values that could be the quantity, unit price and total price.
# @param numerics Matrix of extracted numeric values.
# @return Vector of columns in numerics that might be the quantity, unit price and total price or NA.
.identifyQuantityPriceTribble <- function(numerics) {
  fit1 <- vector()
  fit2 <- vector()
  l <- ncol(numerics)
  for (i in 1:nrow(numerics)) {
    line <- numerics[i,]
    pattern1 <- rep(0, (l-2) * (l-1))
    pattern2 <- pattern1
    combinations <- combn(line, 2, prod)
    removals <- which(combinations <= 0)
    if (length(removals) > 0) {
      combinations[removals] <- NA
    }
    pattern1 <- colSums(sapply(combinations, FUN = .double_equal, line), na.rm = T)
    pattern2 <- colSums(sapply(line, FUN = .double_equal, combinations), na.rm = T)
    fit1 <- rbind(fit1, pattern1)
    fit2 <- rbind(fit2, pattern2)
  }
  pair1 <- which(colMeans(fit1) == 1)
  pair2 <- which(colMeans(fit2) == 1)
  result <- NA
  if (length(pair1) == 1 && length(pair2) == 1) {
    m <- matrix(rep(0, l^2), nrow = l)
    pos <- which(upper.tri(m))[pair1]
    row <- pos %% l
    row <- ifelse(row == 0, l, row)
    col <- pos %/% l + 1
    result <- c(row, col, pair2)
  }
  return(result)
}

# Extracts a numeric matrix from a given table.
# @param table Table extracted from an order document.
# @return Matrix of numeric values that occur in the description column as well as in the quantity, unit price and total price column.
.extractNumericMatrix <- function(table) {
  numerics <- list()
  for (j in 1:nrow(table)) {
    rowA <- table[j,]$description %>% str_split(" ") %>% unlist()
    rowB <- table[j,]$description %>% str_replace_all("\\.", "") %>% str_replace_all(",", "\\.") %>% str_split(" ") %>% unlist()
    numericsA <- .prepareNumerics(rowA)
    numericsB <- .prepareNumerics(rowB)
    if (sum(!is.na(numericsA)) > sum(!is.na(numericsB))) {
      rowNumerics <- numericsA[which(!is.na(numericsA))]
    } else {
      rowNumerics <- numericsB[which(!is.na(numericsB))]
    }
    removal <- which(rowNumerics > 1e+6)
    if (length(removal) > 0) {
      rowNumerics <- rowNumerics[-removal]
    }
    numerics[[j]] <- c(rowNumerics, table[j,]$quantity, table[j,]$unitPrice, table[j,]$totalPrice)
  }
  if (length(unique(lengths(numerics))) == 1) {
    setnafill(numerics, fill = -1)
    numerics <- matrix(unlist(numerics), nrow = length(numerics), byrow = T)
    tribble <- .identifyQuantityPriceTribble(numerics)
    quantity = F
    unitPrice = F
    if (!any(is.na(tribble))) {
      for (k in 1:length(tribble)) {
        name <- .annotateColumn(numerics[,tribble[k]], numerics[,tribble[k]], 0, 0, F)
        if (is.na(name)) {
          if (!quantity) {
            name <- "quantity"
          } else if (!unitPrice) {
            name <- "unitPrice"
          } else {
            name <- "totalPrice"
          }
        }
        if (name == "quantity") {
          quantity <- T
        } else if (name == "price" && !unitPrice) {
          unitPrice <- T
          name <- "unitPrice"
        } else {
          name <- "totalPrice"
        }
        table[[name]] <- numerics[,tribble[k]]
      }
    }
  }
  return(table)
}

# Function to identify the quantity, unit price and total price in extracted tables if they are missing.
# @param tables A list of extracted tables.
# @return List of extracted tables with corrected quantity, unit price and total price.
.correctQuantityPrice <- function(tables) {
  for (i in 1:length(tables)) {
    table <- tables[[i]]$data
    inconsistency <- !.double_equal(table$quantity * table$unitPrice, table$totalPrice)
    inconsistency <- ifelse(is.na(inconsistency), T, inconsistency)
    if (any(inconsistency)) {
      remainder <- table$totalPrice %% table$unitPrice
      remainder <- ifelse(is.na(remainder), 1, remainder)
      test1 <- .double_equal(remainder, 0)
      test2 <- ifelse(is.na(test1), F, test1)
      test2 <- .double_equal(remainder - table$unitPrice, 0)
      test2 <- ifelse(is.na(test2), F, test2)
      if (all(test1 | test2) && all(table$quantity > 10000)) {
        if (all(table$id == "")) {
          table$id <- table$quantity
        }
        table$quantity <- round(table$totalPrice / table$unitPrice, 0)
        tables[[i]]$data <- table
        inconsistency <- F
      }
    }
    if (any(is.na(c(table$quantity, table$unitPrice, table$totalPrice))) || any(inconsistency)) {
      table <- .extractNumericMatrix(table)
      tables[[i]]$data <- table
    }
  }
  return(tables)
}

#' Extract tables from a given words-dataframe
#' @description
#' This function extracts order-position-tables from PDF-based order documents.
#' It tries to identify table rows based on a clustering approach and thereafter
#' identifies the column structure. A table row can consist of multiple text rows
#' and the text rows can span different columns. This function furthermore tries
#' to identify the meaning of the columns (position, articleID, description,
#' quantity, quanity unit, unit price, total price, currency, date).
#'
#' @param text List including several representations of text extracted from a PDF file. This list is generated by the function extractText.
#' @param minCols Number of columns a table must minimal consist of
#' @param maxDistance Number of text lines that can maximally exist between the start of two table rows
#' @param entityNames A list of four name vectors (currencyUnits, quantityUnits, headerNames, noTableNames). Each vector contains strings that correspond to currency units, quantity units, header names or names of entities not being a table.
#' @return List of lists describing the tables. Each sublist includes a data frame (data) which is the identified table, the position of text lines
#' that constitute the table and the position of the significant lines.
#' @examples
#' file <- system.file("extdata", "OrderDocument_en.pdf", package = "orderanalyzer")
#' text <- extractText(file)
#'
#' # Extracting order tables without any further information
#' tables <- extractTables(text)
#' tables[[1]]$data
#'
#' # Extracting order tables with further information
#' tables <- extractTables(text,
#'   entityNames = list(currencyUnits = enc2utf8(c("eur", "euro", "\u20AC")),
#'                      quantityUnits = enc2utf8(c("pcs", "pcs.")),
#'                      headerNames = enc2utf8(c("pos", "item", "quantity")),
#'                      noTableNames = enc2utf8(c("order total", "supplier number")))
#' )
#' tables[[1]]$data
#'
#' # Extracting order tables from a German document
#' file <- system.file("extdata", "OrderDocument_de.pdf", package = "orderanalyzer")
#' text <- extractText(file)
#' tables <- extractTables(text)
#' tables[[1]]$data
#'
#' @export
extractTables <- function(text, minCols = 3, maxDistance = 20, entityNames = NA) {
  if (!any(is.na(entityNames))) {
    if (!is.null(entityNames$currencyUnits)) {
      pkg.conf$currencyUnits <- entityNames$currencyUnits
    }
    if (!is.null(entityNames$quantityUnits)) {
      pkg.conf$quantityUnits <- entityNames$quantityUnits
    }
    if (!is.null(entityNames$headerNames)) {
      pkg.conf$headerNames <- entityNames$headerNames
    }
    if (!is.null(entityNames$currencyUnits)) {
      pkg.conf$noTableNames <- entityNames$noTableNames
    }
  }
  words <- text$words
  lines <- text$lines
  headerPos <- .getHeaderPos(text)
  words <- .removeSeparators(words)
  pageBreaks <- words %>%
    group_by(page) %>%
    summarize(maxLine = max(absLine), .groups = "drop") %>%
    select(maxLine) %>%
    unlist() %>%
    as.numeric()
  minRows <- 2# max(words$page)
  m <- .getBinaryMatrix(words)
  tables <- list()
  positions <- .extractSignificantLines(words, lines, m, minRows, minCols)
  if (nrow(positions$positions) > 0) {
    skeletons <- .extractTableSkeletons(positions, m, minCols, maxDistance, pageBreaks, headerPos)
    if (length(skeletons) > 0) {
      tables <- .buildTables(skeletons, words, m)
      tables <- .clearTables(tables)
      tables <- .annotateTables(tables)
      if (length(tables) > 1) {
        tables <- .joinTables(tables)
      }
      if (length(tables) > 0) {
        tables <- .enrichTables(tables)
        tables <- .expandTableDescription(tables)
        tables <- .annotateAbsolutePositions(tables, words)
        tables <- .standardizeTables(tables)
        tables <- .correctQuantityPrice(tables)
      }
    }
  }
  if (length(tables) == 0) {
    m3 <- .getBinaryMatrix(words, type = 3)
    tables <- list()
    positions <- .extractSignificantLines(words, lines, m3, minRows, minCols)
    if (nrow(positions$positions) > 0) {
      skeletons <- .extractTableSkeletons(positions, m3, minCols, maxDistance, pageBreaks, headerPos)
      if (length(skeletons) > 0) {
        tables <- .buildTables(skeletons, words, m3)
        tables <- .clearTables(tables)
        tables <- .annotateTables(tables)
        if (length(tables) > 1) {
          tables <- .joinTables(tables)
        }
        if (length(tables) > 0) {
          tables <- .enrichTables(tables)
          tables <- .expandTableDescription(tables)
          tables <- .annotateAbsolutePositions(tables, words)
          tables <- .standardizeTables(tables)
          tables <- .correctQuantityPrice(tables)
        }
      }
    }
  }
  return(tables)
}
