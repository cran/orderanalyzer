utils::globalVariables(c("absLine",
                         "candidate", "characterDiff", "correction", "corShift", "corShift2", "count", "cumDiff", "cumShift",
                         "diff", "diff2", "dup",
                         "emptyLines", "end", "end2", "endTemp", "estPuffer",
                         "height", "length", "line", "lineBox", "lineDiff", "lineNum", "linePos", "lines", "lineX1", "lineX2",
                         "lastOnPage", "len", "lineY1", "lineY2",
                         "maxLine", "minDiff", "description", "currency", "pos", "quantityUnit", "totalPrice",
                         "n", "newLine", "newShift", "newShift2", "nonDescCols",
                         "shift3", "cumShift3", "puffer3", "offeredPuffer3", "pufferDiff3", "usedPuffer3",
                         "corShift3", "newShift3", "start3", "newLine3", "end3", "score.x",
                         "ocr_carea_id", "ocr_carea_title", "ocr_line_id", "ocr_line_title", "ocr_page_id", "ocr_page_title",
                         "ocr_par_id", "ocr_par_lang", "ocr_par_title", "ocrx_word_id", "ocrx_word_tag", "ocrx_word_title",
                         "ocrx_word_value",
                         "page", "pageBreak", "pageStartPatternDiff", "power", "puffer", "puffer2", "pufferDiff",
                         "rows",
                         "shift", "shift2", "space", "start", "start2", "startTemp",
                         "tempShift", "tempShift2", "type",
                         "val", "value",
                         "width", "wordBox", "wordLength", "wordX1", "wordX2", "wordY1", "wordY2",
                         "x",
                         "y", "yBottom",
                         ".",
                         "id"
                         ))

pkg.conf <- new.env()
pkg.conf$currencyUnits <- vector()
pkg.conf$quantityUnits <- vector()
pkg.conf$headerNames <- vector()
pkg.conf$noTableNames <- vector()
