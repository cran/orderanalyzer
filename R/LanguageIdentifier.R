# Most frequent trigrams for Czech
czech <- data.table(trigram = c("byl", "pro", "ost", "pri", "jak", "eni", "ale", "tak", "ova", "sta",
                                "pre", "ako", "kdy", "ylo", "ach", "ter", "sti", "pod", "del", "eho",
                                "neb", "kte", "jeh", "val", "nos", "hod", "mel", "ech", "edn", "sem"),
                    frequency = c(.84, .61, .47, .45, .40, .35, .34, .32, .32, .31,
                                  .31, .28, .27, .26, .26, .25, .25, .25, .24, .22,
                                  .22, .22, .22, .22, .21, .21, .21, .21, .20, .20))

dutch <- data.table(trigram = c("een", "aar", "het", "ver", "van", "gen", "oor", "nde", "den", "cht",
                                "nie", "ing", "ten", "sch", "eer", "der", "dat", "iet", "ste", "and",
                                "voo", "ren", "ken", "lij", "met", "ere", "lle", "maa", "ter", "zij"),
                    frequency = c(1.40, 1.14, 1.08, .96, .92, .77, .76, .71, .71, .69,
                                  .67, .67, .63, .61, .61, .59, .59, .58, .53, .50,
                                  .50, .49, .48, .47, .47, .47, .45, .44, .44, .42))

# Most frequent trigrams for English.
english <- data.table(trigram = c("the", "and", "ing", "ent", "ion", "her", "for", "tha", "nth", "int",
                                  "ere", "tio", "ter", "est", "ers", "ati", "hat", "ate", "all", "eth",
                                  "hes", "ver", "his", "oft", "ith", "fth", "sth", "oth", "res", "ont"),
                      frequency = c(1.81, .73, .72, .42, .42, .36, .34, .33, .33, .32,
                                    .31, .31, .30, .28, .28, .26, .26, .25, .25, .24,
                                    .24, .24, .24, .22, .21, .21, .21, .21, .21, .20))

# Most frequent trigrams for French.
french <- data.table(trigram = c("ent", "les", "ion", "des", "ede", "que", "est", "tio", "ant", "par",
                                 "men", "del", "ela", "sde", "lle", "our", "res", "son", "tre", "ont",
                                 "eur", "ati", "une", "con", "eme", "ans", "ons", "esd", "tde", "nde"),
                     frequency = c(.86, .71, .56, .54, .52, .51, .49, .42, .38, .37,
                                   .37, .37, .37, .37, .36, .35, .32, .31, .31, .31,
                                   .31, .30, .29, .29, .28, .27, .27, .27, .26, .26))

# Most frequent trigrams for German.
german <- data.table(trigram = c("der", "ein", "sch", "ich", "nde", "die", "che", "den", "ten", "und",
                                 "ine", "ter", "gen", "end", "ers", "ste", "cht", "ung", "das", "ere",
                                 "ber", "ens", "nge", "rde", "ver", "eit", "hen", "erd", "rei", "ind"),
                     frequency = c(1.04, .83, .76, .75, .72, .62, .58, .56, .51, .48,
                                   .48, .44, .44, .44, .42, .42, .41, .39, .38, .38,
                                   .36, .36, .35, .35, .34, .33, .31, .30, .30, .29))

# Most frequent trigrams for Spanish.
spanish <- data.table(trigram = c("del", "que", "ent", "ion", "ela", "con", "sde", "ade", "cio", "nte",
                                  "est", "los", "ode", "ado", "res", "sta", "aci", "las", "ara", "ene",
                                  "par", "des", "ese", "ien", "ala", "por", "one", "nde", "tra", "nes"),
                      frequency = c(.75, .74, .67, .56, .55, .54, .52, .51, .50, .49,
                                    .48, .47, .47, .45, .40, .38, .36, .35, .34, .32,
                                    .32, .31, .30, .30, .29, .29, .29, .29, .28, .27))

# Most frequent trigrams for Latvian.
latvian <- data.table(trigram = c("ais", "cij", "gad", "iba", "iek", "iem", "ien", "ies", "iet", "ija",
                                  "ika", "ina", "isk", "jas", "kas", "lat", "lie", "nie", "par", "pie",
                                  "san","ska", "tie", "tik", "val", "vie"),
                      frequency = c(.27, .29, .48, .34, .30, .59, .40, .28, .31, 1.01,
                                    .33, .26, .28, .61, .29, .28, .34, .32, .31, .35,
                                    .32, .29, .38, .28, .27, .44))

# Most frequent trigrams for Lithuanian.
lithuanian <- data.table(trigram = c("ini", "tin", "ali", "ijo", "tai", "usi", "jos", "pri", "ant", "kai",
                                     "sta", "rin", "iau", "ist", "men", "mas", "iai", "uri", "kur", "eik",
                                     "ent", "aus", "ais", "uvo", "ink", "tik", "pas", "tas", "ija", "gal"),
                         frequency = c(.68, .54, .44, .44, .41, .39, .39, .37, .36, .35,
                                       .35, .34, .34, .34, .34, .33, .32, .31, .31, .30,
                                       .29, .29, .29, .29, .28, .28, .28, .27, .27, .27))

#' Identifies the language of a given text based on frequent trigrams
#' @param text String for which the language should be identified
#' @description
#' This function identifies the language of a given string based on the most
#' frequent trigrams in different languages. Supported languages are Czech,
#' Dutch, English, French, German, Spanish, Latvian and Lithuanian.
#'
#' @return Name of the detected language.
#' @examples
#'
#' text <- "The tea in the cup still is hot."
#' language <- identifyLanguage(text)
#' language
#'
#' @export
identifyLanguage <- function(text) {
  trigrams <- tokens_ngrams(tokens(text, "character"), n = 3, concatenator = "") %>%
    unlist() %>%
    str_to_lower() %>%
    as.data.table()
  names(trigrams) <- "trigram"
  scores <- data.table(language = c("czech", "dutch", "english", "french", "german", "spanish", "latvian", "lithuanian"),
                       score = c(sum(inner_join(trigrams, czech, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, dutch, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, english, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, french, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, german, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, spanish, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, latvian, by = "trigram")$frequency),
                                 sum(inner_join(trigrams, lithuanian, by = "trigram")$frequency)))
  return(scores$language[which(scores$score == max(scores$score))])
}
