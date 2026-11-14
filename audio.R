# ---------------------------------------------------------------------------
# audio.R : play and check the audio files linked to the lexicon
# ---------------------------------------------------------------------------

#' Play the audio file of a word
#'
#' Looks for the file `<word>_<language>.wav` in `audio_dir` and opens it with
#' the default audio player of the computer.
#'
#' @param word A single word in French.
#' @param lexicon A data frame with a `french` column.
#' @param language Target language, used in the file name. Default `"bassa"`.
#' @param audio_dir Folder containing the `.wav` files. Default `"audio"`.
#'
#' @return Invisibly, the path of the file, or `NULL` if the word or the file
#'   is not found.
#'
#' @examples
#' \dontrun{
#' play_word("bonjour", lexicon)
#' play_word("pluie", lexicon, language = "bafang")
#' }
#'
#' @export
#' @importFrom utils browseURL
play_word <- function(word, lexicon, language = "bassa", audio_dir = "audio") {
  check_lexicon(lexicon, "french")

  i <- match(tolower(word), tolower(lexicon$french))
  if (is.na(i)) {
    message("Mot absent du lexique : ", word)
    return(invisible(NULL))
  }

  path <- file.path(audio_dir, paste0(tolower(word), "_", language, ".wav"))
  if (!file.exists(path)) {
    message("Fichier audio introuvable : ", path)
    return(invisible(NULL))
  }

  utils::browseURL(normalizePath(path))
  invisible(path)
}

#' Translate a word and play its audio
#'
#' Prints the translation of `word`, then plays its audio file.
#'
#' @inheritParams play_word
#' @param word A single word in French.
#'
#' @return Invisibly, the translation.
#'
#' @examples
#' \dontrun{
#' translate_and_play("bonjour", lexicon)
#' }
#'
#' @export
translate_and_play <- function(word, lexicon, language = "bassa",
                               audio_dir = "audio") {
  translation <- translate_text(word, lexicon, from = "french", to = language)
  print(translation)
  play_word(word, lexicon, language = language, audio_dir = audio_dir)
  invisible(translation)
}

#' Check which words of the lexicon have an audio file
#'
#' @inheritParams play_word
#'
#' @return A data frame with the columns `french`, `file` and `exists`.
#'   A message lists the words that have no audio file yet.
#'
#' @examples
#' \dontrun{
#' check_audio(lexicon, language = "bassa")
#' }
#'
#' @export
check_audio <- function(lexicon, language = "bassa", audio_dir = "audio") {
  check_lexicon(lexicon, "french")

  words <- tolower(as.character(lexicon$french))
  words <- words[!is.na(words) & words != ""]
  files <- file.path(audio_dir, paste0(words, "_", language, ".wav"))

  result <- data.frame(french = words, file = files,
                       exists = file.exists(files), stringsAsFactors = FALSE)

  missing_words <- result$french[!result$exists]
  if (length(missing_words) > 0) {
    message(length(missing_words), " mot(s) sans audio : ",
            paste(missing_words, collapse = ", "))
  }
  result
}
