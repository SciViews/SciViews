#' Ask a question to a chatbot
#'
#' The question is sent to the chatbot server, and the answer is returned. The
#' default chatbot server is ollama running locally, and the default model is
#' mistral:7b-instruct-v0.2-q6_K
#'
#' @param question A character string with the question to ask.
#' @param context An R object used as context (usually a data frame).
#' @param max_tokens The maximum number of tokens to return in the answer. By
#'   default, it is 1000.
#' @param lang The language to use for the answer. Default is "en". You can also
#'   use "fr".
#' @param url The URL of the chatbot server. Default is
#'   http://localhost:11434/api/chat
#' @param model The LLM (large language model) to use. Default is a variant of
#'   Mistral 7B.
#' @param verbose Should more information be printed? `FALSE` by default.
#'
#' @return
#' The answer is returned invisibly. The function is used for its side-effect of
#' displaying the chatbot help page with the question, answer and examples
#' @export
#'
#' @examples
#' \dontrun{
#' ask("What is a chatbot?")
#' ask("Write R code to filter a data frame.")
#' }
ask <- function(question, context = NULL,
    max_tokens = getOption("SciViews.chatbot.max_tokens", 1000),
    lang = getOption("data.io_lang", "en"),
    url = getOption("SciViews.chatbot.url",
      "http://localhost:11434/api/chat"), # Default is using Ollama locally
    model = getOption("SciViews.chatbot.model",
      "mistral:7b-instruct-v0.2-q6_K"), # Default model is Mistral 7B
    verbose = FALSE) {

  # We use three pages and alternate between these pages...
  max_page <- 3L
  last_page <- getOption("SciViews.chatbot.page", max_page)
  page <- last_page + 1L
  if (page > max_page)
    page <- 1L
  chat_page <- paste0("SciViews.chatbot.page", page)

  question <- paste(question, collapse = "\n")
  # In case there are % signs, we need to escape them for a Rd file
  question2 <- gsub("%", "\\%", question, fixed = TRUE)

  # First clean up the corresponding section
  chat_data <- list(question = "", context = "", answer = "", examples = "")

  # Check if the question is the same as the previous one
  last_data <- .get_temp(paste0("SciViews.chatbot.page", last_page), chat_data)
  if (last_data$question == question2) {
    # Reuse the previous answer
    page <- last_page
  } else {
    # Update chat_data
    chat_data$question <- question2
    .assign_temp(chat_page, chat_data)

    # TODO: build context
    #chat_data$context = ...

    if (tolower(lang) == "fr")
      question <- paste(question, "R\u00e9pond en fran\u00e7ais", sep = "\n")

    request <- httr2::request(url) |>
      httr2::req_headers("Accept" = "application/json") |>
      httr2::req_body_json(list(
        model = model,
        messages = list(
          list(role = "user", content = question)
        ),
        `tool-choice` = "none",
        `response-format` = "text",
        max_tokens = max_tokens,
        stream = FALSE,
        raw = TRUE,
        stop = c("[/ANS]", "[/SOL]", "[/INST]")
      ))
    .assign_temp("SciViews.chatbot.last_request", request)
    if (isTRUE(verbose)) {
      message("Request:")
      print(request)
    }

    # Perform the request
    res <- httr2::req_perform(request)
    .assign_temp(".SciViews.chatbot_last_result", res)
    if (isTRUE(verbose)) {
      message("Result:")
      print(res)
    }

    # Extract message
    body <- httr2::resp_body_json(res)
    answer <- body$message$content
    if (isTRUE(verbose)) {
      message("Answer:")
      print(body)
    }
    # Use Roxygen2 to transfor Markdown tags to Rd tags
    markdown_on <- getNamespace("roxygen2")$markdown_on
    old_mkdown <- markdown_on()
    markdown_on(TRUE)
    tagged <- roxygen2::roxy_tag("description", answer) |>
      roxygen2::tag_markdown()
    chat_data$answer <- tagged$val

    markdown_on(old_mkdown)

    # Extract examples
    examples <- gsub("Output:\n\n```[Rr]\n", "Output:\n\n```\n", examples)
    examples <- strsplit(answer, "```", fixed = TRUE)[[1]]
    examples <- examples[grepl("^[Rr]\n", examples)]
    examples <- sub("^[Rr]\n", "", examples)
    examples <- paste(examples, collapse = "\n")
    chat_data$examples <- examples
    .assign_temp(chat_page, chat_data)
  }

  # Record the page number
  options(SciViews.chatbot.page = page)

  # Display the chatbot help page with question, answer and examples inserted
  print(help(paste0("chatbot", page), package = "SciViews"))

  # Return the answer invisibly
  invisible(chat_data$answer)
}

#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$answer else "No answer" }
#'
#' @name chatbot1
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page1')) SciViews.chatbot.page1$examples else "# No example" }
NULL

#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$answer else "No answer" }
#'
#' @name chatbot2
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page2')) SciViews.chatbot.page2$examples else "# No example" }
NULL

#' SciViews chatbot
#'
#' @description
#' *A chatbot does not always provide reliable results. Take this with a grain of salt!*
#'
#' Question: \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$question else "No question" }
#'
#' @section Answer:
#' \Sexpr[eval=TRUE,stage=render,results=rd]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$answer else "No answer" }
#'
#' @name chatbot3
#' @examples
#' \Sexpr[eval=TRUE,stage=render,results=text]{ if (exists('SciViews.chatbot.page3')) SciViews.chatbot.page3$examples else "# No example" }
NULL
