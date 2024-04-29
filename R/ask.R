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
          list(role = "system", content = "<s>[INST]I am SciViews chatbot, a large language model. I am trained to answer questions and provide information. My answers are about about the R language, statistics and data science. I answer with precision. I stop answering if I am not sure to tell the truth. The transcript uses Markdown tags. R code is between ```R and ``` tags. Outputs are between ``` and ```. I explain code. I give short examples. My code is executable. I use ggplot() preferrably to plot().[/INST]"),
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
    examples <- gsub("Output ?:\n+```[Rr]\n", "Output:\n\n```\n", answer)
    examples <- strsplit(examples, "```", fixed = TRUE)[[1]]
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

#ask(r"--(<s>[INST]I am SciViews chatbot, a large language model. I am trained to answer questions and provide information. My answers are about about the R language, statistics and data science. I answer with precision. I stop answering if I am not sure to tell the truth. The transcript uses Markdown tags. R code is between ```R and ``` tags. Outputs are between ``` and ```. I explain code. I give short examples. My code is executable. I use ggplot() preferrably to plot().[/INST][INST]How to determine which model is better for nested models using an ANOVA?[\INST])--", verbose = TRUE)

#ask("How to decide which model is better for nested models using an ANOVA? Do not use the aov1 package.")

#ask("Explain subsetting in R in simple terms.")

#ask("Can I use R^2 to compare models of different conplexities?")

#ask("Which metric is better for species x stations datasets?")

# Explique ou est l'erreur dans ce code: y <- c(1, 5, 7, NA, -Inf, 8, ).
# Que fait AIC()? Donne un exemple.
# Qu'est ce que l'hétéroscédasticité
# How to determine which model is better for nested models using an ANOVA?

#<s>[INST] Transcription textuelle d'un dialogue, dans laquelle [[USER_NAME]] interagit avec un assistant IA nommé [[AI_NAME]].
#[[AI_NAME]] est serviable et répond aux demandes de [[USER_NAME]] en français et avec précision.
#[[AI_NAME]] arrête de répondre si il n'est pas certain de dire la vérité.
#La transcription comprend uniquement du texte, elle n'inclut pas de balisage comme HTML et Markdown. [/INST]

#[INST] [[USER_NAME]] : Bonjour, [[AI_NAME]] ! [/INST]
#[[AI_NAME]] : Bonjour [[USER_NAME]] !
#[INST] [[USER_NAME]] : En quelle année sommes-nous ? [/INST]
#[[AI_NAME]] : Nous sommes en [[DATE_YEAR]].
#[INST] [[USER_NAME]] : Veuillez me dire quelle est la plus grande ville d'Europe. [/INST]
#[[AI_NAME]] : la plus grande ville d'Europe est Moscou, la capitale de la Russie.
#[INST] [[USER_NAME]] : Qu'est-ce qu'un chat ? [/INST]
#[[AI_NAME]] : un chat est une espèce domestique de petit mammifère carnivore. C'est la seule espèce domestiquée de la famille des félidés.
#[INST] [[USER_NAME]] : nommez une couleur. [/INST]
#[[AI_NAME]] : bleu.
#[INST] [[USER_NAME]] : Quelle heure est-il ? [/INST]
#[[AI_NAME]] : Il est [[DATE_TIME]].









# \ifelse{latex}{\out{$\alpha$}}{\ifelse{html}{\out{<button onclick='window.location.replace("../html/chatbot2.html")'><img src="../../../doc/html/left.jpg"/></button><button onclick='window.location.replace("../html/chatbot3.html")'><img src="../../../doc/html/up.jpg"/></button></br>}}{SciViews chatbot}}
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

# @section Test:
# This is a test...
# \Sexpr[eval=TRUE,stage=render,results=rd]{ "Test \\\\emph{italic}" }
# \ifelse{latex}{\out{$\alpha$}}{\ifelse{html}{\out{<a href="../html/chatbot1.html" onclick="window.alert(\"test\"); window.parent.helpNavigate(this ref); return false">link</a>}}{alpha}}

# Test of runnable links: \Sexpr[eval=TRUE,stage=render,results=rd]{ cli::ansi_has_hyperlink_support() }
#
# TODO: render differently in text of pdf
# \Sexpr[eval=TRUE,stage=render,results=verbatim]{ '<a href="../chatbot1" onclick="window.parent.helpNavigate(this.href); return false">Test link injection</a>' }

# cli::cli_text("{.run [Click here](rstudio:run:rlang::last_trace())}")
# #' \href{"../chatbot1" onclick="window.parent.helpNavigate(this ref); return false"}{Click here to see the answer in the help page}
# Show md of html file in fenêtre (lite) with curl http://127.0.0.1:23489/open\?url\=<path>

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
