#' Render all helper modals, introduction, and notes documents
#'
#' @param .output_dir character, directory location for output markdowns
#' @param .actor_id character, username or identifier. Default is Sys.getenv("USER").
#'
#' @return nothing
#' @export
#'

#' @importFrom rmarkdown render
render_guides <- function(.output_dir = "inst/app/www",
                          .actor_id = Sys.getenv("USER")) {
  rmarkdown::render("./inst/guides/methods.Rmd",
    output_format = "github_document", # -----
    output_file = "methods.md",
    output_dir = .output_dir,
    params = list(
      actor_id = "esch",
      data_date = Sys.Date(),
      sha = system("git rev-parse --short HEAD",
        intern = TRUE
      )
    )
  )


  rmarkdown::render("./inst/guides/faq.Rmd",
    output_format = "github_document", # -----
    output_file = "faq.md",
    output_dir = .output_dir,
    params = list(
      actor_id = "esch",
      data_date = Sys.Date(),
      sha = system("git rev-parse --short HEAD",
        intern = TRUE
      )
    )
  )

  rmarkdown::render("./inst/guides/PresetHelp.Rmd", # "inst/app/www/PresetHelp.Rmd",
    output_format = "github_document", # -----
    output_file = "PresetHelp.md",
    output_dir = .output_dir,
    params = list(
      actor_id = "esch",
      data_date = Sys.Date(),
      sha = system("git rev-parse --short HEAD",
        intern = TRUE
      )
    )
  )

  rmarkdown::render("./inst/guides/otherresources.Rmd",
    output_format = "github_document", # -----
    output_file = "otherresources.md",
    output_dir = .output_dir,
    params = list(
      actor_id = .actor_id,
      data_date = Sys.Date(),
      sha = system("git rev-parse --short HEAD",
        intern = TRUE
      )
    )
  )
}
