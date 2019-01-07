gimme <- function(path = getwd(), max_size = 1024) {
  
  # go to album, and save html
  
  df <- tibble::tibble(path = fs::dir_ls(glue::glue("{path}/Gimme_files"), regex = ".jpeg$"),
               name = basename(path)) %>%
    dplyr::filter(! stringr::str_detect(name, "\\).jpeg")) %>%
    dplyr::mutate(info = purrr::map(path, ~magick::image_read(.x) %>%
                                      magick::image_info()),
           width = purrr::map_int(info, ~.x$width[1]),
           height = purrr::map_int(info, ~.x$height[1]),
           size = ifelse(width >= height,
                         glue::glue("{max_size}x0"),
                         glue::glue("0x{max_size}")),
           url = glue::glue("https://img.gimme.eu/unsafe/{size}/",
                      "s3-eu-central-1.amazonaws.com/uploads.gimme.eu/",
                      "{name}"))
  
  purrr::walk2(df$url, df$name, ~ magick::image_read(.x) %>%
          magick::image_write(.y))
  
  unlink(glue::glue("{path}/Gimme_files"), recursive = TRUE)
  unlink(glue::glue("{path}/Gimme.html"))
  
  # alternative, login with rvest::html_session and rvest::html_form
  # get image urls from online album html
  
  # or try using chrome cookies of gimme.eu?
  # https://stackoverflow.com/questions/31021764/where-does-chrome-store-cookies
  
}