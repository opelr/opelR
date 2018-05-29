#' Not in
#'
#' Opposite of `%in%`
#' @usage `"a" %!in% letters`
#' @export

`%not in%` <- Negate(`%in%`)
`%ni%` <- Negate(`%in%`)
`%!in%` <- Negate(`%in%`)
