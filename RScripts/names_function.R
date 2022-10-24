names_function <- function(.tbl, ...) {
grouped <- group_by(.tbl, ...)
names_split <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))
return(names_split)
}



