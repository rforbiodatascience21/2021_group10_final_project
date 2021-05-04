# Define project functions ------------------------------------------------

suffix_numeric_cols <- function(df, string){
  df %>% 
    rename_with(.cols = where(is.numeric),
              ~ str_c(.,
                      string))
}
