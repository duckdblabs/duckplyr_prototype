con <- DBI::dbConnect(duckdb::duckdb())

q <- rlang::quo(cyl == 4L || mpg > 20)

print(duckdb::expr_ref("asdf"))
print(duckdb::expr_scalar(42))

duckdb_encode_expression <- function(e, env) {
	switch(typeof(e),
		language = {
			# here we have to do function mapping
			fun_name <- as.character(e[[1]])
			# TODO how do we determine whether something is an infix op?
			res <- vapply(e[2:length(e)], duckdb_encode_expression, "character", env)
			return(sprintf("%s(%s)", fun_name, paste(res, collapse = ",")))
      	},
      	symbol = {
      		# TODO check whether symbol is in table, in which case we return identifier
      		# if its not, we try to
      		return(as.character(e))
  		},
  		integer = {
  			return(as.character(e))
  		},
  		double = {
  			return(as.character(e))
  		}
  	)
  	stop("Unexpected symbol type ", typeof(e))

}
duckdb_encode_expression(rlang::quo_get_expr(q), rlang::quo_get_env(q))



# library('duckplyr')
# limit_mpg <- 20
# mtcars %>% filter(cyl == 4, mpg > limit_mpg)

# battle plan:
# do we create expressions in the R API? probably...
# export relational API to R, too