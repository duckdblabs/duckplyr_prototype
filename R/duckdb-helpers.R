duckdb_names_env <- function(rel) {
	res <- new.env(parent=emptyenv())
	for (n in names(rel)) {
		assign(n, duckdb::expr_reference(n), envir=res)
	}
	rlang::new_data_mask(res)
}

duckdb_encode_expression <- function(e, env, rel) {
	if (inherits(e, "duckdb_expr")) {
		return(e)
	}
	switch(typeof(e),
		language = {
			name <- as.character(e[[1]])
			if (name == "(") {
				stopifnot(length(e) == 2)
				return(duckdb_encode_expression(e[[2]], env, rel))
			}
			args <- list()
			if (length(e) > 1) {
				args <- lapply(e[2:length(e)], duckdb_encode_expression, env, rel)
      		} 
      		return(duckdb::expr_function(name, args))

      	},
      	symbol = {
      		return(duckdb_encode_expression(rlang::eval_tidy(e, data = duckdb_names_env(rel), env=env), env, rel))
  		},
  		list = {
  			stopifnot(inherits(e, "duckdb_expr"))
  			return(e)
  			},
  		integer = {
  			return(duckdb::expr_constant(e))
  		},
  		double = {
  			return(duckdb::expr_constant(e))
  		},
  		logical = {
  			return(duckdb::expr_constant(e))
  		},
  		character = {
  			return(duckdb::expr_constant(e))
  		}
  	)
  	stop("Unexpected symbol type ", typeof(e))
}


# singleton DuckDB instance since we need only one really
# we need a finalizer to disconnect on exit otherwise we get a warning
default_duckdb_connection <- new.env(parent=emptyenv())
get_default_duckdb_connection <- function() {
	if(!exists("con", default_duckdb_connection)) {
	  default_duckdb_connection$con <- DBI::dbConnect(duckdb::duckdb())
	  reg.finalizer(default_duckdb_connection, function(x) {
	  DBI::dbDisconnect(x$con, shutdown=TRUE)
	  }, onexit=TRUE)
	}
	default_duckdb_connection$con
}




