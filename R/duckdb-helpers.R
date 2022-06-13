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

            # behold
            name <- switch(name,
            '|' = 'or',
            '&' = 'and',
            name)

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
	  con <- DBI::dbConnect(duckdb::duckdb())
	  # comparisons
	  DBI::dbExecute(con, 'CREATE MACRO "<"(a, b) AS a < b')
	  DBI::dbExecute(con, 'CREATE MACRO "<="(a, b) AS a <= b')
  	  DBI::dbExecute(con, 'CREATE MACRO "=="(a, b) AS a = b')
  	  DBI::dbExecute(con, 'CREATE MACRO ">="(a, b) AS a >= b')
  	  DBI::dbExecute(con, 'CREATE MACRO ">"(a, b) AS a > b')
  	  # casts
  	  DBI::dbExecute(con, 'CREATE MACRO "as.Date"(a) AS CAST(a as DATE)')
	  DBI::dbExecute(con, 'CREATE MACRO "as.integer"(a) AS CAST(a as INTEGER)')
      # random stuff
	  DBI::dbExecute(con, 'CREATE MACRO "or"(a, b) AS a OR b')
	  DBI::dbExecute(con, 'CREATE MACRO "and"(a, b) AS a AND b')
	  DBI::dbExecute(con, 'CREATE MACRO "ifelse"(c, t, f) AS CASE WHEN c THEN t ELSE f END')
	  DBI::dbExecute(con, 'CREATE MACRO "grepl"(r, s) AS regexp_matches(s, r)')
      # aggregates
	  DBI::dbExecute(con, 'CREATE MACRO "n"() AS COUNT(*)')

	  default_duckdb_connection$con <- con
	  reg.finalizer(default_duckdb_connection, function(x) {
	  DBI::dbDisconnect(x$con, shutdown=TRUE)
	  }, onexit=TRUE)
	}
	default_duckdb_connection$con
}




