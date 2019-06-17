R_files <- list.files(pattern = "\\.R$")
R_files <- R_files[R_files != "make.R"]

RData_files <- paste0(R_files, "Data")

R_files <- sample(R_files)

RData_files <- paste0(R_files, "Data")

###################################################################

over <- length(RData_files) %% 3
out_names <- if(over > 0) c(RData_files, rep("", 3 - over)) else RData_files
deps <- matrix(out_names, nrow = 3)
deps <- apply(deps, 2, function(x) paste("\t", paste(x, collapse = " "), "\\\n"))

make_depend <- paste(deps, collapse = "")
make_depend <- substring(make_depend, 3)
make_depend <- substring(make_depend, 1, nchar(make_depend) - 2)

make_operations <- 
  paste0(RData_files, 
         ": ", paste("../chicago.RData", R_files), " ",
         "\n\t @date '+ %Y-%m-%d %H:%M:%S: starting  ", R_files, "'",
         "\n\t @$(RCMD) BATCH --vanilla ", R_files,
         "\n\t @date '+ %Y-%m-%d %H:%M:%S: finishing ", R_files, "'\n\n")


cat(paste("SHELL = /bin/bash\n",
          "R    ?= R \n",
          "RCMD =@$(R) CMD\n",
          "all: ",
          make_depend,
          "\n\n",
          paste0(make_operations, collapse = ""),
          sep = ""),
    file = "makefile")
