library("waddR")
library("tictoc")

# parameters for each run
SEEDEX = 24
PERMNUM = 10000
METHOD = "OS"

OUTDIR = "tables/0.2.5"
DATA_DIR = "data"

# specific to this case data
INPUT_DATA = "data/DataMatrices_ScSubsets.dat"
TABLE_NAMES = c("sc.f19.blood", "sc.f19.decidua", "sc.f20.blood",  "sc.f20.decidua", "sc.f25.blood", "sc.f25.decidua", "sc.f27.blood", "sc.f27.decidua")
# TESTING SETUP
EXCLUDE = c("sc_f19_bloodXsc_f19_decidua", "sc_f19_bloodXsc_f20_blood", "sc_f19_bloodXsc_f20_decidua",
            "sc_f19_bloodXsc_f25_blood", "sc_f19_bloodXsc_f25_decidua", "sc_f19_bloodXsc_f27_blood",
            "sc_f19_bloodXsc_f27_decidua", "sc_f19_deciduaXsc_f20_blood", "sc_f19_deciduaXsc_f20_decidua",
            "sc_f19_deciduaXsc_f25_blood", "sc_f19_deciduaXsc_f25_decidua", "sc_f19_deciduaXsc_f27_blood",
            "sc_f19_deciduaXsc_f27_decidua", "sc_f20_bloodXsc_f25_blood", 
            "sc_f20_bloodXsc_f25_decidua", "sc_f20_bloodXsc_f27_blood", "sc_f20_bloodXsc_f27_decidua",
            "sc_f20_deciduaXsc_f25_blood", "sc_f20_deciduaXsc_f25_decidua", "sc_f20_deciduaXsc_f27_blood",
            "sc_f20_deciduaXsc_f27_decidua", "sc_f25_bloodXsc_f25_decidua", "sc_f25_bloodXsc_f27_blood", 
            "sc_f25_bloodXsc_f27_decidua", "sc_f25_deciduaXsc_f27_blood", "sc_f25_deciduaXsc_f27_decidua", 
            "sc_f27_bloodXsc_f27_decidua"
#            , "sc_f20_bloodXsc_f20_decidua"
) 

wasserstein.sc.timed <- function(name, x, y) {
  stopifnot(dim(x)[1] == dim(y)[1])
  
  # setup
  data <- cbind(x, y)
  x.num_cells <- dim(x)[2]
  y.num_cells <- dim(y)[2]
  
  condition <- c(rep(1,x.num_cells), rep(2,y.num_cells))
  
  # wasserstein.sc run
  cat("... wasserstein.sc test ", name, "\n")
  tic(paste("    Compuation: ", name))
  result <- wasserstein.sc(data, condition=condition, permnum=PERMNUM, method=METHOD)
  toc()
  
  # write results
  if (!endsWith(name, ".csv") && !endsWith(name, ".tsv")) {
   name = paste0(name, ".csv")
  }
  outfile = file.path(OUTDIR, name)
  tic(paste("    Writing results:"))
  write.table(x=result, file=outfile, col.names=TRUE, dec=".")
  toc()
  cat("\n")
}

run_all_combinations <- function() {
  
  # check if all table names exists
  stopifnot(all(sapply(TABLE_NAMES, exists)))
  
  combn(TABLE_NAMES, 2, function(x) {
    name1 = x[1]
    name2 = x[2]
    combined_name <- gsub("\\.", "_", paste0(name1, "X", name2))
    if (!combined_name %in% EXCLUDE){
      wasserstein.sc.timed(combined_name, eval(as.name(name1)), eval(as.name(name2)))
    } else {
      cat("... skipping:", combined_name, "\n\n")
    }
    return(combined_name)
  })
}

if (!interactive()) {

  stopifnot(all(dir.exists(c(DATA_DIR))))

  if (!dir.exists(OUTDIR)) {
    dir.create(OUTDIR)
  }
  
  cat("[*] LOADING DATA ...", "\n")
  load(INPUT_DATA)
    
  cat("[*] RUNNING WASSERSTEIN TESTS ", "\n\n")  
  run_all_combinations()
}

