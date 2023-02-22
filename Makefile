R = R/functions.R R/packages.R R/preprocess.R R/regression.R R/visualize.R R/download-data.R
DATAPURGE =  data/*.RData data/log* data/numerical_results/*.RData data.zip
DATACLEAN = data draft/assets/r/*
FIGSPURGE = figs draft/assets/figs

all: $(R)
	@./run.sh

purge:
	@rm -rf $(DATAPURGE) $(FIGSPURGE)

clean: purge
	@rm -rf $(DATACLEAN)

.PHONY: all purge clean