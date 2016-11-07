

annotations.csv: obi-merged.owl resources/annotations.rq
	robot query \
		--input $(word 1,$^) \
		--select $(word 2,$^) $@

.PHONY: assays.csv
assays.csv:
	curl -L "https://docs.google.com/spreadsheets/d/16Jyn-LEXlSOdqAWlPIkaYCoq9SQIJPHeWPojSBEDppA/export?format=csv&id=16Jyn-LEXlSOdqAWlPIkaYCoq9SQIJPHeWPojSBEDppA&gid=0" \
	| sed 's///g' \
	> $@

.PHONY: all
all: assays.csv
	mkdir -p reports/
	rm -f reports/*
	lein run

