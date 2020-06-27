1. "elecONts_server.R": Modified for running on server, major modifications includes:
	- comment out graphs;
	- save all models for comparison later (saved in "~/output/")

2 "output/*": all models saved from "elecONts_server.R"
	- File naming pattern: "fc.+covariate name+_year.rds"
	- e.g.: "output/fc.cov3_5.rds": model with Snow depth + irradiance at surface as cov, in year 2005

3. "analysis_ex.R": Examples for reading in the models in "output/"