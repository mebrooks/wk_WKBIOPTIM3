
# 2016-10-30: adaptacao do script para casos em que var2=""
# 2017-06-08: import script from WKSDO to WKBIOPTIM
# 2017-06-08: removed implementation of localMaxima that was giving some problems; new localMaxima2 solves problems of plateaus while ascending and plateaus in maxima 
# 2017-06-10: removed implementation of localMaxima that was giving some problems; new localMaxima2 solves problems of plateaus while ascending and plateaus in maxima 
# 2017-06-10: fixed error in sims sample size per strata
# 2017-06-11: added target_var3 [version V3]
# 2017-06-11: added sample_size dependency on number of length classes
# 2017-06-12: significant change to simulation algorithm: simplification and adaptation to two stage sampling [version V4] and remaining types of sampling [version V5]
# 2017-06-18: creations of faz_sim_sample and significant other changes and simplifications [version V7]
# 2017-06-19: reviewed code, added graphs; added median to summary
# 2017-06-19: adaptated to RDB CA
# 2017-06-21: fix one issue with mode determination during wkbioptim
# 2017-06-22: simplification, for cycle 
# 2017-06-28: ?? [version V9]
# 2017-08-XX: improved annotation and handling of results [version V10]
# 2017-08-XX: exploratory analysis of smooth and proportion for mode acceptance moved outside loop [version V10]
# 2017-08-XX: included parallel processing to speed up time ~ 5 times faster [version V11]
# 2017-09-15: open project Shrimp RCM MED [Maria Falciani] 
# 2017-09-15: added lookup conversion table for column names
# 2017-09-15: added check on var distribution of samples selected for simulation
# 2018-05-22: renamed to "002_sim_data.r" [previously "teste2_v11_share_pand_clean.r"]
# 2018-05-22: renamed var sample_id->sampId
# 2018-05-22: added check of consequences of min_n selection [now also in no of samples]
# 2018-05-22: added automatic detection of original_class_span
# 2018-05-27: added automatic determination of weight-length parameters
# 2018-05-28: added mode tests on categorical variables

	# 2018-09-10: annotation and directory structure improved
	# 2018-09-10: renamed to 003_sim_data to match new directory structure
	# 2018-09-10: moved sampling options to the outside of the loop (makes more sense, offers more flexibility if that is ever needed)
	# 2018-09-12: improved model section:
					# added VBGF (from Patricia)
					# fixed output (function was not compiling model results)
					# added rules to issue different combinations of models

		# Wishlist
			# couple sampling by weight [see fishPI algorithm...]
			# adapt to CA table
			# estratificacao desigual (e.g., 1 por class comp nos pequenos, todos os grandes)
			# improve format specification
			
			# add details of original design
			# add emdist
			# Kullback, S., Leibler, R.A. (1951). On information and sufficiency. Annals of Mathematical Statistics, 22: 79-86

			# add simulation of population from stratified samples (via hans gerritsen var?)

# =================	
# Preamble
# =================			
			
	rm(list=ls())			

# read functions
	#source("sample_level_funs1.R") # contains "expl.analysis.smooth.and.modes", sim_sample

# read packages
	library(rlist) # list.merge
	#library(reshape)
	library(parallel)		

# load file
	load("001_Prepared_Inputs\\Input_data.Rdata")
	head(df0)

# read variable table
	variable_table <- read.csv2("000_Auxiliary_Tables\\variable_table.csv", as.is=TRUE)
	variable_table <- variable_table[1:2,]
	head(variable_table)
	
# =================	
# Sampling design
# =================

		# set sampling design of sample data
		sampling_design <- list (stratified = FALSE, strata_var = "")
		# CHECK IF DEFINED
		# sampling_design <- list (stratified = TRUE, strata_var = "lenCls")
		# sampling_design <- list (stratified = TRUE, strata_var = "matStage")
		# sampling_design <- list (stratified = TRUE, strata_var = "sex")
	
	
# =================	
# Define Sim population 
# =================

	# setting of the minimum number of individuals considered representative
		load("001_Prepared_Inputs\\Min_n.Rdata")
		#min_n<-100

		table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n]; 
		samples_to_analyze<-names(table_select_samples)		
		
		
# ======================
# Mode determination		
# ======================
	
	# wishlist:
		# include ouputs from mixdist package in LocalMaxima2
		# compare outputs with Julia's find_mode()
		# rename "ls_auto_modes" to something more appropriate

	source("000_Auxiliary_Funs\\func_expl_analysis_smooth_and_modes.r")
	source("000_Auxiliary_Funs\\func_detect_modes_in_samples.r")
	source("000_Auxiliary_Funs\\func_localMaxima.r")
	source("000_Auxiliary_Funs\\func_localMaxima2.r")
	source("000_Auxiliary_Funs\\func_faz_sim_sample.r")
	source("000_Auxiliary_Funs\\func_make_summary_numeric.r")
	source("000_Auxiliary_Funs\\func_make_summary_categorical.r")
	source("000_Auxiliary_Funs\\func_make_models_random.r")
	source("000_Auxiliary_Funs\\func_lrPerc.r")
		
		
		for (var1 in variable_table$variable)
			{
			#source("sample_level_funs1.R")
			print(var1)
			if (var1==variable_table$variable[1])
				{
				ls_auto_modes<-func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	
				} else {
						#source("sample_level_funs1.R")	
						ls_auto_modes<-list.merge(ls_auto_modes, func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	)
						#ls_auto_modes<-list.merge(ls_auto_modes, func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% "2029_999",]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	)
						}
			}
	
	str(ls_auto_modes,3)	
	ls_auto_modes[["2017_2049"]]$lenCls
	ls_auto_modes[["2017_2049"]]$age
	# the following are not defined for example
	#ls_auto_modes[["2017_2049"]]$matStage
	#ls_auto_modes[["2017_2049"]]$sex
	#ls_auto_modes[["2017_2049"]]$mature
	
	
# ======================
# Weight - Length Relationship		
# ======================

	# determines an overall [unweighed] Weight - Length Relationship to be used in determining weights of samples
	
	# automatic determination
		# this is a bit brute force but provides an approximation - check the parameters against known values and if needed define them manually
		coefs_weight_length<-coef(lm(log(df0$indWt)~log(df0$lenCls)))
		names(coefs_weight_length)<-c("a","b")
		coefs_weight_length
	# use code below if you was to define manually
		# coefs_weight_length <- c(a= -7.40, b=3.04)
	
# =======================
# Starting values for VBGF
# =======================

	# automatic
		require(FSA)
		start_values_vbgf<-unlist(vbStarts(lenCls~age, data = df0))
		names(start_values_vbgf)<-c("Linf_teo","K_teo","t0_teo")
	# manual
		start_values_vbgf<-c(Linf_teo = 140, K_teo = 0.40, t0_teo = 1.6)
		svTypical <- list(Linf = start_values_vbgf[["Linf_teo"]],K = start_values_vbgf[["K_teo"]],t0 = start_values_vbgf[["t0_teo"]]) ##Initial parameters to run the VBGM
		vbTypical <- lenCls~Linf*(1-exp(-K*(age-t0)))
		control<- nls.control(maxiter=100)
		fitTypical<-nls(vbTypical,data=df0,start=svTypical,control)
		start_values_vbgf<-c(Linf_teo = 215.6136, K_teo = 0.4365, t0_teo = -2.7327)
		
# =======================
# Simulations	
# =======================

		# if your data is a simple random sample of individuals from which all biological variables were sampled you will be able to test all sampling strategies
		# if your data was stratified with regards to one of the variables you have to take that into account. This means:
			# you can maintain the stratification and look into the consequences of a reduction (or increase, by recycling) in the number of individuals samples per strata
			# you can attempt to unstratify, creating a pseudo random sample and then test scenarios in it - this will only be simple if you are dealing with 1 variable only

	#source("sample_level_funs1.R")			
									
	
	# creates a storage object for the simulations
		# if in development
		#samples_to_analyze<-c("2014_2535", "2014_2542", "2015_2562", "2015_2565")
		#samples_to_analyze<-c("2017_2549")
		# if in production
		ls_DT_compiled<-sapply(samples_to_analyze, function(x) NULL)

	
	# sets sampling options	
		# wishlist
			# # Sampling of different number of individuals without replacement (sample size dependent of size classes in the sample)
				# tmp.n_classes<-length(ls_auto_modes[["Length_class"]][["original_breaks"]])
				# sampling_options = list (n_sims = 100, stratified = FALSE, replacement=FALSE, sample_all_available = TRUE, sample_all_available_warning = TRUE, stages="one", samp_sizes = c(1:5*tmp.n_classes), strata_var = "none", vars_to_keep = c("Length_class", "Weight", "Age", "Sex", "Maturity_stage", "Mature"))
				
		
		ls_sampling_options<-sapply(samples_to_analyze, function(x) NULL)
		
		for (sampId in samples_to_analyze)
			{
			ls_sampling_options[[sampId]] <- list (n_sims = 50, 
												stages="one", 																							# no of stages
													stratified = FALSE, strata_var = "", 																# stratification details
														#stage1_samp_size=NA, samp_sizes = c(10), 														# samp sizes
														stage1_samp_size = NA, samp_sizes = c(seq(10,90, by=10), nrow(df0[df0$sampId == sampId,])), 	# samp sizes
															replacement = TRUE, 	sample_all_available = FALSE, sample_all_available_warning = FALSE, # replacement options
																models = c("weight-length","VBGF"))														# models to run: "weight-length","sex-ratio","L50","VBGF"
			}
		
		seed<-1
		set.seed(seed)
		ptc1<-Sys.time()	
		# approx 6 min for 200 replicates from 46 samples, 36 samples sizes
		# approx 26 min for 500 replicates from 46 samples, 36 samples sizes, without replacement
		# approx 2.1hr min for 500 replicates from 38 SD25 HER samples, 9+1 samples sizes, without replacement
	
for (sampId in samples_to_analyze)
	{
	source("000_Auxiliary_Funs\\func_make_models_random.r")	
	source("000_Auxiliary_Funs\\func_make_summary_categorical.r")	
	source("000_Auxiliary_Funs\\func_faz_sim_sample.r")	
	print("==========================")
	print(sampId)
	print("==========================")
	
		#ptc2<-Sys.time()
	
	# selects sample
		df1<-df0[df0$sampId == sampId,]
		# use this instead if you want to look at a specific sample
			#df1<-df0[df0$sampId == "2017_2066",]
	
	# selects sampling options
		sampling_options<-ls_sampling_options[[sampId]]
				
# ===============			
# Simulations of samples
# ===============
	
				ls_sims1<-faz_sim_sample(sampDes = sampling_design, sampOpt = sampling_options, df1o = df1)	

		
# ====================
# Building of sample statistics
# ====================


	# creates storage object
	ls_sims_stats<-lapply(sapply(as.character(sampling_options$samp_sizes), function(x) NULL), function(x) sapply(c(variable_table$variable, sampling_options$models), function(x) NULL)) 

	vars_numerical<-variable_table$variable[variable_table$type=="numerical"]
	vars_categorical<-variable_table$variable[variable_table$type=="categorical"]
	
	detectCores(all.tests = FALSE, logical = TRUE)
	cl <- makeCluster(3)
	#clusterExport(cl, varlist = c("make_summary_numeric","variable","df1","ls_auto_modes","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	clusterExport(cl, varlist = c("make_summary_numeric","df1","ls_auto_modes","coefs_weight_length","start_values_vbgf","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	
	#sampId = "2001_999"
	ls_auto_modes_sample<-ls_auto_modes[[sampId]]
	#j=1

	
	
	for (j in 1:length(sampling_options$samp_sizes))
			{
			if(!sampling_options$stratified & (sampling_options$stages =="one" | sampling_options$stages =="two")) # not stratified, one or two stages
				{
				print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
				if(sampling_options$stages == "one") w <- "1st_Stage" else w <- "2nd_Stage"
				 for (variable in vars_numerical)
					{	
					#ptc1<-Sys.time()
					print(paste(".",variable, sep=""))
					#source("sample_level_funs1.R")
					browser()
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric(x[[w]], variable, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))
					#ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims1[[j]], function(x, variable, coefs_weight_length) make_summary_numeric(x, variable = variable, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])))				
					#print(Sys.time()-ptc1)
					}
				 for (variable in vars_categorical)
					{	
					#ptc1<-Sys.time()
					print(paste(".",variable, sep=""))
					#source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical(x[[w]], variable)}))
					#ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims1[[j]], function(x) make_summary_categorical(x[[w]], variable)))				
					#print(Sys.time()-ptc1)
					}	

				# adds weight estimate from lenCls to all variables
					if ("lenCls" %in% vars_numerical)
						{
						for (variable in c(vars_numerical, vars_categorical)[c(vars_numerical, vars_categorical) != "lenCls"] )
							{
						ls_sims_stats[[j]][[variable]]$estim_weight<-ls_sims_stats[[j]][["lenCls"]]$estim_weight
							}
						}

				# ==================
				# models: 
				# ==================	
				
			
					print(".models")
					if("weight-length" %in% sampling_options$models) ls_sims_stats[[j]][["weight-length"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="weight-length")}))
					if("sex-ratio" %in% sampling_options$models) ls_sims_stats[[j]][["sex-ratio"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="sex-ratio")}))
					if("L50" %in% sampling_options$models) ls_sims_stats[[j]][["L50"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="L50")}))
					if("VBGF" %in% sampling_options$models) ls_sims_stats[[j]][["VBGF"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="VBGF")}))
				}
			}
			
			#browser()	
				ls_DT_compiled[[sampId]]<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)
				
				DT<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)
				
				target_object = ls_sims_stats
				for (variable in c(vars_categorical, vars_numerical,sampling_options$models))
				{
				# compilation of results
				DT[[variable]]<-data.frame()
				for (i in names(target_object))
					{
					DT_sim<-data.frame(sampId = df1$sampId[1], sim = as.numeric(as.character(i)), target_object[[i]][[variable]])
					DT[[variable]]<-rbind(DT[[variable]], DT_sim)
					}
				ls_DT_compiled[[sampId]][[variable]]<-DT[[variable]]
				}
			stopCluster(cl)
			
			#print(Sys.time()-ptc2)
			
}
Sys.time()-ptc1	

	
		save(ls_DT_compiled,df0, file=paste("003_Simdata\\ls_DT_compiled_teste_",format(Sys.time(), "%Y%m%d%H%M"),".rdata", sep=""))
	

	

# =====================			
			
			
			
# bugs 
	# chisq_test_prob = NA in random sampling one stage
	# chisq_test_prob = NA in random sampling one stage
	

	for (j in 1:length(sampling_options$samp_sizes))
			{
			if(sampling_options$stratified)
			{
			print(paste("Processing sample size", sampling_options$samp_sizes[j]))				
				 # estimate for the stratification variable - these are the population
			 if(sampling_options$stages =="one")
				{
				source("sample_level_funs1.R")
				 # print(paste(".","strata_var", sep=""))
				 # if (sampling_options$strata_var %in% vars_numerical)
					# {
					# ls_sims_stats[[j]][[sampling_options$strata_var]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric(x$"1st_Stage", sampling_options$strata_var, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))
					# } else {
							# ls_sims_stats[[j]][[sampling_options$strata_var]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical(x$"1st_Stage", sampling_options$strata_var)}))
							# }
					
				 #for (variable in vars_numerical[vars_numerical!=sampling_options$strata_var]) # se variavel é estraficada - amplia e depois calcula
				 for (variable in vars_numerical) # se variavel é estraficada - amplia e depois calcula
					{
					print(paste(".",variable, sep=""))
					source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric_stratified(x, variable, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))
					#ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims1[[j]], function(x) make_summary_numeric(x, variavel = variable)))				
					}						
				 #for (variable in vars_categorical[vars_categorical!=sampling_options$strata_var]) # se variavel é estraficada - amplia e depois calcula
				 for (variable in vars_categorical) # se variavel é estraficada - amplia e depois calcula
					{
					print(paste(".",variable, sep=""))
					source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical_stratified(x, variable)}))
					#ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims1[[j]], function(x) make_summary_numeric(x, variavel = variable)))				
					}		
				}
				
			if(sampling_options$stages =="two")
				{
				source("sample_level_funs1.R")
				 print(paste(".","strata_var", sep=""))
				 # if (sampling_options$strata_var %in% vars_numerical)
					# {
					# ls_sims_stats[[j]][[sampling_options$strata_var]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric(x$"2nd_Stage", sampling_options$strata_var, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))
					# } else {
							# ls_sims_stats[[j]][[sampling_options$strata_var]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical(x$"2nd_Stage",  sampling_options$strata_var)}))
							# }
							
				 #for (variable in vars_numerical[vars_numerical!=sampling_options$strata_var]) # se variavel é estraficada - amplia e depois calcula
				for (variable in vars_numerical) # se variavel é estraficada - amplia e depois calcula
					{
					print(paste(".",variable, sep=""))
					source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric_stratified(x, variable, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))				
					}						
				 #for (variable in vars_categorical[vars_categorical!=sampling_options$strata_var]) # se variavel é estraficada - amplia e depois calcula
				 for (variable in vars_categorical) # se variavel é estraficada - amplia e depois calcula
					{
					print(paste(".",variable, sep=""))
					source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical_stratified(x, variable)}))
					#ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims1[[j]], function(x) make_summary_numeric(x, variavel = variable)))				
					}		
				}				
			
			# ==================
			# models: 
			# ==================			
			
					print(".models")
					ls_sims_stats[[j]][["weight-length"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_stratified(x, model="weight-length")}))
					ls_sims_stats[[j]][["sex-ratio"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_stratified(x, model="sex-ratio")}))
					ls_sims_stats[[j]][["L50"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_stratified(x, model="L50")}))
					
				}			
				}
	
				
ls_sims_stats[[1]]$matStage
ls_sims_stats[[2]]$matStage
ls_sims_stats[[1]]$sex
ls_sims_stats[[2]]$sex
ls_sims_stats[[1]]$mature
ls_sims_stats[[2]]$mature
ls_sims_stats[[1]]$age
ls_sims_stats[[2]]$age
ls_sims_stats[[1]]$lenCl
ls_sims_stats[[2]]$lenCl
ls_sims_stats[[2]][["weight-length"]]
ls_sims_stats[[2]][["sex-ratio"]]
ls_sims_stats[[2]][["L50"]]

















	
	
	for (j in 1:length(sampling_options$samp_sizes))
			{
			print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
			
			 for (variable in c("lenCls"))
				{
				#if(variable %in% sampling_options$vars_to_keep)
				#{
				#wishlist: put a chekc if variables exist
				
				print(paste(".",variable, sep=""))
				#if(!sampling_options$stratified)
				if(!sampling_options$stratified & sampling_options$stages =="one")
					{
						source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric(x$'1st_Stage', variable)}))
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",parLapply(cl, X = ls_sims[[j]], function(x) make_summary_numeric(x, variavel = variable)))
					} else {
						ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims[[j]], function(x){
																					sample_structure<-prop.table(table(x[,variable], x[,"strata_var"], useNA="al"),2)
																					sample_structure[is.na(sample_structure)]<-0
																					a<-unique(x[,c("strata_var", "strata_size")])
																					strata_totals<-table(rep(a[,"strata_var"], a[,"strata_size"]), useNA="al")
																					x <- t(sample_structure %*% strata_totals)
																					x <- rep(as.numeric(colnames(x)), x)
																					make_summary_numeric(x, variavel = variable) }))
						}
				}
			} 
			stopCluster(cl)	

				target_var="Length_class"
				target_object = ls_sims_stats
				# compilation of results
				DT<-data.frame()
				for (i in names(target_object))
					{
					DT_sim<-data.frame(sampId = df1$sampId[1], sim = as.numeric(as.character(i)), target_object[[i]][[target_var]])
					DT<-rbind(DT, DT_sim)
					}
				ls_DT_compiled[[df1$sampId[1]]]<-DT
}

difftime(Sys.time(), ptc1, units="min")	
	
	save(df0, ls_DT_compiled, sampling_options, seed, file="002_Simdata\\min_1000_replic_100.Rdata")

			# mean weights from samples
				df_sl<-read.table("001_inputs\\PAN_SD_20_2016_subset.txt", header=TRUE, sep="\t")
				df_sl$sampId<-paste(df_sl$Trip_number, df_sl$Station)
				
				df_sl<-df_sl[df_sl$sampId !="1405 1",]
				
				table(df0$sampId)
				tapply(df_sl$Subsample_weight, df_sl$sampId, sum)
			
				# average weights [based on mean weights from samples]
					n_indiv<-250
					tapply(df_sl$Subsample_weight, df_sl$sampId, sum)/table(df0$sampId)
					tapply(df_sl$Subsample_weight, df_sl$sampId, sum)/table(df0$sampId)*n_indiv
					boxplot(c(tapply(df_sl$Subsample_weight, df_sl$sampId, sum)/table(df0$sampId)*n_indiv))
					range(tapply(df_sl$Subsample_weight, df_sl$sampId, sum)/table(df0$sampId)*n_indiv)

	#setwd("\\\\storage-lk.slu.se\\home$\\nupr0001\\My Documents\\006 - ICES WGs\\20170620_ICES_WKBIOPTIM\\work")
	#load("003_Outputs\\PAN_SD_20_2016_min_220_replic_500_with_replacement.Rdata")
	load("003_Outputs\\PAN_SD_20_2016_min_350_replic_500.Rdata")
	
	# Object description: ls_DT_compiled
	# ls_DT_compiled is a list where each branch is a sampId and contains a data.frame with the calculated indicators. 
	# In each of the data.frames $sim indicates the sample size of the replicates (n_sims lines for replicate)
	
	str(ls_DT_compiled,1)
	
	
	head(ls_DT_compiled[[2]])				
	tail(ls_DT_compiled[[2]])				
	head(ls_DT_compiled[[1]])				
				
				
			#DT2 <- ls_DT_compiled[["1401 1"]]	
			#DT2 <- ls_DT_compiled[["1411 3"]]	
			DT2 <- ls_DT_compiled[["01_18_2016 999"]]	
			DT2 <- ls_DT_compiled[["02_18_2016 999"]]	
			
			# plot main outputs	(for one sample)
			
			# outputs one sample 1
				windows(15,7); par(mfrow=c(2,2))
					boxplot(mean~sim, data=DT2, main="mean of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					plot(mean~sim, data=DT2, main="mean of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(cv~sim, data=DT2, main="cv of the mean", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(ttest_prob~sim, data=DT2, main="ttest_prob", las=2, cex.axis=1.1, cex.main=1.5)
					abline(h=0.05, col="red", lty=2, lwd=2)
					#title(outer=TRUE, main=paste(DT2$sampId[1]), line=-1, cex.main=1.5)
		
		
			# outputs one sample 2
				windows(15,7); par(mfrow=c(2,2))
					boxplot(min~sim, data=DT2, main="minimum of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(max~sim, data=DT2, main="maximum of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(median~sim, data=DT2, main="median of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(n_class_sampled~sim, data=DT2, main="No. length classes sampled", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					#title(outer=TRUE, main=paste("sample",DT2$sampId[1]), line=-1)
			
			# outputs one sample 3		
				windows(15,7); par(mfrow=c(2,2))					
					#boxplot(n_modes~sim, data=DT2, main="Number of modes (original)", xlab = "sample size")
					boxplot(n_modes_smooth~sim, data=DT2, main="Number of modes (smooth)", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					#boxplot(n_modes_correct~sim, data=DT2, main="Number of modes correct (original)", xlab = "sample size")
					boxplot(ks_prob~sim, data=DT2, main="ks_prob", las=2, cex.axis=1.1, cex.main=1.5)
					abline(h=0.05, col="red", lty=2, lwd=2)
					boxplot(n_modes_correct_smooth~sim, data=DT2, main="Number of modes correct (smooth)", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					boxplot(MWCV~sim, data=DT2, main="MWCV of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
					abline(h=tail(DT2)$MWCV, col="blue", lty=2, lwd=2)
					#abline(h=20, col="red", lty=2, lwd=2)
				
				
				boxplot(modes_correct_smooth~sim, data=DT2, main="modes_correct_smooth", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
				
				# weight
					windows(15,7); par(oma=c(4,1,1,1))
					boxplot(MWCV~cut(round(DT2$estim_weight_pand), breaks = seq(0,2000, by=50)), data=DT2, main="MWCV of the sample vs. Weight", xlab = "sample weight", las=2, cex.axis=1.1, cex.main=1.5)
		
		# Exploratory: MWCV of the samples
			res<-c()
			for (i in 1:length(names(ls_DT_compiled)))
			{
			res<-c(res, tail(ls_DT_compiled[[i]])$n_modes_smooth[1])
			}
			res
			range(res)		
		# Exploratory: MWCV of the samples
			res<-c()
			for (i in 1:length(names(ls_DT_compiled)))
			{
			res<-c(res, tail(ls_DT_compiled[[i]])$MWCV[1])
			}
			res
			range(res)
			
		# Exploratory: MWCV to sample size relationship
			res<-data.frame()
			for (i in 1:length(names(ls_DT_compiled)))
			{
			res<-rbind(res, data.frame(sim = tail(ls_DT_compiled[[i]])$sim[1], mean = tail(ls_DT_compiled[[i]])$mean[1], MWCV = tail(ls_DT_compiled[[i]])$MWCV[1], n_class_sampled = tail(ls_DT_compiled[[i]])$n_class_sampled[1]))
			}
			res
			plot(res$MWCV~res$sim)			
			cor(res$MWCV,res$sim)			
			plot(res$MWCV~res$mean)
			text(res$MWCV~res$mean, lab=res$sim)
			cor(res$MWCV,res$mean)
			plot(res$MWCV~res$n_class_sampled)			
					

		# selection of the most appropriate sample size
		target_indicator<-"MWCV"
		# for each sample, we calculate the median of each sample size, then we pool everything together in a single table
		ls1<-lapply(ls_DT_compiled, function(x, ind = target_indicator){ out<-tapply(x[[ind]], x$sim, max); 
			# ATT '350' is manual! [ = min_n in sample size selection]
			if(names(out)[length(out)]==350){out<-c(out, out[length(out)])}; names(out)[length(out)]<-"sample"; out})
		res<-do.call("rbind", ls1)
		apply(res, 2, function(x)sum(x<20))/49
		
		target_indicator<-"modes_correct_smooth"
		# for each sample, we calculate the median of each sample size, then we pool everything together in a single table
		ls1<-lapply(ls_DT_compiled, function(x, ind = target_indicator){out<-tapply(x[[ind]], x$sim, sum)/500; 
			# ATT '350' is manual! [ = min_n in sample size selection]
			if(names(out)[length(out)]==350){out<-c(out, out[length(out)])}; names(out)[length(out)]<-"sample"; out})
		res<-do.call("rbind", ls1)
		apply(res, 2, function(x)sum(x>0.9))		

		target_indicator<-"median"
		# for each sample, we calculate the median of each sample size, then we pool everything together in a single table
		ls1<-lapply(ls_DT_compiled, function(x, ind = target_indicator){true_value=tail(x[[ind]])[1]; out<-tapply(x[[ind]], x$sim, function(x){sum(x==true_value)})/500; 
			# ATT '350' is manual! [ = min_n in sample size selection]
			if(names(out)[length(out)]==350){out<-c(out, out[length(out)])}; names(out)[length(out)]<-"sample"; out})
		res<-do.call("rbind", ls1)
		apply(res, 2, function(x)sum(x>0.9))		


		target_indicator<-"estim_weight_pand"
		# for each sample, we calculate the median of each sample size, then we pool everything together in a single table
		ls1<-lapply(ls_DT_compiled, function(x, ind = target_indicator){out<-tapply(x[[ind]], x$sim, function(x){max(x)}); 
			# ATT '350' is manual! [ = min_n in sample size selection]
			if(names(out)[length(out)]==350){out<-c(out, out[length(out)])}; names(out)[length(out)]<-"sample"; out})
		res<-do.call("rbind", ls1)
		apply(res, 2, function(x)max(x))	
		
		#worst case scenario:
		for (n_indiv in seq(100, 250, by=10)){
		print(quantile(tapply(df_sl$Subsample_weight, df_sl$sampId, sum)/table(df0$sampId)*n_indiv, .90))
		}

# ==================================
# ignore from here downwards[left overs of development]
# ==================================
		
			# analysis: two ways:
				# We set define a criteria (e.g., MWCV) and use it as a threshold to determine a sample size that meets it in the vast majority of times 
					# how we can define the criteria
						# by the book (if there is one)
						# from visual inspections of a few replicates
				# We determine the number based on the curve properties
					# e.g., when the curve slope is higher than -1
					# when the change in some property is less than x%
				# we determine the number from an accepted lost relative to a reference sample size or the one we obtained	
				
			

				# 200 for 25 % 
				# 
				
		
			# boxplot of the medians per sample size
				temp<-melt(res); temp<-temp[order(as.character(temp$X2)),]
				boxplot(value~as.numeric(as.character(X2)), data= temp[temp$X2 != "sample",], ylim=c(0, 100), las=2)
	
			
	
	
	
		# we need to define how much MWCV we are willing to loose relative to the original sample or a specific sample size
	
		# number of classes accepted to be lost 
		target = 10
		select<-table(apply(res, 1, function(x){names(which(x < x["sample"]+target))[1]}))
		barplot(select[order(as.numeric(names(select)))])
		
	
	
		# example:
		variable = "Length_class"
		samp_size = 250
		res<-data.frame()
		for (sampId in samples_to_analyze[1])
		{

		# selects sample
			df1<-df0[df0$sampId == sampId,]
			niveis = seq(min(df0[[variable]]), max(df0[[variable]]), by=1)
			windows(15,7)
			par(mfrow=c(2,4))
			barplot(table(factor(df1$Length_class, levels = niveis)), main=paste("original:",nrow(df1)), col="gray", las=2, cex.names=0.7)
			res<-rbind(res, table(sample(factor(df1[[variable]], levels=niveis), size=samp_size, replace=FALSE)))
			for (i in 1:7)
			{
				barplot(table(sample(factor(df1[[variable]], levels=niveis), size=samp_size, replace=FALSE)), las=2, cex.names=0.7,  main=samp_size_aprox_weight, col="gray")
			}
			title(df1$sampId[1], outer=T, line=-1)
			savePlot(filename = paste("003_Selected_Sample_Size\\001_",variable,"_",df1$sampId[1],"_",samp_size_aprox_weight,".png", sep=""), type = "png")
			graphics.off()
		}				
					
					
		# example:					
		DT2<-ls_DT_compiled[[3]]	
			# plot main outputs	
			df1[,target_var]<-as.numeric(as.character(df1[,target_var]))
			
			windows(15,7); par(mfrow=c(2,2))
					boxplot(mean~sim, data=DT2, main="mean of the sample", xlab = "sample size")
					plot(mean~sim, data=DT2, main="mean of the sample", xlab = "sample size")
					boxplot(cv~sim, data=DT2, main="cv of the mean", xlab = "sample size")
					#abline(h=tail(DT2)$cv, col="blue", lty=2)
					#abline(h=tail(DT2)$cv+5, col="red", lty=2)
					boxplot(MWCV~sim, data=DT2, main="MWCV of the sample", xlab = "sample size")
					abline(h=tail(DT2)$MWCV, col="blue", lty=2)
					abline(h=tail(DT2)$MWCV+5, col="red", lty=2)
					title(outer=TRUE, main=paste("sample",DT2$sampId[1]), line=-1)					
					
					
		# results of sample size other indicators
		samp_size = 250
		target_indicator<-"cv"
		res<-do.call("rbind",lapply(ls_DT_compiled, function(x, ind = target_indicator){out<-apply(x[x$sim==samp_size,6:21],2, median);out}))					
		res			
					
					
					
					
					
					
					
# example of MWCV
			windows(15,7)
			par(mfrow=c(4,4))
barplot(table(factor(df1$Length_class, levels = niveis)), main=paste("original:",nrow(df1)), las=2, cex.names=0.7, col="red")
plot.new()
plot.new()
plot.new()
a<-ls_sims_stats$`80`$Length_class
			for (i in rownames(a[a$MWCV>34,])[1:4])
				{
				dados<-ls_sims[["80"]][[i]]
				barplot(table(factor(dados, levels = niveis)), main=paste("example: 34-35%"), col="gray", las=2, cex.names=0.7)
				}
a<-ls_sims_stats$`200`$Length_class
			for (i in rownames(a[a$MWCV>19 | a$MWCV<20,])[1:4])
				{
				dados<-ls_sims[["200"]][[i]]
				barplot(table(factor(dados, levels = niveis)), main=paste("example: 19-20%"), col="gray", las=2, cex.names=0.7)
				}
a<-ls_sims_stats$`400`$Length_class
			for (i in rownames(a[a$MWCV<15,])[1:4])
				{
				dados<-ls_sims[["400"]][[i]]
				barplot(table(factor(dados, levels = niveis)), main=paste("example: 14-15%"), col="gray", las=2, cex.names=0.7)
				}				
		
rownames(a[a$MWCV<15,])		


		#
		target_indicator<-"MWCV"
		res<-do.call("rbind",lapply(ls_DT_compiled, function(x, ind = target_indicator){out<-tapply(x[[ind]], x$sim, median); names(out)[length(out)]<-"sample"; out}))
		
	
	
		


		
			# plot main outputs	
			df1[,target_var]<-as.numeric(as.character(df1[,target_var]))
			
			windows(15,7); par(mfrow=c(2,2))
					boxplot(mean~sim, data=DT, main="mean", xlab = "sample size")
						abline(h=mean(df1[,target_var], na.rm=T), col="red", lty=2)
					plot(mean~sim, data=DT, main="mean", xlab = "sample size")
						 abline(h=mean(df1[,target_var], na.rm=T), col="red", lty=2)
					boxplot(se~sim, data=DT, main="se of mean", xlab = "sample size")
					boxplot(cv~sim, data=DT, main="cv of mean", xlab = "sample size")
			windows(15,7); par(mfrow=c(2,2))		
					boxplot(median~sim, data=DT, main="median", xlab = "sample size")
						abline(h=median(df1[,target_var], na.rm=T), col="red", lty=2)
					boxplot(min~sim, data=DT, main="min", xlab = "sample size")
						abline(h=min(df1[,target_var], na.rm=T), col="red", lty=2)
					boxplot(max~sim, data=DT, main="max", xlab = "sample size")
						abline(h=max(df1[,target_var], na.rm=T), col="red", lty=2)
					boxplot(n_class_sampled~sim, data=DT, main="n_class_sampled", xlab = "sample size")
						abline(h=length(unique(df1[,target_var])), col="red", lty=2)


	
	