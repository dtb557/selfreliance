# Code for tables
# (taking 10 imputations and averaging over them)
# (based on file called "prelim_analysis_july2017.R")
# Goal = loop over bootstrap samples, 
#        create set of tables from each sample,
#        and append them into list of tables
#        so that this list can be used to extract measures of 
#        uncertainty around key point estimates
# September 2017

###
# R packages
###

library(data.table)
library(Hmisc)
library(weights)

fam_adj <- TRUE
exclude_alloc <- FALSE
exclude_top_2_pct <- FALSE
exclude_top_decile_female_earners <- FALSE
exclude_top_decile_male_earners <- FALSE


###
# Directories and files
###

DATA_DIR <- "main/6a_make_analysis_dataset_imputed"
DATA_FILE_PATTERN <- "1_imps_%d_analysis_vars.Rdata" # "%d" is placeholder for year
OUT_DIR <- "main/9_make_tables"
INTERMEDIATE_OUT_FILE <- "1_all_bootstrap_decomp_tables.Rdata"
OUT_FILE <- "1_tabs_list.Rdata"


###
# Parameters
###

NBOOT <- 1000 # set number of bootstrap samples


### 
# Functions
# (run outside the bootstrap loop)
### 


## Derek's function to create decomp components
source("functions/make_decomp_component_table.R")


## Functions to calculate numbers that go into tables
#
# Function to calculate self-reliance r from components 
fun.sr <- function(dat.decomp) {
          r.out <- (1/(dat.decomp$sigma_x*dat.decomp$sigma_y))*
                    sum(dat.decomp$pi_g*(dat.decomp$var_xg+
                                         dat.decomp$cov_xg_partnerg+
                                         dat.decomp$cov_xg_otherg)+
                        dat.decomp$pi_g*(dat.decomp$mu_gx-dat.decomp$mu_x)*
                                        (dat.decomp$mu_gy-dat.decomp$mu_y))
          return(r.out[1])
          }
#          
# Function to apply SR calculation for each gender/year by imputation    
fun.sr.tab <- function(dat.decomp) {     
              sr.w <- c(fun.sr(dat.decomp$f.70),fun.sr(dat.decomp$f.80),fun.sr(dat.decomp$f.90),
                        fun.sr(dat.decomp$f.00),fun.sr(dat.decomp$f.10))
              sr.m <- c(fun.sr(dat.decomp$m.70),fun.sr(dat.decomp$m.80),fun.sr(dat.decomp$m.90),
                        fun.sr(dat.decomp$m.00),fun.sr(dat.decomp$m.10))
               return(list(sr.w=sr.w,sr.m=sr.m))
               }    
#
# Function for decomposing change in self-reliance,
# full population (partnered+unparntered)
# (separate function for partnered-only population below)
#
fun.sr.decomp.sex.yr.5piece <- function(dat.decomp,dat.decomp.base) {
	                         r.change.obs <- fun.sr(dat.decomp) - fun.sr(dat.decomp.base)
	                         r.ev.g <- (1/(dat.decomp$sigma_x*dat.decomp$sigma_y))*
	                                   (dat.decomp$cov_xg_yg+(dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_gy-dat.decomp$mu_y))
	                         r.ev.g.b <- (1/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y))*
	                                     (dat.decomp.base$cov_xg_yg+(dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*
	                                                                (dat.decomp.base$mu_gy-dat.decomp.base$mu_y))            
	                         r.change.pi <- sum((dat.decomp$pi_g-dat.decomp.base$pi_g)*r.ev.g) 
	                         r.change.ev <- sum(dat.decomp.base$pi_g*(r.ev.g-r.ev.g.b)) 
	                         # getting adjusted weights
	                         pi.t.f <- c(rep(sum(dat.decomp$pi_g[1:3]),3)/rep(sum(dat.decomp.base$pi_g[1:3]),3),
	                                     rep(sum(dat.decomp$pi_g[4:5]),2)/rep(sum(dat.decomp.base$pi_g[4:5]),2))*dat.decomp.base$pi_g
	                         # substantive pieces of change
	                         r.change.sp <- sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) ) 
	                         r.change.ot <- sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_oth-dat.decomp$mu_other)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )
	                         r.change.xx <- sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)^2/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)^2/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )  
	                         r.change.pi.f <- sum((pi.t.f-dat.decomp.base$pi_g)*r.ev.g)      
	                         r.change.pi.e <- sum((dat.decomp$pi_g-pi.t.f)*r.ev.g)         
	                         # adding info on share of substantive components of change that 
	                         # are due to change within versus change between 
	                         r.change.sp.win.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ))/r.change.sp 
	                         r.change.sp.bt.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )/r.change.sp 
	                         r.change.ot.win.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ))/r.change.ot
	                         r.change.ot.bt.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_oth-dat.decomp$mu_other)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )/r.change.ot
	                         r.change.xx.win.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )/r.change.xx  
	                         r.change.xx.bt.share <- 100*sum(dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)^2/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)^2/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) ) )/r.change.xx 
	                         r.ev.g.win <- (1/(dat.decomp$sigma_x*dat.decomp$sigma_y))*
	                                       (dat.decomp$cov_xg_yg)
	                         r.ev.g.bt <- (1/(dat.decomp$sigma_x*dat.decomp$sigma_y))*
	                                       ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_gy-dat.decomp$mu_y))
	                         r.ev.g.b.win <- (1/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y))*
	                                          (dat.decomp.base$cov_xg_yg)  
	                         r.ev.g.b.bt <- (1/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y))*
	                                        ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*
	                                                                (dat.decomp.base$mu_gy-dat.decomp.base$mu_y))    
	                         r.change.ev.win.share <- 100*sum(dat.decomp.base$pi_g*(r.ev.g.win-r.ev.g.b.win))/r.change.ev      
	                         r.change.ev.bt.share <- 100*sum(dat.decomp.base$pi_g*(r.ev.g.bt-r.ev.g.b.bt))/r.change.ev
	                         # substantive pieces of change,
	                         # now by group (not summing across groups)
	                         r.change.sp.g <- dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )  
	                         r.change.ot.g <- dat.decomp.base$pi_g*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_g_oth-dat.decomp$mu_other)/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)*(dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other)/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )  
	                         r.change.xx.g <- dat.decomp.base$pi_g*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )+
	                                            dat.decomp.base$pi_g*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x)^2/
	                                              (dat.decomp$sigma_x*dat.decomp$sigma_y))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x)^2/
	                                              (dat.decomp.base$sigma_x*dat.decomp.base$sigma_y)) )    
	                         r.change.pi.f.g <- (pi.t.f-dat.decomp.base$pi_g)*r.ev.g      
	                         r.change.pi.e.g <- (dat.decomp$pi_g-pi.t.f)*r.ev.g   
	                         # two-part comp.assoc change 
	                         # now by group (not summing across groups)        
	                         r.change.pi.g <- (dat.decomp$pi_g-dat.decomp.base$pi_g)*r.ev.g 
	                         r.change.ev.g <- dat.decomp.base$pi_g*(r.ev.g-r.ev.g.b)
	                         # check and output
	                         r.change.check <- r.change.ev+r.change.pi  
	                         r.change.check2 <- r.change.sp+r.change.ot+r.change.xx+r.change.pi.f+r.change.pi.e
                             r.change.out.all <- list(r.change.obs=r.change.obs,
                                                      r.change.check=r.change.check,
                                                      r.change.check2=r.change.check2, 
                                                      r.change.pi=r.change.pi,r.change.ev=r.change.ev,
                                                      r.change.sp=r.change.sp,r.change.ot=r.change.ot,r.change.xx=r.change.xx,
                                                      r.change.pi.f=r.change.pi.f,r.change.pi.e=r.change.pi.e, 
                                                      r.change.pi.g=r.change.pi.g,r.change.ev.g=r.change.ev.g,
                                                      r.change.sp.g=r.change.sp.g,r.change.ot.g=r.change.ot.g,r.change.xx.g=r.change.xx.g,
                                                      r.change.pi.f.g=r.change.pi.f.g,r.change.pi.e.g=r.change.pi.e.g, 
                                                      r.change.sp.win.share=r.change.sp.win.share,
                                                      r.change.sp.bt.share=r.change.sp.bt.share,
                                                      r.change.ot.win.share=r.change.ot.win.share,
                                                      r.change.ot.bt.share=r.change.ot.bt.share,
                                                      r.change.xx.win.share=r.change.xx.win.share,
                                                      r.change.xx.bt.share=r.change.xx.bt.share,
                                                      r.change.ev.win.share=r.change.ev.win.share,
                                                      r.change.ev.bt.share=r.change.ev.bt.share)    
                             return(r.change.out.all)
                             } 
#
# Function for decomposing change in self-reliance,
# Now limiting to partnered people only
# (approximating, for pooled variance, the partnered-population-only pooled x and y variances)
# (the reason why this is only an approximation is due to the n-1 scaling of variances and because n varies by group;
#  so the application of eve's law isn't totally accurate...) 
#
fun.sr.decomp.sex.yr.5piece.part <- function(dat.decomp,dat.decomp.base) { 
	                         # (re-ordering rows so that partnered, dual-earner is on top,
	                         #  and dropping rows that are for single people)
	                         dat.decomp <- rbind(dat.decomp[2,],dat.decomp[1,],dat.decomp[3,])
	                         dat.decomp.base <- rbind(dat.decomp.base[2,],dat.decomp.base[1,],dat.decomp.base[3,])
	                         # calculating pooled means and variances for the partnered-only population
	                         # (later year)
	                         dat.decomp$pi_g_ponly <- dat.decomp$pi_g/sum(dat.decomp$pi_g)
	                         dat.decomp$mu_x_ponly <- sum(dat.decomp$pi_g_ponly*dat.decomp$mu_gx) 
	                         dat.decomp$mu_y_ponly <- sum(dat.decomp$pi_g_ponly*dat.decomp$mu_gy) 
	                         dat.decomp$mu_pn_ponly <- sum(dat.decomp$pi_g_ponly*dat.decomp$mu_g_pn)
	                         dat.decomp$mu_other_ponly <- sum(dat.decomp$pi_g_ponly*dat.decomp$mu_g_oth)
	                         dat.decomp$sigma_x_ponly <- sqrt(sum(dat.decomp$pi_g_ponly*dat.decomp$sigma_gx^2)+
	                                                     sum(dat.decomp$pi_g_ponly*(dat.decomp$mu_gx-dat.decomp$mu_x_ponly)^2)) 
	                         dat.decomp$sigma_y_ponly <- sqrt(sum(dat.decomp$pi_g_ponly*dat.decomp$sigma_gy^2)+
	                                                     sum(dat.decomp$pi_g_ponly*(dat.decomp$mu_gy-dat.decomp$mu_y_ponly)^2)) 
	                         # (base year)          
	                         dat.decomp.base$pi_g_ponly <- dat.decomp.base$pi_g/sum(dat.decomp.base$pi_g)
	                         dat.decomp.base$mu_x_ponly <- sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$mu_gx) 
	                         dat.decomp.base$mu_y_ponly <- sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$mu_gy) 
	                         dat.decomp.base$mu_pn_ponly <- sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$mu_g_pn)
	                         dat.decomp.base$mu_other_ponly <- sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$mu_g_oth)
	                         dat.decomp.base$sigma_x_ponly <- sqrt(sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$sigma_gx^2)+
	                                                          sum(dat.decomp.base$pi_g_ponly*(dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)^2)) 
	                         dat.decomp.base$sigma_y_ponly <- sqrt(sum(dat.decomp.base$pi_g_ponly*dat.decomp.base$sigma_gy^2)+
	                                                          sum(dat.decomp.base$pi_g_ponly*(dat.decomp.base$mu_gy-dat.decomp.base$mu_y_ponly)^2))                   
	                         # self-reliance r and change
	                         r <- (1/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))*
                                  sum(dat.decomp$pi_g_ponly*(dat.decomp$var_xg+
                                                             dat.decomp$cov_xg_partnerg+
                                                             dat.decomp$cov_xg_otherg)+
                                      dat.decomp$pi_g_ponly*(dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*
                                                            (dat.decomp$mu_gy-dat.decomp$mu_y_ponly))
	                         r.base <- (1/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly))*
                                       sum(dat.decomp.base$pi_g_ponly*(dat.decomp.base$var_xg+
                                                                  dat.decomp.base$cov_xg_partnerg+
                                                                  dat.decomp.base$cov_xg_otherg)+
                                           dat.decomp.base$pi_g_ponly*(dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
                                                                 (dat.decomp.base$mu_gy-dat.decomp.base$mu_y_ponly))  
	                         r.change.obs <- r - r.base
	                         r.ev.g <- (1/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))*
	                                   (dat.decomp$cov_xg_yg+(dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_gy-dat.decomp$mu_y_ponly))
	                         r.ev.g.b <- (1/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly))*
	                                     (dat.decomp.base$cov_xg_yg+(dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                                                (dat.decomp.base$mu_gy-dat.decomp.base$mu_y_ponly))            
	                         r.change.pi <- sum((dat.decomp$pi_g_ponly-dat.decomp.base$pi_g_ponly)*r.ev.g) 
	                         r.change.ev <- sum(dat.decomp.base$pi_g_ponly*(r.ev.g-r.ev.g.b)) 
	                         # (no adjusted weights here since only looking at partnered people)
	                         # pi.t.f <- c(rep(sum(dat.decomp$pi_g_ponly[1:3]),3)/rep(sum(dat.decomp.base$pi_g_ponly[1:3]),3),
	                         #            rep(sum(dat.decomp$pi_g_ponly[4:5]),2)/rep(sum(dat.decomp.base$pi_g_ponly[4:5]),2))*
	                         #                    dat.decomp.base$pi_g_ponly
	                         # substantive pieces of change
	                         r.change.sp <- sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) ) 
	                         r.change.ot <- sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_oth-dat.decomp$mu_other_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )
	                         r.change.xx <- sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)^2/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)^2/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )  
	                         r.change.pi <- sum((dat.decomp$pi_g_ponly-dat.decomp.base$pi_g_ponly)*r.ev.g)            
	                         # adding info on share of substantive components of change that 
	                         # are due to change within versus change between 
	                         r.change.sp.win.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ))/
	                                             r.change.sp 
	                         r.change.sp.bt.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )/r.change.sp 
	                         r.change.ot.win.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ))/
	                                             r.change.ot
	                         r.change.ot.bt.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_oth-dat.decomp$mu_other_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )/r.change.ot
	                         r.change.xx.win.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )/
	                                             r.change.xx  
	                         r.change.xx.bt.share <- 100*sum(dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)^2/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)^2/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) ) )/r.change.xx 
	                         r.ev.g.win <- (1/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))*
	                                       (dat.decomp$cov_xg_yg)
	                         r.ev.g.bt <- (1/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))*
	                                       ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_gy-dat.decomp$mu_y_ponly))
	                         r.ev.g.b.win <- (1/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly))*
	                                          (dat.decomp.base$cov_xg_yg)  
	                         r.ev.g.b.bt <- (1/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly))*
	                                        ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                                                (dat.decomp.base$mu_gy-dat.decomp.base$mu_y_ponly))    
	                         r.change.ev.win.share <- 100*sum(dat.decomp.base$pi_g_ponly*(r.ev.g.win-r.ev.g.b.win))/r.change.ev      
	                         r.change.ev.bt.share <- 100*sum(dat.decomp.base$pi_g_ponly*(r.ev.g.bt-r.ev.g.b.bt))/r.change.ev
	                         # substantive pieces of change,
	                         # now by group (not summing across groups)
	                         r.change.sp.g <- dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_partnerg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_partnerg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_pn-dat.decomp$mu_pn_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_pn-dat.decomp.base$mu_pn_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )  
	                         r.change.ot.g <- dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$cov_xg_otherg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$cov_xg_otherg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)*(dat.decomp$mu_g_oth-dat.decomp$mu_other_ponly)/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)*
	                                             (dat.decomp.base$mu_g_oth-dat.decomp.base$mu_other_ponly)/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )  
	                         r.change.xx.g <- dat.decomp.base$pi_g_ponly*(
	                                             (dat.decomp$var_xg/(dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             (dat.decomp.base$var_xg/(dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )+
	                                            dat.decomp.base$pi_g_ponly*(
	                                             ((dat.decomp$mu_gx-dat.decomp$mu_x_ponly)^2/
	                                              (dat.decomp$sigma_x_ponly*dat.decomp$sigma_y_ponly))-
	                                             ((dat.decomp.base$mu_gx-dat.decomp.base$mu_x_ponly)^2/
	                                              (dat.decomp.base$sigma_x_ponly*dat.decomp.base$sigma_y_ponly)) )    
	                         r.change.pi.g <- (dat.decomp$pi_g_ponly-dat.decomp.base$pi_g_ponly)*r.ev.g 
	                         # two-part comp.assoc change 
	                         # now by group (not summing across groups)   
	                         # (no need for composition change here again, since above composition change doesn't have family 
	                         #  component since only partnered people)      
	                         r.change.ev.g <- dat.decomp.base$pi_g_ponly*(r.ev.g-r.ev.g.b)
	                         # check and output
	                         r.change.check <- r.change.ev+r.change.pi  
	                         r.change.check2 <- r.change.sp+r.change.ot+r.change.xx+r.change.pi
                             r.change.out.all <- list(r=r,r.base=r.base,r.change.obs=r.change.obs,
                                                      r.change.check=r.change.check,
                                                      r.change.check2=r.change.check2, 
                                                      r.change.pi=r.change.pi,r.change.ev=r.change.ev,
                                                      r.change.sp=r.change.sp,r.change.ot=r.change.ot,r.change.xx=r.change.xx,
                                                      r.change.pi=r.change.pi, 
                                                      r.change.sp.g=r.change.sp.g,r.change.ot.g=r.change.ot.g,r.change.xx.g=r.change.xx.g,
                                                      r.change.pi.g=r.change.pi.g,  
                                                      r.change.ev.g=r.change.ev.g,
                                                      r.change.sp.win.share=r.change.sp.win.share,
                                                      r.change.sp.bt.share=r.change.sp.bt.share,
                                                      r.change.ot.win.share=r.change.ot.win.share,
                                                      r.change.ot.bt.share=r.change.ot.bt.share,
                                                      r.change.xx.win.share=r.change.xx.win.share,
                                                      r.change.xx.bt.share=r.change.xx.bt.share,
                                                      r.change.ev.win.share=r.change.ev.win.share,
                                                      r.change.ev.bt.share=r.change.ev.bt.share)    
                             return(r.change.out.all)
                             } 
#
# Function to get output from the functions (above) decomposing change in self-relience
# (applying function to data by imputation and within imputation by sex/year/full vs. partnered pop)
#
fun.sr.decomp.5piece.apply <- function(dat.decomp) {
                              # (full pop)
                              # (women)
                              f.7010.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$f.10,dat.decomp$f.70)
                                f.80.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$f.80,dat.decomp$f.70)
                                f.90.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$f.90,dat.decomp$f.80)
                                f.00.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$f.00,dat.decomp$f.90)
                                f.10.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$f.10,dat.decomp$f.00)
                              # (men)
                              m.7010.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$m.10,dat.decomp$m.70)
                                m.80.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$m.80,dat.decomp$m.70)
                                m.90.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$m.90,dat.decomp$m.80)
                                m.00.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$m.00,dat.decomp$m.90)
                                m.10.5 <- fun.sr.decomp.sex.yr.5piece(dat.decomp$m.10,dat.decomp$m.00)
                              # (partnered-only pop)
                              # (women)
                              f.7010.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$f.10,dat.decomp$f.70)
                                f.80.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$f.80,dat.decomp$f.70)
                                f.90.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$f.90,dat.decomp$f.80)
                                f.00.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$f.00,dat.decomp$f.90)
                                f.10.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$f.10,dat.decomp$f.00)
                              # (men)
                              m.7010.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$m.10,dat.decomp$m.70)
                                m.80.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$m.80,dat.decomp$m.70)
                                m.90.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$m.90,dat.decomp$m.80)
                                m.00.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$m.00,dat.decomp$m.90)
                                m.10.5.part <- fun.sr.decomp.sex.yr.5piece.part(dat.decomp$m.10,dat.decomp$m.00)
                              # output
                              return(list(f.7010.5=f.7010.5,f.80.5=f.80.5,f.90.5=f.90.5,f.00.5=f.00.5,f.10.5=f.10.5,
                                          m.7010.5=m.7010.5,m.80.5=m.80.5,m.90.5=m.90.5,m.00.5=m.00.5,m.10.5=f.10.5,
                                          f.7010.5.part=f.7010.5.part,f.80.5.part=f.80.5.part,f.90.5.part=f.90.5.part,
                                                                      f.00.5.part=f.00.5.part,f.10.5.part=f.10.5.part,
                                          m.7010.5.part=m.7010.5.part,m.80.5.part=m.80.5.part,m.90.5.part=m.90.5.part,
                                                                      m.00.5.part=m.00.5.part,m.10.5.part=m.10.5.part))  
                              }  
#
# Function to calculate the type of correlations that when you multiply them with subgroup shares  
# and then sum over the subgroups, you get the total self-reliance for that gender/year
fun.r.g <- function(dat.decomp) {
           r.ev.g <- (1/(dat.decomp$sigma_x*dat.decomp$sigma_y))*
	                 (dat.decomp$cov_xg_yg+(dat.decomp$mu_gx-dat.decomp$mu_x)*(dat.decomp$mu_gy-dat.decomp$mu_y))
           }
#           
# Function to calculate grand-mean-standardized correlations from summary stats 
fun.r.grand.stand <- function(dat.decomp) { 
	                         # (calculate group-specific grand-mean-standardized correlations)
	                         r.grand.stand <- ((dat.decomp$r_g*dat.decomp$sigma_gx*dat.decomp$sigma_gy+
	                                            dat.decomp$mu_gx*dat.decomp$mu_gy-dat.decomp$mu_gx*dat.decomp$mu_y-
	                                            dat.decomp$mu_x*dat.decomp$mu_gy+dat.decomp$mu_x*dat.decomp$mu_y)/
	                                            (dat.decomp$sigma_x*dat.decomp$sigma_y))/ 
	                                           (sqrt(dat.decomp$sigma_gx^2+dat.decomp$mu_gx^2-2*dat.decomp$mu_gx*dat.decomp$mu_x+dat.decomp$mu_x^2)*
	                                            sqrt(dat.decomp$sigma_gy^2+dat.decomp$mu_gy^2-2*dat.decomp$mu_gy*dat.decomp$mu_y+dat.decomp$mu_y^2)/
	                                            (dat.decomp$sigma_x*dat.decomp$sigma_y))   
                             return(r.grand.stand)
                             } 
#
# Function to replace some NAs in spreadsheets with zeros and to separate male/female data by decade
fun.data.prep <- function(dat.decomp) {
                   # (spreadsheet has NA for corrs and covars among people with no earnings; replace those NAs w/ zeros)
                   dat.decomp$r_g[is.na(dat.decomp$r_g)] <- 0
                   dat.decomp$cov_xg_partnerg[is.na(dat.decomp$cov_xg_partnerg)] <- 0
                   dat.decomp$cov_xg_otherg[is.na(dat.decomp$cov_xg_otherg)] <- 0
                   dat.decomp$cov_xg_yg[is.na(dat.decomp$cov_xg_yg)] <- 0
                   # (separate male and female data, and by decade)
                   dat.decomp.f.70 <- dat.decomp[dat.decomp$sex=="Female" & dat.decomp$decade==1970,]
                   dat.decomp.f.80 <- dat.decomp[dat.decomp$sex=="Female" & dat.decomp$decade==1980,]
                   dat.decomp.f.90 <- dat.decomp[dat.decomp$sex=="Female" & dat.decomp$decade==1990,]
                   dat.decomp.f.00 <- dat.decomp[dat.decomp$sex=="Female" & dat.decomp$decade==2000,]
                   dat.decomp.f.10 <- dat.decomp[dat.decomp$sex=="Female" & dat.decomp$decade==2010,]
                   dat.decomp.m.70 <- dat.decomp[dat.decomp$sex=="Male" & dat.decomp$decade==1970,]
                   dat.decomp.m.80 <- dat.decomp[dat.decomp$sex=="Male" & dat.decomp$decade==1980,]
                   dat.decomp.m.90 <- dat.decomp[dat.decomp$sex=="Male" & dat.decomp$decade==1990,]
                   dat.decomp.m.00 <- dat.decomp[dat.decomp$sex=="Male" & dat.decomp$decade==2000,]
                   dat.decomp.m.10 <- dat.decomp[dat.decomp$sex=="Male" & dat.decomp$decade==2010,] 
                   # (output)
                   output <- list(full=dat.decomp,
                                  f.70=dat.decomp.f.70,f.80=dat.decomp.f.80,
                                  f.90=dat.decomp.f.90,f.00=dat.decomp.f.00,f.10=dat.decomp.f.10,
                                  m.70=dat.decomp.m.70,m.80=dat.decomp.m.80,
                                  m.90=dat.decomp.m.90,m.00=dat.decomp.m.00,m.10=dat.decomp.m.10)
                   return(output)
                   }


## Functions to take output from above functions and 
## put them into our five tables of results
#
# Function to create table 1 from each imputed data set
fun.tab01 <- function(out.decomp,dat.decomp) {    
	         sr <- fun.sr.tab(dat.decomp)     
             tab01 <- rbind(c(sr$sr.w[1],sr$sr.m[1]),c(sr$sr.w[5],sr$sr.m[5]), 
                            c(sr$sr.w[5]-sr$sr.w[1],sr$sr.m[5]-sr$sr.m[1]),
                            c(100*(sr$sr.w[5]-sr$sr.w[1])/sr$sr.w[1],100*(sr$sr.m[5]-sr$sr.m[1])/sr$sr.m[1]),  
                            c(out.decomp$f.7010.5.part$r.base[1],out.decomp$m.7010.5.part$r.base[1]),
                            c(out.decomp$f.7010.5.part$r[1],out.decomp$m.7010.5.part$r[1]),
                            c(out.decomp$f.7010.5.part$r.change.obs[1],out.decomp$m.7010.5.part$r.change.obs[1]),
                            c(100*out.decomp$f.7010.5.part$r.change.obs[1]/out.decomp$f.7010.5.part$r.base[1],
                              100*out.decomp$m.7010.5.part$r.change.obs[1]/out.decomp$m.7010.5.part$r.base[1])) 
            colnames(tab01) <- c("Women","Men")  
            rownames(tab01) <- c("Self-reliance, 1970","Self-reliance, 2010","Raw Change, 1970-2010","% Change, 1970-2010",
                                 "P.Self-reliance, 1970","P.Self-reliance, 2010","P.Raw Change, 1970-2010","P.% Change, 1970-2010" )            
            return(tab01)
             }       
#     
# Function to to create table 2 from each imputed data set
fun.tab02 <- function(dat.decomp) {
                    # (assumption of equal share of men/women)
                    pi_s <- .5
                    # (1970 pooled men+women stats)
                    mu_x.70 <- sum(pi_s*c(dat.decomp$f.70$mu_x[1],dat.decomp$m.70$mu_x[1]))
                    mu_y.70 <- sum(pi_s*c(dat.decomp$f.70$mu_y[1],dat.decomp$m.70$mu_y[1]))
                    sigma_x.70 <- sqrt(sum(pi_s*c(dat.decomp$f.70$sigma_x[1]^2,dat.decomp$m.70$sigma_x[1]^2))+
                                       sum(pi_s*c((dat.decomp$f.70$mu_x[1]-mu_x.70)^2,(dat.decomp$m.70$mu_x[1]-mu_x.70)^2)))
                    sigma_y.70 <- sqrt(sum(pi_s*c(dat.decomp$f.70$sigma_y[1]^2,dat.decomp$m.70$sigma_y[1]^2))+
                                       sum(pi_s*c((dat.decomp$f.70$mu_y[1]-mu_y.70)^2,(dat.decomp$m.70$mu_y[1]-mu_y.70)^2)))
                    # (2010 pooled men+women stats)
                    mu_x.10 <- sum(pi_s*c(dat.decomp$f.10$mu_x[1],dat.decomp$m.10$mu_x[1]))
                    mu_y.10 <- sum(pi_s*c(dat.decomp$f.10$mu_y[1],dat.decomp$m.10$mu_y[1]))
                    sigma_x.10 <- sqrt(sum(pi_s*c(dat.decomp$f.10$sigma_x[1]^2,dat.decomp$m.10$sigma_x[1]^2))+
                                       sum(pi_s*c((dat.decomp$f.10$mu_x[1]-mu_x.10)^2,(dat.decomp$m.10$mu_x[1]-mu_x.10)^2)))
                    sigma_y.10 <- sqrt(sum(pi_s*c(dat.decomp$f.10$sigma_y[1]^2,dat.decomp$m.10$sigma_y[1]^2))+
                                       sum(pi_s*c((dat.decomp$f.10$mu_y[1]-mu_y.10)^2,(dat.decomp$m.10$mu_y[1]-mu_y.10)^2)))
                    # (sr by gender and year)
                    sr <- fun.sr.tab(dat.decomp)     
                    tab01 <- rbind(c(sr$sr.w[1],sr$sr.m[1]),c(sr$sr.w[5],sr$sr.m[5]))
                    # (1970 within/btw components)
                    r.win.70 <- sum(pi_s*tab01[1,]*c(dat.decomp$f.70$sigma_x[1],dat.decomp$m.70$sigma_x[1])*
                                                   c(dat.decomp$f.70$sigma_y[1],dat.decomp$m.70$sigma_y[1])/(sigma_x.70*sigma_y.70))
                    r.btw.70 <- sum(pi_s*c((dat.decomp$f.70$mu_x[1]-mu_x.70),(dat.decomp$m.70$mu_x[1]-mu_x.70))*
                                         c((dat.decomp$f.70$mu_y[1]-mu_y.70),(dat.decomp$m.70$mu_y[1]-mu_y.70))/(sigma_x.70*sigma_y.70))
                    r.70 <- r.win.70+r.btw.70
                    r.win.70.pct <- 100*r.win.70/r.70
                    r.btw.70.pct <- 100*r.btw.70/r.70
                    # (2010 within/btw components)
                    r.win.10 <- sum(pi_s*tab01[2,]*c(dat.decomp$f.10$sigma_x[1],dat.decomp$m.10$sigma_x[1])*
                                                   c(dat.decomp$f.10$sigma_y[1],dat.decomp$m.10$sigma_y[1])/(sigma_x.10*sigma_y.10))
                    r.btw.10 <- sum(pi_s*c((dat.decomp$f.10$mu_x[1]-mu_x.10),(dat.decomp$m.10$mu_x[1]-mu_x.10))*
                                         c((dat.decomp$f.10$mu_y[1]-mu_y.10),(dat.decomp$m.10$mu_y[1]-mu_y.10))/(sigma_x.10*sigma_y.10))
                    r.10 <- r.win.10+r.btw.10
                    r.win.10.pct <- 100*r.win.10/r.10
                    r.btw.10.pct <- 100*r.btw.10/r.10
                    # table
                    tab02 <- rbind(c(tab01[1,1],tab01[2,1]),c(tab01[1,2],tab01[2,2]),c(r.70,r.10),
                                   c(r.win.70.pct,r.win.10.pct),c(r.btw.70.pct,r.btw.10.pct))
                    colnames(tab02) <- c(1970,2010)
                    rownames(tab02) <- c("Women","Men","Pooled",
                                         "Pooled: Within-Gender Contribution (%)",
                                         "Pooled: Between-Gender Contribution (%)")
                    # (output)
                    return(tab02)
                    }
#  
# Function to to create table 3 from each imputed data set
fun.tab03 <- function(out.decomp) {
             tab03 <- rbind(cbind(c(out.decomp$f.7010.5$r.change.obs[1],
                                    out.decomp$f.7010.5$r.change.pi,
                                    out.decomp$f.7010.5$r.change.ev,
                                    100*out.decomp$f.7010.5$r.change.pi/out.decomp$f.7010.5$r.change.obs[1],
                                    100*out.decomp$f.7010.5$r.change.ev/out.decomp$f.7010.5$r.change.obs[1]),
                               c(out.decomp$m.7010.5$r.change.obs[1],
                                    out.decomp$m.7010.5$r.change.pi,
                                    out.decomp$m.7010.5$r.change.ev, 
                                    100*out.decomp$m.7010.5$r.change.pi/out.decomp$m.7010.5$r.change.obs[1],
                                    100*out.decomp$m.7010.5$r.change.ev/out.decomp$m.7010.5$r.change.obs[1])),
                         cbind(c(out.decomp$f.7010.5.part$r.change.obs[1],
                                    out.decomp$f.7010.5.part$r.change.pi,
                                    out.decomp$f.7010.5.part$r.change.ev,
                                    100*out.decomp$f.7010.5.part$r.change.pi/out.decomp$f.7010.5.part$r.change.obs[1],
                                    100*out.decomp$f.7010.5.part$r.change.ev/out.decomp$f.7010.5.part$r.change.obs[1]),
                               c(out.decomp$m.7010.5.part$r.change.obs[1],
                                    out.decomp$m.7010.5.part$r.change.pi,
                                    out.decomp$m.7010.5.part$r.change.ev, 
                                    100*out.decomp$m.7010.5.part$r.change.pi/out.decomp$m.7010.5.part$r.change.obs[1],
                                    100*out.decomp$m.7010.5.part$r.change.ev/out.decomp$m.7010.5.part$r.change.obs[1])))
             rownames(tab03) <- c("Raw Change, 1970-2010","Due to Composition","Due to Association",
                                   "% Due to Composition","% Due to Association",
                                   "P.Raw Change, 1970-2010","P.Due to Composition","P.Due to Association",
                                   "P.% Due to Composition","P.% Due to Association")
             colnames(tab03) <- c("Women","Men")   
             return(tab03)
             }                                            
# 
# Function to create table 4 from each imputed data set
# (note that the "association" not standardized is the association that when weighted and summed over groups gives us 
#  the total self-reliance in that gender/year; it is not equal to the "r_g" column in the spreadsheet)
fun.tab04 <- function(dat.decomp) {
             tab04 <- cbind(dat.decomp$f.70[,"pi_g"],dat.decomp$f.10[,"pi_g"],
                             fun.r.g(dat.decomp$f.70),fun.r.g(dat.decomp$f.10), 
                             fun.r.grand.stand(dat.decomp$f.70),fun.r.grand.stand(dat.decomp$f.10), 
                             dat.decomp$m.70[,"pi_g"],dat.decomp$m.10[,"pi_g"],
                             fun.r.g(dat.decomp$m.70),fun.r.g(dat.decomp$m.10), 
                             fun.r.grand.stand(dat.decomp$m.70),fun.r.grand.stand(dat.decomp$m.10))
             # (re-ordering rows so that partnered, dual-earner is on top) 
             tab04 <- tab04[c(2,1,3:5),]
             rownames(tab04) <- c("Partnered, Both earning","Partnered, One earning","Partnered, Neither earning",
                                  "Single, Earning","Single, Not earning")
             colnames(tab04) <- c("W Share 1970","W Share 2010","W Association 1970","W Association 2010","W Std. Assoc. 1970","W Std. Assoc. 2010",
                                  "M Share 1970","M Share 2010","M Association 1970","M Association 2010","M Std. Assoc. 1970","M Std. Assoc. 2010") 
             return(as.matrix(tab04))                                          
             } 
#
# Function to create table 5 from each imputed data set
fun.tab05 <- function(out.decomp) {
    # (full population)
    tab05 <- cbind(c(out.decomp$f.7010.5$r.change.obs[1],
                     out.decomp$f.7010.5$r.change.ev,out.decomp$f.7010.5$r.change.pi,
                     100*out.decomp$f.7010.5$r.change.ev/out.decomp$f.7010.5$r.change.obs[1],
                     100*out.decomp$f.7010.5$r.change.pi/out.decomp$f.7010.5$r.change.obs[1],
                     out.decomp$f.7010.5$r.change.sp,out.decomp$f.7010.5$r.change.ot,
                     out.decomp$f.7010.5$r.change.xx,out.decomp$f.7010.5$r.change.pi.f,
                     out.decomp$f.7010.5$r.change.pi.e,
                     100*out.decomp$f.7010.5$r.change.sp/out.decomp$f.7010.5$r.change.obs[1],
                     100*out.decomp$f.7010.5$r.change.ot/out.decomp$f.7010.5$r.change.obs[1],
                     100*out.decomp$f.7010.5$r.change.xx/out.decomp$f.7010.5$r.change.obs[1],
                     100*out.decomp$f.7010.5$r.change.pi.f/out.decomp$f.7010.5$r.change.obs[1],
                     100*out.decomp$f.7010.5$r.change.pi.e/out.decomp$f.7010.5$r.change.obs[1]),
                   c(999,out.decomp$f.7010.5$r.change.ev.win.share*out.decomp$f.7010.5$r.change.ev/100,999,999,999,
                     out.decomp$f.7010.5$r.change.sp.win.share*out.decomp$f.7010.5$r.change.sp/100,
                     out.decomp$f.7010.5$r.change.ot.win.share*out.decomp$f.7010.5$r.change.ot/100,
                     out.decomp$f.7010.5$r.change.xx.win.share*out.decomp$f.7010.5$r.change.xx/100,rep(999,7)),
                   c(999,out.decomp$f.7010.5$r.change.ev.bt.share*out.decomp$f.7010.5$r.change.ev/100,999,999,999,
                     out.decomp$f.7010.5$r.change.sp.bt.share*out.decomp$f.7010.5$r.change.sp/100,
                     out.decomp$f.7010.5$r.change.ot.bt.share*out.decomp$f.7010.5$r.change.ot/100,
                     out.decomp$f.7010.5$r.change.xx.bt.share*out.decomp$f.7010.5$r.change.xx/100,rep(999,7)),
                   c(out.decomp$m.7010.5$r.change.obs[1],
                     out.decomp$m.7010.5$r.change.ev,out.decomp$m.7010.5$r.change.pi,
                     100*out.decomp$m.7010.5$r.change.ev/out.decomp$m.7010.5$r.change.obs[1],
                     100*out.decomp$m.7010.5$r.change.pi/out.decomp$m.7010.5$r.change.obs[1],
                     out.decomp$m.7010.5$r.change.sp,out.decomp$m.7010.5$r.change.ot,
                     out.decomp$m.7010.5$r.change.xx,out.decomp$m.7010.5$r.change.pi.f,
                     out.decomp$m.7010.5$r.change.pi.e,
                     100*out.decomp$m.7010.5$r.change.sp/out.decomp$m.7010.5$r.change.obs[1],
                     100*out.decomp$m.7010.5$r.change.ot/out.decomp$m.7010.5$r.change.obs[1],
                     100*out.decomp$m.7010.5$r.change.xx/out.decomp$m.7010.5$r.change.obs[1],
                     100*out.decomp$m.7010.5$r.change.pi.f/out.decomp$m.7010.5$r.change.obs[1],
                     100*out.decomp$m.7010.5$r.change.pi.e/out.decomp$m.7010.5$r.change.obs[1]),
                   c(999,out.decomp$m.7010.5$r.change.ev.win.share*out.decomp$m.7010.5$r.change.ev/100,999,999,999,
                     out.decomp$m.7010.5$r.change.sp.win.share*out.decomp$m.7010.5$r.change.sp/100,
                     out.decomp$m.7010.5$r.change.ot.win.share*out.decomp$m.7010.5$r.change.ot/100,
                     out.decomp$m.7010.5$r.change.xx.win.share*out.decomp$m.7010.5$r.change.xx/100,rep(999,7)),
                   c(999,out.decomp$m.7010.5$r.change.ev.bt.share*out.decomp$m.7010.5$r.change.ev/100,999,999,999,
                     out.decomp$m.7010.5$r.change.sp.bt.share*out.decomp$m.7010.5$r.change.sp/100,
                     out.decomp$m.7010.5$r.change.ot.bt.share*out.decomp$m.7010.5$r.change.ot/100,
                     out.decomp$m.7010.5$r.change.xx.bt.share*out.decomp$m.7010.5$r.change.xx/100,rep(999,7))   )
    colnames(tab05) <- c("W 1970-2010","W Within","W Between",
                         "M 1970-2010","M Within","M Between")
    rownames(tab05) <- c("Change","Due to Association","Due to Composition",
                         "% due to Association","% due to Composition",
                         "Due to Spousal Assoc.","Due to Redistribution",
                         "Due to Earnings Distributions","Due to Family Structure",
                         "Due to Labor force Participation", 
                         "% due to Spousal Assoc.","% due to Redistribution",
                         "% due to Earnings Distributions","% due to Family Structure",
                         "% due to Labor force Participation")
    # (partnered-only version)
    tab05p <- cbind(c(out.decomp$f.7010.5.part$r.change.obs[1],
                      out.decomp$f.7010.5.part$r.change.ev,out.decomp$f.7010.5.part$r.change.pi,
                      100*out.decomp$f.7010.5.part$r.change.ev/out.decomp$f.7010.5.part$r.change.obs[1],
                      100*out.decomp$f.7010.5.part$r.change.pi/out.decomp$f.7010.5.part$r.change.obs[1],
                      out.decomp$f.7010.5.part$r.change.sp,out.decomp$f.7010.5.part$r.change.ot,
                      out.decomp$f.7010.5.part$r.change.xx,out.decomp$f.7010.5.part$r.change.pi, 
                      100*out.decomp$f.7010.5.part$r.change.sp/out.decomp$f.7010.5.part$r.change.obs[1],
                      100*out.decomp$f.7010.5.part$r.change.ot/out.decomp$f.7010.5.part$r.change.obs[1],
                      100*out.decomp$f.7010.5.part$r.change.xx/out.decomp$f.7010.5.part$r.change.obs[1],
                      100*out.decomp$f.7010.5.part$r.change.pi/out.decomp$f.7010.5.part$r.change.obs[1]), 
                    c(999,out.decomp$f.7010.5.part$r.change.ev.win.share*out.decomp$f.7010.5.part$r.change.ev/100,999,999,999,
                      out.decomp$f.7010.5.part$r.change.sp.win.share*out.decomp$f.7010.5.part$r.change.sp/100,
                      out.decomp$f.7010.5.part$r.change.ot.win.share*out.decomp$f.7010.5.part$r.change.ot/100,
                      out.decomp$f.7010.5.part$r.change.xx.win.share*out.decomp$f.7010.5.part$r.change.xx/100,rep(999,5)),
                    c(999,out.decomp$f.7010.5.part$r.change.ev.bt.share*out.decomp$f.7010.5.part$r.change.ev/100,999,999,999,
                      out.decomp$f.7010.5.part$r.change.sp.bt.share*out.decomp$f.7010.5.part$r.change.sp/100,
                      out.decomp$f.7010.5.part$r.change.ot.bt.share*out.decomp$f.7010.5.part$r.change.ot/100,
                      out.decomp$f.7010.5.part$r.change.xx.bt.share*out.decomp$f.7010.5.part$r.change.xx/100,rep(999,5)),
                    c(out.decomp$m.7010.5.part$r.change.obs[1],
                      out.decomp$m.7010.5.part$r.change.ev,out.decomp$m.7010.5.part$r.change.pi,
                      100*out.decomp$m.7010.5.part$r.change.ev/out.decomp$m.7010.5.part$r.change.obs[1],
                      100*out.decomp$m.7010.5.part$r.change.pi/out.decomp$m.7010.5.part$r.change.obs[1],
                      out.decomp$m.7010.5.part$r.change.sp,out.decomp$m.7010.5.part$r.change.ot,
                      out.decomp$m.7010.5.part$r.change.xx,out.decomp$m.7010.5.part$r.change.pi,
                      100*out.decomp$m.7010.5.part$r.change.sp/out.decomp$m.7010.5.part$r.change.obs[1],
                      100*out.decomp$m.7010.5.part$r.change.ot/out.decomp$m.7010.5.part$r.change.obs[1],
                      100*out.decomp$m.7010.5.part$r.change.xx/out.decomp$m.7010.5.part$r.change.obs[1],
                      100*out.decomp$m.7010.5.part$r.change.pi/out.decomp$m.7010.5.part$r.change.obs[1]),
                    c(999,out.decomp$m.7010.5.part$r.change.ev.win.share*out.decomp$m.7010.5.part$r.change.ev/100,999,999,999,
                      out.decomp$m.7010.5.part$r.change.sp.win.share*out.decomp$m.7010.5.part$r.change.sp/100,
                      out.decomp$m.7010.5.part$r.change.ot.win.share*out.decomp$m.7010.5.part$r.change.ot/100,
                      out.decomp$m.7010.5.part$r.change.xx.win.share*out.decomp$m.7010.5.part$r.change.xx/100,rep(999,5)),
                    c(999,out.decomp$m.7010.5.part$r.change.ev.bt.share*out.decomp$m.7010.5.part$r.change.ev/100,999,999,999,
                      out.decomp$m.7010.5.part$r.change.sp.bt.share*out.decomp$m.7010.5.part$r.change.sp/100,
                      out.decomp$m.7010.5.part$r.change.ot.bt.share*out.decomp$m.7010.5.part$r.change.ot/100,
                      out.decomp$m.7010.5.part$r.change.xx.bt.share*out.decomp$m.7010.5.part$r.change.xx/100,rep(999,5))   )
    colnames(tab05p) <- c("W 1970-2010","W Within","W Between",
                          "M 1970-2010","M Within","M Between")
    rownames(tab05p) <- c("Change","Due to Association","Due to Composition",
                          "% due to Association","% due to Composition",
                          "Due to Spousal Assoc.","Due to Redistribution",
                          "Due to Earnings Distributions",
                          "Due to Labor-force Participation", 
                          "% due to Spousal Assoc.","% due to Redistribution",
                          "% due to Earnings Distributions",
                          "% due to Labor-force Participation")
    return(list(tab05=tab05,tab05p=tab05p))       
}
#
# Function to create appendix table A1 from each imputed data set
fun.tab.a01 <- function(out.decomp) {
               tab.a01 <- cbind(c(out.decomp$f.7010.5$r.change.obs[1],
                                out.decomp$f.7010.5$r.change.ev,out.decomp$f.7010.5$r.change.pi,
                                100*out.decomp$f.7010.5$r.change.ev/out.decomp$f.7010.5$r.change.obs[1],
                                100*out.decomp$f.7010.5$r.change.pi/out.decomp$f.7010.5$r.change.obs[1],
                                out.decomp$f.7010.5$r.change.sp,out.decomp$f.7010.5$r.change.ot,
                                out.decomp$f.7010.5$r.change.xx,out.decomp$f.7010.5$r.change.pi.f,
                                out.decomp$f.7010.5$r.change.pi.e,
                                100*out.decomp$f.7010.5$r.change.sp/out.decomp$f.7010.5$r.change.obs[1],
                                100*out.decomp$f.7010.5$r.change.ot/out.decomp$f.7010.5$r.change.obs[1],
                                100*out.decomp$f.7010.5$r.change.xx/out.decomp$f.7010.5$r.change.obs[1],
                                100*out.decomp$f.7010.5$r.change.pi.f/out.decomp$f.7010.5$r.change.obs[1],
                                100*out.decomp$f.7010.5$r.change.pi.e/out.decomp$f.7010.5$r.change.obs[1]),
                              c(out.decomp$f.80.5$r.change.obs[1],
                                out.decomp$f.80.5$r.change.ev,out.decomp$f.80.5$r.change.pi,
                                100*out.decomp$f.80.5$r.change.ev/out.decomp$f.80.5$r.change.obs[1],
                                100*out.decomp$f.80.5$r.change.pi/out.decomp$f.80.5$r.change.obs[1],
                                out.decomp$f.80.5$r.change.sp,out.decomp$f.80.5$r.change.ot,
                                out.decomp$f.80.5$r.change.xx,out.decomp$f.80.5$r.change.pi.f,
                                out.decomp$f.80.5$r.change.pi.e,
                                100*out.decomp$f.80.5$r.change.sp/out.decomp$f.80.5$r.change.obs[1],
                                100*out.decomp$f.80.5$r.change.ot/out.decomp$f.80.5$r.change.obs[1],
                                100*out.decomp$f.80.5$r.change.xx/out.decomp$f.80.5$r.change.obs[1],
                                100*out.decomp$f.80.5$r.change.pi.f/out.decomp$f.80.5$r.change.obs[1],
                                100*out.decomp$f.80.5$r.change.pi.e/out.decomp$f.80.5$r.change.obs[1]),
                              c(out.decomp$f.90.5$r.change.obs[1],
                                out.decomp$f.90.5$r.change.ev,out.decomp$f.90.5$r.change.pi,
                                100*out.decomp$f.90.5$r.change.ev/out.decomp$f.90.5$r.change.obs[1],
                                100*out.decomp$f.90.5$r.change.pi/out.decomp$f.90.5$r.change.obs[1],
                                out.decomp$f.90.5$r.change.sp,out.decomp$f.90.5$r.change.ot,
                                out.decomp$f.90.5$r.change.xx,out.decomp$f.90.5$r.change.pi.f,
                                out.decomp$f.90.5$r.change.pi.e,
                                100*out.decomp$f.90.5$r.change.sp/out.decomp$f.90.5$r.change.obs[1],
                                100*out.decomp$f.90.5$r.change.ot/out.decomp$f.90.5$r.change.obs[1],
                                100*out.decomp$f.90.5$r.change.xx/out.decomp$f.90.5$r.change.obs[1],
                                100*out.decomp$f.90.5$r.change.pi.f/out.decomp$f.90.5$r.change.obs[1],
                                100*out.decomp$f.90.5$r.change.pi.e/out.decomp$f.90.5$r.change.obs[1]),
                              c(out.decomp$f.00.5$r.change.obs[1],
                                out.decomp$f.00.5$r.change.ev,out.decomp$f.00.5$r.change.pi,
                                100*out.decomp$f.00.5$r.change.ev/out.decomp$f.00.5$r.change.obs[1],
                                100*out.decomp$f.00.5$r.change.pi/out.decomp$f.00.5$r.change.obs[1],
                                out.decomp$f.00.5$r.change.sp,out.decomp$f.00.5$r.change.ot,
                                out.decomp$f.00.5$r.change.xx,out.decomp$f.00.5$r.change.pi.f,
                                out.decomp$f.00.5$r.change.pi.e,
                                100*out.decomp$f.00.5$r.change.sp/out.decomp$f.00.5$r.change.obs[1],
                                100*out.decomp$f.00.5$r.change.ot/out.decomp$f.00.5$r.change.obs[1],
                                100*out.decomp$f.00.5$r.change.xx/out.decomp$f.00.5$r.change.obs[1],
                                100*out.decomp$f.00.5$r.change.pi.f/out.decomp$f.00.5$r.change.obs[1],
                                100*out.decomp$f.00.5$r.change.pi.e/out.decomp$f.00.5$r.change.obs[1]),
                              c(out.decomp$f.10.5$r.change.obs[1],
                                out.decomp$f.10.5$r.change.ev,out.decomp$f.10.5$r.change.pi,
                                100*out.decomp$f.10.5$r.change.ev/out.decomp$f.10.5$r.change.obs[1],
                                100*out.decomp$f.10.5$r.change.pi/out.decomp$f.10.5$r.change.obs[1],
                                out.decomp$f.10.5$r.change.sp,out.decomp$f.10.5$r.change.ot,
                                out.decomp$f.10.5$r.change.xx,out.decomp$f.10.5$r.change.pi.f,
                                out.decomp$f.10.5$r.change.pi.e,
                                100*out.decomp$f.10.5$r.change.sp/out.decomp$f.10.5$r.change.obs[1],
                                100*out.decomp$f.10.5$r.change.ot/out.decomp$f.10.5$r.change.obs[1],
                                100*out.decomp$f.10.5$r.change.xx/out.decomp$f.10.5$r.change.obs[1],
                                100*out.decomp$f.10.5$r.change.pi.f/out.decomp$f.10.5$r.change.obs[1],
                                100*out.decomp$f.10.5$r.change.pi.e/out.decomp$f.10.5$r.change.obs[1]),
                              c(out.decomp$m.7010.5$r.change.obs[1],
                                out.decomp$m.7010.5$r.change.ev,out.decomp$m.7010.5$r.change.pi,
                                100*out.decomp$m.7010.5$r.change.ev/out.decomp$m.7010.5$r.change.obs[1],
                                100*out.decomp$m.7010.5$r.change.pi/out.decomp$m.7010.5$r.change.obs[1],
                                out.decomp$m.7010.5$r.change.sp,out.decomp$m.7010.5$r.change.ot,
                                out.decomp$m.7010.5$r.change.xx,out.decomp$m.7010.5$r.change.pi.f,
                                out.decomp$m.7010.5$r.change.pi.e,
                                100*out.decomp$m.7010.5$r.change.sp/out.decomp$m.7010.5$r.change.obs[1],
                                100*out.decomp$m.7010.5$r.change.ot/out.decomp$m.7010.5$r.change.obs[1],
                                100*out.decomp$m.7010.5$r.change.xx/out.decomp$m.7010.5$r.change.obs[1],
                                100*out.decomp$m.7010.5$r.change.pi.f/out.decomp$m.7010.5$r.change.obs[1],
                                100*out.decomp$m.7010.5$r.change.pi.e/out.decomp$m.7010.5$r.change.obs[1]),
                              c(out.decomp$m.80.5$r.change.obs[1],
                                out.decomp$m.80.5$r.change.ev,out.decomp$m.80.5$r.change.pi,
                                100*out.decomp$m.80.5$r.change.ev/out.decomp$m.80.5$r.change.obs[1],
                                100*out.decomp$m.80.5$r.change.pi/out.decomp$m.80.5$r.change.obs[1],
                                out.decomp$m.80.5$r.change.sp,out.decomp$m.80.5$r.change.ot,
                                out.decomp$m.80.5$r.change.xx,out.decomp$m.80.5$r.change.pi.f,
                                out.decomp$m.80.5$r.change.pi.e,
                                100*out.decomp$m.80.5$r.change.sp/out.decomp$m.80.5$r.change.obs[1],
                                100*out.decomp$m.80.5$r.change.ot/out.decomp$m.80.5$r.change.obs[1],
                                100*out.decomp$m.80.5$r.change.xx/out.decomp$m.80.5$r.change.obs[1],
                                100*out.decomp$m.80.5$r.change.pi.f/out.decomp$m.80.5$r.change.obs[1],
                                100*out.decomp$m.80.5$r.change.pi.e/out.decomp$m.80.5$r.change.obs[1]),
                              c(out.decomp$m.90.5$r.change.obs[1],
                                out.decomp$m.90.5$r.change.ev,out.decomp$m.90.5$r.change.pi,
                                100*out.decomp$m.90.5$r.change.ev/out.decomp$m.90.5$r.change.obs[1],
                                100*out.decomp$m.90.5$r.change.pi/out.decomp$m.90.5$r.change.obs[1],
                                out.decomp$m.90.5$r.change.sp,out.decomp$m.90.5$r.change.ot,
                                out.decomp$m.90.5$r.change.xx,out.decomp$m.90.5$r.change.pi.f,
                                out.decomp$m.90.5$r.change.pi.e,
                                100*out.decomp$m.90.5$r.change.sp/out.decomp$m.90.5$r.change.obs[1],
                                100*out.decomp$m.90.5$r.change.ot/out.decomp$m.90.5$r.change.obs[1],
                                100*out.decomp$m.90.5$r.change.xx/out.decomp$m.90.5$r.change.obs[1],
                                100*out.decomp$m.90.5$r.change.pi.f/out.decomp$m.90.5$r.change.obs[1],
                                100*out.decomp$m.90.5$r.change.pi.e/out.decomp$m.90.5$r.change.obs[1]),
                              c(out.decomp$m.00.5$r.change.obs[1],
                                out.decomp$m.00.5$r.change.ev,out.decomp$m.00.5$r.change.pi,
                                100*out.decomp$m.00.5$r.change.ev/out.decomp$m.00.5$r.change.obs[1],
                                100*out.decomp$m.00.5$r.change.pi/out.decomp$m.00.5$r.change.obs[1],
                                out.decomp$m.00.5$r.change.sp,out.decomp$m.00.5$r.change.ot,
                                out.decomp$m.00.5$r.change.xx,out.decomp$m.00.5$r.change.pi.f,
                                out.decomp$m.00.5$r.change.pi.e,
                                100*out.decomp$m.00.5$r.change.sp/out.decomp$m.00.5$r.change.obs[1],
                                100*out.decomp$m.00.5$r.change.ot/out.decomp$m.00.5$r.change.obs[1],
                                100*out.decomp$m.00.5$r.change.xx/out.decomp$m.00.5$r.change.obs[1],
                                100*out.decomp$m.00.5$r.change.pi.f/out.decomp$m.00.5$r.change.obs[1],
                                100*out.decomp$m.00.5$r.change.pi.e/out.decomp$m.00.5$r.change.obs[1]),
                              c(out.decomp$m.10.5$r.change.obs[1],
                                out.decomp$m.10.5$r.change.ev,out.decomp$m.10.5$r.change.pi,
                                100*out.decomp$m.10.5$r.change.ev/out.decomp$m.10.5$r.change.obs[1],
                                100*out.decomp$m.10.5$r.change.pi/out.decomp$m.10.5$r.change.obs[1],
                                out.decomp$m.10.5$r.change.sp,out.decomp$m.10.5$r.change.ot,
                                out.decomp$m.10.5$r.change.xx,out.decomp$m.10.5$r.change.pi.f,
                                out.decomp$m.10.5$r.change.pi.e,
                                100*out.decomp$m.10.5$r.change.sp/out.decomp$m.10.5$r.change.obs[1],
                                100*out.decomp$m.10.5$r.change.ot/out.decomp$m.10.5$r.change.obs[1],
                                100*out.decomp$m.10.5$r.change.xx/out.decomp$m.10.5$r.change.obs[1],
                                100*out.decomp$m.10.5$r.change.pi.f/out.decomp$m.10.5$r.change.obs[1],
                                100*out.decomp$m.10.5$r.change.pi.e/out.decomp$m.10.5$r.change.obs[1]))
               colnames(tab.a01) <- c("W 1970-2010","W 1970-80","W 1980-90","W 1990-2000","W 2000-10",
                                      "M 1970-2010","M 1970-80","M 1980-90","M 1990-2000","M 2000-10")
               rownames(tab.a01) <- c("Raw Change","Due to Association","Due to Composition",
                                      "% due to Association","% due to Composition",
                                      "Due to Spousal Assoc.","Due to Redistribution",
                                      "Due to Earnings Distributions","Due to Family Structure",
                                      "Due to Labor-force Participation", 
                                      "% due to Spousal Assoc.","% due to Redistribution",
                                      "% due to Earnings Distributions","% due to Family Structure",
                                      "% due to Labor-force Participation")
               return(tab.a01)
               }
#
# Function to apply table functions to 10 imputations
fun.tabs.list <- function(out.decomp.1,out.decomp.2,out.decomp.3,out.decomp.4,out.decomp.5,
                          out.decomp.6,out.decomp.7,out.decomp.8,out.decomp.9,out.decomp.10,
                          dat.decomp.1,dat.decomp.2,dat.decomp.3,dat.decomp.4,dat.decomp.5,
                          dat.decomp.6,dat.decomp.7,dat.decomp.8,dat.decomp.9,dat.decomp.10) {
                 # (apply table 1 function) 
                 tab01.list <- list(fun.tab01(out.decomp.1,dat.decomp.1),
                   fun.tab01(out.decomp.2,dat.decomp.2),
                   fun.tab01(out.decomp.3,dat.decomp.3),
                   fun.tab01(out.decomp.4,dat.decomp.4),
                   fun.tab01(out.decomp.5,dat.decomp.5),
                   fun.tab01(out.decomp.6,dat.decomp.6),
                   fun.tab01(out.decomp.7,dat.decomp.7),
                   fun.tab01(out.decomp.8,dat.decomp.8),
                   fun.tab01(out.decomp.9,dat.decomp.9),
                   fun.tab01(out.decomp.10,dat.decomp.10))
                 tab01 <- Reduce("+",tab01.list)/length(tab01.list) 
                 # (apply table 2 function) 
                 tab02.list <- list(fun.tab02(dat.decomp.1),
                   fun.tab02(dat.decomp.2),
                   fun.tab02(dat.decomp.3),
                   fun.tab02(dat.decomp.4),
                   fun.tab02(dat.decomp.5),
                   fun.tab02(dat.decomp.6),
                   fun.tab02(dat.decomp.7),
                   fun.tab02(dat.decomp.8),
                   fun.tab02(dat.decomp.9),
                   fun.tab02(dat.decomp.10))
                 tab02 <- Reduce("+",tab02.list)/length(tab02.list)
                 # (apply table 3 function)
                 tab03.list <- list(fun.tab03(out.decomp.1),
                   fun.tab03(out.decomp.2),
                   fun.tab03(out.decomp.3),
                   fun.tab03(out.decomp.4),
                   fun.tab03(out.decomp.5),
                   fun.tab03(out.decomp.6),
                   fun.tab03(out.decomp.7),
                   fun.tab03(out.decomp.8),
                   fun.tab03(out.decomp.9),
                   fun.tab03(out.decomp.10))
                 tab03 <- Reduce("+",tab03.list)/length(tab03.list)
                 # (apply table 4 function) 
                 tab04.list <- list(fun.tab04(dat.decomp.1),
                   fun.tab04(dat.decomp.2),
                   fun.tab04(dat.decomp.3),
                   fun.tab04(dat.decomp.4),
                   fun.tab04(dat.decomp.5),
                   fun.tab04(dat.decomp.6),
                   fun.tab04(dat.decomp.7),
                   fun.tab04(dat.decomp.8),
                   fun.tab04(dat.decomp.9),
                   fun.tab04(dat.decomp.10))
                 tab04 <- Reduce("+",tab04.list)/length(tab04.list)
                 # (apply table 5 function) 
                 # (full population)
                 tab05.list <- list(fun.tab05(out.decomp.1)$tab05,
                   fun.tab05(out.decomp.2)$tab05,
                   fun.tab05(out.decomp.3)$tab05,
                   fun.tab05(out.decomp.4)$tab05,
                   fun.tab05(out.decomp.5)$tab05,
                   fun.tab05(out.decomp.6)$tab05,
                   fun.tab05(out.decomp.7)$tab05,
                   fun.tab05(out.decomp.8)$tab05,
                   fun.tab05(out.decomp.9)$tab05,
                   fun.tab05(out.decomp.10)$tab05)
                 tab05 <- Reduce("+",tab05.list)/length(tab05.list)
                 #  (partnered population)
                 tab05p.list <- list(fun.tab05(out.decomp.1)$tab05p,
                    fun.tab05(out.decomp.2)$tab05p,
                    fun.tab05(out.decomp.3)$tab05p,
                    fun.tab05(out.decomp.4)$tab05p,
                    fun.tab05(out.decomp.5)$tab05p,
                    fun.tab05(out.decomp.6)$tab05p,
                    fun.tab05(out.decomp.7)$tab05p,
                    fun.tab05(out.decomp.8)$tab05p,
                    fun.tab05(out.decomp.9)$tab05p,
                    fun.tab05(out.decomp.10)$tab05p)
                 tab05p <- Reduce("+",tab05p.list)/length(tab05p.list)
                 # (apply table A1 function) 
                 tab.a01.list <- list(fun.tab.a01(out.decomp.1),
                     fun.tab.a01(out.decomp.2),
                     fun.tab.a01(out.decomp.3),
                     fun.tab.a01(out.decomp.4),
                     fun.tab.a01(out.decomp.5),
                     fun.tab.a01(out.decomp.6),
                     fun.tab.a01(out.decomp.7),
                     fun.tab.a01(out.decomp.8),
                     fun.tab.a01(out.decomp.9),
                     fun.tab.a01(out.decomp.10))
                 tab.a01 <- Reduce("+",tab.a01.list)/length(tab.a01.list)
                 # (output)
                 out <- list(tab01=tab01,tab02=tab02,tab03=tab03,tab04=tab04,
                             tab05=tab05,tab05p=tab05p,tab.a01=tab.a01)
                 return(out)            
                 }

### 
# (End functions)
###




###
# Read in data, create list of tables,
# and save this list
# (requires looping over bootstrap samples)
###

## Load list of decomp components if it already exists
decomp_component_file <- file.path(OUT_DIR, INTERMEDIATE_OUT_FILE)
if (file.exists(decomp_component_file)) {
    load(decomp_component_file)
} else {
    ## Create list of decomp components
    # Data is saved by decade, but we will combine results across
    # decades by imputation index (1-10) and bootstrap index (1-NBOOT)
    # to yield NBOOT sets of decomp components
    
    decomp_component_list <- vector(mode = "list", length = 10)
    
    for(i in 1:10) {
        decomp_component_list[[i]] <- vector(mode = "list", length = NBOOT)
    }
    
    for(yr in c(1970, 1980, 1990, 2000, 2010)) {
        data_file <- sprintf(DATA_FILE_PATTERN, yr)
        load( # loads list of imputed data.frames named "imps"
            file.path(
                DATA_DIR, 
                data_file
            )
        )
        
        cat(paste0(yr, "\n####\n\n"))
        
        for(imp_i in 1:10) {
            
            cat(paste0("Imputation ", imp_i, ", bootstrap: "))
            
            for(sample_i in 1:NBOOT) {
                cat(sample_i, "")
                bsamp <- sample(
                    1:nrow(imps[[imp_i]]), 
                    nrow(imps[[imp_i]]), 
                    replace = TRUE
                )
                tbl <- make_decomp_component_table(
                    imps[[imp_i]][bsamp, ], 
                    fam_adj = fam_adj, 
                    exclude_alloc = exclude_alloc, 
                    exclude_top_2_pct = exclude_top_2_pct, 
                    exclude_top_decile_female_earners = exclude_top_decile_female_earners, 
                    exclude_top_decile_male_earners = exclude_top_decile_male_earners
                )
                decomp_component_list[[imp_i]][[sample_i]] <- 
                    # Row-bind results across decades by imputation index (imp_i)
                    # and bootstrap index (sample_i)
                    rbindlist(
                        list(
                            decomp_component_list[[imp_i]][[sample_i]],
                            tbl
                        )
                    )
            }
            
            cat("\n\n")
            
        }
        
    }
    
    # Save decomp_component_list in case we need it and don't want to regenerate it
    save(
        decomp_component_list,
        file = decomp_component_file
    )
}




## Initiate lists of tables 
## and run loop to populate the list
#
# (initiate list of tables)
tabs.list <- vector(mode = "list", length = NBOOT)
#
# (run loop)

cat("Making tables...\n################\n")

for(i in 1:NBOOT) {
	
    cat(i, "")
    
    ## Data 
    # (tables are in "decomp_component_list" created above) 
    # 
    
    dat.decomp.imp.list <- lapply(
        decomp_component_list,
        function(x) {
            x[[i]]
        }
    )
    
    # (prep the data using function that replaces some NAs with zeros and separates men/women)
    dat.decomp.list <- lapply(
        dat.decomp.imp.list, 
        fun.data.prep
    )

    # (further prep the data by applying function that 
    #  decomposes change in self-relience to get decomposition output for each imputation)
    out.decomp.list <- lapply(
        dat.decomp.list, 
        fun.sr.decomp.5piece.apply
    )

    
	## Tables
	#
	# (apply function to get all the tables for a given bootstrap sample)
    tabs.boot <- do.call(
        fun.tabs.list, 
        c(
            out.decomp.list,
            dat.decomp.list
        ) 
    )

	# (append all tables to list of tables from all bootstrap samples)
	tabs.list[[i]] <- tabs.boot
	
}

         
## Save list of tables
save(tabs.list,file=file.path(OUT_DIR, OUT_FILE))




                            
