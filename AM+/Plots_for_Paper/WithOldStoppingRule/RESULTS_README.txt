Resources (memory and real time) required to run multiple locus association mapping code


Programs being reviewed
~~~~~~~~~~~~~~~~~~~~~~~
MLMM
piMASS
lmmlasso
lars
Hlasso (BARGEN)
aml  (adaptive mixed lasso - R package Wang et al. 2011) (only for inbreds)
ASGSCA bioconductor Generalized stuctured equation nodels
grlr  (generalized ridge regression - binary traits - only 3 refs of paper) Software availale on request from authors. 
bigRR  impressive package. GPU enabled. Big data enabled. Using HEM algorithm because better than straight ridge regression . 
Cite A Novel Generalized Ridge Regression Method for Quantitative Genetics. They use permuation to get significance thresholds (100 permuations). Comps speed
up by hugeRR_update and cache.
am+

Size of data sets
~~~~~~~~~~~~~~~~~~
Nindv   #SNP     #Geno(M)          Description           Data Set Name
---     -----   ---------        ----------------     -------------------
150     5000      0.75             wheat                    W
1500    50,000    7.5              livestock                L
350     400,000   140              sorghum                  S
2000    500K      1000             Human (small)            HS
4000    1.5M      6000             Arab, rice, maize        A
10,000  1.5M     15000             Human (large)            HL
--------------------------------------------------




                  Computation Time
               ~~~~~~~~~~~~~~~~~~~~~~~
                lmmlasso
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
W   lmmlasso   multithreaded 16 cores  33secs   682M (VIRT from top)
time python timing.py
Problem is that this code cannot easily handle fixed effects such as 
population structure. Using lmmlasso[R] package to see if this helps. 
28/10, No need to redo. lmmlasso cannot handle K covariance structure for random effects. 
S   lmmlasso  multithreaded 16 cores  214m14secs         17.2Gb  
L   lmmlasso  multithreaded 16 cores  109m49sec          8Gb 
HS   "            "             "     2941m              110Gb  
A   lmmlasso     "              "                        * beyond memory capacity of machine (try cherax) 

              piMASS
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
W    pimass single core 35M     186m25secs, 41min, 23min, 32min, 25min, 37min, 37min, 37 min
S    pimass single core 807M    66m25, 186min, 93min, 143min,255, 269, 111m, 177, 175        
L       "               323M    42m39, 17m, 26m, 18, 17, 21, 21, 18, 18, 17
HS      "               4.1G           562m,  692m,  818m, 360m, 500m, 585, 455, 315, 683, 341
A       "            24479608 (24.5G) 1194m, 1131m,  1147m, 1194m,  1243m, 1015m,  1240m,  1247m, 1844m, 1477    HL      "           
HL                   59623860 (59.6G) 1792, 2517m, 1932, 1864, 1916, 2027, 1684, 1958, 1842, 1907

             lars
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## must set use.Gram=FALE otherwise cannot allocate array error
###  max.steps = 50
## lars and lars-lasso perform the same
W  multi-threaded 16 cores  478M      10 secs        
S          "                4.5G      42minutes 46 secs
L          "                2815412 (2.8G)   12min 3 secs 
HS         "                31537080 (31G)    354 minutes
A          "       allocation error  94875544 (95G)                      

          bigRR 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using permutation to determine significance thresholds. stability 
## selection cannot be used here because all variables have non-zero effect
## sizes unlike lasso. 
## two core function bigRR and hugeRR.  
## decided to focus on hugeRR as this is what would be used for big data
## two times given, 1 rep, and 100 reps. Not linear due to cost of reading in data 
##                  hugeRR when marker data is too big to fit into memory. 
W    multiple cores            179208    (172M)   0.2minutes  +  8.45minutes 
S          "                  1531104    (1.5G)   5.566minutes  +  396minutes
L                             2268904    (2.2G)   25.68min  +  2500.68 minutes
HS    old values              7711084    (77.1G)   8.46hours,  153.667 hours
A          "                  59446212    (59.4G)  51.05hrs (1 run), 849hours (100 reps)
HL         "                             (80G)   did not complete in 100 hours of run time


      MLMM
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## number of chunks set to 20
W    16 cores   185212  (185M)     12.7 secs 
S    16 cores  6096296  (6.1G)    11.25  minutes
L              3190316  (3.1G)     3.7 minutes
A            102615952 (102.6G) 1737.017 minutes
A running 20:14 3/11  94883768 (94.8G)   Out of memory


HS            40453772 (40.45G)  141.55 minutes
HL           out of memory 

      AM+  (CPU version)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## memory may be reduced by using noalias !!!!!!!!!!! check this ......
W       186484  (186M)   4.3 secs
S      2535936   (2.5G)  0.78minutes
L      1994768 (1.9G)  1.33  minutes
A     96391740 (96.4G) 156 minutes

HS    16896872 (16.8G) 17.58 minutes
HL     3830408  (38.3G)   








