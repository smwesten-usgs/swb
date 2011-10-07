:: this will need to be modified entirely, based upon the filenames of the
:: climate data and station files downloaded from NOAA NCDC

:: concatenate all of the downloaded DATA files; ensure that we don't include
:: duplicate records; save the result as COOP_Data.txt
cat 2751021742736dat.txt 9081281828285dat.txt 5932871828434dat.txt ^
 6140001828446dat.txt 1314781828450dat.txt 1780511828459dat.txt ^
 3399491832827dat.txt 4818141999950dat.txt 9168931999969dat.txt | uniq > COOP_Data.txt

:: concatenate all of the downloaded STATION files; ensure that we don't include
:: duplicate records; save the result as stations.txt
cat 2751021742736stn.txt 9081281828285stn.txt 5932871828434stn.txt ^
 6140001828446stn.txt 1314781828450stn.txt 1780511828459stn.txt ^
 3399491832827stn.txt 4818141999950stn.txt 9168931999969stn.txt | uniq > stations.txt
