@echo off
TITLE Soil Water Balance
set prompt=$m_$p$g
cd /D %SWB_HOME%\example
mode con: cols=110 lines=35
cmd /T:0A /K swb.exe
