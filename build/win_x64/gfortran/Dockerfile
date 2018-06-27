FROM microsoft/windowsservercore
WORKDIR /swb

# swb and dlls
COPY src/*.exe ./
COPY *.dll ./

ENTRYPOINT ["powershell.exe"]
