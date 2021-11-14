project = ./src/LC3VirtualMachine/LC3VirtualMachine.fsproj

target_win_rt   = win-x64
target_linux_rt = linux-x64

publish_params = --configuration Release 	\
				 --self-contained true   	\
				 -p:PublishReadyToRun=true 	\
				 -p:PublishTrimmed=true 	\
				 -p:PublishSingleFile=true 	\
				 -p:IncludeNativeLibrariesForSelfExtract=true

all: clean restore build publish

clean:
	dotnet clean

restore:
	dotnet restore

build: 
	dotnet build

publish:
	dotnet publish $(project) --runtime $(target_win_rt)   $(publish_params)    
	dotnet publish $(project) --runtime $(target_linux_rt) $(publish_params)

run:
	dotnet run