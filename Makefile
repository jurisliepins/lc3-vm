all : clean restore build publish

clean:
	dotnet clean

restore:
	dotnet restore

build: 
	dotnet build

publish:
	dotnet publish --runtime win-x64 --configuration Release --self-contained true -p:PublishReadyToRun=true -p:PublishTrimmed=true -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true 
	dotnet publish --runtime linux-x64 --configuration Release --self-contained true -p:PublishReadyToRun=true -p:PublishTrimmed=true -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true

run:
	dotnet run