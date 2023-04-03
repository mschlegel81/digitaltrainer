[Setup]
AppName=Digitaltrainer
AppVersion=2.2.1
WizardStyle=modern
DefaultDirName={autopf}\Digitaltrainer
DefaultGroupName=Digitaltrainer
UninstallDisplayIcon={app}\Digitaltrainer.exe
Compression=lzma2/ultra64
SolidCompression=yes
OutputDir=.
OutputBaseFilename=install_digitaltrainer
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
[Files]
Source: "..\dt2.exe"; DestDir: "{app}"; DestName: "Digitaltrainer.exe"
Source: "dt2.workspace"; DestDir: "{localappdata}\Digitaltrainer"; DestName: "workspace" 
[Icons]
Name: "{group}\Digitaltrainer"; Filename: "{app}\Digitaltrainer.exe"
[CustomMessages]
LaunchProgram=Nach Installation starten.
[Run]
Filename: {app}\Digitaltrainer.exe; Description: {cm:LaunchProgram}; Flags: nowait postinstall skipifsilent