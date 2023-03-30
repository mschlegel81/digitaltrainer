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
// The following files only exist to ensure that they are removed properly on uninstall
Source: "dt2.settings"; DestDir: "{localappdata}\Digitaltrainer"; DestName: "settings" 
Source: "dt2.backups"; DestDir: "{localappdata}\Digitaltrainer"; DestName: "backups" 
[Icons]
Name: "{group}\Digitaltrainer"; Filename: "{app}\Digitaltrainer.exe"
