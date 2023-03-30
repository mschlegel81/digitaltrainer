UNIT aboutUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

TYPE

  { TAboutDialog }

  TAboutDialog = class(TForm)
    DownloadLinkLabel: TLabel;
    LicenseLabel: TLabel;
    DownloadLinkShape: TShape;
    LicenseShape: TShape;
    VersionLabel: TLabel;
    PROCEDURE DownloadLinkShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE LicenseShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  private

  public

  end;

FUNCTION AboutDialog: TAboutDialog;

IMPLEMENTATION
USES visuals,lclintf;
VAR myAboutDialog:TAboutDialog=nil;
FUNCTION AboutDialog: TAboutDialog;
  begin
    if myAboutDialog=nil then myAboutDialog:=TAboutDialog.create(nil);
    result:=myAboutDialog;
  end;

{$R *.lfm}

{ TAboutDialog }

PROCEDURE TAboutDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

PROCEDURE TAboutDialog.LicenseShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    OpenURL('https://creativecommons.org/licenses/by-sa/3.0/de/');
  end;

PROCEDURE TAboutDialog.DownloadLinkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    OpenURL('https://github.com/mschlegel81/digitaltrainer/releases/latest');
  end;

{ TAboutDialog }

FINALIZATION
  if myAboutDialog<>nil then FreeAndNil(myAboutDialog);
end.

