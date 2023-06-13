UNIT welcomeDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

TYPE

  { TFirstStartForm }

  TFirstStartForm = class(TForm)
    updatePaletteButton: TShape;
    startTutorialLabel: TLabel;
    WelcomeLabel: TLabel;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE startTutorialLabelMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private

  public
    FUNCTION wantToStartTutorial:boolean;
  end;

FUNCTION FirstStartForm: TFirstStartForm;

IMPLEMENTATION
USES visuals;
VAR alreadyAsked:boolean=false;
    myFirstStartForm: TFirstStartForm=nil;
FUNCTION FirstStartForm: TFirstStartForm;
  begin
    if myFirstStartForm=nil then myFirstStartForm:=TFirstStartForm.create(nil);
    result:=myFirstStartForm;
  end;

{$R *.lfm}

{ TFirstStartForm }

PROCEDURE TFirstStartForm.startTutorialLabelMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrYes;
  end;

PROCEDURE TFirstStartForm.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

FUNCTION TFirstStartForm.wantToStartTutorial: boolean;
  begin
    result:=not(alreadyAsked) and (ShowModal=mrYes);
    alreadyAsked:=true;
  end;

FINALIZATION
  if myFirstStartForm<>nil then FreeAndNil(myFirstStartForm);

end.

