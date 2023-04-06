UNIT taskFinishedUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

TYPE
  T_finishResponse=(fr_backToFreeEdit,fr_restartTask,fr_nextTask,fr_cancel);

  { TTaskFinishedDialog }

  TTaskFinishedDialog = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    NextChallengeLabel: TLabel;
    CancelLabel: TLabel;
    SetEditModeLabel: TLabel;
    RestartChallengeLabel: TLabel;
    SetEditModeShape: TShape;
    RestartChallengeShape: TShape;
    NextChallengeShape: TShape;
    CancelShape: TShape;
    PROCEDURE CancelShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE NextChallengeShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE RestartChallengeShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE SetEditModeShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private

  public
    FUNCTION showAfterTest(CONST success,furtherTasksAvailable:boolean):T_finishResponse;
  end;

FUNCTION TaskFinishedDialog: TTaskFinishedDialog;

IMPLEMENTATION
USES visuals;
VAR
  myTaskFinishedForm: TTaskFinishedDialog=nil;

FUNCTION TaskFinishedDialog: TTaskFinishedDialog;
  begin
    if myTaskFinishedForm=nil then myTaskFinishedForm:=TTaskFinishedDialog.create(nil);
    result:=myTaskFinishedForm;
  end;

{$R *.lfm}

{ TTaskFinishedDialog }

PROCEDURE TTaskFinishedDialog.SetEditModeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrClose;
  end;

PROCEDURE TTaskFinishedDialog.RestartChallengeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrRetry;
  end;

PROCEDURE TTaskFinishedDialog.NextChallengeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrYes;
  end;

PROCEDURE TTaskFinishedDialog.CancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

FUNCTION TTaskFinishedDialog.showAfterTest(CONST success, furtherTasksAvailable: boolean): T_finishResponse;
  VAR
    mr: integer;
  begin
    applyColorScheme(self);
    if success then begin
      Label1.caption:='✔';
      Label2.caption:='✔';
      Label1.Font.color:=colorScheme.CORRECT_COLOR;
      Label2.Font.color:=colorScheme.SHADOW_COLOR;
    end else begin
      Label1.caption:='✘';
      Label2.caption:='✘';
      Label1.Font.color:=colorScheme.INCORRECT_COLOR;
      Label2.Font.color:=colorScheme.SHADOW_COLOR;
    end;
    setEnableButton(NextChallengeShape,NextChallengeLabel,furtherTasksAvailable);
    mr:=ShowModal;
    result:=fr_cancel;
    if mr=mrClose then result:=fr_backToFreeEdit;
    if mr=mrRetry then result:=fr_restartTask;
    if mr=mrYes   then result:=fr_nextTask;
  end;

FINALIZATION
  if myTaskFinishedForm<>nil then FreeAndNil(myTaskFinishedForm);

end.

