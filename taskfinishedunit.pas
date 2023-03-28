UNIT taskFinishedUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

TYPE
  T_finishResponse=(fr_backToFreeEdit,fr_restartTask,fr_nextTask,fr_cancel);

  { TTaskFinishedForm }

  TTaskFinishedForm = class(TForm)
    NextChallengeLabel: TLabel;
    CancelLabel: TLabel;
    SetEditModeLabel: TLabel;
    RestartChallengeLabel: TLabel;
    SetEditModeShape: TShape;
    RestartChallengeShape: TShape;
    NextChallengeShape: TShape;
    CancelShape: TShape;
    SuccessImage: TImage;
    FailedImage: TImage;
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

FUNCTION TaskFinishedForm: TTaskFinishedForm;

IMPLEMENTATION
USES visuals;
VAR
  myTaskFinishedForm: TTaskFinishedForm=nil;

FUNCTION TaskFinishedForm: TTaskFinishedForm;
  begin
    if myTaskFinishedForm=nil then myTaskFinishedForm:=TTaskFinishedForm.create(nil);
    result:=myTaskFinishedForm;
  end;

{$R *.lfm}

{ TTaskFinishedForm }

PROCEDURE TTaskFinishedForm.SetEditModeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrClose;
  end;

PROCEDURE TTaskFinishedForm.RestartChallengeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrRetry;
  end;

PROCEDURE TTaskFinishedForm.NextChallengeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrYes;
  end;

PROCEDURE TTaskFinishedForm.CancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

FUNCTION TTaskFinishedForm.showAfterTest(CONST success, furtherTasksAvailable: boolean): T_finishResponse;
  VAR
    mr: integer;
  begin
    applyColorScheme(self);
    SuccessImage.visible:=success;
    FailedImage.visible:=not(success);
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

