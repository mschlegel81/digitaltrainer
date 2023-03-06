UNIT selectTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls,challenges,workspaces;

TYPE

  { TSelectTaskForm }

  TSelectTaskForm = class(TForm)
    ChallengesMemo: TMemo;
    ChallengesGrid: TStringGrid;
    DeleteTaskLabel: TLabel;
    EditTaskLabel: TLabel;
    EditTaskShape: TShape;
    StartTaskLabel: TLabel;
    DeleteTaskShape: TShape;
    StartTaskShape: TShape;
    PROCEDURE DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ChallengesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    challengeSet:P_challengeSet;
    workspace:P_workspace;
    PROCEDURE updateTable;
  public
    selectedChallengeIndex:longint;
    FUNCTION startTaskAfterShowing(CONST cSet:P_challengeSet):boolean;
  end;

FUNCTION SelectTaskForm(CONST ws:P_workspace): TSelectTaskForm;

IMPLEMENTATION
USES visuals,createTaskUnit;
VAR mySelectTaskForm:TSelectTaskForm=nil;
FUNCTION SelectTaskForm(CONST ws:P_workspace): TSelectTaskForm;
  begin
    if mySelectTaskForm=nil then begin
      mySelectTaskForm:=TSelectTaskForm.create(nil);
      mySelectTaskForm.workspace:=ws;
    end;
    result:=mySelectTaskForm;
  end;

{$R *.lfm}

{ TSelectTaskForm }

PROCEDURE TSelectTaskForm.FormShow(Sender: TObject);
  begin
  end;

PROCEDURE TSelectTaskForm.DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    if not(challengeSet^.editable) or (selectedChallengeIndex<0) then exit;
    dispose(challengeSet^.challenge[selectedChallengeIndex],destroy);
    for i:=selectedChallengeIndex+1 to length(challengeSet^.challenge)-1 do
      challengeSet^.challenge[i-1]:=challengeSet^.challenge[i];
    setLength(challengeSet^.challenge,length(challengeSet^.challenge)-1);
    workspace^.challengeDeleted(selectedChallengeIndex);
    updateTable;
  end;

PROCEDURE TSelectTaskForm.EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    CreateTaskForm.showForExistingChallenge(selectedChallengeIndex,challengeSet);
    updateTable;
  end;

PROCEDURE TSelectTaskForm.ChallengesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin

  end;

PROCEDURE TSelectTaskForm.ChallengesGridSelection(Sender: TObject; aCol,
  aRow: integer);
  begin
    aRow-=1; //ignore header
    if (aRow<0) or (aRow>=length(challengeSet^.challenge)) then exit;
    ChallengesMemo.text:=challengeSet^.challenge[aRow]^.challengeDescription;
    selectedChallengeIndex:=aRow;
    setEnableButton(StartTaskShape,StartTaskLabel,true);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,challengeSet^.editable);
    setEnableButton(EditTaskShape,EditTaskLabel,challengeSet^.editable);
  end;

PROCEDURE TSelectTaskForm.StartTaskShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TSelectTaskForm.updateTable;
  VAR i:longint;
  begin
    ChallengesMemo.text:='';
    ChallengesGrid.rowCount:=1+length(challengeSet^.challenge);
    for i:=0 to length(challengeSet^.challenge)-1 do begin
      ChallengesGrid.Cells[0,i+1]:=challengeSet^.challenge[i]^.challengeTitle;
      ChallengesGrid.Cells[1,i+1]:=BoolToStr(challengeSet^.challenge[i]^.callengeCompleted,checkMark,'');
      ChallengesGrid.Cells[2,i+1]:=StringOfChar('|',challengeSet^.challenge[i]^.challengeLevel);
    end;
    ChallengesGrid.AutoSizeColumns;
    setEnableButton(EditTaskShape,EditTaskLabel,false);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,false);
    setEnableButton(StartTaskShape ,StartTaskLabel,false);
    selectedChallengeIndex:=-1;
  end;

FUNCTION TSelectTaskForm.startTaskAfterShowing(CONST cSet: P_challengeSet): boolean;
  begin
    challengeSet:=cSet;
    updateTable;
    result:=ShowModal=mrOk;
  end;

FINALIZATION
  if mySelectTaskForm<>nil then FreeAndNil(mySelectTaskForm);

end.

