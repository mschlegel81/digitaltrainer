UNIT selectTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls,challenges,workspaces;

TYPE
  TSelectTaskForm = class(TForm)
    ChallengesMemo: TMemo;
    ChallengesGrid: TStringGrid;
    DeleteTaskLabel: TLabel;
    EditTaskLabel: TLabel;
    EditTaskShape: TShape;
    MoveTaskDownLabel: TLabel;
    MoveTaskDownShape: TShape;
    MoveTaskUpLabel: TLabel;
    MoveTaskUpShape: TShape;
    SaveDialog1: TSaveDialog;
    StartTaskLabel: TLabel;
    DeleteTaskShape: TShape;
    StartTaskShape: TShape;
    PROCEDURE ChallengesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
    PROCEDURE ChallengesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
    PROCEDURE DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    challengeSet:P_challengeSet;
    exporting,backupCreated:boolean;
    PROCEDURE updateTable;
    PROCEDURE createBackupOnce(CONST reason:T_workspaceHistorizationTriggerEnum);
  public
    selectedChallengeIndex:longint;
    FUNCTION startTaskAfterShowing(CONST cSet:P_challengeSet):boolean;
    PROCEDURE showForExport(CONST cSet: P_challengeSet);
  end;

FUNCTION SelectTaskForm: TSelectTaskForm;

IMPLEMENTATION
USES visuals,createTaskUnit;
VAR mySelectTaskForm:TSelectTaskForm=nil;
FUNCTION SelectTaskForm: TSelectTaskForm;
  begin
    if mySelectTaskForm=nil then mySelectTaskForm:=TSelectTaskForm.create(nil);
    result:=mySelectTaskForm;
  end;

{$R *.lfm}

{ TSelectTaskForm }

PROCEDURE TSelectTaskForm.createBackupOnce(CONST reason:T_workspaceHistorizationTriggerEnum);
  begin
    if backupCreated then exit;
    addBackup(@workspace,reason);
    backupCreated:=true;
  end;

PROCEDURE TSelectTaskForm.FormShow(Sender: TObject);
  begin
  end;

PROCEDURE TSelectTaskForm.DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    if exporting then begin
      if SaveDialog1.execute then begin
        challengeSet^.exportSelected(SaveDialog1.fileName,false);
        ModalResult:=mrOk;
      end;
    end else begin
      if (selectedChallengeIndex<0) then exit;
      createBackupOnce(wht_beforeDeletingTask);
      dispose(challengeSet^.challenge[selectedChallengeIndex],destroy);
      for i:=selectedChallengeIndex+1 to length(challengeSet^.challenge)-1 do
        challengeSet^.challenge[i-1]:=challengeSet^.challenge[i];

      setLength(challengeSet^.challenge,length(challengeSet^.challenge)-1);
      workspace.challengeDeleted(selectedChallengeIndex);
      updateTable;
    end;
  end;

PROCEDURE TSelectTaskForm.ChallengesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
  begin
    dec(aRow);
    if (not exporting) or (tutorial.equals(challengeSet^.challenge[aRow]))
    then value:=cbGrayed
    else begin
      if challengeSet^.challenge[aRow]^.marked
      then value:=cbChecked
      else value:=cbUnchecked;
    end;
  end;

PROCEDURE TSelectTaskForm.ChallengesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
  VAR i: integer;
      anySelected:boolean=false;
  begin
    dec(aRow);
    if (not exporting) or (tutorial.equals(challengeSet^.challenge[aRow]))
    then exit;

    challengeSet^.challenge[aRow]^.marked:=not(challengeSet^.challenge[aRow]^.marked);
    ChallengesGrid.Cells[3,aRow+1]:=BoolToStr(challengeSet^.challenge[aRow]^.marked,'x','');
    for i:=0 to length(challengeSet^.challenge)-1 do anySelected:=anySelected or challengeSet^.challenge[i]^.marked;
    setEnableButton(StartTaskShape,StartTaskLabel,anySelected);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,anySelected);
  end;

PROCEDURE TSelectTaskForm.EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR
    closeImmediately: boolean;
  begin
    closeImmediately:=CreateTaskForm.showForExistingChallenge(selectedChallengeIndex,challengeSet);
    if closeImmediately then ModalResult:=mrYes else updateTable;
  end;

PROCEDURE TSelectTaskForm.ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    aRow-=1; //ignore header
    if (aRow<0) or (aRow>=length(challengeSet^.challenge)) then exit;
    ChallengesMemo.text:=challengeSet^.challenge[aRow]^.challengeDescription;
    selectedChallengeIndex:=aRow;
    setEnableButton(EditTaskShape,EditTaskLabel,challengeSet^.challenge[aRow]^.editable);
    setEnableButton(MoveTaskUpShape,MoveTaskUpLabel,aRow>0);
    setEnableButton(MoveTaskDownShape,MoveTaskDownLabel,aRow<length(challengeSet^.challenge)-1);
    if exporting then exit;
    setEnableButton(StartTaskShape,StartTaskLabel,true);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,not(tutorial.equals(challengeSet^.challenge[aRow])));
  end;

PROCEDURE TSelectTaskForm.MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR newSelection: TGridRect;
  begin
    newSelection:=ChallengesGrid.selection;
    challengeSet^.moveChallenge(selectedChallengeIndex,false);
    newSelection.top   :=selectedChallengeIndex+1+1;
    newSelection.Bottom:=newSelection.top;
    updateTable;
    ChallengesGrid.selection:=newSelection;
    ChallengesGridSelection(Sender,0,newSelection.top);
  end;

PROCEDURE TSelectTaskForm.MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR newSelection: TGridRect;
  begin
    newSelection:=ChallengesGrid.selection;
    challengeSet^.moveChallenge(selectedChallengeIndex,true);
    newSelection.top   :=selectedChallengeIndex-1+1;
    newSelection.Bottom:=newSelection.top;
    updateTable;
    ChallengesGrid.selection:=newSelection;
    ChallengesGridSelection(Sender,0,newSelection.top);
  end;

PROCEDURE TSelectTaskForm.StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if exporting then begin
      if SaveDialog1.execute then begin
        challengeSet^.exportSelected(SaveDialog1.fileName,true);
        ModalResult:=mrOk;
      end;
    end else ModalResult:=mrOk;
  end;

PROCEDURE TSelectTaskForm.updateTable;
  VAR i:longint;
  begin
    ChallengesMemo.text:='';
    ChallengesGrid.rowCount:=1+length(challengeSet^.challenge);
    ChallengesGrid.Columns[3].visible:=exporting;
    for i:=0 to length(challengeSet^.challenge)-1 do begin
      ChallengesGrid.Cells[0,i+1]:=challengeSet^.challenge[i]^.challengeTitle;
      ChallengesGrid.Cells[1,i+1]:=BoolToStr(challengeSet^.challenge[i]^.callengeCompleted,checkMark,'');
      ChallengesGrid.Cells[2,i+1]:=StringOfChar('|',challengeSet^.challenge[i]^.challengeLevel);
      ChallengesGrid.Cells[3,i+1]:=BoolToStr(challengeSet^.challenge[i]^.marked,'x','');
    end;
    ChallengesGrid.AutoSizeColumns;
    setEnableButton(EditTaskShape,EditTaskLabel,false);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,false);
    setEnableButton(StartTaskShape ,StartTaskLabel,false);
    setEnableButton(MoveTaskUpShape,MoveTaskUpLabel,false);
    setEnableButton(MoveTaskDownShape,MoveTaskDownLabel,false);
    selectedChallengeIndex:=-1;
  end;

FUNCTION TSelectTaskForm.startTaskAfterShowing(CONST cSet: P_challengeSet): boolean;
  begin
    backupCreated:=false;
    challengeSet:=cSet;
    StartTaskLabel.caption:='Aufgabe starten';
    DeleteTaskLabel.caption:='Aufgabe löschen';
    exporting:=false;
    updateTable;
    result:=ShowModal=mrOk;
  end;

PROCEDURE TSelectTaskForm.showForExport(CONST cSet: P_challengeSet);
  VAR i:longint;
  begin
    backupCreated:=false;
    for i:=0 to length(cSet^.challenge)-1 do cSet^.challenge[i]^.marked:=false;
    StartTaskLabel.caption:='Exportieren';
    DeleteTaskLabel.caption:='Exportieren (gesperrt)';
    challengeSet:=cSet;
    exporting:=true;
    updateTable;
    ShowModal;
  end;

FINALIZATION
  if mySelectTaskForm<>nil then FreeAndNil(mySelectTaskForm);

end.

