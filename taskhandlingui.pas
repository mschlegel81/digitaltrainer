UNIT taskHandlingUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls,challenges,workspaces;

TYPE

  { TChallengeHandlingDialog }

  TChallengeHandlingDialog = class(TForm)
    ChallengesMemo: TMemo;
    ChallengesGrid: TStringGrid;
    DeleteTaskLabel: TLabel;
    EditTaskLabel: TLabel;
    EditTaskShape: TShape;
    ExportLabel: TLabel;
    ExportShape: TShape;
    ImportLabel: TLabel;
    ImportShape: TShape;
    MarkAllLabel: TLabel;
    MarkAllShape: TShape;
    MarkNoneLabel: TLabel;
    MarkNoneShape: TShape;
    MoveTaskDownLabel: TLabel;
    MoveTaskDownShape: TShape;
    MoveTaskUpLabel: TLabel;
    MoveTaskUpShape: TShape;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StartTaskLabel: TLabel;
    DeleteTaskShape: TShape;
    StartTaskShape: TShape;
    PROCEDURE ChallengesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
    PROCEDURE ChallengesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
    PROCEDURE DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    challengeSet:P_challengeSet;
    backupCreated:boolean;
    PROCEDURE updateTable;
    PROCEDURE createBackupOnce(CONST reason:T_workspaceHistorizationTriggerEnum);
  public
    selectedChallengeIndex:longint;
    FUNCTION startTaskAfterShowing(CONST cSet:P_challengeSet):boolean;
  end;

FUNCTION ChallengeHandlingDialog: TChallengeHandlingDialog;

IMPLEMENTATION
USES visuals,createChallengeUi;
VAR mySelectTaskForm:TChallengeHandlingDialog=nil;
FUNCTION ChallengeHandlingDialog: TChallengeHandlingDialog;
  begin
    if mySelectTaskForm=nil then mySelectTaskForm:=TChallengeHandlingDialog.create(nil);
    result:=mySelectTaskForm;
  end;

{$R *.lfm}

{ TChallengeHandlingDialog }

PROCEDURE TChallengeHandlingDialog.createBackupOnce(CONST reason:T_workspaceHistorizationTriggerEnum);
  begin
    if backupCreated then exit;
    addBackup(@workspace,reason);
    backupCreated:=true;
  end;

PROCEDURE TChallengeHandlingDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

PROCEDURE TChallengeHandlingDialog.DeleteTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    buttonClicked(DeleteTaskShape);
    if (selectedChallengeIndex<0) then exit;
    createBackupOnce(wht_beforeDeletingTask);
    dispose(challengeSet^.challenge[selectedChallengeIndex],destroy);
    for i:=selectedChallengeIndex+1 to length(challengeSet^.challenge)-1 do
      challengeSet^.challenge[i-1]:=challengeSet^.challenge[i];

    setLength(challengeSet^.challenge,length(challengeSet^.challenge)-1);
    workspace.challengeDeleted(selectedChallengeIndex);
    updateTable;
  end;

PROCEDURE TChallengeHandlingDialog.ChallengesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
  begin
    dec(aRow);
    if (tutorial.equals(challengeSet^.challenge[aRow]))
    then value:=cbGrayed
    else begin
      if challengeSet^.challenge[aRow]^.marked
      then value:=cbChecked
      else value:=cbUnchecked;
    end;
  end;

PROCEDURE TChallengeHandlingDialog.ChallengesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
  VAR i: integer;
      anySelected:boolean=false;
  begin
    dec(aRow);
    if (tutorial.equals(challengeSet^.challenge[aRow]))
    then exit;

    challengeSet^.challenge[aRow]^.marked:=not(challengeSet^.challenge[aRow]^.marked);
    ChallengesGrid.Cells[3,aRow+1]:=BoolToStr(challengeSet^.challenge[aRow]^.marked,'x','');
    for i:=0 to length(challengeSet^.challenge)-1 do anySelected:=anySelected or challengeSet^.challenge[i]^.marked;
    setEnableButton(StartTaskShape,StartTaskLabel,anySelected);
    setEnableButton(ExportShape,ExportLabel,anySelected);
  end;

PROCEDURE TChallengeHandlingDialog.EditTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR
    closeImmediately: boolean;
  begin
    buttonClicked(EditTaskShape);
    closeImmediately:=CreateChallengeDialog.showForExistingChallenge(selectedChallengeIndex,challengeSet);
    if closeImmediately then ModalResult:=mrYes else updateTable;
  end;

PROCEDURE TChallengeHandlingDialog.ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ExportShape);
    if SaveDialog1.execute then challengeSet^.exportSelected(SaveDialog1.fileName,true);
    //TODO: Show notification "export successful"
  end;

PROCEDURE TChallengeHandlingDialog.FormCreate(Sender: TObject);
  begin
    addButton(MarkAllShape,MarkAllLabel);
    addButton(MarkNoneShape,MarkNoneLabel);
    addButton(MoveTaskDownShape,MoveTaskDownLabel);
    addButton(MoveTaskUpShape,MoveTaskUpLabel);
    addButton(ExportShape,ExportLabel);
    addButton(ImportShape,ImportLabel);
    addButton(DeleteTaskShape,DeleteTaskLabel);
    addButton(EditTaskShape,EditTaskLabel);
    addButton(StartTaskShape,StartTaskLabel);
  end;

PROCEDURE TChallengeHandlingDialog.ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    aRow-=1; //ignore header
    if (aRow<0) or (aRow>=length(challengeSet^.challenge)) then exit;
    ChallengesMemo.text:=challengeSet^.challenge[aRow]^.challengeDescription;
    selectedChallengeIndex:=aRow;
    setEnableButton(EditTaskShape,EditTaskLabel,challengeSet^.challenge[aRow]^.editable);
    setEnableButton(MoveTaskUpShape,MoveTaskUpLabel,aRow>0);
    setEnableButton(MoveTaskDownShape,MoveTaskDownLabel,aRow<length(challengeSet^.challenge)-1);
    setEnableButton(StartTaskShape,StartTaskLabel,true);
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,not(tutorial.equals(challengeSet^.challenge[aRow])));
  end;

PROCEDURE TChallengeHandlingDialog.ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ImportShape);
    if OpenDialog1.execute then begin
      challengeSet^.importChallenges(OpenDialog1.fileName,false);
      updateTable;
    end;
  end;

PROCEDURE TChallengeHandlingDialog.MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    buttonClicked(MarkAllShape);
    for i:=0 to length(challengeSet^.challenge)-1 do if not(tutorial.equals(challengeSet^.challenge[i])) then challengeSet^.challenge[i]^.marked:=true;
    //there is still the tutorial, so...
    setEnableButton(ExportShape,ExportLabel,length(challengeSet^.challenge)>1);
    updateTable;
  end;

PROCEDURE TChallengeHandlingDialog.MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    buttonClicked(MarkNoneShape);
    for i:=0 to length(challengeSet^.challenge)-1 do challengeSet^.challenge[i]^.marked:=false;
    setEnableButton(ExportShape,ExportLabel,false);
    updateTable;
  end;

PROCEDURE TChallengeHandlingDialog.MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR newSelection: TGridRect;
  begin
    buttonClicked(MoveTaskDownShape);
    newSelection:=ChallengesGrid.selection;
    challengeSet^.moveChallenge(selectedChallengeIndex,false);
    newSelection.top   :=selectedChallengeIndex+1+1;
    newSelection.Bottom:=newSelection.top;
    updateTable;
    ChallengesGrid.selection:=newSelection;
    ChallengesGridSelection(Sender,0,newSelection.top);
  end;

PROCEDURE TChallengeHandlingDialog.MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR newSelection: TGridRect;
  begin
    buttonClicked(MoveTaskUpShape);
    newSelection:=ChallengesGrid.selection;
    challengeSet^.moveChallenge(selectedChallengeIndex,true);
    newSelection.top   :=selectedChallengeIndex-1+1;
    newSelection.Bottom:=newSelection.top;
    updateTable;
    ChallengesGrid.selection:=newSelection;
    ChallengesGridSelection(Sender,0,newSelection.top);
  end;

PROCEDURE TChallengeHandlingDialog.StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(StartTaskShape);
    ModalResult:=mrOk;
  end;

PROCEDURE TChallengeHandlingDialog.updateTable;
  VAR i:longint;
  begin
    ChallengesMemo.text:='';
    ChallengesGrid.rowCount:=1+length(challengeSet^.challenge);
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

FUNCTION TChallengeHandlingDialog.startTaskAfterShowing(CONST cSet: P_challengeSet): boolean;
  begin
    EditTaskShape  .visible:=not(workspace.simplisticUi);
    EditTaskLabel  .visible:=not(workspace.simplisticUi);
    ExportShape    .visible:=not(workspace.simplisticUi);
    ExportLabel    .visible:=not(workspace.simplisticUi);
    ImportShape    .visible:=not(workspace.simplisticUi);
    ImportLabel    .visible:=not(workspace.simplisticUi);
    DeleteTaskShape.visible:=not(workspace.simplisticUi);
    DeleteTaskLabel.visible:=not(workspace.simplisticUi);
    ChallengesGrid.Columns[3].visible:=not(workspace.simplisticUi);
    MarkAllShape     .visible:=not(workspace.simplisticUi);
    MarkAllLabel     .visible:=not(workspace.simplisticUi);
    MarkNoneShape    .visible:=not(workspace.simplisticUi);
    MarkNoneLabel    .visible:=not(workspace.simplisticUi);
    MoveTaskDownShape.visible:=not(workspace.simplisticUi);
    MoveTaskDownLabel.visible:=not(workspace.simplisticUi);
    MoveTaskUpShape  .visible:=not(workspace.simplisticUi);
    MoveTaskUpLabel  .visible:=not(workspace.simplisticUi);

    backupCreated:=false;
    challengeSet:=cSet;
    updateTable;
    result:=ShowModal=mrOk;
  end;

FINALIZATION
  if mySelectTaskForm<>nil then FreeAndNil(mySelectTaskForm);

end.

