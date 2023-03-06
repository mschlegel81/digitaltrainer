UNIT selectTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls,challenges;

TYPE

  { TSelectTaskForm }

  TSelectTaskForm = class(TForm)
    ChallengesMemo: TMemo;
    ChallengesGrid: TStringGrid;
    DeleteTaskLabel: TLabel;
    StartTaskLabel: TLabel;
    DeleteTaskShape: TShape;
    StartTaskShape: TShape;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ChallengesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    challengeSet:P_challengeSet;

  public
    FUNCTION startTaskAfterShowing(CONST cSet:P_challengeSet):boolean;
  end;

FUNCTION SelectTaskForm: TSelectTaskForm;

IMPLEMENTATION
USES visuals;
VAR mySelectTaskForm:TSelectTaskForm=nil;
FUNCTION SelectTaskForm: TSelectTaskForm;
  begin
    if mySelectTaskForm=nil then mySelectTaskForm:=TSelectTaskForm.create(nil);
    result:=mySelectTaskForm;
  end;

{$R *.lfm}

{ TSelectTaskForm }

PROCEDURE TSelectTaskForm.FormShow(Sender: TObject);
  begin
  end;

PROCEDURE TSelectTaskForm.ChallengesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin

  end;

PROCEDURE TSelectTaskForm.ChallengesGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    aRow-=1;
    if (aRow<0) or (aRow>=length(challengeSet^.challenge)) then exit;
    ChallengesMemo.text:=challengeSet^.challenge[aRow]^.challengeDescription;
    setEnableButton(StartTaskShape,StartTaskLabel,true);
  end;

PROCEDURE TSelectTaskForm.StartTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

FUNCTION TSelectTaskForm.startTaskAfterShowing(CONST cSet: P_challengeSet):boolean;
  VAR i:longint;
  begin
    ChallengesMemo.text:='';
    challengeSet:=cSet;
    ChallengesGrid.rowCount:=1+length(challengeSet^.challenge);
    for i:=0 to length(challengeSet^.challenge)-1 do begin
      ChallengesGrid.Cells[0,i+1]:=challengeSet^.challenge[i]^.challengeTitle;
      ChallengesGrid.Cells[1,i+1]:=BoolToStr(challengeSet^.challenge[i]^.callengeCompleted,checkMark,'');
      ChallengesGrid.Cells[2,i+1]:=StringOfChar('|',challengeSet^.challenge[i]^.challengeLevel);
    end;
    ChallengesGrid.AutoSizeColumns;
    setEnableButton(DeleteTaskShape,DeleteTaskLabel,challengeSet^.editable);
    setEnableButton(StartTaskShape,StartTaskLabel,false);

    result:=ShowModal=mrOk;
  end;

FINALIZATION
  if mySelectTaskForm<>nil then FreeAndNil(mySelectTaskForm);

end.

