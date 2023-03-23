UNIT createTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, visualGates, challenges,compoundGates,testFrameUI,workspaces;

TYPE

  { TCreateTaskForm }

  TCreateTaskForm = class(TForm)
    addTaskLabel: TLabel;
    editTemplateLabel: TLabel;
    editExpectedLabel: TLabel;
    addTaskShape: TShape;
    editTemplateShape: TShape;
    editExpectedShape: TShape;
    Label7: TLabel;
    RadioPanel1: TPanel;
    rbPreconfiguredPaletteWithCounts: TRadioButton;
    rbPreconfiguredPalette: TRadioButton;
    rbUnconfiguredPaletteWithCounts: TRadioButton;
    rbFreePalette: TRadioButton;
    Label5: TLabel;
    TestCreationFrame1: TTestCreationFrame;
    TitleEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DescriptionMemo: TMemo;
    DifficultyTrackBar: TTrackBar;
    PROCEDURE addTaskShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE editExpectedShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE editTemplateShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE rbIncludeAllGatesChange(Sender: TObject);
  private
    challenge:P_challenge;
    challengeIndex:longint;

    FUNCTION minimumDifficulty: longint;
    FUNCTION paletteOption:T_challengePaletteOption;
  public
    PROCEDURE showForNewChallenge     (CONST board:P_visualBoard; CONST challenges:P_challengeSet);
    PROCEDURE showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet);
    PROCEDURE reShowFor               (CONST editedChallenge:P_challenge; CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet);
  end;

VAR workspace:P_workspace;
    uiAdapter:P_uiAdapter;
FUNCTION CreateTaskForm:TCreateTaskForm;
IMPLEMENTATION
{$R *.lfm}
VAR
  myCreateTaskForm: TCreateTaskForm=nil;

FUNCTION CreateTaskForm: TCreateTaskForm;
  begin
    if myCreateTaskForm=nil then myCreateTaskForm:=TCreateTaskForm.create(nil);
    result:=myCreateTaskForm;
  end;

FUNCTION TCreateTaskForm.minimumDifficulty: longint;
  begin
    result:=0;
    if      rbPreconfiguredPalette         .checked then result+=17
    else if rbUnconfiguredPaletteWithCounts.checked then result+=17*2
    else if rbFreePalette                  .checked then result+=17*3;
  end;

FUNCTION TCreateTaskForm.paletteOption: T_challengePaletteOption;
  begin
    if rbPreconfiguredPalette         .checked then exit(co_preconfiguredPalette);
    if rbUnconfiguredPaletteWithCounts.checked then exit(co_unconfiguredPaletteWithCounts);
    if rbFreePalette                  .checked then exit(co_freePalette);
    result:=co_preconfiguredPaletteWithCounts;
  end;

{ TCreateTaskForm }

PROCEDURE TCreateTaskForm.rbIncludeAllGatesChange(Sender: TObject);
  begin
    if DifficultyTrackBar.position<minimumDifficulty then
       DifficultyTrackBar.position:=minimumDifficulty;
  end;

PROCEDURE TCreateTaskForm.FormShow(Sender: TObject);
  begin
  end;

PROCEDURE TCreateTaskForm.addTaskShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TCreateTaskForm.editExpectedShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    workspace^.startEditingChallenge(challenge,challengeIndex,true,uiAdapter);
    ModalResult:=mrYes;
  end;

PROCEDURE TCreateTaskForm.editTemplateShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    //TODO: set visible counts to high before this!
    workspace^.startEditingChallenge(challenge,challengeIndex,false,uiAdapter);
    ModalResult:=mrYes;
  end;

PROCEDURE TCreateTaskForm.showForNewChallenge(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  VAR temp:P_challenge;
  begin
    new(temp,create);
    temp^.initNewChallenge(board);
    temp^.challengeLevel:=minimumDifficulty;
    reShowFor(temp,-1,challenges);
  end;

PROCEDURE TCreateTaskForm.showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet );
  begin
    reShowFor(challenges^.challenge[originalChallengeIndex]^.clone,originalChallengeIndex,challenges);
  end;

PROCEDURE TCreateTaskForm.reShowFor(CONST editedChallenge:P_challenge; CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet);
  begin
    challenge:=editedChallenge;
    challengeIndex:=originalChallengeIndex;
    TitleEdit.text:=challenge^.challengeTitle;
    DescriptionMemo.text:=challenge^.challengeDescription;
    DifficultyTrackBar.position:=challenge^.challengeLevel;
    TestCreationFrame1.setTestGenerator(challenge,MAX_NUMBER_OF_CHALLENGE_CHECKS);
    challenge^.updateTestCaseResults;
    if ShowModal=mrOk then begin
      TestCreationFrame1.detachTestGenerator;
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;
      if originalChallengeIndex>=0 then begin
        dispose(challenges^.challenge[originalChallengeIndex],destroy);
        challenges^.challenge[originalChallengeIndex]:=challenge;
      end else challenges^.add(challenge);
    end else begin
      TestCreationFrame1.detachTestGenerator;
      if ModalResult<>mrYes then dispose(challenge,destroy);
    end;
    challenge:=nil;
  end;

FINALIZATION
  if myCreateTaskForm<>nil then FreeAndNil(myCreateTaskForm);
end.

