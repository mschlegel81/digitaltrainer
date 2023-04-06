UNIT createChallengeUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, visualGates, challenges,compoundGates,testFrameUI,workspaces;

TYPE

  { TCreateChallengeDialog }

  TCreateChallengeDialog = class(TForm)
    addTaskLabel: TLabel;
    editTemplateLabel: TLabel;
    editExpectedLabel: TLabel;
    addTaskShape: TShape;
    resetTemplateLabel: TLabel;
    editTemplateShape: TShape;
    editExpectedShape: TShape;
    resetTemplateShape: TShape;
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
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE rbIncludeAllGatesChange(Sender: TObject);
    PROCEDURE resetTemplateShapeMouseDown(Sender: TObject;
      button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    challenge:P_challenge;
    challengeIndex:longint;

    FUNCTION minimumDifficulty: longint;
    FUNCTION paletteOption:T_challengePaletteOption;
  public
    PROCEDURE showForNewChallenge     (CONST board:P_visualBoard; CONST challenges:P_challengeSet);
    FUNCTION  showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet):boolean;
    FUNCTION reShowFor               (CONST editedChallenge:P_challenge; CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet):boolean;
  end;

VAR uiAdapter:P_uiAdapter;
FUNCTION CreateChallengeDialog:TCreateChallengeDialog;
IMPLEMENTATION
USES visuals;
{$R *.lfm}
VAR
  myCreateChallengeDialog: TCreateChallengeDialog=nil;

FUNCTION CreateChallengeDialog: TCreateChallengeDialog;
  begin
    if myCreateChallengeDialog=nil then myCreateChallengeDialog:=TCreateChallengeDialog.create(nil);
    result:=myCreateChallengeDialog;
  end;

FUNCTION TCreateChallengeDialog.minimumDifficulty: longint;
  begin
    result:=0;
    if      rbPreconfiguredPalette         .checked then result+=17
    else if rbUnconfiguredPaletteWithCounts.checked then result+=17*2
    else if rbFreePalette                  .checked then result+=17*3;
  end;

FUNCTION TCreateChallengeDialog.paletteOption: T_challengePaletteOption;
  begin
    if rbPreconfiguredPalette         .checked then exit(co_preconfiguredPalette);
    if rbUnconfiguredPaletteWithCounts.checked then exit(co_unconfiguredPaletteWithCounts);
    if rbFreePalette                  .checked then exit(co_freePalette);
    result:=co_preconfiguredPaletteWithCounts;
  end;

{ TCreateChallengeDialog }

PROCEDURE TCreateChallengeDialog.rbIncludeAllGatesChange(Sender: TObject);
  begin
    if DifficultyTrackBar.position<minimumDifficulty then
       DifficultyTrackBar.position:=minimumDifficulty;
  end;

PROCEDURE TCreateChallengeDialog.resetTemplateShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    dispose(challenge^.resultTemplate,destroy);
    challenge^.resultTemplate:=challenge^.expectedBehavior^.cloneAsTrueCopy;
    buttonClicked(resetTemplateShape);
  end;

PROCEDURE TCreateChallengeDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

PROCEDURE TCreateChallengeDialog.addTaskShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TCreateChallengeDialog.editExpectedShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    workspace.startEditingChallenge(challenge,challengeIndex,true,uiAdapter);
    ModalResult:=mrYes;
  end;

PROCEDURE TCreateChallengeDialog.editTemplateShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    workspace.startEditingChallenge(challenge,challengeIndex,false,uiAdapter);
    ModalResult:=mrYes;
  end;

PROCEDURE TCreateChallengeDialog.FormCreate(Sender: TObject);
  begin
    addButton(resetTemplateShape,resetTemplateLabel);
    //remaining buttons lead to closing of the dialog and don't require animation
    TestCreationFrame1.initOnCreate;
  end;

PROCEDURE TCreateChallengeDialog.showForNewChallenge(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  VAR temp:P_challenge;
  begin
    new(temp,create);
    temp^.initNewChallenge(board);
    temp^.challengeLevel:=minimumDifficulty;
    reShowFor(temp,-1,challenges);
  end;

FUNCTION TCreateChallengeDialog.showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet ):boolean;
  begin
    result:=reShowFor(challenges^.challenge[originalChallengeIndex]^.clone,originalChallengeIndex,challenges);
  end;

FUNCTION TCreateChallengeDialog.reShowFor(CONST editedChallenge:P_challenge; CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet):boolean;
  begin
    challenge:=editedChallenge;
    challenge^.ensureBehavior;
    challengeIndex:=originalChallengeIndex;
    TitleEdit.text:=challenge^.challengeTitle;
    DescriptionMemo.text:=challenge^.challengeDescription;
    DifficultyTrackBar.position:=challenge^.challengeLevel;
    TestCreationFrame1.setTestGenerator(challenge,MAX_NUMBER_OF_CHALLENGE_CHECKS);
    challenge^.updateTestCaseResults;
    case challenge^.palette^.paletteOption of
      co_preconfiguredPaletteWithCounts: rbPreconfiguredPaletteWithCounts.checked:=true;
      co_preconfiguredPalette          : rbPreconfiguredPalette          .checked:=true;
      co_unconfiguredPaletteWithCounts : rbUnconfiguredPaletteWithCounts .checked:=true;
      co_freePalette                   : rbFreePalette                   .checked:=true;
    end;

    if ShowModal=mrOk then begin
      TestCreationFrame1.detachTestGenerator;
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;
      challenge^.palette^.paletteOption:=paletteOption;
      challenge^.palette^.finalizePalette(challenge^.resultTemplate,challenge^.expectedBehavior);
      challenge^.dropBehavior;
      if originalChallengeIndex>=0 then begin
        dispose(challenges^.challenge[originalChallengeIndex],destroy);
        challenges^.challenge[originalChallengeIndex]:=challenge;
      end else challenges^.add(challenge);
    end else begin
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;
      challenge^.palette^.paletteOption:=paletteOption;

      TestCreationFrame1.detachTestGenerator;
      if ModalResult<>mrYes then dispose(challenge,destroy);
    end;
    challenge:=nil;
    result:=ModalResult=mrYes;
  end;

FINALIZATION
  if myCreateChallengeDialog<>nil then FreeAndNil(myCreateChallengeDialog);
end.

