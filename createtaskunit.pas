UNIT createTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Grids, visualGates, challenges,compoundGates,testFrameUI;

TYPE

  { TCreateTaskForm }

  TCreateTaskForm = class(TForm)
    addTaskLabel: TLabel;
    addTaskShape: TShape;
    Label7: TLabel;
    RadioPanel1: TPanel;
    rbPreconfiguredPaletteWithCounts: TRadioButton;
    rbPreconfiguredPalette: TRadioButton;
    rbUnconfiguredPaletteWithCounts: TRadioButton;
    rbFreePalette: TRadioButton;
    Label5: TLabel;
    RadioPanel: TPanel;
    TestCreationFrame1: TTestCreationFrame;
    TitleEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DescriptionMemo: TMemo;
    rbIncludeAllGates: TRadioButton;
    rbIncludeIO: TRadioButton;
    rbIncludeHalfOfGates: TRadioButton;
    rbIncludeNothing: TRadioButton;
    DifficultyTrackBar: TTrackBar;
    PROCEDURE addTaskShapeChangeBounds(Sender: TObject);
    PROCEDURE addTaskShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE rbIncludeAllGatesChange(Sender: TObject);
  private
    challenge:P_challenge;

    FUNCTION minimumDifficulty: longint;
    FUNCTION paletteOption:T_challengePaletteOption;
    FUNCTION boardOption:T_challengeBoardOption;
  public
    PROCEDURE showFor(CONST board:P_visualBoard; CONST challenges:P_challengeSet);
    PROCEDURE showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet );
  end;

FUNCTION CreateTaskForm:TCreateTaskForm;
IMPLEMENTATION
USES logicalGates;
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
    if      rbIncludeAllGates   .checked then result:=0
    else if rbIncludeHalfOfGates.checked then result:=17
    else if rbIncludeIO         .checked then result:=17*2
                                         else result:=17*3;
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

FUNCTION TCreateTaskForm.boardOption: T_challengeBoardOption;
  begin
    if rbIncludeAllGates   .checked then exit(co_allGates);
    if rbIncludeHalfOfGates.checked then exit(co_halfGates);
    if rbIncludeIO         .checked then exit(co_ioOnly);
    result:=co_none;
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

PROCEDURE TCreateTaskForm.addTaskShapeChangeBounds(Sender: TObject);
  begin
  end;

PROCEDURE TCreateTaskForm.showFor(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  begin
    rbIncludeAllGates.Font.color:=clWhite;

    TitleEdit.text:=StringReplace(board^.getCaption,LineEnding,'\n',[rfReplaceAll]);
    DescriptionMemo.text:=board^.getDescription;

    new(challenge,create);
    challenge^.challengeLevel:=minimumDifficulty;
    challenge^.initNewChallenge(board,boardOption,paletteOption);
    TestCreationFrame1.setTestGenerator(challenge,MAX_NUMBER_OF_CHALLENGE_CHECKS);

    RadioPanel .visible:=true;
    RadioPanel1.visible:=true;
    Label4.visible:=true;
    Label7.visible:=true;

    if ShowModal=mrOk then begin
      TestCreationFrame1.detachTestGenerator;
      challenge^.initNewChallenge(board,boardOption,paletteOption);
      challenge^.challengeLevel:=DifficultyTrackBar.position;
      challenge^.challengeTitle:=TitleEdit.text;
      challenge^.challengeDescription:=DescriptionMemo.text;
      challenges^.add(challenge);
    end else begin
      TestCreationFrame1.detachTestGenerator;
      dispose(challenge,destroy);
    end;
    challenge:=nil;
  end;

PROCEDURE TCreateTaskForm.showForExistingChallenge(CONST originalChallengeIndex:longint; CONST challenges: P_challengeSet );
  begin
    challenge:=challenges^.challenge[originalChallengeIndex]^.partialClone;
    RadioPanel .visible:=false;
    RadioPanel1.visible:=false;
    Label4.visible:=false;
    Label7.visible:=false;

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

      dispose(challenges^.challenge[originalChallengeIndex],destroyPartial);
      challenges^.challenge[originalChallengeIndex]:=challenge;
    end else begin
      TestCreationFrame1.detachTestGenerator;
      dispose(challenge,destroyPartial);
    end;
    challenge:=nil;
  end;

FINALIZATION
  if myCreateTaskForm<>nil then FreeAndNil(myCreateTaskForm);
end.

