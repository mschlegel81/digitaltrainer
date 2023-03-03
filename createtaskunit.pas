UNIT createTaskUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Grids, visualGates, challenges;

TYPE

  { TCreateTaskForm }

  TCreateTaskForm = class(TForm)
    cbAllowAllGates: TCheckBox;
    TestCasesStringGrid: TStringGrid;
    TestCaseCountEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    RadioPanel: TPanel;
    TestInputsPanel: TPanel;
    generateTestCasesLabel: TLabel;
    generateTestCasesShape: TShape;
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
    PROCEDURE rbIncludeAllGatesChange(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  private
    challenge:P_challenge;
    FUNCTION minimumDifficulty: longint;

  public
    PROCEDURE showFor(CONST board:P_visualBoard; CONST challenges:P_challengeSet);
  end;

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

FUNCTION TCreateTaskForm.minimumDifficulty:longint;
  begin
    if      rbIncludeAllGates   .checked then result:=0
    else if rbIncludeHalfOfGates.checked then result:=17
    else if rbIncludeIO         .checked then result:=17*2
                                         else result:=17*3;
    if cbAllowAllGates.checked then result:=(result+17)*2;
  end;

{ TCreateTaskForm }

PROCEDURE TCreateTaskForm.TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
begin

end;

PROCEDURE TCreateTaskForm.TestCasesStringGridEditingDone(Sender: TObject);
begin

end;

PROCEDURE TCreateTaskForm.rbIncludeAllGatesChange(Sender: TObject);
  begin
    if DifficultyTrackBar.position<minimumDifficulty then
       DifficultyTrackBar.position:=minimumDifficulty;
  end;

PROCEDURE TCreateTaskForm.TestCaseCountEditEditingDone(Sender: TObject);
begin

end;

PROCEDURE TCreateTaskForm.showFor(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  begin
    TestCasesStringGrid.editor.Font.color:=clWhite;
    TestCasesStringGrid.editor.color:=clBlack;
    rbIncludeAllGates.Font.color:=clWhite;

    TitleEdit.text:=StringReplace(board^.getCaption,LineEnding,'\n',[rfReplaceAll]);
    DescriptionMemo.text:=board^.getDescription;

    new(challenge,create);

   // challengeLevel      :byte;
   // callengeCompleted   :boolean;
   // board               :P_visualBoard;
   // resultTemplate      :P_visualBoard;
   // expectedBehavior    :P_compoundGate;
   // tests:array of record
   //   inputs:array of T_wireValue;
   //   maxTotalSteps:longint;
   //   timeout:longint;
   // end;
   // palette             :P_challengePalette;
   // challengeTitle      :string;
   // challengeDescription:string;

    //TODO: Add initialization
    if ShowModal=mrOk then begin
      //TODO: Add action on accept...
      //Perform challenge creation
      challenges^.add(challenge);
    end else begin
      dispose(challenge,destroy);
    end;
  end;

FINALIZATION
  if myCreateTaskForm<>nil then FreeAndNil(myCreateTaskForm);
end.

