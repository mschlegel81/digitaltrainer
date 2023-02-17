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
    PROCEDURE TestCasesStringGridEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject;
      IsColumn: boolean; index: integer);
  private

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

{ TCreateTaskForm }

PROCEDURE TCreateTaskForm.TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
begin

end;

PROCEDURE TCreateTaskForm.TestCasesStringGridEditingDone(Sender: TObject);
begin

end;

PROCEDURE TCreateTaskForm.showFor(CONST board: P_visualBoard; CONST challenges: P_challengeSet);
  begin
    TestCasesStringGrid.editor.Font.color:=clWhite;
    TestCasesStringGrid.editor.color:=clBlack;
    rbIncludeAllGates.Font.color:=clWhite;

    //TODO: Add initialization
    if ShowModal=mrOk then begin
      //TODO: Add action on accept...
      //Perform challenge creation
    end;
  end;

FINALIZATION
  if myCreateTaskForm<>nil then FreeAndNil(myCreateTaskForm);
end.

