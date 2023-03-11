UNIT boardTestUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, challenges, visualGates, testFrameUI;

TYPE

  { TBoardTestForm }

  TBoardTestForm = class(TForm)
    TestCreationFrame1: TTestCreationFrame;
  private
    tester: T_testCreator;
  public
    PROCEDURE showForBoard(CONST board:P_visualBoard);
  end;

FUNCTION BoardTestForm: TBoardTestForm;
IMPLEMENTATION
USES compoundGates;
VAR myBoardTestForm: TBoardTestForm=nil;
FUNCTION BoardTestForm: TBoardTestForm;
  begin
    if myBoardTestForm=nil then myBoardTestForm:=TBoardTestForm.create(nil);
    result:=myBoardTestForm;
  end;

{$R *.lfm}

{ TBoardTestForm }

PROCEDURE TBoardTestForm.showForBoard(CONST board: P_visualBoard);
  begin
    tester.createForAnalysis(board);
    TestCreationFrame1.setTestGenerator(@tester);
    ShowModal;
    TestCreationFrame1.detachTestGenerator;
    tester.destroy;
  end;

{ TBoardTestForm }

FINALIZATION
  if myBoardTestForm<>nil then FreeAndNil(myBoardTestForm);

end.

