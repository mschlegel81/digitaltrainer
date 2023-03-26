UNIT boardTestUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Menus, Dialogs, challenges, visualGates,
  testFrameUI, Classes;

TYPE

  { TBoardTestForm }

  TBoardTestForm = class(TForm)
    MainMenu1: TMainMenu;
    miExportToClipboard: TMenuItem;
    miExportToCsv: TMenuItem;
    miExport: TMenuItem;
    SaveDialog1: TSaveDialog;
    TestCreationFrame1: TTestCreationFrame;
    PROCEDURE miExportToClipboardClick(Sender: TObject);
    PROCEDURE miExportToCsvClick(Sender: TObject);
  private
    tester: T_testCreator;
  public
    PROCEDURE showForBoard(CONST board:P_visualBoard);
  end;

FUNCTION BoardTestForm: TBoardTestForm;
IMPLEMENTATION
VAR myBoardTestForm: TBoardTestForm=nil;
FUNCTION BoardTestForm: TBoardTestForm;
  begin
    if myBoardTestForm=nil then myBoardTestForm:=TBoardTestForm.create(nil);
    result:=myBoardTestForm;
  end;

{$R *.lfm}

{ TBoardTestForm }

PROCEDURE TBoardTestForm.miExportToCsvClick(Sender: TObject);
  begin
    if SaveDialog1.execute then TestCreationFrame1.exportTable(SaveDialog1.fileName);
  end;

PROCEDURE TBoardTestForm.miExportToClipboardClick(Sender: TObject);
  begin
    TestCreationFrame1.CopyToClipboard;
  end;

PROCEDURE TBoardTestForm.showForBoard(CONST board: P_visualBoard);
  begin
    tester.createForAnalysis(board);
    TestCreationFrame1.setTestGenerator(@tester,maxLongint);
    ShowModal;
    TestCreationFrame1.detachTestGenerator;
    tester.destroy;
  end;

{ TBoardTestForm }

FINALIZATION
  if myBoardTestForm<>nil then FreeAndNil(myBoardTestForm);

end.

