UNIT duplicateRemovalUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, compoundGates, challenges, workspaces,visualGates,sprites;

TYPE

  { TDuplicateRemovalDialog }

  TDuplicateRemovalDialog = class(TForm)
    CaptionLabelA: TLabel;
    CaptionLabelB: TLabel;
    deleteBLabel: TLabel;
    deleteBShape: TShape;
    generateTestCasesLabel: TLabel;
    generateTestCasesLabel1: TLabel;
    deleteALabel: TLabel;
    generateTestCasesShape: TShape;
    generateTestCasesShape1: TShape;
    CompareCaptionLabel: TLabel;
    deleteAShape: TShape;
    ImageA: TImage;
    ImageB: TImage;
    Label6: TLabel;
    Label7: TLabel;
    StepCountEdit: TEdit;
    CandidatesPanel: TPanel;
    ComparisonPanel: TPanel;
    ElementAPanel: TPanel;
    ElementBPanel: TPanel;
    CandidateStringGrid: TStringGrid;
    TestCasesStringGrid: TStringGrid;
    TestCaseCountEdit: TEdit;
    Timer1: TTimer;
    PROCEDURE CandidateStringGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StepCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    indexA ,indexB  :longint;
    testerA,testerB :P_testCreator;
    //TODO: visualA,visualB :P_visualGate; (Create visual representations, scale them [!!!] to fit into the boxes and paint them...)
    candidateIndexes:T_arrayOfLongint;
    PROCEDURE fillTable;
    PROCEDURE updateTableRow(CONST j:longint);
  public
    PROCEDURE showFor(CONST paletteEntryIndex:longint);

  end;

FUNCTION DuplicateRemovalDialog: TDuplicateRemovalDialog;

IMPLEMENTATION
USES paletteHandling;
VAR myDuplicateRemovalDialog: TDuplicateRemovalDialog=nil;
FUNCTION DuplicateRemovalDialog: TDuplicateRemovalDialog;
  begin
    if myDuplicateRemovalDialog=nil then myDuplicateRemovalDialog:=TDuplicateRemovalDialog.create(nil);
    result:=myDuplicateRemovalDialog;
  end;

{$R *.lfm}

{ TDuplicateRemovalDialog }

PROCEDURE TDuplicateRemovalDialog.TestCaseCountEditEditingDone(Sender: TObject);
begin

end;

PROCEDURE TDuplicateRemovalDialog.TestCasesStringGridHeaderClick(
  Sender: TObject; IsColumn: boolean; index: integer);
begin

end;

PROCEDURE TDuplicateRemovalDialog.TestCasesStringGridValidateEntry(
  Sender: TObject; aCol, aRow: integer; CONST oldValue: string;
  VAR newValue: string);
begin

end;

PROCEDURE TDuplicateRemovalDialog.Timer1Timer(Sender: TObject);
begin

end;

PROCEDURE TDuplicateRemovalDialog.StepCountEditEditingDone(Sender: TObject);
begin

end;

PROCEDURE TDuplicateRemovalDialog.CandidateStringGridSelection(Sender: TObject;
  aCol, aRow: integer);
begin

end;

PROCEDURE TDuplicateRemovalDialog.fillTable;
begin

end;

PROCEDURE TDuplicateRemovalDialog.updateTableRow(CONST j: longint);
begin

end;

PROCEDURE TDuplicateRemovalDialog.showFor(CONST paletteEntryIndex: longint);
  VAR prototypeA: P_visualBoard;
      i,row:longint;
  begin
    if workspace.getWorkspacePalette^.paletteEntries[paletteEntryIndex].prototype=nil
    then exit
    else prototypeA:=workspace.getWorkspacePalette^.paletteEntries[paletteEntryIndex].prototype;

    indexA:=paletteEntryIndex;
    indexB:=-1;

    new(testerA,createForAnalysis(prototypeA));
    testerB:=nil;

    candidateIndexes:=workspace.getWorkspacePalette^.findEntriesWithSameInterfaceAs(indexA);
    CandidateStringGrid.rowCount:=length(candidateIndexes)+1;
    row:=1;
    for i in candidateIndexes do begin
      CandidateStringGrid.Cells[0,row]:=
      StringReplace(titleOf(workspace.getWorkspacePalette^.paletteEntries[i]),LineEnding,'\n',[rfReplaceAll]);
      inc(row);
    end;

    ShowModal;

    dispose(testerA,destroy);
    if testerB<>nil then begin
      dispose(testerB,destroy);
      testerB:=nil;
    end;
  end;

FINALIZATION
  if myDuplicateRemovalDialog<>nil then FreeAndNil(myDuplicateRemovalDialog);

end.

