UNIT paletteHandingUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls,paletteHandling;

TYPE

  { TPaletteForm }

  TPaletteForm = class(TForm)
    entriesGrid: TStringGrid;
    MarkNoneShape: TShape;
    MarkAllLabel: TLabel;
    MarkAllShape: TShape;
    ExportLabel: TLabel;
    ImportLabel: TLabel;
    ExportShape: TShape;
    DeleteLabel: TLabel;
    ImportShape: TShape;
    DeleteShape: TShape;
    MarkNoneLabel: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StartTaskLabel: TLabel;
    PROCEDURE entriesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE DeleteShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE ExportShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE ImportShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    palette:P_workspacePalette;
    lastClicked:longint;
    PROCEDURE fillTable(CONST initial:boolean=false);
    PROCEDURE fillRow(CONST i:longint);
    PROCEDURE updateButtons;
  public
    PROCEDURE showFor(CONST palette_:P_workspacePalette);

  end;

FUNCTION PaletteForm: TPaletteForm;
IMPLEMENTATION
USES visuals,logicalGates;
VAR
  myPaletteForm: TPaletteForm=nil;

{$R *.lfm}

FUNCTION PaletteForm: TPaletteForm;
  begin
    if myPaletteForm=nil then myPaletteForm:=TPaletteForm.create(nil);
    result:=myPaletteForm;
  end;

PROCEDURE TPaletteForm.entriesGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    lastClicked:=aRow-1;
    palette^.markEntryForExportToggle(lastClicked);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteForm.DeleteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    palette^.deleteEntry(lastClicked);
    fillTable(true);
  end;

PROCEDURE TPaletteForm.ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if SaveDialog1.execute then palette^.exportSelected(SaveDialog1.fileName);
  end;

PROCEDURE TPaletteForm.FormCreate(Sender: TObject);
  begin
    palette:=nil;
    entriesGrid.rowCount:=1;
  end;

PROCEDURE TPaletteForm.ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if OpenDialog1.execute then palette^.importPalette(OpenDialog1.fileName);
    fillTable(true);
  end;

PROCEDURE TPaletteForm.MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    palette^.markAllEntriesForExport(true);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteForm.MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    palette^.markAllEntriesForExport(false);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteForm.fillTable(CONST initial: boolean);
  VAR i:longint;
  begin
    entriesGrid.rowCount:=1+length(palette^.paletteEntries);
    //TODO: It would be nicer if the basic gates were not shown; but this would require a more complex handling of the index...
    for i:=0 to length(palette^.paletteEntries)-1 do fillRow(i);
    if not(initial) then exit;
    entriesGrid.AutoSizeColumn(0);
    entriesGrid.AutoSizeColumn(2);
    entriesGrid.AutoSizeColumn(3);
    lastClicked:=-1;
    updateButtons;
  end;

PROCEDURE TPaletteForm.fillRow(CONST i: longint);
  VAR
    tmp: string;
  begin
    with palette^.paletteEntries[i] do begin
      if entryType=gt_compound
      then tmp:=prototype^.getCaption
      else tmp:=C_gateTypeName[entryType];
      EntriesGrid.Cells[0,i+1]:=StringReplace(tmp,LineEnding,'\n',[rfReplaceAll]);

      if entryType=gt_compound
      then tmp:=prototype^.getDescription
      else tmp:=C_gateDefaultDescription[entryType];
      EntriesGrid.Cells[1,i+1]:=StringReplace(tmp,LineEnding,'\n',[rfReplaceAll]);

      EntriesGrid.Cells[2,i+1]:=palette^.subPaletteNames[subPaletteIndex];
      EntriesGrid.Cells[3,i+1]:=BoolToStr((entryType=gt_compound) and markedForExport,'✓','');
    end;
  end;

PROCEDURE TPaletteForm.updateButtons;
  VAR anyMarked:boolean=false;
      i:longint;
  begin
    for i:=0 to length(palette^.paletteEntries)-1 do with palette^.paletteEntries[i] do anyMarked:=anyMarked or (entryType=gt_compound) and markedForExport;
    setEnableButton(DeleteShape,DeleteLabel,
      (lastClicked>=0) and
      (palette^.paletteEntries[lastClicked].entryType=gt_compound) and
      palette^.allowDeletion(lastClicked));
    setEnableButton(ExportShape,ExportLabel,anyMarked);
  end;

PROCEDURE TPaletteForm.showFor(CONST palette_: P_workspacePalette);
  begin
    palette:=palette_;
    fillTable(true);
    ShowModal;
  end;

FINALIZATION
  if myPaletteForm<>nil then FreeAndNil(myPaletteForm);

end.

