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
    MoveTaskDownLabel: TLabel;
    MoveTaskDownShape: TShape;
    MoveTaskUpLabel: TLabel;
    MoveTaskUpShape: TShape;
    OpenDialog1: TOpenDialog;
    SubPaletteStringGrid: TStringGrid;
    SubPalettePanel: TPanel;
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
    PROCEDURE MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE SubPaletteStringGridSelection(Sender: TObject; aCol, aRow: integer
      );
    PROCEDURE SubPaletteStringGridValidateEntry(Sender: TObject; aCol,
      aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    palette:P_workspacePalette;
    lastClicked:longint;
    lastClickedSubPalette:longint;
    PROCEDURE fillTable(CONST initial:boolean=false);
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
  VAR i:longint;
      selection:TGridRect;
  begin
    i:=lastClicked;
    palette^.deleteEntry(lastClicked);
    fillTable(true);
    lastClicked:=i-1;
    selection.Bottom:=lastClicked+1;
    selection.top:=lastClicked+1;
    selection.Left:=0;
    selection.Right:=0;
    entriesGrid.selection:=selection;
    updateButtons;
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

PROCEDURE TPaletteForm.MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selection:TGridRect;
  begin
    palette^.swapPaletteName(lastClickedSubPalette,false);
    inc(lastClickedSubPalette);
    fillTable;
    selection.Bottom:=lastClickedSubPalette+1;
    selection.top:=lastClickedSubPalette+1;
    selection.Left:=0;
    selection.Right:=0;
    SubPaletteStringGrid.selection:=selection;
  end;

PROCEDURE TPaletteForm.MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selection:TGridRect;
  begin
    palette^.swapPaletteName(lastClickedSubPalette,true);
    dec(lastClickedSubPalette);
    fillTable;
    selection.Bottom:=lastClickedSubPalette+1;
    selection.top:=lastClickedSubPalette+1;
    selection.Left:=0;
    selection.Right:=0;
    SubPaletteStringGrid.selection:=selection;
  end;

PROCEDURE TPaletteForm.SubPaletteStringGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    lastClickedSubPalette:=aRow-1;
  end;

PROCEDURE TPaletteForm.SubPaletteStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  begin
    lastClickedSubPalette:=aRow-1;
    if (lastClickedSubPalette<0) or (lastClickedSubPalette>=length(palette^.paletteNames))
    then newValue:=oldValue
    else palette^.paletteNames[lastClickedSubPalette]:=newValue;
    fillTable;
  end;

PROCEDURE TPaletteForm.fillTable(CONST initial: boolean);
  PROCEDURE fillRow(CONST i: longint);
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
  VAR i:longint;
  begin
    entriesGrid.rowCount:=1+length(palette^.paletteEntries);
    //TODO: It would be nicer if the basic gates were not shown; but this would require a more complex handling of the index...
    for i:=0 to length(palette^.paletteEntries)-1 do fillRow(i);

    SubPaletteStringGrid.rowCount:=1+length(palette^.paletteNames);
    for i:=0 to length(palette^.paletteNames)-1 do SubPaletteStringGrid.Cells[0,i+1]:=palette^.paletteNames[i];

    if not(initial) then exit;
    entriesGrid.AutoSizeColumn(0);
    entriesGrid.AutoSizeColumn(2);
    entriesGrid.AutoSizeColumn(3);

    lastClicked:=-1;
    lastClickedSubPalette:=-1;
    updateButtons;
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

