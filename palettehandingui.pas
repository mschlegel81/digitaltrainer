UNIT paletteHandingUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Menus,paletteHandling;

TYPE
//TODO: Info box for currently selected entry with interfaces and dependencies.
  { TPaletteForm }

  TPaletteForm = class(TForm)
    entriesGrid: TStringGrid;
    MainMenu1: TMainMenu;
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
    MenuItem1: TMenuItem;
    miRemoveDuplicatesExact: TMenuItem;
    miRemoveDuplicatesBehavior: TMenuItem;
    MoveTaskDownLabel: TLabel;
    MoveTaskDownShape: TShape;
    MoveTaskUpLabel: TLabel;
    MoveTaskUpShape: TShape;
    OpenDialog1: TOpenDialog;
    Splitter1: TSplitter;
    SubPaletteStringGrid: TStringGrid;
    SubPalettePanel: TPanel;
    SaveDialog1: TSaveDialog;
    StartTaskLabel: TLabel;
    PROCEDURE entriesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer;
      VAR value: TCheckboxState);
    PROCEDURE entriesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE entriesGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
    PROCEDURE entriesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE DeleteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE entriesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer;
      CONST value: TCheckboxState);
    PROCEDURE entriesGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE miRemoveDuplicatesBehaviorClick(Sender: TObject);
    PROCEDURE miRemoveDuplicatesExactClick(Sender: TObject);
    PROCEDURE MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE SubPaletteStringGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE SubPaletteStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    palette:P_workspacePalette;
    lastClicked:longint;
    lastClickedSubPalette:longint;
    sorting:record
      byColumn:byte;
      ascending:boolean;
      index:array of longint;
    end;
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
    updateButtons;
  end;

PROCEDURE TPaletteForm.entriesGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
  begin
    if (editor is TPickListCellEditor) then begin
      TPickListCellEditor(editor).color:=$00703838;
      TPickListCellEditor(editor).Font.color:=$00FFFFFF;
      TPickListCellEditor(editor).style:=csOwnerDrawEditableFixed;
      TPickListCellEditor(editor).AutoComplete:=true;
      TPickListCellEditor(editor).AutoCompleteText:=[cbactEnabled,cbactEndOfLineComplete,cbactSearchAscending];
    end;
  end;

PROCEDURE TPaletteForm.entriesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin
    if not(IsColumn) then exit;
    if sorting.byColumn=index then sorting.ascending:=not(sorting.ascending)
    else begin
      sorting.byColumn:=index;
      sorting.ascending:=true;
    end;
    fillTable(false);
  end;

PROCEDURE TPaletteForm.entriesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
  begin
    if (aCol<>3) then exit;
    aRow-=1;
    if (aRow<0) or (aRow>=length(sorting.index)) then exit;
    if palette^.paletteEntries[sorting.index[aRow]].entryType=gt_compound
    then begin
      if palette^.paletteEntries[sorting.index[aRow]].markedForExport
      then value:=cbChecked
      else value:=cbUnchecked;
    end else value:=cbGrayed;
  end;

PROCEDURE TPaletteForm.DeleteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
      selection:TGridRect;
  begin
    i:=lastClicked;
    if (i<0) or (i>length(sorting.index)) then exit;

    palette^.deleteEntry(sorting.index[i]);
    fillTable(true);
    lastClicked-=1;
    selection.Bottom:=lastClicked+1;
    selection.top:=lastClicked+1;
    selection.Left:=0;
    selection.Right:=0;
    entriesGrid.selection:=selection;
    updateButtons;
  end;

PROCEDURE TPaletteForm.entriesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
  begin
    if (aCol<>3) then exit;
    aRow-=1;
    if (aRow<0) or (aRow>=length(sorting.index)) then exit;
    if palette^.paletteEntries[sorting.index[aRow]].entryType=gt_compound
    then begin
      palette^.markEntryForExport(sorting.index[aRow],value=cbChecked);
      fillTable;
      updateButtons;
    end;
  end;

PROCEDURE TPaletteForm.entriesGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR editedIdx: longint;
  begin
    dec(aRow);
    if (aRow<0) or (aRow>=length(sorting.index)) then exit;
    editedIdx:=sorting.index[aRow];
    if aCol=0 then begin
      //Edit caption
     if not(palette^.setPaletteEntryCaption(editedIdx,StringReplace(newValue,'\n',LineEnding,[rfReplaceAll]))) then newValue:=oldValue
    end else if aCol=1 then begin
      //Edit description
     if not(palette^.setPaletteEntryDescription(editedIdx,StringReplace(newValue,'\n',LineEnding,[rfReplaceAll]))) then newValue:=oldValue
    end else if aCol=2 then begin
      //Edit palette
      palette^.setPaletteEntrySubPalette(editedIdx,newValue);
    end;
  end;

PROCEDURE TPaletteForm.ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if SaveDialog1.execute then palette^.exportSelected(SaveDialog1.fileName);
  end;

PROCEDURE TPaletteForm.FormCreate(Sender: TObject);
  begin
    palette:=nil;
    entriesGrid.rowCount:=1;

    SubPaletteStringGrid.editor     .color:=SubPaletteStringGrid     .color;
    SubPaletteStringGrid.editor.Font.color:=SubPaletteStringGrid.Font.color;
    entriesGrid.editor     .color:=entriesGrid     .color;
    entriesGrid.editor.Font.color:=entriesGrid.Font.color;
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

PROCEDURE TPaletteForm.miRemoveDuplicatesBehaviorClick(Sender: TObject);
  begin
    palette^.removeDuplicates(true);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteForm.miRemoveDuplicatesExactClick(Sender: TObject);
  begin
    palette^.removeDuplicates(false);
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
  FUNCTION titleOf(CONST entry:T_workspacePaletteEntry):string;
    begin
      if entry.entryType=gt_compound
      then result:=entry.prototype^.getCaption
      else result:=C_gateTypeName[entry.entryType];
      result:=StringReplace(result,LineEnding,'\n',[rfReplaceAll]);
    end;

  FUNCTION descriptionOf(CONST entry:T_workspacePaletteEntry):string;
    begin
      if entry.entryType=gt_compound
      then result:=entry.prototype^.getDescription
      else result:=C_gateDefaultDescription[entry.entryType];
      result:=StringReplace(result,LineEnding,'\n',[rfReplaceAll]);
    end;

  PROCEDURE updateSorting;
    FUNCTION comesBefore(CONST i,j:longint):boolean;
      begin
        case sorting.byColumn of
          0: if sorting.ascending
             then result:=uppercase(titleOf(palette^.paletteEntries[i]))<uppercase(titleOf(palette^.paletteEntries[j]))
             else result:=uppercase(titleOf(palette^.paletteEntries[i]))>uppercase(titleOf(palette^.paletteEntries[j]));
          1: if sorting.ascending
             then result:=uppercase(descriptionOf(palette^.paletteEntries[i]))<uppercase(descriptionOf(palette^.paletteEntries[j]))
             else result:=uppercase(descriptionOf(palette^.paletteEntries[i]))>uppercase(descriptionOf(palette^.paletteEntries[j]));
          2: if sorting.ascending
             then result:=palette^.paletteEntries[i].subPaletteIndex<palette^.paletteEntries[j].subPaletteIndex
             else result:=palette^.paletteEntries[i].subPaletteIndex>palette^.paletteEntries[j].subPaletteIndex;
          3: if sorting.ascending
             then result:=palette^.paletteEntries[i].markedForExport<palette^.paletteEntries[j].markedForExport
             else result:=palette^.paletteEntries[i].markedForExport>palette^.paletteEntries[j].markedForExport;
          else exit(false);
        end;
      end;

    VAR i,j,k:longint;
    begin
      with sorting do begin
        if length(index)<>length(palette^.paletteEntries) then begin
          setLength(index,length(palette^.paletteEntries));
          for k:=0 to length(index)-1 do index[k]:=k;
        end;

        if sorting.byColumn in [0..3] then begin
          for i:=1 to length(index)-1 do for j:=0 to i-1 do
          if comesBefore(index[i],index[j]) then begin
            k:=index[i]; index[i]:=index[j]; index[j]:=k;
          end;
        end else for k:=0 to length(index)-1 do index[k]:=k;

      end;
    end;

  PROCEDURE fillRow(CONST i: longint);
    begin
      with palette^.paletteEntries[sorting.index[i]] do begin
        entriesGrid.Cells[0,i+1]:=titleOf      (palette^.paletteEntries[sorting.index[i]]);
        entriesGrid.Cells[1,i+1]:=descriptionOf(palette^.paletteEntries[sorting.index[i]]);
        entriesGrid.Cells[2,i+1]:=palette^.subPaletteNames[subPaletteIndex];
        entriesGrid.Cells[3,i+1]:=BoolToStr(markedForExport,'x',' ');
      end;
    end;
  VAR i:longint;
  begin
    entriesGrid.rowCount:=1+length(palette^.paletteEntries);
    entriesGrid.Columns[3].ValueChecked:='x';
    entriesGrid.Columns[3].ValueUnchecked:=' ';

    updateSorting;
    for i:=0 to length(palette^.paletteEntries)-1 do fillRow(i);
    SubPaletteStringGrid.rowCount:=1+length(palette^.paletteNames);
    entriesGrid.Columns[2].PickList.clear;
    for i:=0 to length(palette^.paletteNames)-1 do begin
      SubPaletteStringGrid.Cells[0,i+1]:=palette^.paletteNames[i];
      entriesGrid.Columns[2].PickList.add(palette^.paletteNames[i]);
    end;

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
      (palette^.paletteEntries[sorting.index[lastClicked]].entryType=gt_compound) and
      palette^.allowDeletion(sorting.index[lastClicked]));
    setEnableButton(ExportShape,ExportLabel,anyMarked);
  end;

PROCEDURE TPaletteForm.showFor(CONST palette_: P_workspacePalette);
  begin
    palette:=palette_;
    sorting.byColumn:=255;
    setLength(sorting.index,0);
    fillTable(true);
    ShowModal;
  end;

FINALIZATION
  if myPaletteForm<>nil then FreeAndNil(myPaletteForm);

end.

