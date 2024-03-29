UNIT paletteHandingUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Menus, paletteHandling,workspaces,visualGates;

TYPE
  { TPaletteHandlingDialog }

  TPaletteHandlingDialog = class(TForm)
    EditBoardLabel: TLabel;
    EditBoardShape: TShape;
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
    DetailsMemo: TMemo;
    MenuItem1: TMenuItem;
    miCleanupManually: TMenuItem;
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
    PROCEDURE EditBoardShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE entriesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
    PROCEDURE entriesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE entriesGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
    PROCEDURE entriesGridSelection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE DeleteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE entriesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
    PROCEDURE entriesGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE miCleanupManuallyClick(Sender: TObject);
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
    backupCreated:boolean;
    PROCEDURE createBackupOnce(CONST reason: T_workspaceHistorizationTriggerEnum);
    PROCEDURE fillTable(CONST initial:boolean=false);
    PROCEDURE updateButtons;
  public
    PROCEDURE showFor(CONST palette_:P_workspacePalette);

  end;

VAR uiAdapter:P_uiAdapter;
FUNCTION PaletteHandlingDialog: TPaletteHandlingDialog;
IMPLEMENTATION
USES visuals,logicalGates,paletteImportUi,duplicateRemovalUi;
VAR
  myPaletteForm: TPaletteHandlingDialog=nil;

{$R *.lfm}

FUNCTION PaletteHandlingDialog: TPaletteHandlingDialog;
  begin
    if myPaletteForm=nil then myPaletteForm:=TPaletteHandlingDialog.create(nil);
    result:=myPaletteForm;
  end;

PROCEDURE TPaletteHandlingDialog.entriesGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    lastClicked:=aRow-1;
    if (lastClicked>=0) and (lastClicked<length(sorting.index))
    then DetailsMemo.text:=palette^.describeEntry(sorting.index[lastClicked]);
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.entriesGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
  begin
    if (editor is TPickListCellEditor) then begin
      TPickListCellEditor(editor).color:=colorScheme.editorBackgroundColor;
      TPickListCellEditor(editor).Font.color:=colorScheme.ENABLED_TEXT_COLOR;
      TPickListCellEditor(editor).style:=csOwnerDrawEditableFixed;
      TPickListCellEditor(editor).AutoComplete:=true;
      TPickListCellEditor(editor).AutoCompleteText:=[cbactEnabled,cbactEndOfLineComplete,cbactSearchAscending];
    end;
  end;

PROCEDURE TPaletteHandlingDialog.entriesGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin
    if not(IsColumn) then exit;
    if sorting.byColumn=index then sorting.ascending:=not(sorting.ascending)
    else begin
      sorting.byColumn:=index;
      sorting.ascending:=true;
    end;
    fillTable(false);
  end;

PROCEDURE TPaletteHandlingDialog.entriesGridGetCheckboxState(Sender: TObject; aCol, aRow: integer; VAR value: TCheckboxState);
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

PROCEDURE TPaletteHandlingDialog.EditBoardShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
  begin
    i:=lastClicked;
    if (i<0) or (i>length(sorting.index)) then exit else i:=sorting.index[i];
    if palette^.paletteEntries[i].entryType=gt_compound
    then begin
      workspace.editPaletteEntry(palette^.paletteEntries[i].prototype,uiAdapter);
      ModalResult:=mrYes;
    end;
  end;

PROCEDURE TPaletteHandlingDialog.DeleteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR i:longint;
      selection:TGridRect;
  begin
    buttonClicked(DeleteShape);
    i:=lastClicked;
    if (i<0) or (i>length(sorting.index)) then exit;

    createBackupOnce(wht_beforeDeletingEntry);
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

PROCEDURE TPaletteHandlingDialog.entriesGridSetCheckboxState(Sender: TObject; aCol, aRow: integer; CONST value: TCheckboxState);
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

PROCEDURE TPaletteHandlingDialog.entriesGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
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

PROCEDURE TPaletteHandlingDialog.ExportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ExportShape);
    if SaveDialog1.execute then palette^.exportSelected(SaveDialog1.fileName);
  end;

PROCEDURE TPaletteHandlingDialog.FormCreate(Sender: TObject);
  begin
    palette:=nil;
    entriesGrid.rowCount:=1;
    SubPaletteStringGrid.editor     .color:=SubPaletteStringGrid     .color;
    SubPaletteStringGrid.editor.Font.color:=SubPaletteStringGrid.Font.color;
    entriesGrid.editor     .color:=entriesGrid     .color;
    entriesGrid.editor.Font.color:=entriesGrid.Font.color;

    addButton(MoveTaskUpShape,MoveTaskUpLabel);
    addButton(MoveTaskDownShape,MoveTaskDownLabel);
    addButton(MarkAllShape,MarkAllLabel);
    addButton(MarkNoneShape,MarkNoneLabel);
    addButton(ExportShape,ExportLabel);
    addButton(ImportShape,ImportLabel);
    addButton(DeleteShape,DeleteLabel);
  end;

PROCEDURE TPaletteHandlingDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.ImportShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ImportShape);
    createBackupOnce(wht_beforePaletteImport);
    if OpenDialog1.execute then begin
      if PaletteImportForm.ShowModal=mrOk
      then palette^.importPalette(OpenDialog1.fileName,PaletteImportForm.importOptions)
      else exit;
    end else exit;
    fillTable(true);
  end;

PROCEDURE TPaletteHandlingDialog.MarkAllShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(MarkAllShape);
    palette^.markAllEntriesForExport(true);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.MarkNoneShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(MarkNoneShape);
    palette^.markAllEntriesForExport(false);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.miCleanupManuallyClick(Sender: TObject);
  begin

    if (lastClicked>=0) and (lastClicked<length(sorting.index))
    then begin
      DuplicateRemovalDialog.showFor(sorting.index[lastClicked]);
      fillTable(true);
    end;
  end;

PROCEDURE TPaletteHandlingDialog.miRemoveDuplicatesBehaviorClick(Sender: TObject);
  begin
    createBackupOnce(wht_beforeDuplicateRemoval);
    palette^.removeDuplicates(true);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.miRemoveDuplicatesExactClick(Sender: TObject);
  begin
    createBackupOnce(wht_beforeDuplicateRemoval);
    palette^.removeDuplicates(false);
    fillTable;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.MoveTaskDownShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selection:TGridRect;
  begin
    buttonClicked(MoveTaskDownShape);
    palette^.swapPaletteName(lastClickedSubPalette,false);
    inc(lastClickedSubPalette);
    fillTable;
    selection.Bottom:=lastClickedSubPalette+1;
    selection.top:=lastClickedSubPalette+1;
    selection.Left:=0;
    selection.Right:=0;
    SubPaletteStringGrid.selection:=selection;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.MoveTaskUpShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selection:TGridRect;
  begin
    buttonClicked(MoveTaskUpShape);
    palette^.swapPaletteName(lastClickedSubPalette,true);
    dec(lastClickedSubPalette);
    fillTable;
    selection.Bottom:=lastClickedSubPalette+1;
    selection.top:=lastClickedSubPalette+1;
    selection.Left:=0;
    selection.Right:=0;
    SubPaletteStringGrid.selection:=selection;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.SubPaletteStringGridSelection(Sender: TObject; aCol, aRow: integer);
  begin
    lastClickedSubPalette:=aRow-1;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.SubPaletteStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  begin
    lastClickedSubPalette:=aRow-1;
    if (lastClickedSubPalette<0) or (lastClickedSubPalette>=length(palette^.paletteNames))
    then newValue:=oldValue
    else palette^.paletteNames[lastClickedSubPalette]:=newValue;
    fillTable;
  end;

PROCEDURE TPaletteHandlingDialog.createBackupOnce(CONST reason: T_workspaceHistorizationTriggerEnum);
  begin
    if backupCreated then exit;
    addBackup(@workspace,reason);
    backupCreated:=true;
  end;

PROCEDURE TPaletteHandlingDialog.fillTable(CONST initial: boolean);
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
        entriesGrid.Cells[0,i+1]:=StringReplace(titleOf      (palette^.paletteEntries[sorting.index[i]]),LineEnding,'\n',[rfReplaceAll]);
        entriesGrid.Cells[1,i+1]:=StringReplace(descriptionOf(palette^.paletteEntries[sorting.index[i]]),LineEnding,'\n',[rfReplaceAll]);
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

    entriesGrid.Columns[3].visible:=not(workspace.simplisticUi);
    ImportShape      .visible:=not(workspace.simplisticUi);
    ImportLabel      .visible:=not(workspace.simplisticUi);
    ExportShape      .visible:=not(workspace.simplisticUi);
    ExportLabel      .visible:=not(workspace.simplisticUi);
    DeleteShape      .visible:=not(workspace.simplisticUi);
    DeleteLabel      .visible:=not(workspace.simplisticUi);
    MarkAllShape     .visible:=not(workspace.simplisticUi);
    MarkAllLabel     .visible:=not(workspace.simplisticUi);
    MarkNoneShape    .visible:=not(workspace.simplisticUi);
    MarkNoneLabel    .visible:=not(workspace.simplisticUi);
    MoveTaskDownShape.visible:=not(workspace.simplisticUi);
    MoveTaskDownLabel.visible:=not(workspace.simplisticUi);
    MoveTaskUpShape  .visible:=not(workspace.simplisticUi);
    MoveTaskUpLabel  .visible:=not(workspace.simplisticUi);

    if not(initial) then exit;
    entriesGrid.AutoSizeColumn(0);
    entriesGrid.AutoSizeColumn(2);
    entriesGrid.AutoSizeColumn(3);

    lastClicked:=-1;
    lastClickedSubPalette:=-1;
    updateButtons;
  end;

PROCEDURE TPaletteHandlingDialog.updateButtons;
  VAR anyMarked:boolean=false;
      i:longint;
  begin
    for i:=0 to length(palette^.paletteEntries)-1 do with palette^.paletteEntries[i] do anyMarked:=anyMarked or (entryType=gt_compound) and markedForExport;
    setEnableButton(DeleteShape,DeleteLabel,
      (lastClicked>=0) and
      (palette^.paletteEntries[sorting.index[lastClicked]].entryType=gt_compound) and
      palette^.allowDeletion(sorting.index[lastClicked]));
    setEnableButton(EditBoardShape,EditBoardLabel,
      (lastClicked>=0) and
      (palette^.paletteEntries[sorting.index[lastClicked]].entryType=gt_compound));
    setEnableButton(ExportShape,ExportLabel,anyMarked);

    setEnableButton(MoveTaskUpShape,MoveTaskUpLabel,lastClickedSubPalette>0);
    setEnableButton(MoveTaskDownShape,MoveTaskDownLabel,(lastClickedSubPalette>=0) and (lastClickedSubPalette<length(palette^.paletteNames)-1));
  end;

PROCEDURE TPaletteHandlingDialog.showFor(CONST palette_: P_workspacePalette);
  begin
    lastClicked:=-1;
    lastClickedSubPalette:=-1;
    backupCreated:=false;
    palette:=palette_;
    sorting.byColumn:=255;
    setLength(sorting.index,0);
    fillTable(true);
    updateButtons;
    ShowModal;
  end;

FINALIZATION
  if myPaletteForm<>nil then FreeAndNil(myPaletteForm);

end.

