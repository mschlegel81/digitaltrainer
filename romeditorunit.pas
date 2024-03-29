UNIT romEditorUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, Menus,logicalGates;

TYPE

  { TRomEditorForm }
  TRomEditorForm = class(TForm)
    MainMenu1: TMainMenu;
    miExportToCsv: TMenuItem;
    miExportToClipboard: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    OpenDialog1: TOpenDialog;
    propRemDataLabel: TLabel;
    propRemDataShape: TShape;
    propOkLabel: TLabel;
    propAddDataLabel: TLabel;
    propOkShape: TShape;
    propAddDataShape: TShape;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE miExportClick(Sender: TObject);
    PROCEDURE miExportToClipboardClick(Sender: TObject);
    PROCEDURE miImportClick(Sender: TObject);
    PROCEDURE propAddDataShapeMouseDown(Sender: TObject; button: TMouseButton;  Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propRemDataShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StringGrid1ValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    data:T_wireValueArray;
    PROCEDURE updateDataRow(CONST adress:longint; CONST wireValue:T_wireValue);
    PROCEDURE fillTable;
  public
    FUNCTION showFor(VAR dataIO: T_romContents): boolean;

  end;

FUNCTION RomEditorForm: TRomEditorForm;
IMPLEMENTATION
USES visuals;
VAR
  myRomEditorForm: TRomEditorForm=nil;

FUNCTION RomEditorForm: TRomEditorForm;
  begin
    if myRomEditorForm=nil then myRomEditorForm:=TRomEditorForm.create(nil);
    result:=myRomEditorForm;
  end;

{$R *.lfm}

{ TRomEditorForm }

PROCEDURE TRomEditorForm.propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TRomEditorForm.propRemDataShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if length(data)>0 then begin
      setLength(data,length(data)-1);
      fillTable;
    end;
  end;

PROCEDURE TRomEditorForm.FormCreate(Sender: TObject);
  begin
    initialize(data);
    setLength(data,0);
  end;

PROCEDURE TRomEditorForm.miExportClick(Sender: TObject);
  begin
    if SaveDialog1.execute
    then StringGrid1.SaveToCSVFile(SaveDialog1.fileName,';');
  end;

PROCEDURE TRomEditorForm.miExportToClipboardClick(Sender: TObject);
  begin
    StringGrid1.CopyToClipboard();
  end;

PROCEDURE TRomEditorForm.miImportClick(Sender: TObject);
  VAR col,row:longint;
      restored: TGridColumn;
      rowOk:boolean=false;
      fixedValue: string;
  begin
    if OpenDialog1.execute then begin
      StringGrid1.LoadFromCSVFile(OpenDialog1.fileName,';',false,1);
      while StringGrid1.Columns.count>3 do StringGrid1.Columns.delete(3);
      while StringGrid1.Columns.count<3 do begin
        restored:=StringGrid1.Columns.add;
        restored.Alignment:=taRightJustify;
        if StringGrid1.Columns.count=3 then restored.title.caption:='2cmp' else
        if StringGrid1.Columns.count=2 then restored.title.caption:='dec' else
                                            restored.title.caption:='bin';
      end;
      setLength(data,StringGrid1.rowCount-1);
      for row:=1 to StringGrid1.rowCount-1 do begin
        rowOk:=false;
        for col:=1 to 3 do if not (rowOk) then begin
          fixedValue:=StringGrid1.Cells[col,row];
          StringGrid1ValidateEntry(Sender,col,row,StringGrid1.Cells[col,row],fixedValue);
          if StringGrid1.Cells[col,row]=fixedValue
          then rowOk:=true
          else StringGrid1.Cells[col,row]:=fixedValue;
        end;
      end;
      fillTable;
    end;
  end;

PROCEDURE TRomEditorForm.propAddDataShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if length(data)<65536 then begin
      setLength(data,length(data)+1);
      fillTable;
    end;
  end;

PROCEDURE TRomEditorForm.StringGrid1ValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR wire:T_wireValue;
  begin
    if (aRow<=0) or (aCol<=0) then exit;
    aRow-=1;
    case byte(aCol) of
      1: wire:=parseWireBin(newValue,16);
      2: wire:=parseWireDecimal(newValue,16);
      3: wire:=parseWire2Complement(newValue,16);
    else wire:=data[aRow];
    end;
    data[aRow]:=wire;
    if Sender is TStringGrid then updateDataRow(aRow,wire);
  end;

PROCEDURE TRomEditorForm.updateDataRow(CONST adress: longint; CONST wireValue: T_wireValue);
  begin
    StringGrid1.Cells[0,adress+1]:=intToStr(adress);
    StringGrid1.Cells[1,adress+1]:=getBinaryString     (wireValue);
    StringGrid1.Cells[2,adress+1]:=getDecimalString    (wireValue);
    StringGrid1.Cells[3,adress+1]:=get2ComplementString(wireValue);
  end;

PROCEDURE TRomEditorForm.fillTable;
  VAR i:longint;
  begin
    StringGrid1.rowCount:=1+length(data);
    for i:=0 to length(data)-1 do updateDataRow(i,data[i]);
    StringGrid1.AutoSizeColumns;
  end;

FUNCTION TRomEditorForm.showFor(VAR dataIO: T_romContents):boolean;
  PROCEDURE clone(CONST source:T_romContents; VAR dest:T_wireValueArray);
    VAR i:longint;
    begin
      setLength(dest,length(source));
      for i:=0 to length(dest)-1 do dest[i]:=wordToWire16(source[i]);
    end;

  PROCEDURE clone(CONST source:T_wireValueArray; VAR dest:T_romContents);
    VAR i:longint;
    begin
      setLength(dest,length(source));
      for i:=0 to length(dest)-1 do dest[i]:=wire16ToWord(source[i]);
    end;

  begin
    StringGrid1.editor.color:=StringGrid1.color;
    StringGrid1.editor.Font.color:=StringGrid1.Font.color;
    clone(dataIO,data);
    fillTable;
    applyColorScheme(self);
    result:=ShowModal=mrOk;
    if result then clone(data,dataIO);
  end;

FINALIZATION
  if myRomEditorForm<>nil then FreeAndNil(myRomEditorForm);

end.

