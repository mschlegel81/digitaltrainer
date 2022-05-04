UNIT propertyDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ValEdit, Buttons,
  StdCtrls,logicGates,gateProperties, Grids,baseGate;

TYPE

  { TgatePropertyDialog }

  TgatePropertyDialog = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    ValueListEditor: TValueListEditor;
    PROCEDURE ValueListEditorValidateEntry(Sender: TObject; aCol,aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    propertyValues:T_gatePropertyValues;
  public
    FUNCTION showForGate(CONST gate:P_abstractGate; CONST adapter:P_uiAdapter):boolean;
  end;

VAR
  gatePropertyDialog: TgatePropertyDialog;

IMPLEMENTATION

{$R *.lfm}

{ TgatePropertyDialog }

PROCEDURE TgatePropertyDialog.ValueListEditorValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  begin
    if aCol=0 then begin
      newValue:=oldValue;
    end else begin
      if not(propertyValues.acceptNewValue(aRow-1,newValue)) then newValue:=oldValue;
    end;
  end;

FUNCTION TgatePropertyDialog.showForGate(CONST gate: P_abstractGate; CONST adapter:P_uiAdapter):boolean;
  VAR i:longint;
    cp: TPoint;
  begin
    cp:=mouse.CursorPos;
    cp.x-=width  shr 1; if cp.x<0 then cp.x:=0;
    cp.y-=height shr 1; if cp.y<0 then cp.y:=0;
    Left:=cp.X;
    top :=cp.y;

    propertyValues.create(gate);
    ValueListEditor.clear;
    ValueListEditor.rowCount:=propertyValues.count;
    for i:=0 to propertyValues.count-1 do begin
      ValueListEditor.Cells[0,i+1]:=propertyValues.key(i);
      ValueListEditor.Cells[1,i+1]:=propertyValues.value(i);
    end;
    ValueListEditor.AutoSizeColumn(0);
    if ShowModal=mrOk then begin
      adapter^.saveStateToUndoList;
      propertyValues.applyValues;
      result:=true;
    end else result:=false;
    propertyValues.destroy;
  end;

end.

