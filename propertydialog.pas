UNIT propertyDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ValEdit, Buttons,
  StdCtrls,logicGates,gateProperties, Grids;

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
    PROCEDURE showForGate(CONST gate:P_abstractGate);
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

PROCEDURE TgatePropertyDialog.showForGate(CONST gate: P_abstractGate);
  VAR i:longint;
  begin
    propertyValues.create(gate);
    ValueListEditor.clear;
    ValueListEditor.rowCount:=propertyValues.count;
    for i:=0 to propertyValues.count-1 do begin
      ValueListEditor.Cells[0,i+1]:=propertyValues.key(i);
      ValueListEditor.Cells[1,i+1]:=propertyValues.value(i);
    end;
    if ShowModal=mrOk then propertyValues.applyValues;
    propertyValues.destroy;
  end;

end.

