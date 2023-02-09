UNIT addToPaletteDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  paletteHandling,visualGates,visuals;

TYPE

  { TAddToPaletteForm }

  TAddToPaletteForm = class(TForm)
    PaletteCombobox: TComboBox;
    captionEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DescriptionMemo: TMemo;
    propCancelLabel: TLabel;
    propCancelShape: TShape;
    propOkLabel: TLabel;
    propOkLabel1: TLabel;
    propOkShape: TShape;
    propOkShape1: TShape;
    PROCEDURE captionEditEditingDone(Sender: TObject);
    PROCEDURE DescriptionMemoEditingDone(Sender: TObject);
    PROCEDURE propCancelShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShape1MouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    currentBoard:P_visualBoard;
    currentPalette:P_workspacePalette;

  public
    FUNCTION showFor(CONST palette:P_workspacePalette; CONST board:P_visualBoard):boolean;

  end;

FUNCTION AddToPaletteForm: TAddToPaletteForm;
IMPLEMENTATION
VAR
  myAddToPaletteForm: TAddToPaletteForm=nil;

FUNCTION AddToPaletteForm: TAddToPaletteForm;
  begin
    if myAddToPaletteForm=nil then
    myAddToPaletteForm:=TAddToPaletteForm.create(nil);
    result:=myAddToPaletteForm;
  end;

{$R *.lfm}

{ TAddToPaletteForm }

PROCEDURE TAddToPaletteForm.captionEditEditingDone(Sender: TObject);
  begin
    currentBoard^.setCaption(captionEdit.text);
  end;

PROCEDURE TAddToPaletteForm.DescriptionMemoEditingDone(Sender: TObject);
  begin
    currentBoard^.setDescription(DescriptionMemo.text);
  end;

PROCEDURE TAddToPaletteForm.propCancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

PROCEDURE TAddToPaletteForm.propOkShape1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentPalette^.updateEntry(currentBoard,PaletteCombobox.ItemIndex,PaletteCombobox.text);
  end;

PROCEDURE TAddToPaletteForm.propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentPalette^.addBoard(currentBoard,PaletteCombobox.ItemIndex,PaletteCombobox.text);
  end;

FUNCTION TAddToPaletteForm.showFor(CONST palette: P_workspacePalette; CONST board: P_visualBoard): boolean;
  VAR s:string;
  begin
    currentBoard  :=board;
    currentPalette:=palette;

    captionEdit    .text:=board^.getCaption;
    DescriptionMemo.text:=board^.getDescription;
    PaletteCombobox.items.clear;

    setEnableButton(propOkShape1,propOkLabel1,board^.getIndexInPalette>=0);

    for s in palette^.subPaletteNames do PaletteCombobox.items.add(s);
    if ShowModal=mrOk
    then result:=true
    else result:=false;
  end;

FINALIZATION
  if myAddToPaletteForm<>nil then FreeAndNil(myAddToPaletteForm);
end.

