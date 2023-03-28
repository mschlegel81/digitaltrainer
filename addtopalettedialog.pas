UNIT addToPaletteDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  paletteHandling,visualGates,visuals, types;

TYPE

  { TAddToPaletteForm }

  TAddToPaletteForm = class(TForm)
    paletteComboBox: TComboBox;
    CaptionEdit: TEdit;
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
    PROCEDURE CaptionEditEditingDone(Sender: TObject);
    PROCEDURE DescriptionMemoEditingDone(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE PaletteComboboxDrawItem(control: TWinControl; index: integer;
      ARect: TRect; state: TOwnerDrawState);
    PROCEDURE propCancelShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShape1MouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE setSubpalette(CONST idx:longint);
  private
    currentBoard:P_visualBoard;
    currentPalette:P_workspacePalette;
    subPaletteIndex:longint;

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

PROCEDURE TAddToPaletteForm.CaptionEditEditingDone(Sender: TObject);
  begin
    currentBoard^.setCaption(StringReplace(CaptionEdit.text,'\n',LineEnding,[rfReplaceAll]));
  end;

PROCEDURE TAddToPaletteForm.DescriptionMemoEditingDone(Sender: TObject);
  begin
    currentBoard^.setDescription(DescriptionMemo.text);
  end;

PROCEDURE TAddToPaletteForm.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

PROCEDURE TAddToPaletteForm.PaletteComboboxDrawItem(control: TWinControl; index: integer; ARect: TRect; state: TOwnerDrawState);
  begin
    if not(control is TComboBox) then exit;
    paletteComboBox.Canvas.FillRect(ARect);                                                 //first paint normal background
    paletteComboBox.Canvas.TextRect(ARect, 5, ARect.top, paletteComboBox.items[index]);  //paint item text
  end;

PROCEDURE TAddToPaletteForm.propCancelShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

PROCEDURE TAddToPaletteForm.propOkShape1MouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentPalette^.updateEntry(currentBoard,paletteComboBox.ItemIndex,paletteComboBox.text);
  end;

PROCEDURE TAddToPaletteForm.propOkShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentPalette^.addBoard(currentBoard,paletteComboBox.ItemIndex,paletteComboBox.text);
  end;

PROCEDURE TAddToPaletteForm.setSubpalette(CONST idx: longint);
  begin
    subPaletteIndex:=idx;
  end;

FUNCTION TAddToPaletteForm.showFor(CONST palette: P_workspacePalette; CONST board: P_visualBoard): boolean;
  VAR s:string;
  begin
    currentBoard  :=board;
    currentPalette:=palette;

    CaptionEdit    .text:=StringReplace(board^.getCaption,LineEnding,'\n',[rfReplaceAll]);
    DescriptionMemo.text:=board^.getDescription;
    paletteComboBox.items.clear;

    setEnableButton(propOkShape1,propOkLabel1,board^.getIndexInPalette>=0);

    for s in palette^.subPaletteNames do paletteComboBox.items.add(s);
    paletteComboBox.ItemIndex:=subPaletteIndex;

    if ShowModal=mrOk
    then result:=true
    else result:=false;
  end;

FINALIZATION
  if myAddToPaletteForm<>nil then FreeAndNil(myAddToPaletteForm);
end.

