UNIT addToPaletteUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  paletteHandling,visualGates,visuals, types;

TYPE

  { TAddToPaletteDialog }

  TAddToPaletteDialog = class(TForm)
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
  private
    currentBoard:P_visualBoard;
    currentPalette:P_workspacePalette;
    subPaletteIndex:longint;

  public
    FUNCTION showFor(CONST palette:P_workspacePalette; CONST board:P_visualBoard):boolean;

  end;

FUNCTION addToPaletteDialog: TAddToPaletteDialog;
IMPLEMENTATION
USES workspaces;
VAR
  myAddToPaletteForm: TAddToPaletteDialog=nil;

FUNCTION addToPaletteDialog: TAddToPaletteDialog;
  begin
    if myAddToPaletteForm=nil then
    myAddToPaletteForm:=TAddToPaletteDialog.create(nil);
    result:=myAddToPaletteForm;
  end;

{$R *.lfm}

{ TAddToPaletteDialog }

PROCEDURE TAddToPaletteDialog.CaptionEditEditingDone(Sender: TObject);
  begin
    currentBoard^.setCaption(StringReplace(CaptionEdit.text,'\n',LineEnding,[rfReplaceAll]));
  end;

PROCEDURE TAddToPaletteDialog.DescriptionMemoEditingDone(Sender: TObject);
  begin
    currentBoard^.setDescription(DescriptionMemo.text);
  end;

PROCEDURE TAddToPaletteDialog.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
    setEnableButton(propOkShape1,propOkLabel1,currentBoard^.getIndexInPalette>=0);
  end;

PROCEDURE TAddToPaletteDialog.PaletteComboboxDrawItem(control: TWinControl; index: integer; ARect: TRect; state: TOwnerDrawState);
  begin
    if not(control is TComboBox) then exit;
    paletteComboBox.Canvas.FillRect(ARect);                                                 //first paint normal background
    paletteComboBox.Canvas.TextRect(ARect, 5, ARect.top, paletteComboBox.items[index]);  //paint item text
  end;

PROCEDURE TAddToPaletteDialog.propCancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

PROCEDURE TAddToPaletteDialog.propOkShape1MouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentBoard^.underlyingPrototype:=currentPalette^.updateEntry(currentBoard,paletteComboBox.ItemIndex,paletteComboBox.text);
    currentBoard^.modified:=false;
  end;

PROCEDURE TAddToPaletteDialog.propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
    currentBoard^.underlyingPrototype:=currentPalette^.addBoard(currentBoard,paletteComboBox.ItemIndex,paletteComboBox.text);
    currentPalette^.setFilter(currentBoard^.underlyingPrototype^.getIndexInPalette);
    currentBoard^.modified:=false;
  end;

FUNCTION TAddToPaletteDialog.showFor(CONST palette: P_workspacePalette; CONST board: P_visualBoard): boolean;
  VAR s:string;
  begin
    currentBoard  :=board;
    currentPalette:=palette;

    CaptionEdit    .text:=StringReplace(board^.getCaption,LineEnding,'\n',[rfReplaceAll]);
    DescriptionMemo.text:=board^.getDescription;
    paletteComboBox.items.clear;

    setEnableButton(propOkShape1,propOkLabel1,board^.getIndexInPalette>=0);

    for s in palette^.subPaletteNames do paletteComboBox.items.add(s);
    paletteComboBox.ItemIndex:=workspace.currentPaletteAssociation;

    if ShowModal=mrOk
    then result:=true
    else result:=false;
  end;

FINALIZATION
  if myAddToPaletteForm<>nil then FreeAndNil(myAddToPaletteForm);
end.

