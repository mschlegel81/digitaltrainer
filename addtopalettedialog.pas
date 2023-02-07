unit addToPaletteDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  paletteHandling,visualGates,visuals;

type

  { TAddToPaletteForm }

  TAddToPaletteForm = class(TForm)
    PaletteCombobox: TComboBox;
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
    procedure CaptionEditEditingDone(Sender: TObject);
    procedure DescriptionMemoEditingDone(Sender: TObject);
    procedure propCancelShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure propOkShape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure propOkShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    currentBoard:P_visualBoard;
    currentPalette:P_workspacePalette;

  public
    FUNCTION showFor(CONST palette:P_workspacePalette; CONST board:P_visualBoard):boolean;

  end;

FUNCTION AddToPaletteForm: TAddToPaletteForm;
implementation
var
  myAddToPaletteForm: TAddToPaletteForm=nil;

function AddToPaletteForm: TAddToPaletteForm;
  begin
    if myAddToPaletteForm=nil then
    myAddToPaletteForm:=TAddToPaletteForm.Create(nil);
    result:=myAddToPaletteForm;
  end;

{$R *.lfm}

{ TAddToPaletteForm }

procedure TAddToPaletteForm.CaptionEditEditingDone(Sender: TObject);
  begin
    currentBoard^.setCaption(CaptionEdit.Text);
  end;

procedure TAddToPaletteForm.DescriptionMemoEditingDone(Sender: TObject);
  begin
    currentBoard^.setDescription(DescriptionMemo.Text);
  end;

procedure TAddToPaletteForm.propCancelShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    ModalResult:=mrCancel;
  end;

procedure TAddToPaletteForm.propOkShape1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    ModalResult:=mrOK;
    currentPalette^.updateEntry(currentBoard,PaletteCombobox.ItemIndex,PaletteCombobox.Text);
  end;

procedure TAddToPaletteForm.propOkShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    ModalResult:=mrOK;
    currentPalette^.addBoard(currentBoard,PaletteCombobox.ItemIndex,PaletteCombobox.Text);
  end;

function TAddToPaletteForm.showFor(const palette: P_workspacePalette; const board: P_visualBoard): boolean;
  VAR s:string;
  begin
    currentBoard  :=board;
    currentPalette:=palette;

    CaptionEdit    .Text:=board^.getCaption;
    DescriptionMemo.Text:=board^.getDescription;
    PaletteCombobox.items.Clear;

    setEnableButton(propOkShape1,propOkLabel1,board^.getIndexInPalette>=0);

    for s in palette^.subPaletteNames do PaletteCombobox.Items.Add(s);
    if ShowModal=mrOK
    then result:=true
    else result:=false;
  end;

finalization
  if myAddToPaletteForm<>nil then FreeAndNil(myAddToPaletteForm);
end.

