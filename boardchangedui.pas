UNIT boardChangedUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

TYPE

  { TboardChangedDialog }

  TboardChangedDialog = class(TForm)
    CancelButton: TShape;
    updatePaletteLabel: TLabel;
    propIgnoreButton: TShape;
    ignoreLabel: TLabel;
    updatePaletteButton: TShape;
    boardWasChangedLabel: TLabel;
    CancelLabel: TLabel;
    PROCEDURE cancelButtonMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ignoreLabelMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE updatePaletteButtonMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private

  public
    FUNCTION showFor(CONST hasRelatedPaletteEntry:boolean):TModalResult;
  end;

FUNCTION boardChangedDialog: TboardChangedDialog;
IMPLEMENTATION
USES visuals;
VAR myBoardChangedDialog:TboardChangedDialog=nil;
FUNCTION boardChangedDialog: TboardChangedDialog;
  begin
    if myBoardChangedDialog=nil then myBoardChangedDialog:=TboardChangedDialog.create(nil);
    result:=myBoardChangedDialog;
  end;

{$R *.lfm}

{ TboardChangedDialog }

PROCEDURE TboardChangedDialog.cancelButtonMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

PROCEDURE TboardChangedDialog.ignoreLabelMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrIgnore;
  end;

PROCEDURE TboardChangedDialog.updatePaletteButtonMouseDown(Sender: TObject;  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrYes;
  end;

FUNCTION TboardChangedDialog.showFor(CONST hasRelatedPaletteEntry: boolean): TModalResult;
  begin
    setEnableButton(updatePaletteButton,updatePaletteLabel,hasRelatedPaletteEntry);
    result:=ShowModal;
  end;

FINALIZATION
  if myBoardChangedDialog<>nil then FreeAndNil(myBoardChangedDialog);

end.

