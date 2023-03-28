UNIT paletteImportUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, paletteHandling;

TYPE

  { TPaletteImportForm }

  TPaletteImportForm = class(TForm)
    CancelLabel: TLabel;
    CancelShape: TShape;
    Label8: TLabel;
    Panel2: TPanel;
    prefixEdit: TEdit;
    prefixOrNameLabel: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    rbRemoveExactDuplicates: TRadioButton;
    rbRemoveBehavDuplicates: TRadioButton;
    rbKeepPaletteNames: TRadioButton;
    rbAddPrefix: TRadioButton;
    rbAllToSamePalette: TRadioButton;
    rbNoCleanup: TRadioButton;
    ExecuteLabel: TLabel;
    ExecuteShape: TShape;
    PROCEDURE CancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ExecuteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE prefixEditEditingDone(Sender: TObject);
    PROCEDURE rbKeepPaletteNamesChange(Sender: TObject);
    PROCEDURE rbNoCleanupChange(Sender: TObject);
  private

  public
    importOptions:T_paletteImportOptions;
  end;

FUNCTION PaletteImportForm: TPaletteImportForm;

IMPLEMENTATION
USES visuals;
VAR myPaletteImportForm: TPaletteImportForm=nil;
FUNCTION PaletteImportForm: TPaletteImportForm;
  begin
    if myPaletteImportForm=nil then myPaletteImportForm:=TPaletteImportForm.create(nil);
    result:=myPaletteImportForm;
  end;

{$R *.lfm}

{ TPaletteImportForm }

PROCEDURE TPaletteImportForm.rbKeepPaletteNamesChange(Sender: TObject);
  begin
    importOptions.addPrefixToGroupName:=rbAddPrefix.checked;
    importOptions.moveAllToSameGroup:=rbAllToSamePalette.checked;
    prefixEdit.enabled:=importOptions.addPrefixToGroupName or importOptions.moveAllToSameGroup;
    if importOptions.addPrefixToGroupName
    then prefixOrNameLabel.caption:='Präfix:'
    else prefixOrNameLabel.caption:='Importieren nach:';
  end;

PROCEDURE TPaletteImportForm.rbNoCleanupChange(Sender: TObject);
  begin
    importOptions.removeExactDuplicatesOnImport:=rbRemoveExactDuplicates.checked;
    importOptions.removeBehavDuplicatesOnImport:=rbRemoveBehavDuplicates.checked;
  end;

PROCEDURE TPaletteImportForm.ExecuteShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrOk;
  end;

PROCEDURE TPaletteImportForm.FormCreate(Sender: TObject);
  begin
    importOptions.removeBehavDuplicatesOnImport:=false;
    importOptions.removeExactDuplicatesOnImport:=false;
    importOptions.moveAllToSameGroup:=false;
    importOptions.addPrefixToGroupName:=false;
    importOptions.prefixOrSharedGroupName:='';
  end;

PROCEDURE TPaletteImportForm.FormShow(Sender: TObject);
  begin
    applyColorScheme(self);
  end;

PROCEDURE TPaletteImportForm.prefixEditEditingDone(Sender: TObject);
  begin
    importOptions.prefixOrSharedGroupName:=prefixEdit.text;
  end;

PROCEDURE TPaletteImportForm.CancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    ModalResult:=mrCancel;
  end;

FINALIZATION
  if myPaletteImportForm<>nil then FreeAndNil(myPaletteImportForm);
end.

