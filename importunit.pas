UNIT importUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,baseGate;

TYPE

  { TImportForm }

  TImportForm = class(TForm)
    executeButton: TButton;
    CancelButton: TButton;
    CheckListBox1: TCheckListBox;
    GroupBox1: TGroupBox;
    PROCEDURE CheckListBox1ItemClick(Sender: TObject; index: integer);
  private
    currentWorkspace,importSource:P_workspace;
    importItems:T_importItems;
  public
    PROCEDURE executeForFile(CONST current:P_workspace; CONST fileName:string);

  end;

VAR
  ImportForm: TImportForm;

IMPLEMENTATION

{$R *.lfm}

{ TImportForm }

PROCEDURE TImportForm.CheckListBox1ItemClick(Sender: TObject; index: integer);
  VAR k:longint;
  begin
    importItems.items[index]^.selectedForImport:=not(importItems.items[index]^.selectedForImport);
    for k:=0 to length(importItems.items)-1 do
      CheckListBox1.checked    [k]:=    importItems.items[k]^.selectedForImport;
  end;

PROCEDURE TImportForm.executeForFile(CONST current: P_workspace; CONST fileName: string);
  VAR importAction: integer=mrNone;
      k:longint;
  begin
    caption:='Import aus '+fileName;
    currentWorkspace:=current;
    new(importSource,create(nil,nil));
    if importSource^.loadFromFile(fileName)
    then begin
      importItems.create(importSource,current);
      CheckListBox1.items.clear;

      for k:=0 to length(importItems.items)-1 do begin
        CheckListBox1.items.add(importItems.items[k]^.itemCaption);
        CheckListBox1.ItemEnabled[k]:=not(importItems.items[k]^.alreadyExists);
        CheckListBox1.checked    [k]:=    importItems.items[k]^.selectedForImport;
      end;

      importAction:=ShowModal;
      if importAction=mrOk then importItems.executeImport;
      importItems.destroy;
    end;
    dispose(importSource,destroy);
  end;

end.

