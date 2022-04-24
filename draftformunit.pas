UNIT draftFormUnit;

{$mode objfpc}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, baseGate;

TYPE
  { TdraftsForm }

  TdraftsForm = class(TForm)
    DeleteButton: TButton;
    EditButton: TButton;
    ListBox1: TListBox;
    PROCEDURE DeleteButtonClick(Sender: TObject);
    PROCEDURE EditButtonClick(Sender: TObject);
  private

    workspace:P_workspace;
  public
    PROCEDURE updateListing;
    PROCEDURE updateAndShow(CONST workspace_:P_workspace);
  end;

VAR
  draftsForm: TdraftsForm;

IMPLEMENTATION

{$R *.lfm}

{ TdraftsForm }

PROCEDURE TdraftsForm.DeleteButtonClick(Sender: TObject);
  begin
    if (ListBox1.ItemIndex<0) or (ListBox1.ItemIndex>=workspace^.numberOfDraftEntries) then exit;
    if QuestionDlg('Löschen?','Soll der Eintrag wirklich endgültig gelöscht werden?',TMsgDlgType.mtConfirmation,[mrYes, 'Ja', mrNo, 'Nein', 'IsDefault'],'')=mrNo then exit;
    workspace^.removeDraftEntry(ListBox1.ItemIndex);
  end;

PROCEDURE TdraftsForm.EditButtonClick(Sender: TObject);
  begin
    if (ListBox1.ItemIndex<0) or (ListBox1.ItemIndex>=workspace^.numberOfDraftEntries) then exit;
    workspace^.editDraftEntry(ListBox1.ItemIndex);
    ModalResult:=mrOk;
  end;

PROCEDURE TdraftsForm.updateListing;
  VAR i:longint;
  begin
    ListBox1.items.clear;
    for i:=0 to workspace^.numberOfDraftEntries-1 do
      ListBox1.items.add(workspace^.draftEntryName(i));
  end;

PROCEDURE TdraftsForm.updateAndShow(CONST workspace_:P_workspace);
  begin
    workspace:=workspace_;
    updateListing;
    ShowModal;
  end;

end.

