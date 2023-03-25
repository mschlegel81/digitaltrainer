UNIT RestoreWorkspaceUi;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, workspaces;

TYPE

  { TRestoreWorkspaceDialog }

  TRestoreWorkspaceDialog = class(TForm)
    backupsGrid: TStringGrid;
    RestoreLabel: TLabel;
    ErrorOcurredLabel: TLabel;
    RestoreShape: TShape;
    PROCEDURE backupsGridSelectCell(Sender: TObject; aCol, aRow: integer;
      VAR CanSelect: boolean);
    PROCEDURE RestoreShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    index:T_workspaceHistoryEntryIndex;
    entryToRestore:T_workspaceHistoryEntryMetaData;
    PROCEDURE fillTable;
  public
    FUNCTION showFor(CONST afterErrorOnStartup:boolean):boolean;
  end;

FUNCTION RestoreWorkspaceDialog: TRestoreWorkspaceDialog;

IMPLEMENTATION
USES visuals;
VAR myRestoreWorkspaceUi: TRestoreWorkspaceDialog=nil;
FUNCTION RestoreWorkspaceDialog: TRestoreWorkspaceDialog;
  begin
    if myRestoreWorkspaceUi=nil then myRestoreWorkspaceUi:=TRestoreWorkspaceDialog.create(nil);
    result:=myRestoreWorkspaceUi;
  end;

{$R *.lfm}

{ TRestoreWorkspaceDialog }

PROCEDURE TRestoreWorkspaceDialog.RestoreShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR
    backupSavePoint: T_workspaceHistoryEntryMetaData;
  begin
    backupSavePoint:=addBackup(@workspace,wht_beforeRestore);
    if tryRestoreBackup(@workspace,entryToRestore) then ModalResult:=mrOk
    else begin
      ErrorOcurredLabel.caption:='Das Backup das Du ausgewählt hast scheint'+LineEnding+
                                 'unbrauchbar zu sein.'+LineEnding+
                                 'Ich habe es daher aus der Liste entfernt.';
      ErrorOcurredLabel.visible:=true;
      index:=dropBackup(entryToRestore);
      tryRestoreBackup(@workspace,backupSavePoint);
      fillTable;
    end;
  end;

PROCEDURE TRestoreWorkspaceDialog.backupsGridSelectCell(Sender: TObject; aCol,aRow: integer; VAR CanSelect: boolean);
  begin
    aRow-=1;
    if (aRow>=0) and (aRow<index.size) then begin
      setEnableButton(RestoreShape,RestoreLabel,true);
      entryToRestore:=index.entries[index.size-1-aRow];
    end else setEnableButton(RestoreShape,RestoreLabel,false);
  end;

PROCEDURE TRestoreWorkspaceDialog.fillTable;
  VAR k,r:longint;
  begin
    backupsGrid.rowCount:=1+index.size;
    r:=1;
    for k:=index.size-1 downto 0 do with index.entries[k] do begin
      backupsGrid.Cells[0,r]:=FormatDateTime('dd.mm.yyyy hh:nn:ss',datetime);
      backupsGrid.Cells[1,r]:=C_workspaceHistorizationTrigger[triggeredBy];
      backupsGrid.Cells[2,r]:=intToStr(numberOfPaletteEntries);
      backupsGrid.Cells[3,r]:=intToStr(numberOfTasks);
      backupsGrid.Cells[4,r]:=intToStr(dataStartAt);
      backupsGrid.Cells[5,r]:=intToStr(dataSize);
      inc(r);
    end;
    backupsGrid.AutoSizeColumns;

  end;

FUNCTION TRestoreWorkspaceDialog.showFor(CONST afterErrorOnStartup: boolean): boolean;
  begin
    if afterErrorOnStartup then begin
      ErrorOcurredLabel.caption:='Leider ist beim Starten etwas schiefgelaufen'+LineEnding+
                                 'und der Arbeitsplatz konnte nicht wiederhergestellt werden.'+LineEnding+
                                 'Vielleicht hilft eines der folgenden Backups dir weiter.';
      ErrorOcurredLabel.visible:=true;
    end else ErrorOcurredLabel.visible:=false;
    index:=getBackupsIndex;
    if afterErrorOnStartup and (index.size=0) then exit(false);
    setEnableButton(RestoreShape,RestoreLabel,false);
    fillTable;
    result:=(ShowModal=mrOk);
  end;

FINALIZATION
  if myRestoreWorkspaceUi<>nil then FreeAndNil(myRestoreWorkspaceUi);

end.

