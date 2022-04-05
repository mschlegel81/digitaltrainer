UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Menus,baseGate,logicGates,propertyDialog,analysisDialog;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    MenuItem4: TMenuItem;
    miAddToPalette: TMenuItem;
    miAnalyzeBoard: TMenuItem;
    resetButton: TButton;
    ButtonAddCustom: TButton;
    ButtonAddNxor: TButton;
    ButtonAddNor: TButton;
    ButtonAddNand: TButton;
    ButtonAddClock: TButton;
    ButtonAddXor: TButton;
    ButtonAddNot: TButton;
    ButtonAddOr: TButton;
    ButtonAddAnd: TButton;
    ButtonAddOutput: TButton;
    ButtonAddInput: TButton;
    DeleteButton: TButton;
    captionEdit: TEdit;
    FlowPanel1: TFlowPanel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    descriptionMemo: TMemo;
    GroupBox5: TGroupBox;
    CustomGateListBox: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miGateProperties: TMenuItem;
    MenuItem7: TMenuItem;
    miDelete: TMenuItem;
    miDeselectAll: TMenuItem;
    miLoad: TMenuItem;
    miNew: TMenuItem;
    miQuit: TMenuItem;
    miSave: TMenuItem;
    miSelectAll: TMenuItem;
    OpenDialog1: TOpenDialog;
    GateListBoxPopupMenu: TPopupMenu;
    AnyGatePopupMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SimTimer: TTimer;
    Splitter2: TSplitter;
    wireImage: TImage;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    ZoomTrackBar: TTrackBar;
    speedTrackBar: TTrackBar;
    PROCEDURE AnyGatePopupMenuPopup(Sender: TObject);
    PROCEDURE ButtonAddClockClick(Sender: TObject);
    PROCEDURE ButtonAddCustomClick(Sender: TObject);
    PROCEDURE ButtonAddAndClick(Sender: TObject);
    PROCEDURE ButtonAddInputClick(Sender: TObject);
    PROCEDURE ButtonAddNandClick(Sender: TObject);
    PROCEDURE ButtonAddNorClick(Sender: TObject);
    PROCEDURE ButtonAddNotClick(Sender: TObject);
    PROCEDURE ButtonAddNxorClick(Sender: TObject);
    PROCEDURE ButtonAddOrClick(Sender: TObject);
    PROCEDURE ButtonAddOutputClick(Sender: TObject);
    PROCEDURE ButtonAddXorClick(Sender: TObject);
    PROCEDURE captionEditEditingDone(Sender: TObject);
    PROCEDURE CustomGateListBoxSelectionChange(Sender: TObject; user: boolean);
    PROCEDURE DeleteButtonClick(Sender: TObject);
    PROCEDURE descriptionMemoEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE MenuItem2Click(Sender: TObject);
    PROCEDURE MenuItem3Click(Sender: TObject);
    PROCEDURE miAddToPaletteClick(Sender: TObject);
    PROCEDURE miAnalyzeBoardClick(Sender: TObject);
    PROCEDURE miDeleteClick(Sender: TObject);
    PROCEDURE miDeselectAllClick(Sender: TObject);
    PROCEDURE miGatePropertiesClick(Sender: TObject);
    PROCEDURE miLoadClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miQuitClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSelectAllClick(Sender: TObject);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE SimTimerTimer(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE ZoomTrackBarChange(Sender: TObject);
  private
    workspace:T_workspace;
    visualGateForContextPopup:P_visualGate;
    PROCEDURE updateSidebar;
  public
  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION

{$R *.lfm}
FUNCTION workspaceFilename:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.workspace');
  end;

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  begin
    workspace.create;
    workspace.loadFromFile(workspaceFilename);
    workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu);
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.saveToFile(workspaceFilename);
    workspace.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.DeleteButtonClick(Sender: TObject);
  begin
    workspace.currentBoard^.deleteMarkedElements;
  end;

PROCEDURE TDigitaltrainerMainForm.descriptionMemoEditingDone(Sender: TObject);
  begin
    workspace.currentBoard^.description:=descriptionMemo.text;
  end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddInputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_input,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNandClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nandGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_norGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNotClick(Sender: TObject);
  begin workspace.addBaseGate(gt_notGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNxorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nxorGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOrClick(Sender: TObject);
  begin workspace.addBaseGate(gt_orGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddAndClick(Sender: TObject);
  begin workspace.addBaseGate(gt_andGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddCustomClick(Sender: TObject);
  begin workspace.addCustomGate(CustomGateListBox.ItemIndex,0,0); end;

PROCEDURE TDigitaltrainerMainForm.AnyGatePopupMenuPopup(Sender: TObject);
  begin
    visualGateForContextPopup:=workspace.currentBoard^.lastClickedGate;
  end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddClockClick(Sender: TObject);
  begin workspace.addBaseGate(gt_clock,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOutputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_output,0,0);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddXorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_xorGate,0,0);end;

PROCEDURE TDigitaltrainerMainForm.captionEditEditingDone(Sender: TObject);
  begin
    workspace.currentBoard^.name:=captionEdit.text;
  end;

PROCEDURE TDigitaltrainerMainForm.CustomGateListBoxSelectionChange(Sender: TObject; user: boolean);
  VAR i:longint;
  begin
    i:=CustomGateListBox.ItemIndex;
    if (i>=0) and (i<length(workspace.paletteEntries)) then begin
      CustomGateListBox.Hint:=workspace.paletteEntries[i]^.description;
      ButtonAddCustom.enabled:=true;
    end else ButtonAddCustom.enabled:=false;
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.currentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.MenuItem2Click(Sender: TObject);
  begin
    if QuestionDlg('Löschen?','Soll der Eintrag wirklich endgültig gelöscht werden?',TMsgDlgType.mtConfirmation,[mrYes, 'Ja', mrNo, 'Nein', 'IsDefault'],'')=mrNo then exit;
    workspace.removePaletteEntry(CustomGateListBox.ItemIndex);
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.MenuItem3Click(Sender: TObject);
  begin
    workspace.editPaletteEntry(CustomGateListBox.ItemIndex);
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  begin
    workspace.addCurrentBoardToPalette;
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeBoardClick(Sender: TObject);
  begin
    analysisForm.showForBoard(workspace.currentBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.miDeleteClick(Sender: TObject);
  begin
    workspace.currentBoard^.deleteMarkedElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miDeselectAllClick(Sender: TObject);
  begin
    workspace.currentBoard^.setSelectForAll(false);
  end;

PROCEDURE TDigitaltrainerMainForm.miGatePropertiesClick(Sender: TObject);
  begin
    if visualGateForContextPopup=nil then exit;
    gatePropertyDialog.showForGate(visualGateForContextPopup^.getBehavior);
    visualGateForContextPopup^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miLoadClick(Sender: TObject);
  VAR temp:T_workspace;
  begin
    if OpenDialog1.execute then begin
      temp.create;
      if temp.loadFromFile(OpenDialog1.fileName) then begin
        workspace.destroy;
        workspace.create;
        workspace.loadFromFile(OpenDialog1.fileName);
        workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu);
        workspace.currentBoard^.Repaint;
        updateSidebar;
      end;
      temp.destroy;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miNewClick(Sender: TObject);
  begin
    workspace.currentBoard^.clear;
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miQuitClick(Sender: TObject);
  begin
    close;
  end;

PROCEDURE TDigitaltrainerMainForm.miSaveClick(Sender: TObject);
  begin
    if SaveDialog1.execute then workspace.saveToFile(SaveDialog1.fileName);
  end;

PROCEDURE TDigitaltrainerMainForm.miSelectAllClick(Sender: TObject);
  begin
    workspace.currentBoard^.setSelectForAll(true);
  end;

PROCEDURE TDigitaltrainerMainForm.resetButtonClick(Sender: TObject);
  begin
    workspace.currentBoard^.reset;
  end;

PROCEDURE TDigitaltrainerMainForm.SimTimerTimer(Sender: TObject);
  begin
    workspace.currentBoard^.simulateStep;
  end;

PROCEDURE TDigitaltrainerMainForm.speedTrackBarChange(Sender: TObject);
  begin
    if speedTrackBar.position=0
    then SimTimer.enabled:=false
    else begin
      SimTimer.enabled:=true;
      SimTimer.interval:=round(1000*exp(ln(5/1000)*(speedTrackBar.position-1)/(speedTrackBar.max-1)));
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomTrackBarChange(Sender: TObject);
  begin
    workspace.currentBoard^.setZoom(ZoomTrackBar.position);
  end;

PROCEDURE TDigitaltrainerMainForm.updateSidebar;
  VAR i:longint;
  begin
    descriptionMemo.text:=workspace.currentBoard^.description;
    captionEdit    .text:=workspace.currentBoard^.name;

    CustomGateListBox.items.clear;
    for i:=0 to length(workspace.paletteEntries)-1 do
    if (workspace.currentBoard^.paletteIndex=-1) or
       (workspace.currentBoard^.paletteIndex>i)
    then CustomGateListBox.items.add(workspace.paletteEntries[i]^.name);

    if (CustomGateListBox.items.count=0) then ButtonAddCustom.enabled:=false;
  end;

end.

