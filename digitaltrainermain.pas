UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Menus,baseGate,logicGates,propertyDialog,analysisDialog;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    ButtonAdd1to4: TButton;
    ButtonAdd8to4: TButton;
    ButtonAdd4to8: TButton;
    ButtonAdd8to1: TButton;
    ButtonAdd1to8: TButton;
    ButtonAdd4to1: TButton;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miAnalyze: TMenuItem;
    miAnalyzeGate: TMenuItem;
    speedLabel: TLabel;
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
    PROCEDURE ButtonAdd1to4Click(Sender: TObject);
    PROCEDURE ButtonAdd1to8Click(Sender: TObject);
    PROCEDURE ButtonAdd4to1Click(Sender: TObject);
    PROCEDURE ButtonAdd4to8Click(Sender: TObject);
    PROCEDURE ButtonAdd8to1Click(Sender: TObject);
    PROCEDURE ButtonAdd8to4Click(Sender: TObject);
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
    PROCEDURE miAnalyzeClick(Sender: TObject);
    PROCEDURE miAnalyzeGateClick(Sender: TObject);
    PROCEDURE miCopyClick(Sender: TObject);
    PROCEDURE miDeleteClick(Sender: TObject);
    PROCEDURE miDeselectAllClick(Sender: TObject);
    PROCEDURE miGatePropertiesClick(Sender: TObject);
    PROCEDURE miLoadClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miPasteClick(Sender: TObject);
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
    stepsPerTimer:longint;
    PROCEDURE updateSidebar;
    PROCEDURE restartTimerCallback;
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
    workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu,@restartTimerCallback);
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
  begin workspace.addBaseGate(gt_input);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNandClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nandGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_norGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNotClick(Sender: TObject);
  begin workspace.addBaseGate(gt_notGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddNxorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_nxorGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOrClick(Sender: TObject);
  begin workspace.addBaseGate(gt_orGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddAndClick(Sender: TObject);
  begin workspace.addBaseGate(gt_andGate);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddCustomClick(Sender: TObject);
  begin workspace.addCustomGate(CustomGateListBox.ItemIndex); end;

PROCEDURE TDigitaltrainerMainForm.AnyGatePopupMenuPopup(Sender: TObject);
  begin
    visualGateForContextPopup:=workspace.currentBoard^.lastClickedGate;
  end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd1to4Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter1to4); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd1to8Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter1to8); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd4to1Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter4to1); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd4to8Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter4to8); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd8to1Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter8to1); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAdd8to4Click(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter8to4); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddClockClick(Sender: TObject);
  begin workspace.addBaseGate(gt_clock);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOutputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_output);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddXorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_xorGate);end;

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
    restartTimerCallback;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  begin
    workspace.addCurrentBoardToPalette;
    updateSidebar;
    workspace.currentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeBoardClick(Sender: TObject);
  begin
    analysisForm.showForBoard(workspace.currentBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeClick(Sender: TObject);
  begin
    if (CustomGateListBox.ItemIndex>=0) and
       (CustomGateListBox.ItemIndex<length(workspace.paletteEntries))
    then analysisForm.showForBoard(workspace.paletteEntries[CustomGateListBox.ItemIndex]);
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeGateClick(Sender: TObject);
  begin
    if visualGateForContextPopup=nil then exit;
    analysisForm.showForGate(visualGateForContextPopup^.getBehavior);
  end;

PROCEDURE TDigitaltrainerMainForm.miCopyClick(Sender: TObject);
  begin
    workspace.currentBoard^.copySelectionToClipboard;
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
    workspace.currentBoard^.deleteInvalidWires;
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
        workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu,@restartTimerCallback);
        workspace.currentBoard^.Repaint;
        updateSidebar;
        restartTimerCallback;
      end;
      temp.destroy;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miNewClick(Sender: TObject);
  begin
    workspace.currentBoard^.clear;
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miPasteClick(Sender: TObject);
  begin
    workspace.currentBoard^.pasteFromClipboard;
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
    restartTimerCallback;
  end;

CONST SPEED_SETTING:array[0..35] of record
        timerInterval,
        simSteps:longint;
        labelCaption:string;
      end=((timerInterval:   0; simSteps:   0; labelCaption:'gestoppt'),
           (timerInterval:1000; simSteps:   1; labelCaption:'1.00Hz'),
           (timerInterval: 707; simSteps:   1; labelCaption:'1.41Hz'),
           (timerInterval: 500; simSteps:   1; labelCaption:'2.00Hz'),
           (timerInterval: 354; simSteps:   1; labelCaption:'2.82Hz'),
           (timerInterval: 250; simSteps:   1; labelCaption:'4.00Hz'),
           (timerInterval: 177; simSteps:   1; labelCaption:'5.65Hz'),
           (timerInterval: 125; simSteps:   1; labelCaption:'8.00Hz'),
           (timerInterval:  88; simSteps:   1; labelCaption:'11.36Hz'),
           (timerInterval:  62; simSteps:   1; labelCaption:'16.13Hz'),
           (timerInterval:  44; simSteps:   1; labelCaption:'22.73Hz'),
           (timerInterval:  40; simSteps:   1; labelCaption:' 25Hz'),
           (timerInterval:  40; simSteps:   2; labelCaption:' 50Hz'),
           (timerInterval:  40; simSteps:   3; labelCaption:' 75Hz'),
           (timerInterval:  40; simSteps:   4; labelCaption:'100Hz'),
           (timerInterval:  40; simSteps:   5; labelCaption:'125Hz'),
           (timerInterval:  40; simSteps:   7; labelCaption:'175Hz'),
           (timerInterval:  40; simSteps:  10; labelCaption:'250Hz'),
           (timerInterval:  40; simSteps:  14; labelCaption:'350Hz'),
           (timerInterval:  40; simSteps:  20; labelCaption:'500Hz'),
           (timerInterval:  40; simSteps:  29; labelCaption:'725Hz'),
           (timerInterval:  40; simSteps:  41; labelCaption:'1.0kHz'),
           (timerInterval:  40; simSteps:  58; labelCaption:'1.5kHz'),
           (timerInterval:  40; simSteps:  82; labelCaption:'2.1kHz'),
           (timerInterval:  40; simSteps: 116; labelCaption:'2.9kHz'),
           (timerInterval:  40; simSteps: 164; labelCaption:'4.1kHz'),
           (timerInterval:  40; simSteps: 232; labelCaption:'5.8kHz'),
           (timerInterval:  40; simSteps: 328; labelCaption:'8.2kHz'),
           (timerInterval:  40; simSteps: 463; labelCaption:'11.6kHz'),
           (timerInterval:  40; simSteps: 655; labelCaption:'16.4kHz'),
           (timerInterval:  40; simSteps: 927; labelCaption:'23.2kHz'),
           (timerInterval:  40; simSteps:1311; labelCaption:'32.8kHz'),
           (timerInterval:  40; simSteps:1854; labelCaption:'46.4kHz'),
           (timerInterval:  40; simSteps:2621; labelCaption:'65.5kHz'),
           (timerInterval:  40; simSteps:3707; labelCaption:'92.7kHz'),
           (timerInterval:  40; simSteps:5243; labelCaption:'131.1kHz'));

PROCEDURE TDigitaltrainerMainForm.SimTimerTimer(Sender: TObject);
  begin
    if not(workspace.currentBoard^.simulateSteps(stepsPerTimer)) then begin
      SimTimer.enabled:=false;
      speedLabel.caption:=SPEED_SETTING[speedTrackBar.position].labelCaption+' (pausiert)';
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.restartTimerCallback;
  begin
    if SimTimer.enabled or (speedTrackBar.position=0) then exit;
    SimTimer.enabled:=true;
    speedLabel.caption:=SPEED_SETTING[speedTrackBar.position].labelCaption;
  end;

PROCEDURE TDigitaltrainerMainForm.speedTrackBarChange(Sender: TObject);
  begin
    if speedTrackBar.position=0
    then begin
      SimTimer.enabled:=false;
      speedLabel.caption:='gestoppt';
    end
    else begin
      SimTimer.enabled:=true;
      with SPEED_SETTING[speedTrackBar.position] do begin
        SimTimer.interval :=timerInterval;
        stepsPerTimer     :=simSteps;
        speedLabel.caption:=labelCaption;
      end;
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

