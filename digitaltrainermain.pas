UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Menus,baseGate,logicGates,propertyDialog,analysisDialog,draftFormUnit;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    ButtonAddAdapter: TButton;
    ButtonAddGatedClock: TButton;
    ButtonAddConstantTrue: TButton;
    ButtonAddConstantFalse: TButton;
    MenuItem2: TMenuItem;
    miToggleAllowDiagonalWires: TMenuItem;
    miToggleAllowShortcuts: TMenuItem;
    miRewire: TMenuItem;
    miAddNewCategory: TMenuItem;
    miSetCategoryRoot: TMenuItem;
    miDrafts: TMenuItem;
    miAddToDrafts: TMenuItem;
    miEditCopyOfPaletteEntry: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
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
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miDeletePaletteEntry: TMenuItem;
    miEditPaletteEntry: TMenuItem;
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
    PaletteTreeView: TTreeView;
    wireImage: TImage;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    zoomTrackBar: TTrackBar;
    speedTrackBar: TTrackBar;
    PROCEDURE AnyGatePopupMenuPopup(Sender: TObject);
    PROCEDURE ButtonAddAdapterClick(Sender: TObject);
    PROCEDURE ButtonAddClockClick(Sender: TObject);
    PROCEDURE ButtonAddConstantFalseClick(Sender: TObject);
    PROCEDURE ButtonAddConstantTrueClick(Sender: TObject);
    PROCEDURE ButtonAddCustomClick(Sender: TObject);
    PROCEDURE ButtonAddAndClick(Sender: TObject);
    PROCEDURE ButtonAddGatedClockClick(Sender: TObject);
    PROCEDURE ButtonAddInputClick(Sender: TObject);
    PROCEDURE ButtonAddNandClick(Sender: TObject);
    PROCEDURE ButtonAddNorClick(Sender: TObject);
    PROCEDURE ButtonAddNotClick(Sender: TObject);
    PROCEDURE ButtonAddNxorClick(Sender: TObject);
    PROCEDURE ButtonAddOrClick(Sender: TObject);
    PROCEDURE ButtonAddOutputClick(Sender: TObject);
    PROCEDURE ButtonAddXorClick(Sender: TObject);
    PROCEDURE captionEditEditingDone(Sender: TObject);
    PROCEDURE DeleteButtonClick(Sender: TObject);
    PROCEDURE descriptionMemoEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAddNewCategoryClick(Sender: TObject);
    PROCEDURE miDeletePaletteEntryClick(Sender: TObject);
    PROCEDURE miEditPaletteEntryClick(Sender: TObject);
    PROCEDURE miAddToDraftsClick(Sender: TObject);
    PROCEDURE miAddToPaletteClick(Sender: TObject);
    PROCEDURE miAnalyzeBoardClick(Sender: TObject);
    PROCEDURE miAnalyzeClick(Sender: TObject);
    PROCEDURE miAnalyzeGateClick(Sender: TObject);
    PROCEDURE miCopyClick(Sender: TObject);
    PROCEDURE miDeleteClick(Sender: TObject);
    PROCEDURE miDeselectAllClick(Sender: TObject);
    PROCEDURE miDraftsClick(Sender: TObject);
    PROCEDURE miEditCopyOfPaletteEntryClick(Sender: TObject);
    PROCEDURE miGatePropertiesClick(Sender: TObject);
    PROCEDURE miLoadClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miPasteClick(Sender: TObject);
    PROCEDURE miQuitClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miRewireClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSelectAllClick(Sender: TObject);
    PROCEDURE miToggleAllowDiagonalWiresClick(Sender: TObject);
    PROCEDURE miToggleAllowShortcutsClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
    PROCEDURE resetButtonClick(Sender: TObject);
    PROCEDURE SimTimerTimer(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE ZoomTrackBarChange(Sender: TObject);
  private
    currentBoardIsDraft:boolean;
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
USES wiringUtil;

{$R *.lfm}
FUNCTION workspaceFilename:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.workspace');
  end;

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  begin
    workspace.create(miSetCategoryRoot,PaletteTreeView);
    workspace.loadFromFile(workspaceFilename);
    workspace.getCurrentBoard^.attachGUI(zoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu,@restartTimerCallback);
    ScrollBox1.color:=BackgroundColor;
    currentBoardIsDraft:=workspace.getCurrentBoard^.paletteIndex<0;
    updateSidebar;

    miToggleAllowDiagonalWires.checked:=wiringUtil.allowDiagonals;
    miToggleAllowShortcuts    .checked:=wiringUtil.enableShortcuts;
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.saveToFile(workspaceFilename);
    workspace.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.DeleteButtonClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.deleteMarkedElements;
  end;

PROCEDURE TDigitaltrainerMainForm.descriptionMemoEditingDone(Sender: TObject);
  begin
    workspace.getCurrentBoard^.saveStateToUndoList;
    workspace.getCurrentBoard^.description:=descriptionMemo.text;
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

PROCEDURE TDigitaltrainerMainForm.ButtonAddGatedClockClick(Sender: TObject);
  begin workspace.addBaseGate(gt_gatedClock); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddCustomClick(Sender: TObject);
  begin
    workspace.addSelectedCustomGate;
  end;

PROCEDURE TDigitaltrainerMainForm.AnyGatePopupMenuPopup(Sender: TObject);
  begin
    visualGateForContextPopup:=workspace.getCurrentBoard^.lastClickedGate;
  end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddAdapterClick(Sender: TObject);
  begin workspace.addBaseGate(gt_adapter); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddClockClick(Sender: TObject);
  begin workspace.addBaseGate(gt_clock);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddConstantFalseClick(Sender: TObject);
  begin workspace.addBaseGate(gt_false); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddConstantTrueClick(Sender: TObject);
  begin workspace.addBaseGate(gt_true); end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddOutputClick(Sender: TObject);
  begin workspace.addBaseGate(gt_output);end;

PROCEDURE TDigitaltrainerMainForm.ButtonAddXorClick(Sender: TObject);
  begin workspace.addBaseGate(gt_xorGate);end;

PROCEDURE TDigitaltrainerMainForm.captionEditEditingDone(Sender: TObject);
  begin
    workspace.getCurrentBoard^.saveStateToUndoList;
    workspace.getCurrentBoard^.name:=captionEdit.text;
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddNewCategoryClick(Sender: TObject);
  begin
    workspace.addCategory;
  end;

PROCEDURE TDigitaltrainerMainForm.miDeletePaletteEntryClick(Sender: TObject);
  begin
    if QuestionDlg('Löschen?','Soll der Eintrag wirklich endgültig gelöscht werden?',TMsgDlgType.mtConfirmation,[mrYes, 'Ja', mrNo, 'Nein', 'IsDefault'],'')=mrNo then exit;
    workspace.deleteSelectedTreeItem;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditPaletteEntryClick(Sender: TObject);
  begin
    BeginFormUpdate;
    workspace.editSelectedTreeItem(false);
    updateSidebar;
    restartTimerCallback;
    EndFormUpdate;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToDraftsClick(Sender: TObject);
  begin
    workspace.addCurrentBoardToDrafts();
    updateSidebar;
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  begin
    workspace.addCurrentBoardToPalette;
    updateSidebar;
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeBoardClick(Sender: TObject);
  begin
    analysisForm.showForBoard(workspace.getCurrentBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeClick(Sender: TObject);
  VAR node:TTreeNode;
      data:P_paletteTreeData;
  begin
    node:=PaletteTreeView.Selected;
    if node=nil then exit;
    data:=P_paletteTreeData(node.data);
    if (data=nil) or (data^.getBoard=nil) then exit;
    analysisForm.showForBoard(data^.getBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.miAnalyzeGateClick(Sender: TObject);
  begin
    if visualGateForContextPopup=nil then exit;
    analysisForm.showForGate(visualGateForContextPopup^.getBehavior);
  end;

PROCEDURE TDigitaltrainerMainForm.miCopyClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.copySelectionToClipboard;
  end;

PROCEDURE TDigitaltrainerMainForm.miDeleteClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.deleteMarkedElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miDeselectAllClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.setSelectForAll(false);
  end;

PROCEDURE TDigitaltrainerMainForm.miDraftsClick(Sender: TObject);
  begin
    draftsForm.updateAndShow(@workspace);
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditCopyOfPaletteEntryClick(Sender: TObject);
  begin
    BeginFormUpdate;
    workspace.editSelectedTreeItem(true);
    updateSidebar;
    restartTimerCallback;
    EndFormUpdate;
  end;

PROCEDURE TDigitaltrainerMainForm.miGatePropertiesClick(Sender: TObject);
  begin
    if visualGateForContextPopup=nil then exit;
    if gatePropertyDialog.showForGate(visualGateForContextPopup^.getBehavior,workspace.getCurrentBoard) then begin
      visualGateForContextPopup^.forcedFullRepaint;
      workspace.getCurrentBoard^.deleteInvalidWires;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miLoadClick(Sender: TObject);
  VAR temp:T_workspace;
  begin
    if OpenDialog1.execute then begin
      temp.create(miSetCategoryRoot,PaletteTreeView);
      if temp.loadFromFile(OpenDialog1.fileName) then begin
        workspace.destroy;
        workspace.create(miSetCategoryRoot,PaletteTreeView);
        workspace.loadFromFile(OpenDialog1.fileName);
        workspace.getCurrentBoard^.attachGUI(zoomTrackBar.position,ScrollBox1,wireImage,AnyGatePopupMenu,@restartTimerCallback);
        workspace.getCurrentBoard^.Repaint;
        updateSidebar;
        restartTimerCallback;
      end;
      temp.destroy;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miNewClick(Sender: TObject);
  begin
    workspace.clearCurrentBoard;
    updateSidebar;
  end;

PROCEDURE TDigitaltrainerMainForm.miPasteClick(Sender: TObject);
  begin
    BeginFormUpdate;
    workspace.getCurrentBoard^.pasteFromClipboard;
    EndFormUpdate;
  end;

PROCEDURE TDigitaltrainerMainForm.miQuitClick(Sender: TObject);
  begin
    close;
  end;

PROCEDURE TDigitaltrainerMainForm.miRedoClick(Sender: TObject);
  begin
    BeginFormUpdate;
    workspace.getCurrentBoard^.performRedo;
    updateSidebar;
    EndFormUpdate;
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miRewireClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.rewire(true);
    BeginFormUpdate;
    workspace.getCurrentBoard^.Repaint;
    EndFormUpdate;
  end;

PROCEDURE TDigitaltrainerMainForm.miSaveClick(Sender: TObject);
  begin
    if SaveDialog1.execute then workspace.saveToFile(SaveDialog1.fileName);
  end;

PROCEDURE TDigitaltrainerMainForm.miSelectAllClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.setSelectForAll(true);
  end;

PROCEDURE TDigitaltrainerMainForm.miToggleAllowDiagonalWiresClick(Sender: TObject);
  begin
    miToggleAllowDiagonalWires.checked:=not(miToggleAllowDiagonalWires.checked);
    wiringUtil.allowDiagonals         :=    miToggleAllowDiagonalWires.checked;
    workspace.getCurrentBoard^.rewire(true);
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miToggleAllowShortcutsClick(Sender: TObject);
  begin
    miToggleAllowShortcuts.checked:=not(miToggleAllowShortcuts.checked);
    wiringUtil.enableShortcuts    :=    miToggleAllowShortcuts.checked;
    workspace.getCurrentBoard^.rewire(true);
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miUndoClick(Sender: TObject);
  begin
    BeginFormUpdate;
    workspace.getCurrentBoard^.performUndo;
    updateSidebar;
    EndFormUpdate;
    workspace.getCurrentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.resetButtonClick(Sender: TObject);
  begin
    workspace.getCurrentBoard^.saveStateToUndoList;
    workspace.getCurrentBoard^.reset;
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
  VAR  startTicks: qword;
  begin
    startTicks:=GetTickCount64;
    if workspace.getCurrentBoard^.simulateSteps(stepsPerTimer)
    then begin
      if GetTickCount64-startTicks>SPEED_SETTING[speedTrackBar.position].timerInterval
      then begin
        speedTrackBar.position:=speedTrackBar.position-1;
        with SPEED_SETTING[speedTrackBar.position] do begin
          SimTimer.interval :=timerInterval;
          stepsPerTimer     :=simSteps;
          speedLabel.caption:=labelCaption;
        end;
      end;

    end else begin
      SimTimer.enabled:=false;
      speedLabel.caption:=SPEED_SETTING[speedTrackBar.position].labelCaption+' (pausiert)';
    end
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
    BeginFormUpdate;
    workspace.getCurrentBoard^.setZoom(zoomTrackBar.position);
    EndFormUpdate;
  end;

PROCEDURE TDigitaltrainerMainForm.updateSidebar;
  begin
    descriptionMemo.text:=workspace.getCurrentBoard^.description;
    captionEdit    .text:=workspace.getCurrentBoard^.name;
  end;

end.

