UNIT dtMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Menus, ValEdit, Grids, visualGates, logicalGates,
  paletteHandling, gateProperties, addToPaletteDialog, visuals, workspaces,
  createTaskUnit, taskHandlingUi, taskFinishedUnit, paletteHandingUi,
  challenges, types, LCLType;

TYPE
  { TDigitaltrainerMainForm }
  TDigitaltrainerMainForm = class(TForm)
    boardHorizontalScrollBar: TScrollBar;
    boardImage: TImage;
    MenuItem1: TMenuItem;
    miColorSchemeDark: TMenuItem;
    miColorSchemeMediumGray: TMenuItem;
    miColorSchemeRust: TMenuItem;
    miColorSchemeNeon: TMenuItem;
    miColorSchemeDefault: TMenuItem;
    miColorSchemeBlackOnWhite: TMenuItem;
    miAddToPalette: TMenuItem;
    miStartNextUnsolvedTask: TMenuItem;
    miManagePalette: TMenuItem;
    miPalette: TMenuItem;
    miCreateTask: TMenuItem;
    miBackups: TMenuItem;
    miGoBack: TMenuItem;
    miZoomOut: TMenuItem;
    miZoomIn: TMenuItem;
    miSimpleUI: TMenuItem;
    miShrink: TMenuItem;
    miTestBoard: TMenuItem;
    miChallengesMenu: TMenuItem;
    miViewTasks: TMenuItem;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    WireTimer: TIdleTimer;
    ioEdit: TEdit;
    infoLabel: TLabel;
    miRedo: TMenuItem;
    miUndo: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miEdit: TMenuItem;
    propEditPanel: TPanel;
    propDeleteLabel: TLabel;
    propEditLabel: TLabel;
    propOkLabel: TLabel;
    propCancelLabel: TLabel;
    propDeleteButton: TShape;
    propEditShape: TShape;
    propOkShape: TShape;
    propCancelShape: TShape;
    selectionShape: TShape;
    TestLabel: TLabel;
    TestShape: TShape;
    SubPaletteComboBox: TComboBox;
    speedLabel: TLabel;
    miEditMode: TMenuItem;
    miNewBoard: TMenuItem;
    miBoard: TMenuItem;
    miFullScreen: TMenuItem;
    miView: TMenuItem;
    paletteScrollBar: TScrollBar;
    boardVerticalScrollBar: TScrollBar;
    SimulationTimer: TTimer;
    speedTrackBar: TTrackBar;
    ValueListEditor1: TValueListEditor;
    ZoomInLabel: TLabel;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    ZoomOutLabel: TLabel;
    ZoomInShape: TShape;
    AnimationTimer: TTimer;
    ResetLabel: TLabel;
    PlayPauseLabel: TLabel;
    ZoomOutShape: TShape;
    ResetShape: TShape;
    PlayPauseShape: TShape;
    SpeedBgShape: TShape;
    PROCEDURE AnimationTimerTimer(Sender: TObject);
    PROCEDURE BoardImageClick(Sender: TObject);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE miAddToPaletteClick(Sender: TObject);
    PROCEDURE miBackupsClick(Sender: TObject);
    PROCEDURE miColorSchemeDefaultClick(Sender: TObject);
    PROCEDURE miCopyClick(Sender: TObject);
    PROCEDURE miEditModeClick(Sender: TObject);
    PROCEDURE miEditPaletteClick(Sender: TObject);
    PROCEDURE miFullScreenClick(Sender: TObject);
    PROCEDURE miGoBackClick(Sender: TObject);
    PROCEDURE miMarkChallengesUnsolvedClick(Sender: TObject);
    PROCEDURE miNewBoardClick(Sender: TObject);
    PROCEDURE miPasteClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miSaveAsTaskClick(Sender: TObject);
    PROCEDURE miShrinkClick(Sender: TObject);
    PROCEDURE miSimpleUIClick(Sender: TObject);
    PROCEDURE miStartNextUnsolvedTaskClick(Sender: TObject);
    PROCEDURE miTasksClick(Sender: TObject);
    PROCEDURE miTestBoardClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
    PROCEDURE miZoomInClick(Sender: TObject);
    PROCEDURE miZoomOutClick(Sender: TObject);
    PROCEDURE PaletteScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
    PROCEDURE PlayPauseShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propCancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propDeleteButtonMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propEditShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ResetShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE selectionShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE SimulationTimerTimer(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE SubPaletteComboBoxDrawItem(control: TWinControl; index: integer; ARect: TRect; state: TOwnerDrawState);
    PROCEDURE SubPaletteComboBoxEditingDone(Sender: TObject);
    PROCEDURE TestShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE WireTimerTimer(Sender: TObject);
    PROCEDURE ZoomInShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton;  Shift: TShiftState; X, Y: integer);
  private
    Buttons:array of T_shapeAndLabel;

    uiAdapter:T_uiAdapter;
    gateProperties  :T_gatePropertyValues;
    pauseByUser:boolean;

    PROCEDURE buttonClicked(Shape:TShape);
    PROCEDURE propertyValueChanged(Sender: TObject);
    PROCEDURE repositionPropertyEditor(CONST mouseX,mouseY:longint; CONST hideEditor:boolean);
    PROCEDURE showPropertyEditor(CONST gate:P_visualGate; CONST fromBoard:boolean; CONST mouseX,mouseY:longint);
    PROCEDURE boardChanged;
    PROCEDURE testFinished;
    PROCEDURE updateUiElements;
    FUNCTION continueWithOtherBoard:boolean;
  public

  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION
USES compoundGates, boardTestUnit, boardChangedUi,welcomeDialog,RestoreWorkspaceUi,sprites;
CONST playPauseGlyph:array[false..true] of string=('⏵','⏸');

{$R *.lfm}

{ TDigitaltrainerMainForm }

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  PROCEDURE addButton(Shape:TShape; lab:TLabel);
    VAR k:longint;
    begin
      lab.OnMouseDown:=Shape.OnMouseDown;
      k:=length(Buttons);
      setLength(Buttons,k+1);
      Buttons[k].Shape:=Shape;
      Buttons[k].labl :=lab;
      Buttons[k].colorIndex:=0;
      Shape.Tag:=k;
    end;

  begin
    addButton(ZoomInShape,ZoomInLabel);
    addButton(ZoomOutShape,ZoomOutLabel);
    addButton(ResetShape,ResetLabel);
    addButton(PlayPauseShape,PlayPauseLabel);
    addButton(propDeleteButton,propDeleteLabel);
    addButton(propEditShape,propEditLabel);
    addButton(propOkShape,propOkLabel);
    addButton(propCancelShape,propCancelLabel);
    addButton(TestShape,TestLabel);

    uiAdapter.create(selectionShape,
                     boardImage,
                     SubPaletteComboBox,
                     boardHorizontalScrollBar,
                     boardVerticalScrollBar,
                     paletteScrollBar,
                     ioEdit,
                     @showPropertyEditor,
                     @repositionPropertyEditor,
                     @boardChanged,
                     @workspace.prototypeUpdated);

    createTaskUnit  .uiAdapter:=@uiAdapter;
    paletteHandingUi.uiAdapter:=@uiAdapter;

    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.reset(true);
    miSimpleUI.checked:=workspace.simplisticUi;
    applyColorScheme(self);
    selectionShape.Pen.color:=colorScheme.MARK_COLOR;
    updateUiElements;

    pauseByUser:=false;

    if getColorSchemeIndex=1
    then miColorSchemeBlackOnWhite.checked:=true
    else if getColorSchemeIndex=2
    then miColorSchemeNeon.checked:=true
    else if getColorSchemeIndex=3
    then miColorSchemeRust.checked:=true
    else if getColorSchemeIndex=4
    then miColorSchemeMediumGray.checked:=true
    else if getColorSchemeIndex=5
    then miColorSchemeDark.checked:=true
    else miColorSchemeDefault.checked:=true;

    Application.AddOnKeyDownHandler(@FormKeyDown,false);
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.activeBoard  ^.detachUI;
    workspace.activePalette^.detachUI;
    uiAdapter.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if Sender.ClassNameIs('TComboBox') or
       Sender.ClassNameIs('TEdit') or
       Sender.ClassNameIs('TPickListCellEditor') or
       Sender.ClassNameIs('TStringCellEditor') or
       Sender.ClassNameIs('TStringGrid') or
       Sender.ClassNameIs('TMemo') then exit;
    {$ifdef debugMode}
    writeln('FormKeyDown by "',Sender.ClassName,'" key=',key);
    {$endif}
    workspace.activeBoard^.handleInputKey(key,ssShift in Shift);
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.activePalette^.checkSizes;
    uiAdapter.paintAll;
  end;

PROCEDURE TDigitaltrainerMainForm.FormShow(Sender: TObject);
  begin
    if errorOnLoadWorkspace and RestoreWorkspaceDialog.showFor(true) then begin
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.reset(true);
      updateUiElements;
    end;
    if workspace.firstStart and FirstStartForm.wantToStartTutorial then begin
      workspace.startChallenge(0);
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.reset(true);
      uiAdapter.paintAll;
      updateUiElements;
      infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  VAR timerEnabledBefore:boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;
    if (workspace.state in [editingNewBoard,editingPaletteEntry]) and AddToPaletteForm.showFor(P_workspacePalette(workspace.activePalette),workspace.activeBoard) then begin
      repositionPropertyEditor(0,0,true);
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activePalette^.checkSizes;
      workspace.activeBoard^.clear;
      uiAdapter.clearUndoList;
      uiAdapter.paintAll;
      infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    end;
    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miBackupsClick(Sender: TObject);
  VAR timerEnabledBefore:boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;
    RestoreWorkspaceDialog.showFor(false);
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.reset(true);
    updateUiElements;
    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miColorSchemeDefaultClick(Sender: TObject);
  begin
    if miColorSchemeBlackOnWhite.checked
    then setColorScheme(1)
    else if miColorSchemeNeon.checked
    then setColorScheme(2)
    else if miColorSchemeRust.checked
    then setColorScheme(3)
    else if miColorSchemeMediumGray.checked
    then setColorScheme(4)
    else if miColorSchemeDark.checked
    then setColorScheme(5)
    else setColorScheme(0);
    applyColorScheme(self);

    selectionShape.Pen.color:=colorScheme.MARK_COLOR;

    clearSpriteCaches;
    uiAdapter.updateTitleLayer;
    workspace.activeBoard^.forceWireRepaint;
    uiAdapter.paintAll;
    repaint;
    DefocusControl(speedTrackBar,false);
    speedTrackBar.repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.miCopyClick(Sender: TObject);
  begin
    workspace.activeBoard^.copySelectionToClipboard;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditModeClick(Sender: TObject);
  begin
    if (workspace.state in [editingNewBoard,editingPaletteEntry,editingChallengeSolution,editingChallengeTemplate]) then exit;
    repositionPropertyEditor(0,0,false);
    workspace.setFreeEditMode;
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    uiAdapter.paintAll;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditPaletteClick(Sender: TObject);
  VAR
    timerEnabledBefore: boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;

    PaletteForm.showFor(workspace.getWorkspacePalette);
    workspace.activePalette^.detachUI;
    workspace.activePalette^.attachUI(@uiAdapter);
    updateUiElements;
    uiAdapter.resetState;

    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miFullScreenClick(Sender: TObject);
  begin
    if miFullScreen.checked then begin
      WindowState:=wsMaximized;
      BorderStyle:=bsSizeable;
    end else begin
      WindowState:=wsFullScreen;
      BorderStyle:=bsNone;
    end;
    miFullScreen.checked:=not(miFullScreen.checked);
  end;

PROCEDURE TDigitaltrainerMainForm.miGoBackClick(Sender: TObject);
  VAR
    challenge: P_challenge;
    originalChallengeIndex: longint;
  begin
    if workspace.canGoBack and ((workspace.state in [editingChallengeSolution,editingChallengeTemplate]) or continueWithOtherBoard) then begin
      workspace.goBack(@uiAdapter,challenge,originalChallengeIndex);
      if challenge<>nil then CreateTaskForm.reShowFor(challenge,originalChallengeIndex,workspace.getChallenges);
    end;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miMarkChallengesUnsolvedClick(Sender: TObject);
  begin
    workspace.getChallenges^.markAllAsPending;
  end;

PROCEDURE TDigitaltrainerMainForm.miNewBoardClick(Sender: TObject);
  VAR fullInit:boolean;
  begin
    if not(continueWithOtherBoard) then exit;

    fullInit:=workspace.getActiveChallenge<>nil;
    if fullInit then workspace.setFreeEditMode;
                     workspace.clearBoard(@uiAdapter);
    if fullInit then workspace.activePalette^.attachUI(@uiAdapter)
                else begin
                       workspace.activePalette^.ensureVisualPaletteItems;
                       workspace.activePalette^.checkSizes;
                     end;
    if fullInit then workspace.activeBoard^.attachUI(@uiAdapter);
    uiAdapter.paintAll;
    infoLabel.color:=clNone;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miPasteClick(Sender: TObject);
  begin
    workspace.activeBoard^.pasteFromClipboard(
      round((mouse.CursorPos.X-Left-boardImage.Left+boardHorizontalScrollBar.position)/uiAdapter.getZoom),
      round((mouse.CursorPos.Y-top -boardImage.top +boardVerticalScrollBar  .position)/uiAdapter.getZoom));
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.miRedoClick(Sender: TObject);
  begin
    uiAdapter.performRedo(@workspace.setActiveBoard);
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.miSaveAsTaskClick(Sender: TObject);
  VAR
    timerEnabledBefore: boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;

    if (workspace.state in [editingNewBoard,editingPaletteEntry]) then CreateTaskForm.showForNewChallenge(workspace.activeBoard,workspace.getChallenges);
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    updateUiElements;

    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miShrinkClick(Sender: TObject);
  begin
    workspace.activeBoard^.checkBoardExtend(false,false,true);
  end;

PROCEDURE TDigitaltrainerMainForm.miSimpleUIClick(Sender: TObject);
  begin
    workspace.simplisticUi:=not(workspace.simplisticUi);
    miSimpleUI.checked:=workspace.simplisticUi;
    if workspace.simplisticUi then begin
      pauseByUser:=false;
      speedTrackBar.position:=6;
    end;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miStartNextUnsolvedTaskClick(Sender: TObject);
  begin
    workspace.startChallenge(workspace.nextUncompletedChallenge);
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    uiAdapter.paintAll;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miTasksClick(Sender: TObject);
  VAR
    timerEnabledBefore: boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;

    if SelectTaskForm.startTaskAfterShowing(workspace.getChallenges) and
       continueWithOtherBoard and
       workspace.startChallenge(SelectTaskForm.selectedChallengeIndex) then begin
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.reset(true);
      uiAdapter.paintAll;
    end;
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    updateUiElements;

    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miTestBoardClick(Sender: TObject);
  VAR
    timerEnabledBefore: boolean;
  begin
    if workspace.getActiveChallenge<>nil
    then begin
      TestShapeMouseDown(Sender,mbRight,[],0,0)
    end else begin
      timerEnabledBefore:=SimulationTimer.enabled;
      SimulationTimer.enabled:=false;

      BoardTestForm.showForBoard(workspace.activeBoard);

      SimulationTimer.enabled:=timerEnabledBefore;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miUndoClick(Sender: TObject);
  begin
    uiAdapter.performUndo(@workspace.setActiveBoard);
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.miZoomInClick(Sender: TObject);
  begin
    ZoomInShapeMouseDown(Sender,mbLeft,[],0,0);
  end;

PROCEDURE TDigitaltrainerMainForm.miZoomOutClick(Sender: TObject);
  begin
    ZoomOutShapeMouseDown(Sender,mbLeft,[],0,0);
  end;

PROCEDURE TDigitaltrainerMainForm.PaletteScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
  VAR g:P_visualGate;
  begin
    workspace.activePalette^.paint;
    if uiAdapter.getState=uas_propertyEditFromPalette then begin
      g:=uiAdapter.draggedGate;
      repositionPropertyEditor(
        g^.canvasPos[0]+g^.getGridWidth*uiAdapter.getZoom+boardImage.Left,
        g^.canvasPos[1]                                  +boardImage.top,false);
    end;
    boardImage.repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.PlayPauseShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(PlayPauseShape);
    SimulationTimer.enabled:=not(SimulationTimer.enabled);
    if SimulationTimer.enabled
    then pauseByUser:=false
    else pauseByUser:=true;
    PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
  end;

PROCEDURE TDigitaltrainerMainForm.propCancelShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(propCancelShape);
    ValueListEditor1.OnValidateEntry:=nil;
    gateProperties.destroy;
    propEditPanel.visible:=false;
    uiAdapter.resetState;
  end;

PROCEDURE TDigitaltrainerMainForm.propDeleteButtonMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(propDeleteButton);
    ValueListEditor1.OnValidateEntry:=nil;
    if gateProperties.arePropertiesForBoard
    then workspace.activeBoard^.remove(uiAdapter.draggedGate,false)
    else begin
      addBackup(@workspace,wht_beforeDeletingEntry);
      P_workspacePalette(workspace.activePalette)^.deleteEntry(P_compoundGate(uiAdapter.draggedGate^.getBehavior)^.prototype);
    end;
    gateProperties.destroy;
    propEditPanel.visible:=false;
    uiAdapter.resetState;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.propEditShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR prototype:P_visualBoard;
  begin
    prototype:=P_visualBoard(P_compoundGate(uiAdapter.draggedGate^.getBehavior)^.prototype);

    buttonClicked(propEditShape);
    ValueListEditor1.OnValidateEntry:=nil;
    if continueWithOtherBoard
    then begin
      AddToPaletteForm.setSubpalette(workspace.activePalette^.lastSubPaletteIndex);
      workspace.editPaletteEntry(prototype,@uiAdapter);
      updateUiElements;
    end;
    gateProperties.destroy;
    propEditPanel.visible:=false;
    uiAdapter.resetState;
  end;

PROCEDURE TDigitaltrainerMainForm.propOkShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(propOkShape);
    propEditPanel.visible:=false;
    if gateProperties.applyValues then begin
      uiAdapter.draggedGate^.propertyEditDone;
      workspace.activeBoard^.afterGatePropertiesEdited(uiAdapter.draggedGate,gateProperties.arePropertiesForBoard);
      if not(gateProperties.arePropertiesForBoard) then begin
        workspace.activePalette^.ensureVisualPaletteItems;
        workspace.activePalette^.checkSizes;
      end;
    end;
    ValueListEditor1.OnValidateEntry:=nil;
    gateProperties.destroy;
    uiAdapter.resetState;
    uiAdapter.paintAll;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.ResetShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ResetShape);
    workspace.activeBoard^.reset;
    if workspace.getActiveChallenge<>nil then workspace.getActiveChallenge^.expectedBehavior^.reset;
    uiAdapter.paintAll;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.selectionShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    uiAdapter.endSelectionDrag;
  end;

CONST SPEED_SETTING:array[0..34] of record
        timerInterval,
        simSteps:longint;
        labelCaption:string;
      end=((timerInterval:1000; simSteps:   1; labelCaption:'1.00Hz'),
           (timerInterval: 707; simSteps:   1; labelCaption:'1.41Hz'),
           (timerInterval: 500; simSteps:   1; labelCaption:'2.00Hz'),
           (timerInterval: 354; simSteps:   1; labelCaption:'2.82Hz'),
           (timerInterval: 250; simSteps:   1; labelCaption:'4.00Hz'),
           (timerInterval: 177; simSteps:   1; labelCaption:'5.65Hz'),
           (timerInterval: 125; simSteps:   1; labelCaption:'8.00Hz'),   //DEFAULT
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
           (timerInterval:1000; simSteps:maxLongint; labelCaption:'MAX!!!')); //Do whatever you can do in 1000 ms

VAR lastSimTime:qword=0;
    averageSpeed:double=8;
CONST PLANNED_IDLE_TICKS_DURING_SIMULATION=5;
PROCEDURE TDigitaltrainerMainForm.SimulationTimerTimer(Sender: TObject);
  VAR stepsSimulated: longint;
      stepsToSimulate, timeForSimlulation:longint;
      elapsed, speed: qword;
  begin
    if (uiAdapter.getState<>uas_initial) or (propEditPanel.visible) then exit;
    stepsToSimulate   :=SPEED_SETTING[speedTrackBar.position].simSteps;
    timeForSimlulation:=SPEED_SETTING[speedTrackBar.position].timerInterval-PLANNED_IDLE_TICKS_DURING_SIMULATION;
    if workspace.state<>solvingChallenge
    then stepsSimulated:=workspace.activeBoard^.simulateSteps  (stepsToSimulate,timeForSimlulation)
    else begin
      if workspace.getActiveChallenge^.currentlyTesting
      then begin
        stepsSimulated:=workspace.getActiveChallenge^.testStep(stepsToSimulate,timeForSimlulation,workspace.activeBoard);
        uiAdapter.paintAll;
        if not(workspace.getActiveChallenge^.currentlyTesting)
        then testFinished;
      end
      else stepsSimulated:=workspace.activeBoard^.coSimulateSteps(stepsToSimulate,timeForSimlulation,workspace.getActiveChallenge^.ensureBehavior);
    end;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);

    elapsed:=GetTickCount64-lastSimTime;
    lastSimTime:=GetTickCount64;

    if elapsed>0 then begin
      speed:=stepsSimulated*1000 div elapsed;
      if speedTrackBar.position=length(SPEED_SETTING)-1
      then averageSpeed:=speed
      else averageSpeed:=averageSpeed*0.95+speed*0.05;
      speed:=round(averageSpeed);
      if speed>1000 then begin
        speed:=speed div 1000;
        speedLabel.caption:='Speed: '+intToStr(speed)+'kHz ('+SPEED_SETTING[speedTrackBar.position].labelCaption+')';
      end else if speed>0 then begin
        speedLabel.caption:='Speed: '+intToStr(speed)+'Hz ('+SPEED_SETTING[speedTrackBar.position].labelCaption+')';
      end;
    end;

    if (stepsSimulated=0) and ((workspace.getActiveChallenge=nil) or not(workspace.getActiveChallenge^.currentlyTesting)) then begin
      SimulationTimer.enabled:=false;
      PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.speedTrackBarChange(Sender: TObject);
  begin
    SimulationTimer.interval:=SPEED_SETTING[speedTrackBar.position].timerInterval;
    speedLabel.caption:='Speed: ('+SPEED_SETTING[speedTrackBar.position].labelCaption+')';
    averageSpeed:=SPEED_SETTING[speedTrackBar.position].simSteps/SPEED_SETTING[speedTrackBar.position].timerInterval*1000;
  end;

PROCEDURE TDigitaltrainerMainForm.SubPaletteComboBoxDrawItem(control: TWinControl; index: integer; ARect: TRect; state: TOwnerDrawState);
  begin
    if not(control is TComboBox) then exit;
    SubPaletteComboBox.Canvas.FillRect(ARect);                                                 //first paint normal background
    SubPaletteComboBox.Canvas.TextRect(ARect, 5, ARect.top, SubPaletteComboBox.items[index]);  //paint item text
  end;

PROCEDURE TDigitaltrainerMainForm.SubPaletteComboBoxEditingDone(Sender: TObject);
  begin
    if speedTrackBar.visible
    then speedTrackBar.SetFocus
    else DefocusControl(SubPaletteComboBox,false);
  end;

PROCEDURE TDigitaltrainerMainForm.TestShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(TestShape);
    if workspace.getActiveChallenge<>nil then begin
      workspace.getActiveChallenge^.startTesting(workspace.activeBoard);
      SimulationTimer.enabled:=true;
      PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.WireTimerTimer(Sender: TObject);
  begin
    if (workspace.activeBoard<>nil) and (workspace.activeBoard^.wiresHaveChangedInBackground)
    then uiAdapter.paintAll;
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomInShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomInShape);
    uiAdapter.zoomIn;
    workspace.activePalette^.checkSizes;
    uiAdapter.paintAll;
    uiAdapter.repaintImage;
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomOutShape);
    uiAdapter.zoomOut;
    workspace.activePalette^.checkSizes;
    uiAdapter.paintAll;
    uiAdapter.repaintImage;
  end;

PROCEDURE TDigitaltrainerMainForm.buttonClicked(Shape: TShape);
  begin
    Buttons[Shape.Tag].colorIndex:=10;
    Shape.Brush.color:=colorScheme.buttonColorTable[10];
    if not(AnimationTimer.enabled) then AnimationTimer.enabled:=true;
  end;

PROCEDURE TDigitaltrainerMainForm.propertyValueChanged(Sender: TObject);
  begin
    setEnableButton(propOkShape,propOkLabel,true);
  end;

PROCEDURE TDigitaltrainerMainForm.repositionPropertyEditor(CONST mouseX,mouseY:longint; CONST hideEditor:boolean);
  begin
    if hideEditor and propEditPanel.visible then begin
      propCancelShapeMouseDown(nil,mbLeft,[],mouseX,mouseY);
      exit;
    end;

    if mouseX>width-propEditPanel.width
    then propEditPanel.Left:=width-propEditPanel.width
    else propEditPanel.Left:=mouseX;
    if mouseY>height-propEditPanel.height
    then propEditPanel.top:=height-propEditPanel.height
    else if mouseY<boardImage.top
         then propEditPanel.top:=boardImage.top
         else propEditPanel.top:=mouseY;

    if propEditPanel.Left+propEditPanel.width>width then propEditPanel.Left:=width-propEditPanel.width;
    if propEditPanel.top+propEditPanel.height>height then propEditPanel.top:=height-propEditPanel.height;
    propEditPanel.BringToFront;
  end;

PROCEDURE TDigitaltrainerMainForm.showPropertyEditor(CONST gate: P_visualGate; CONST fromBoard: boolean; CONST mouseX, mouseY: longint);
  VAR deletionHintText:string;
  begin
    propEditPanel.visible:=true;
    repositionPropertyEditor(mouseX,mouseY,false);
    if fromBoard
    then gateProperties.createForBoardEntry  (ValueListEditor1,@propertyValueChanged,gate^.getBehavior,workspace.activePalette)
    else gateProperties.createForPaletteEntry(ValueListEditor1,@propertyValueChanged,gate^.getBehavior,workspace.activePalette);

    uiAdapter.propertyEditorShown(gate,fromBoard);
    setEnableButton(propEditShape   ,propEditLabel  ,{not(fromBoard) and} (gate^.getBehavior^.gateType=gt_compound) and (workspace.state in [editingNewBoard,editingPaletteEntry]));
    setEnableButton(propDeleteButton,propDeleteLabel,
      fromBoard or ((gate^.getBehavior^.gateType=gt_compound) and
                    (workspace.activePalette^.allowDeletion(gate^.getBehavior,deletionHintText))));
    if not(propDeleteButton.enabled) then begin
      propDeleteButton.ShowHint:=false;
      propDeleteLabel .ShowHint:=false;
      propDeleteButton.Hint:=deletionHintText;
      propDeleteLabel .Hint:=deletionHintText;
      propDeleteButton.ShowHint:=true;
      propDeleteLabel .ShowHint:=true;
    end;
    setEnableButton(propOkShape     ,propOkLabel    ,false);
  end;

PROCEDURE TDigitaltrainerMainForm.boardChanged;
  begin
    if speedTrackBar.position=speedTrackBar.max then speedTrackBar.position:=speedTrackBar.position-1;
    if not(SimulationTimer.enabled) and not(pauseByUser) then begin
      SimulationTimer.enabled:=true;
      PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
    end;
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
  end;

PROCEDURE TDigitaltrainerMainForm.testFinished;
  begin
    case TaskFinishedForm.showAfterTest(workspace.getActiveChallenge^.testSucceeded,workspace.nextUncompletedChallenge>=0) of
      fr_backToFreeEdit: miNewBoardClick(nil);
      fr_restartTask   : begin
        workspace.restartChallenge;
        workspace.activePalette^.attachUI(@uiAdapter);
        workspace.activeBoard  ^.attachUI(@uiAdapter);
        uiAdapter.paintAll;
      end;
      fr_nextTask: begin
        workspace.startChallenge(workspace.nextUncompletedChallenge);
        workspace.activePalette^.attachUI(@uiAdapter);
        workspace.activeBoard  ^.attachUI(@uiAdapter);
        uiAdapter.paintAll;
      end;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.updateUiElements;
  VAR showAll:boolean;
  begin
    miCopy .enabled:=workspace.state in [editingNewBoard,editingPaletteEntry];
    miPaste.enabled:=workspace.state in [editingNewBoard,editingPaletteEntry];
    //TODO: Undo/Redo functions would be nice for challenges, but we would have to keep track of the gate counts in the palette.
    miUndo .enabled:=workspace.state in [editingNewBoard,editingPaletteEntry,editingChallengeSolution];
    miRedo .enabled:=workspace.state in [editingNewBoard,editingPaletteEntry,editingChallengeSolution];
    miEditMode.checked:=workspace.state in [editingNewBoard,editingPaletteEntry];
    TestShape.visible:=workspace.state in [solvingChallenge,editingChallengeTemplate];
    TestLabel.visible:=workspace.state in [solvingChallenge,editingChallengeTemplate];
    infoLabel.caption:=workspace.getInfoLabelText(uiAdapter.getState=uas_initial);
    PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
    miGoBack.enabled:=workspace.canGoBack;

    showAll:=not(workspace.simplisticUi);

    ZoomInShape       .visible:=showAll;
    ZoomInLabel       .visible:=showAll;
    ZoomOutShape      .visible:=showAll;
    ZoomOutLabel      .visible:=showAll;
    PlayPauseShape    .visible:=showAll;
    PlayPauseLabel    .visible:=showAll;
    SpeedBgShape      .visible:=showAll;
    speedLabel        .visible:=showAll;
    speedTrackBar     .visible:=showAll;
    miShrink          .visible:=showAll;
    miGoBack          .visible:=showAll or (workspace.state in [editingChallengeSolution,editingChallengeTemplate]);
    miNewBoard        .enabled:=not(workspace.state in [editingChallengeSolution,editingChallengeTemplate]);
    miViewTasks       .enabled:=not(workspace.state in [editingChallengeSolution,editingChallengeTemplate]);
    miEditMode        .enabled:=not(workspace.state in [editingChallengeSolution,editingChallengeTemplate]);
    miTestBoard       .enabled:=not(workspace.state in [editingChallengeSolution,editingChallengeTemplate]);
    miCreateTask      .visible:=showAll;
    miAddToPalette    .visible:=showAll;
  end;

FUNCTION TDigitaltrainerMainForm.continueWithOtherBoard: boolean;
  VAR mr: TModalResult;
      simulationEnabledBefore:boolean;
      challenge: P_challenge;
      originalChallengeIndex: longint;
  begin

    if not(workspace.isEditingNewChallenge) and not(workspace.activeBoard^.isModified) then exit(true);
    if    (workspace.state=solvingChallenge) then exit(true);
    simulationEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;
    mr:=boardChangedDialog.showFor(workspace.state);
    if (mr=mrYes) and
       (workspace.state=editingPaletteEntry) and
       (workspace.activeBoard^.getIndexInPalette>=0)
    then begin
      if workspace.state=editingPaletteEntry
      then P_workspacePalette(workspace.activePalette)^.updateEntry(workspace.activeBoard)
      else begin
        workspace.goBack(@uiAdapter,challenge,originalChallengeIndex);
        if challenge<>nil then CreateTaskForm.reShowFor(challenge,originalChallengeIndex,workspace.getChallenges);
      end;
    end;
    SimulationTimer.enabled:=simulationEnabledBefore;
    result:=mr<>mrCancel;
  end;

PROCEDURE TDigitaltrainerMainForm.AnimationTimerTimer(Sender: TObject);
  VAR i:longint;
      anythingDone:boolean=false;
  begin
    for i:=0 to length(Buttons)-1 do with Buttons[i] do if colorIndex>0 then begin
      anythingDone:=true;
      dec(colorIndex);
      Shape.Brush.color:=colorScheme.buttonColorTable[colorIndex];
    end;
    if not(anythingDone) then AnimationTimer.enabled:=false;
  end;

PROCEDURE TDigitaltrainerMainForm.BoardImageClick(Sender: TObject);
  begin
    if SubPaletteComboBox.Focused then DefocusControl(SubPaletteComboBox,false);
  end;

PROCEDURE TDigitaltrainerMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=not(workspace.state in [editingChallengeSolution,editingChallengeTemplate]) or continueWithOtherBoard;
  end;

end.

