UNIT dtMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Menus, ValEdit, Grids, visualGates, logicalGates,
  challenges, paletteHandling, gateProperties, addToPaletteDialog, visuals,workspaces,
  createTaskUnit,selectTaskUnit,taskFinishedUnit,paletteHandingUi;

TYPE
  { TDigitaltrainerMainForm }
  TDigitaltrainerMainForm = class(TForm)
    boardHorizontalScrollBar: TScrollBar;
    boardImage: TImage;
    miMarkChallengesUnsolved: TMenuItem;
    miChallengesMenu: TMenuItem;
    miExportChallenges: TMenuItem;
    miImportChallenges: TMenuItem;
    miImportAdd: TMenuItem;
    miImportOverwrite: TMenuItem;
    miViewTasks: TMenuItem;
    miEditPalette: TMenuItem;
    OpenDialog1: TOpenDialog;
    Separator1: TMenuItem;
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
    miSaveAsTask: TMenuItem;
    miAddToPalette: TMenuItem;
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
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAddToPaletteClick(Sender: TObject);
    PROCEDURE miCopyClick(Sender: TObject);
    PROCEDURE miEditModeClick(Sender: TObject);
    PROCEDURE miEditPaletteClick(Sender: TObject);
    PROCEDURE miExportChallengesClick(Sender: TObject);
    PROCEDURE miFullScreenClick(Sender: TObject);
    PROCEDURE miImportAddClick(Sender: TObject);
    PROCEDURE miImportOverwriteClick(Sender: TObject);
    PROCEDURE miMarkChallengesUnsolvedClick(Sender: TObject);
    PROCEDURE miNewBoardClick(Sender: TObject);
    PROCEDURE miPasteClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miSaveAsTaskClick(Sender: TObject);
    PROCEDURE miTasksClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
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
    PROCEDURE TestShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE WireTimerTimer(Sender: TObject);
    PROCEDURE ZoomInShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton;  Shift: TShiftState; X, Y: integer);
  private
    Buttons:array of T_shapeAndLabel;

    uiAdapter:T_uiAdapter;
    gateProperties  :T_gatePropertyValues;
    pauseByUser:boolean;
    workspace:T_workspace;

    PROCEDURE buttonClicked(Shape:TShape);
    PROCEDURE propertyValueChanged(Sender: TObject);
    PROCEDURE repositionPropertyEditor(CONST mouseX,mouseY:longint);
    PROCEDURE showPropertyEditor(CONST gate:P_visualGate; CONST fromBoard:boolean; CONST mouseX,mouseY:longint);
    PROCEDURE boardChanged;
    PROCEDURE testFinished;
    PROCEDURE updateUiElements;
  public

  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION
USES compoundGates;
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
                     @BeginFormUpdate,
                     @EndFormUpdate,
                     @Application.ProcessMessages);

    workspace.create;
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    updateUiElements;

    pauseByUser:=false;

    Application.AddOnKeyDownHandler(@FormKeyDown,false);
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.destroy;
    uiAdapter.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if Sender.ClassNameIs('TComboBox') or
       Sender.ClassNameIs('TEdit') or
       Sender.ClassNameIs('TPickListCellEditor') or
       Sender.ClassNameIs('TStringCellEditor') or
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

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  VAR timerEnabledBefore:boolean;
  begin
    timerEnabledBefore:=SimulationTimer.enabled;
    SimulationTimer.enabled:=false;
    if workspace.EditorMode and AddToPaletteForm.showFor(P_workspacePalette(workspace.activePalette),workspace.activeBoard) then begin
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activePalette^.checkSizes;
      workspace.activeBoard^.clear;
      uiAdapter.clearUndoList;
      uiAdapter.paintAll;
      infoLabel.caption:=workspace.getInfoLabelText;
    end;
    SimulationTimer.enabled:=timerEnabledBefore;
  end;

PROCEDURE TDigitaltrainerMainForm.miCopyClick(Sender: TObject);
  begin
    workspace.activeBoard^.copySelectionToClipboard;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditModeClick(Sender: TObject);
  begin
    if workspace.EditorMode then exit;
    workspace.setFreeEditMode;
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    uiAdapter.paintAll;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditPaletteClick(Sender: TObject);
  begin
    PaletteForm.showFor(workspace.getWorkspacePalette);
    workspace.activePalette^.detachUI;
    workspace.activePalette^.attachUI(@uiAdapter);
  end;

PROCEDURE TDigitaltrainerMainForm.miExportChallengesClick(Sender: TObject);
  begin
    SelectTaskForm(@workspace).showForExport(workspace.getChallenges);
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

PROCEDURE TDigitaltrainerMainForm.miImportAddClick(Sender: TObject);
  begin
    if OpenDialog1.execute
    then workspace.getChallenges^.importChallenges(OpenDialog1.fileName,false);
  end;

PROCEDURE TDigitaltrainerMainForm.miImportOverwriteClick(Sender: TObject);
  begin
    if OpenDialog1.execute then begin
      workspace.getChallenges^.importChallenges(OpenDialog1.fileName,true);
      workspace.setFreeEditMode;
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.attachUI(@uiAdapter);
      updateUiElements;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miMarkChallengesUnsolvedClick(Sender: TObject);
  begin
    workspace.getChallenges^.markAllAsPending;
  end;

PROCEDURE TDigitaltrainerMainForm.miNewBoardClick(Sender: TObject);
  begin
    workspace.setFreeEditMode;
    workspace.clearBoard(@uiAdapter);
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    uiAdapter.paintAll;
    updateUiElements;
  end;

PROCEDURE TDigitaltrainerMainForm.miPasteClick(Sender: TObject);
  begin
    workspace.activeBoard^.pasteFromClipboard(
      round((mouse.CursorPos.X-Left-boardImage.Left+boardHorizontalScrollBar.position)/uiAdapter.getZoom),
      round((mouse.CursorPos.Y-top -boardImage.top +boardVerticalScrollBar  .position)/uiAdapter.getZoom));
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.miRedoClick(Sender: TObject);
  begin
    uiAdapter.performRedo(@workspace.setActiveBoard);
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.miSaveAsTaskClick(Sender: TObject);
  begin
    if workspace.EditorMode then CreateTaskForm.showFor(workspace.activeBoard,workspace.getChallenges);
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.miTasksClick(Sender: TObject);
  begin
    if SelectTaskForm(@workspace).startTaskAfterShowing(workspace.getChallenges) and
       workspace.startChallenge(SelectTaskForm(@workspace).selectedChallengeIndex) then begin
      workspace.activePalette^.attachUI(@uiAdapter);
      workspace.activeBoard  ^.attachUI(@uiAdapter);
      uiAdapter.paintAll;
      updateUiElements;
    end;
    workspace.activePalette^.attachUI(@uiAdapter);
    workspace.activeBoard  ^.attachUI(@uiAdapter);
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.miUndoClick(Sender: TObject);
  begin
    uiAdapter.performUndo(@workspace.setActiveBoard);
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.PaletteScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
  VAR g:P_visualGate;
  begin
    workspace.activePalette^.paint;
    if uiAdapter.getState=uas_propertyEditFromPalette then begin
      g:=uiAdapter.draggedGate;
      repositionPropertyEditor(
        g^.canvasPos[0]+g^.getGridWidth*uiAdapter.getZoom+boardImage.Left,
        g^.canvasPos[1]                                  +boardImage.top);
    end;
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

PROCEDURE TDigitaltrainerMainForm.propCancelShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(propCancelShape);
    ValueListEditor1.OnValidateEntry:=nil;
    gateProperties.destroy;
    propEditPanel.visible:=false;
    uiAdapter.resetState;
  end;

PROCEDURE TDigitaltrainerMainForm.propDeleteButtonMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(propDeleteButton);
    ValueListEditor1.OnValidateEntry:=nil;
    if gateProperties.arePropertiesForBoard
    then workspace.activeBoard^.remove(uiAdapter.draggedGate,false)
    else P_workspacePalette(workspace.activePalette)^.deleteEntry(P_compoundGate(uiAdapter.draggedGate^.getBehavior)^.prototype);
    gateProperties.destroy;
    propEditPanel.visible:=false;
    uiAdapter.resetState;
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.propEditShapeMouseDown(Sender: TObject;button: TMouseButton; Shift: TShiftState; X, Y: integer);  begin

    buttonClicked(propEditShape);
    ValueListEditor1.OnValidateEntry:=nil;
    if not(gateProperties.arePropertiesForBoard)
    then begin
      AddToPaletteForm.setSubpalette(workspace.activePalette^.lastSubPaletteIndex);

      workspace.editPaletteEntry(P_visualBoard(P_compoundGate(uiAdapter.draggedGate^.getBehavior)^.prototype),@uiAdapter);
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
      uiAdapter.draggedGate^.propertyEditDone(not(gateProperties.arePropertiesForBoard),
        boardImage.Left-boardHorizontalScrollBar.position,
        boardImage.top -boardVerticalScrollBar.position);
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
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.ResetShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ResetShape);
    workspace.activeBoard^.reset;
    if workspace.getActiveChallenge<>nil then workspace.getActiveChallenge^.expectedBehavior^.reset;
    uiAdapter.paintAll;
    infoLabel.caption:=workspace.getInfoLabelText;
  end;

PROCEDURE TDigitaltrainerMainForm.selectionShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
           (timerInterval:  40; simSteps:5243; labelCaption:'131.1kHz'));

VAR lastSimTime:qword=0;
    averageSpeed:double=8;
PROCEDURE TDigitaltrainerMainForm.SimulationTimerTimer(Sender: TObject);
  VAR stepsSimulated: longint;
      stepsToSimulate:longint;
      elapsed, speed: qword;
  begin
    if uiAdapter.getState<>uas_initial then exit;
    stepsToSimulate:=SPEED_SETTING[speedTrackBar.position].simSteps;
    if workspace.EditorMode
    then stepsSimulated:=workspace.activeBoard^.simulateSteps  (stepsToSimulate,SPEED_SETTING[speedTrackBar.position].timerInterval)
    else begin
      if workspace.getActiveChallenge^.currentlyTesting
      then begin
        stepsSimulated:=workspace.getActiveChallenge^.testStep(stepsToSimulate,SPEED_SETTING[speedTrackBar.position].timerInterval,workspace.activeBoard);
        uiAdapter.paintAll;
        if not(workspace.getActiveChallenge^.currentlyTesting)
        then testFinished;
      end
      else stepsSimulated:=workspace.activeBoard^.coSimulateSteps(stepsToSimulate,SPEED_SETTING[speedTrackBar.position].timerInterval,workspace.getActiveChallenge^.expectedBehavior);
    end;
    infoLabel.caption:=workspace.getInfoLabelText;

    elapsed:=GetTickCount64-lastSimTime;
    lastSimTime:=GetTickCount64;

    if elapsed>0 then begin
      speed:=stepsSimulated*1000 div elapsed;
      averageSpeed:=averageSpeed*0.95+speed*0.05;
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
    speedLabel.caption:='Speed: '+SPEED_SETTING[speedTrackBar.position].labelCaption;
    averageSpeed:=SPEED_SETTING[speedTrackBar.position].simSteps/SPEED_SETTING[speedTrackBar.position].timerInterval*1000;
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
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomOutShape);
    uiAdapter.zoomOut;
    workspace.activePalette^.checkSizes;
    uiAdapter.paintAll;
  end;

PROCEDURE TDigitaltrainerMainForm.buttonClicked(Shape: TShape);
  begin
    Buttons[Shape.Tag].colorIndex:=10;
    Shape.Brush.color:=$00FF7F7F;
    if not(AnimationTimer.enabled) then AnimationTimer.enabled:=true;
  end;

PROCEDURE TDigitaltrainerMainForm.propertyValueChanged(Sender: TObject);
  begin
    setEnableButton(propOkShape,propOkLabel,true);
  end;

PROCEDURE TDigitaltrainerMainForm.repositionPropertyEditor(CONST mouseX,mouseY:longint);
  begin
    if mouseX>width-propEditPanel.width
    then propEditPanel.Left:=width-propEditPanel.width
    else propEditPanel.Left:=mouseX;
    if mouseY>height-propEditPanel.height
    then propEditPanel.top:=height-propEditPanel.height
    else propEditPanel.top:=mouseY;
    if propEditPanel.Left+propEditPanel.width>width then propEditPanel.Left:=width-propEditPanel.width;
    if propEditPanel.top+propEditPanel.height>height then propEditPanel.top:=height-propEditPanel.height;
    propEditPanel.BringToFront;
  end;

PROCEDURE TDigitaltrainerMainForm.showPropertyEditor(CONST gate: P_visualGate; CONST fromBoard: boolean; CONST mouseX, mouseY: longint);
  begin
    propEditPanel.visible:=true;
    repositionPropertyEditor(mouseX,mouseY);
    if fromBoard
    then gateProperties.createForBoardEntry  (ValueListEditor1,@propertyValueChanged,gate^.getBehavior)
    else gateProperties.createForPaletteEntry(ValueListEditor1,@propertyValueChanged,gate^.getBehavior,workspace.activePalette);

    uiAdapter.propertyEditorShown(gate,fromBoard);
    setEnableButton(propEditShape   ,propEditLabel  ,not(fromBoard) and (gate^.getBehavior^.gateType=gt_compound) and workspace.EditorMode);
    setEnableButton(propDeleteButton,propDeleteLabel,
      fromBoard or ((gate^.getBehavior^.gateType=gt_compound) and
                    (workspace.activeBoard^.getIndexInPalette<0) and
                    (workspace.activePalette^.allowDeletion(gate^.getBehavior))));
    setEnableButton(propOkShape     ,propOkLabel    ,false);
  end;

PROCEDURE TDigitaltrainerMainForm.boardChanged;
  begin
    if not(SimulationTimer.enabled) and not(pauseByUser) then begin
      SimulationTimer.enabled:=true;
      PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
    end;
    infoLabel.caption:=workspace.getInfoLabelText;
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
  begin
    miCopy.enabled:=workspace.EditorMode;
    miPaste.enabled:=workspace.EditorMode;
    miEditMode.checked:=workspace.EditorMode;
    setEnableButton(TestShape,TestLabel,not(workspace.EditorMode));
    infoLabel.caption:=workspace.getInfoLabelText;
    PlayPauseLabel.caption:=playPauseGlyph[SimulationTimer.enabled];
  end;

PROCEDURE TDigitaltrainerMainForm.AnimationTimerTimer(Sender: TObject);
  CONST buttonColorTable:array[0..9] of longint=($00603030,$00703838,$00804040,$00904848,$00A05050,$00B05858,$00BF5F5F,$00CF6767,$00DF6F6F,$00EF7777);
  VAR i:longint;
      anythingDone:boolean=false;
  begin
    for i:=0 to length(Buttons)-1 do with Buttons[i] do if colorIndex>0 then begin
      anythingDone:=true;
      dec(colorIndex);
      Shape.Brush.color:=buttonColorTable[colorIndex];
    end;
    if not(anythingDone) then AnimationTimer.enabled:=false;
  end;

end.

