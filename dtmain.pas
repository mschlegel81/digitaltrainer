UNIT dtMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Menus, ValEdit, Grids, visualGates, logicalGates,
  challenges, paletteHandling, gateProperties, addToPaletteDialog, visuals,workspaces;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    BoardHorizontalScrollBar: TScrollBar;
    BoardImage: TImage;
    miRedo: TMenuItem;
    miUndo: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miEdit: TMenuItem;
    miTasks: TMenuItem;
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
    Label1: TLabel;
    miEditMode: TMenuItem;
    miNewBoard: TMenuItem;
    miSaveAsTask: TMenuItem;
    miAddToPalette: TMenuItem;
    miBoard: TMenuItem;
    miFullScreen: TMenuItem;
    miView: TMenuItem;
    PaletteScrollBar: TScrollBar;
    BoardVerticalScrollbar: TScrollBar;
    PaletteBgShape: TShape;
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
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAddToPaletteClick(Sender: TObject);
    PROCEDURE miCopyClick(Sender: TObject);
    PROCEDURE miEditModeClick(Sender: TObject);
    PROCEDURE miFullScreenClick(Sender: TObject);
    PROCEDURE miPasteClick(Sender: TObject);
    PROCEDURE miRedoClick(Sender: TObject);
    PROCEDURE miUndoClick(Sender: TObject);
    PROCEDURE PlayPauseShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE propCancelShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propDeleteButtonMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propEditShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE propOkShapeMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE ResetShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE SimulationTimerTimer(Sender: TObject);
    PROCEDURE ZoomInShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton;  Shift: TShiftState; X, Y: integer);
  private
    Buttons:array of T_shapeAndLabel;

    uiAdapter:T_uiAdapter;
    gateProperties  :T_gatePropertyValues;

    workspace:T_workspace;

    PROCEDURE buttonClicked(Shape:TShape);
    PROCEDURE startChallenge(CONST challenge:P_challenge);
    PROCEDURE propertyValueChanged(Sender: TObject);
    PROCEDURE showPropertyEditor(CONST gate:P_visualGate; CONST fromBoard:boolean; CONST mouseX,mouseY:longint);
  public

  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION

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

    uiAdapter.create(self,selectionShape,@showPropertyEditor,
                     BoardImage,BoardHorizontalScrollBar,BoardVerticalScrollbar,
                     @BeginFormUpdate,@EndFormUpdate);

    workspace.create;
    workspace.activePalette^.attachUI(PaletteBgShape,SubPaletteComboBox,PaletteScrollBar        ,@uiAdapter);
    workspace.activeBoard  ^.attachUI(BoardImage,BoardHorizontalScrollBar,BoardVerticalScrollbar,@uiAdapter);
  end;

PROCEDURE TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin
    workspace.destroy;
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.activePalette^.checkSizes;
    workspace.activeBoard  ^.checkSizes;
  end;

PROCEDURE TDigitaltrainerMainForm.miAddToPaletteClick(Sender: TObject);
  begin
    if workspace.EditorMode and AddToPaletteForm.showFor(P_workspacePalette(workspace.activePalette),workspace.activeBoard) then begin
      workspace.activeBoard^.clear;
      workspace.activeBoard^.paintWires;
      workspace.activePalette^.attachUI(PaletteBgShape,SubPaletteComboBox,PaletteScrollBar,@uiAdapter);
      workspace.activePalette^.checkSizes;
      uiAdapter.clearUndoList;
    end;
  end;

PROCEDURE TDigitaltrainerMainForm.miCopyClick(Sender: TObject);
  begin
    workspace.activeBoard^.copySelectionToClipboard;
  end;

PROCEDURE TDigitaltrainerMainForm.miEditModeClick(Sender: TObject);
  begin
    //TODO
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

PROCEDURE TDigitaltrainerMainForm.miPasteClick(Sender: TObject);
  begin
    workspace.activeBoard^.pasteFromClipboard;
  end;

PROCEDURE TDigitaltrainerMainForm.miRedoClick(Sender: TObject);
  begin
    uiAdapter.performRedo(@workspace.setActiveBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.miUndoClick(Sender: TObject);
  begin
    uiAdapter.performUndo(@workspace.setActiveBoard);
  end;

PROCEDURE TDigitaltrainerMainForm.PlayPauseShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(PlayPauseShape);
    //Todo: Change icon
    SimulationTimer.enabled:=not(SimulationTimer.enabled);
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
  gateProperties.destroy;
  propEditPanel.visible:=false;
  uiAdapter.resetState;
end;

PROCEDURE TDigitaltrainerMainForm.propEditShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  buttonClicked(propEditShape);
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
  propEditPanel.visible:=false;
  uiAdapter.resetState;
end;

PROCEDURE TDigitaltrainerMainForm.propOkShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  buttonClicked(propEditShape);
  propEditPanel.visible:=false;
  if gateProperties.applyValues then begin
    uiAdapter.draggedGate^.propertyEditDone(not(gateProperties.arePropertiesForBoard),
      BoardImage.Left-BoardHorizontalScrollBar.position,
      BoardImage.top -BoardVerticalScrollbar.position);
    workspace.activeBoard^.afterGatePropertiesEdited(uiAdapter.draggedGate);
    if not(gateProperties.arePropertiesForBoard) then begin
      workspace.activePalette^.ensureVisualPaletteItems;
      workspace.activePalette^.checkSizes;
    end;
  end;
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
  uiAdapter.resetState;
end;

PROCEDURE TDigitaltrainerMainForm.ResetShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ResetShape);
  end;

PROCEDURE TDigitaltrainerMainForm.SimulationTimerTimer(Sender: TObject);
  begin
    workspace.activeBoard^.simulateSteps(1);
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomInShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomInShape);
    uiAdapter.zoomIn;
    workspace.activePalette^.checkSizes;
    workspace.activeBoard^.checkSizes;
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomOutShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomOutShape);
    uiAdapter.zoomOut;
    workspace.activePalette^.checkSizes;
    workspace.activeBoard^.checkSizes;
  end;

PROCEDURE TDigitaltrainerMainForm.buttonClicked(Shape: TShape);
  begin
    Buttons[Shape.Tag].colorIndex:=10;
    Shape.Brush.color:=$00FF7F7F;
    if not(AnimationTimer.enabled) then AnimationTimer.enabled:=true;
  end;

PROCEDURE TDigitaltrainerMainForm.startChallenge(CONST challenge: P_challenge);
  begin
    //TODO...
    //currentChallenge:=challenge;
    //activeBoard:=challenge^.getBoard;
    //uiAdapter^.setActiveBoard(activeBoard);
    //TestShape.Visible:=true;
    //TestLabel.Visible:=true;
    //SubPaletteComboBox.Visible:=false;
  end;

PROCEDURE TDigitaltrainerMainForm.propertyValueChanged(Sender: TObject);
  begin
    setEnableButton(propOkShape,propOkLabel,true);
  end;

PROCEDURE TDigitaltrainerMainForm.showPropertyEditor(CONST gate: P_visualGate; CONST fromBoard: boolean; CONST mouseX, mouseY: longint);
  begin
    propEditPanel.visible:=true;
    propEditPanel.Left:=mouseX;
    propEditPanel.top:=mouseY;
    if propEditPanel.Left+propEditPanel.width>width then propEditPanel.Left:=width-propEditPanel.width;
    if propEditPanel.top+propEditPanel.height>height then propEditPanel.top:=height-propEditPanel.height;
    propEditPanel.BringToFront;

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

