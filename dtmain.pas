UNIT dtMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, Menus, ValEdit,visualGates,logicalGates,challenges,paletteHandling,
  gateProperties;

TYPE
  T_shapeAndLabel=record colorIndex:byte; Shape:TShape; labl:TLabel; end;

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    BoardHorizontalScrollBar: TScrollBar;
    BoardImage: TImage;
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
    PROCEDURE BoardVerticalScrollbarChange(Sender: TObject);
    PROCEDURE BoardVerticalScrollbarScroll(Sender: TObject;
      ScrollCode: TScrollCode; VAR ScrollPos: integer);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miEditModeClick(Sender: TObject);
    PROCEDURE miFullScreenClick(Sender: TObject);
    PROCEDURE PlayPauseShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure propCancelShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure propDeleteButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure propEditShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure propOkShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    PROCEDURE ResetShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE SimulationTimerTimer(Sender: TObject);
    PROCEDURE ZoomInShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ZoomOutShapeMouseDown(Sender: TObject; button: TMouseButton;  Shift: TShiftState; X, Y: integer);
  private
    Buttons:array of T_shapeAndLabel;

    uiAdapter:T_uiAdapter;

    activeBoard     :P_visualBoard;
    currentChallenge:P_challenge;
    currentPalette  :P_palette;
    gateProperties  :T_gatePropertyValues;

    PROCEDURE buttonClicked(Shape:TShape);
    PROCEDURE setEnableButton(Shape:TShape; CONST enable:boolean);
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

procedure TDigitaltrainerMainForm.FormCreate(Sender: TObject);
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


    uiAdapter.create(self,@showPropertyEditor);

    new(P_workspacePalette(currentPalette),create);
    P_workspacePalette(currentPalette)^.initDefaults;
    currentPalette^.attachUI(PaletteBgShape,SubPaletteComboBox,PaletteScrollBar,@uiAdapter);

    new(activeBoard,create(currentPalette));
    activeBoard^.attachUI(BoardImage,BoardHorizontalScrollBar,BoardVerticalScrollbar,@uiAdapter);

    currentChallenge:=nil;
  end;

procedure TDigitaltrainerMainForm.FormDestroy(Sender: TObject);
  begin

  end;

procedure TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    currentPalette^.checkSizes;
    activeBoard^.checkSizes;
  end;

procedure TDigitaltrainerMainForm.miEditModeClick(Sender: TObject);
  begin

  end;

procedure TDigitaltrainerMainForm.miFullScreenClick(Sender: TObject);
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

procedure TDigitaltrainerMainForm.PlayPauseShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(PlayPauseShape);
    //Todo: Change icon
    SimulationTimer.Enabled:=not(SimulationTimer.Enabled);
  end;

procedure TDigitaltrainerMainForm.propCancelShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  buttonClicked(propCancelShape);
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
  propEditPanel.Visible:=false;
end;

procedure TDigitaltrainerMainForm.propDeleteButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  buttonClicked(propDeleteButton);
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
  propEditPanel.Visible:=false;
end;

procedure TDigitaltrainerMainForm.propEditShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  buttonClicked(propEditShape);
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
  propEditPanel.Visible:=false;
end;

procedure TDigitaltrainerMainForm.propOkShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  buttonClicked(propEditShape);
  propEditPanel.Visible:=false;
  if gateProperties.applyValues then begin
    uiAdapter.draggedGate^.propertyEditDone(not(gateProperties.arePropertiesForBoard),
      BoardImage.Left-BoardHorizontalScrollBar.Position,
      BoardImage.Top -BoardVerticalScrollbar.Position);
    activeBoard^.afterGatePropertiesEdited(uiAdapter.draggedGate);
    if not(gateProperties.arePropertiesForBoard) then currentPalette^.ensureVisualPaletteItems;
  end;
  ValueListEditor1.OnValidateEntry:=nil;
  gateProperties.destroy;
end;

procedure TDigitaltrainerMainForm.ResetShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ResetShape);
  end;

procedure TDigitaltrainerMainForm.SimulationTimerTimer(Sender: TObject);
  begin
    activeBoard^.simulateSteps(1);
  end;


procedure TDigitaltrainerMainForm.ZoomInShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomInShape);
    uiAdapter.zoomIn;
    currentPalette^.checkSizes;
    activeBoard^.checkSizes;
  end;

procedure TDigitaltrainerMainForm.ZoomOutShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    buttonClicked(ZoomOutShape);
    uiAdapter.zoomOut;
    currentPalette^.checkSizes;
    activeBoard^.checkSizes;
  end;

procedure TDigitaltrainerMainForm.buttonClicked(Shape: TShape);
  begin
    Buttons[Shape.Tag].colorIndex:=10;
    Shape.Brush.color:=$00FF7F7F;
    if not(AnimationTimer.enabled) then AnimationTimer.enabled:=true;
  end;

procedure TDigitaltrainerMainForm.setEnableButton(Shape: TShape; const enable: boolean);
  VAR labl:TLabel;
  begin
    labl:=Buttons[shape.Tag].labl;
    shape.Enabled:=enable;
    labl .Enabled:=enable;
    if enable then begin
      Shape.Brush.color:=$00603030;
      labl.Font.Color:=clWhite;
    end else begin
      Shape.Brush.color:=$00703838;
      labl.Font.Color:=clSilver;
    end;
  end;

procedure TDigitaltrainerMainForm.startChallenge(const challenge: P_challenge);
  begin
    //currentChallenge:=challenge;
    //activeBoard:=challenge^.getBoard;
    //uiAdapter^.setActiveBoard(activeBoard);
    //TestShape.Visible:=true;
    //TestLabel.Visible:=true;
    //SubPaletteComboBox.Visible:=false;
  end;

procedure TDigitaltrainerMainForm.propertyValueChanged(Sender: TObject);
  begin
    setEnableButton(propOkShape,true);
  end;

procedure TDigitaltrainerMainForm.showPropertyEditor(const gate: P_visualGate;
  const fromBoard: boolean; const mouseX, mouseY: longint);
  begin
    propEditPanel.Visible:=true;
    propEditPanel.Left:=mouseX;
    propEditPanel.top:=mouseY;
    if propEditPanel.Left+propEditPanel.Width>width then propEditPanel.Left:=Width-propEditPanel.Width;
    if propEditPanel.Top+propEditPanel.Height>Height then propEditPanel.Top:=Height-propEditPanel.Height;
    propEditPanel.BringToFront;

    if fromBoard
    then gateProperties.createForBoardEntry  (ValueListEditor1,@propertyValueChanged,gate^.getBehavior)
    else gateProperties.createForPaletteEntry(ValueListEditor1,@propertyValueChanged,gate^.getBehavior,currentPalette);

    uiAdapter.propertyEditorShown(gate,fromBoard);
    setEnableButton(propEditShape   ,not(fromBoard) and (gate^.getBehavior^.gateType=gt_compound));
    setEnableButton(propDeleteButton,fromBoard or (gate^.getBehavior^.gateType=gt_compound));
    setEnableButton(propOkShape     ,false);
  end;

procedure TDigitaltrainerMainForm.AnimationTimerTimer(Sender: TObject);
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

procedure TDigitaltrainerMainForm.BoardVerticalScrollbarChange(Sender: TObject);
begin

end;

procedure TDigitaltrainerMainForm.BoardVerticalScrollbarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
begin

end;

end.

