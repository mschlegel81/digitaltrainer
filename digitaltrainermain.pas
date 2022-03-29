UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, Menus,baseGate;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    DeleteButton: TButton;
    captionEdit: TEdit;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    descriptionMemo: TMemo;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    SimTimer: TTimer;
    Splitter2: TSplitter;
    ToggleBoxConnectIn: TToggleBox;
    ToggleBoxConnectOut: TToggleBox;
    ToggleBoxJunctionPoint: TToggleBox;
    WireImage: TImage;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    ToggleBoxBgNAND: TToggleBox;
    ToggleBoxBgOr: TToggleBox;
    ToggleBoxBGNot: TToggleBox;
    ToggleBoxBgAND: TToggleBox;
    ToggleBoxBgNor: TToggleBox;
    ToggleBoxBgXor: TToggleBox;
    ToggleBoxBgNxor: TToggleBox;
    ZoomTrackBar: TTrackBar;
    PROCEDURE DeleteButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE SimTimerTimer(Sender: TObject);
    PROCEDURE ToggleBoxBgANDClick(Sender: TObject);
    PROCEDURE WireImageClick(Sender: TObject);
    PROCEDURE WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ZoomTrackBarChange(Sender: TObject);
  private
    workspace:T_workspace;
    toggleButtons:array of TToggleBox;
  public
  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TDigitaltrainerMainForm }

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  begin
    setLength(toggleButtons,7);
    toggleButtons[0]:=ToggleBoxBgNAND;
    toggleButtons[1]:=ToggleBoxBgOr  ;
    toggleButtons[2]:=ToggleBoxBGNot ;
    toggleButtons[3]:=ToggleBoxBgAND ;
    toggleButtons[4]:=ToggleBoxBgNor ;
    toggleButtons[5]:=ToggleBoxBgXor ;
    toggleButtons[6]:=ToggleBoxBgNxor;
    workspace.create;
    workspace.currentBoard^.attachGUI(ZoomTrackBar.position,ScrollBox1,WireImage);
  end;

PROCEDURE TDigitaltrainerMainForm.DeleteButtonClick(Sender: TObject);
  begin
    workspace.currentBoard^.deleteMarkedGate;
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  begin
    workspace.currentBoard^.Repaint;
  end;

PROCEDURE TDigitaltrainerMainForm.SimTimerTimer(Sender: TObject);
  begin
    workspace.currentBoard^.simulateStep;
  end;

PROCEDURE TDigitaltrainerMainForm.ToggleBoxBgANDClick(Sender: TObject);
  VAR otherToggle:TToggleBox;
  begin
    for otherToggle in toggleButtons do if otherToggle<>Sender then
    otherToggle.checked:=false;
  end;

PROCEDURE TDigitaltrainerMainForm.WireImageClick(Sender: TObject);
  begin

  end;

PROCEDURE TDigitaltrainerMainForm.WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR x0,y0:longint;
      toAdd:T_gateType=gt_compound;
  begin
    x0:=round(x/ZoomTrackBar.position);
    y0:=round(y/ZoomTrackBar.position);
    if ToggleBoxBGNot .checked then toAdd:=gt_notGate else
    if ToggleBoxBgAND .checked then toAdd:=gt_andGate else
    if ToggleBoxBgOr  .checked then toAdd:=gt_orGate else
    if ToggleBoxBgXor .checked then toAdd:=gt_xorGate else
    if ToggleBoxBgNAND.checked then toAdd:=gt_nandGate else
    if ToggleBoxBgNOr .checked then toAdd:=gt_norGate else
    if ToggleBoxBgNXor.checked then toAdd:=gt_nxorGate;

    workspace.addBaseGate(toAdd,x0,y0);
  end;

PROCEDURE TDigitaltrainerMainForm.ZoomTrackBarChange(Sender: TObject);
  begin
    workspace.currentBoard^.setZoom(ZoomTrackBar.position);
  end;

end.

