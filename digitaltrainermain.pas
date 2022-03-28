UNIT digitaltrainerMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons,baseGate;

TYPE

  { TDigitaltrainerMainForm }

  TDigitaltrainerMainForm = class(TForm)
    DeleteButton: TButton;
    FlowPanel1: TFlowPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SimTimer: TTimer;
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
    gates:array of P_abstractGate;
    toggleButtons:array of TToggleBox;
    paintContext:T_paintContext;
    PROCEDURE gateMarked(CONST gate:P_abstractGate);
  public
  end;

VAR
  DigitaltrainerMainForm: TDigitaltrainerMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TDigitaltrainerMainForm }

PROCEDURE TDigitaltrainerMainForm.FormCreate(Sender: TObject);
  begin
    paintContext.container:=ScrollBox1;
    paintContext.zoom:=3;
    paintContext.gateMarked:=@gateMarked;
    setLength(toggleButtons,7);
    toggleButtons[0]:=ToggleBoxBgNAND;
    toggleButtons[1]:=ToggleBoxBgOr  ;
    toggleButtons[2]:=ToggleBoxBGNot ;
    toggleButtons[3]:=ToggleBoxBgAND ;
    toggleButtons[4]:=ToggleBoxBgNor ;
    toggleButtons[5]:=ToggleBoxBgXor ;
    toggleButtons[6]:=ToggleBoxBgNxor;
  end;

PROCEDURE TDigitaltrainerMainForm.DeleteButtonClick(Sender: TObject);
  VAR i:longint;
      j:longint=0;
  begin
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.marked
      then dispose(gates[i],destroy)
      else begin
        gates[j]:=gates[i];
        inc(j);
      end;
    end;
    setLength(gates,j);
  end;

PROCEDURE TDigitaltrainerMainForm.FormResize(Sender: TObject);
  VAR gate:P_abstractGate;
  begin
    for gate in gates do gate^.Repaint(paintContext);
  end;

PROCEDURE TDigitaltrainerMainForm.SimTimerTimer(Sender: TObject);
  VAR gate:P_abstractGate;
  begin
    for gate in gates do begin
      gate^.simulateStep;
    end;
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
      newGate:P_abstractGate=nil;
  begin
    x0:=round(x/paintContext.zoom/8)*paintContext.zoom;
    y0:=round(y/paintContext.zoom/8)*paintContext.zoom;

    if ToggleBoxBGNot .checked then new(P_notGate (newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgAND .checked then new(P_andGate (newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgOr  .checked then new(P_orGate  (newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgXor .checked then new(P_xorGate (newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgNAND.checked then new(P_nandGate(newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgNOr .checked then new(P_norGate (newGate),create(x0,y0,paintContext)) else
    if ToggleBoxBgNXor.checked then new(P_nxorGate(newGate),create(x0,y0,paintContext));

    if newGate<>nil then begin
      setLength(gates,length(gates)+1);
      gates[length(gates)-1]:=newGate;
      newGate^.Repaint(paintContext);
    end;

  end;

PROCEDURE TDigitaltrainerMainForm.ZoomTrackBarChange(Sender: TObject);
  VAR gate:P_abstractGate;
  begin
    paintContext.zoom:=ZoomTrackBar.position;
    for gate in gates do gate^.Repaint(paintContext);
  end;

PROCEDURE TDigitaltrainerMainForm.gateMarked(CONST gate: P_abstractGate);
  VAR otherGate:P_abstractGate;
  begin
    for otherGate in gates do if (otherGate<>gate) then otherGate^.marked:=false;
  end;

end.

