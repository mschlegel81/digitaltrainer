UNIT baseGate;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls,Classes,Controls,StdCtrls;
TYPE
  T_valueArray=array of byte;

  P_abstractGate=^T_abstractGate;
  F_gateMarked=PROCEDURE(CONST gate:P_abstractGate) of object;
  T_paintContext=record
    zoom:longint;
    container:TWinControl;
    gateMarked:F_gateMarked;
  end;

  T_contact=record
    gate:P_abstractGate;
    index:longint;
  end;

  T_wire=object
    source:T_contact;
    sinks:array of T_contact;
    points:array of longint; //always ordered: x,y,x,y,...

   // CONSTRUCTOR createFromSource(CONST source_:T_contact);
   // CONSTRUCTOR createFromSink  (CONST sink_  :T_contact);
  end;

  { T_abstractGate }

  T_abstractGate=object
    x0,y0:longint;
    zoom1Width,zoom1Height:longint;
    //visual
    lastPaintContext:T_paintContext;
    gatelabel:TLabel;
    shapes:array of TShape;
    //mouse interaction
    dragX,dragY:longint;
    dragging,marked:boolean;

    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    DESTRUCTOR destroy; virtual;

    FUNCTION  caption:string;          virtual; abstract;
    FUNCTION  numberOfInputs :longint; virtual; abstract;
    FUNCTION  numberOfOutputs:longint; virtual; abstract;

    PROCEDURE simulateStep;                                    virtual; abstract;
    FUNCTION  getOutput(CONST index:longint):byte;             virtual; abstract;
    PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
    FUNCTION  getInput(CONST index:longint):byte;              virtual; abstract;

    FUNCTION getInputPositionInZoom1(CONST index:longint):TPoint;
    FUNCTION getOutputPositionInZoom1(CONST index:longint):TPoint;
    PROCEDURE Repaint(CONST paintContext:T_paintContext);
  protected
    PROCEDURE setOutput(CONST index:longint; CONST value:byte);

  private
    PROCEDURE inputClick(Sender: TObject);
    PROCEDURE mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  end;

  { T_notGate }

  P_notGate=^T_notGate;
  T_notGate=object(T_abstractGate)
    private
      inputState:byte;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);

      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;

      PROCEDURE simulateStep;                                    virtual;
      FUNCTION  getOutput(CONST index:longint):byte;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
      FUNCTION  getInput(CONST index:longint):byte;              virtual;
  end;

  { T_binaryBaseGate }

  T_binaryBaseGate=object(T_abstractGate)
    private
      inputState:array[0..1] of byte;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      PROCEDURE simulateStep; virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
      FUNCTION  getInput(CONST index:longint):byte;              virtual;
  end;

  { T_andGate }
  P_andGate=^T_andGate;
  T_andGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

  { T_orGate }
  P_orGate=^T_orGate;
  T_orGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

  { T_xorGate }
  P_xorGate=^T_xorGate;
  T_xorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

  { T_nandGate }
  P_nandGate=^T_nandGate;
  T_nandGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

  { T_norGate }
  P_norGate=^T_norGate;
  T_norGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

  { T_nxorGate }
  P_nxorGate=^T_nxorGate;
  T_nxorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
  end;

IMPLEMENTATION
USES math,Graphics;

{ T_nxorGate }

CONSTRUCTOR T_nxorGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_nxorGate.caption: string;
  begin
    result:='NXOR';
  end;

FUNCTION T_nxorGate.getOutput(CONST index: longint): byte;
  begin
    result:=not((inputState[0] shr 1) xor
                (inputState[1] shr 1));
  end;

{ T_norGate }

CONSTRUCTOR T_norGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_norGate.caption: string;
  begin
    result:='NOR';
  end;

FUNCTION T_norGate.getOutput(CONST index: longint): byte;
  begin
    result:=not((inputState[0] shr 1) or
                (inputState[1] shr 1));
  end;

{ T_nandGate }

CONSTRUCTOR T_nandGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_nandGate.caption: string;
  begin
    result:='NAND';
  end;

FUNCTION T_nandGate.getOutput(CONST index: longint): byte;
  begin
    result:=not((inputState[0] shr 1) and
                (inputState[1] shr 1));
  end;

{ T_xorGate }

CONSTRUCTOR T_xorGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_xorGate.caption: string;
  begin
    result:='XOR';
  end;

FUNCTION T_xorGate.getOutput(CONST index: longint): byte;
  begin
    result:=(inputState[0] shr 1) xor
            (inputState[1] shr 1);
  end;

{ T_orGate }

CONSTRUCTOR T_orGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_orGate.caption: string;
  begin
    result:='OR';
  end;

FUNCTION T_orGate.getOutput(CONST index: longint): byte;
  begin
    result:=(inputState[0] shr 1) or
            (inputState[1] shr 1);
  end;

{ T_andGate }

CONSTRUCTOR T_andGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_andGate.caption: string;
  begin
    result:='AND';
  end;

FUNCTION T_andGate.getOutput(CONST index: longint): byte;
  begin
    result:=(inputState[0] shr 1) and
            (inputState[1] shr 1);
  end;

{ T_binaryBaseGate }

CONSTRUCTOR T_binaryBaseGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_binaryBaseGate.numberOfInputs: longint;
  begin
    result:=2;
  end;

FUNCTION T_binaryBaseGate.numberOfOutputs: longint;
  begin
    result:=1;
  end;

PROCEDURE T_binaryBaseGate.simulateStep;
  begin
    inputState[0]:=(inputState[0] shl 1) or (inputState[0] and 1);
    inputState[1]:=(inputState[1] shl 1) or (inputState[1] and 1);
    setOutput(0,getOutput(0));
  end;

PROCEDURE T_binaryBaseGate.setInput(CONST index: longint; CONST value: byte);
  begin
    inherited;
    inputState[index]:=(inputState[index] and 254) or value;
  end;

FUNCTION T_binaryBaseGate.getInput(CONST index: longint): byte;
  begin
    result:=inputState[index] and 1;
  end;

{ T_notGate }

CONSTRUCTOR T_notGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  begin
    inherited;
  end;

FUNCTION T_notGate.caption: string;
  begin
    result:='not';
  end;

FUNCTION T_notGate.numberOfInputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_notGate.numberOfOutputs: longint;
  begin
    result:=1;
  end;

PROCEDURE T_notGate.simulateStep;
  begin
    inputState:=(inputState shl 1) or (inputState and 1);
    setOutput(0,getOutput(0));
  end;

FUNCTION T_notGate.getOutput(CONST index: longint): byte;
  begin
    result:=not(inputState shr 1);
  end;

PROCEDURE T_notGate.setInput(CONST index: longint; CONST value: byte);
  begin
    inherited;
    inputState:=(inputState and 254) or (value and 1);
  end;

FUNCTION T_notGate.getInput(CONST index: longint): byte;
  begin
    result:=inputState and 1;
  end;

{ T_abstractGate }

CONSTRUCTOR T_abstractGate.create(CONST x0_, y0_: longint; CONST paintContext: T_paintContext);
  VAR shapeIndex:longint=1;
      k,yCenter:longint;
  begin
    x0:=x0_;
    y0:=y0_;
    zoom1Width:=32;
    zoom1Height:=max(32,16*max(numberOfInputs,numberOfInputs));

    dragging:=false;
    marked  :=false;
    lastPaintContext:=paintContext;

    if paintContext.container<>nil then begin
      setLength(shapes,1+numberOfInputs+numberOfOutputs);
      shapes[0]:=TShape.create(paintContext.container);
      shapes[0].parent:=paintContext.container;
      shapes[0].Shape :=stRectangle;
      shapes[0].OnMouseDown:=@mainShapeMouseDown;
      shapes[0].OnMouseMove:=@mainShapeMouseMove;
      shapes[0].OnMouseUp  :=@mainShapeMouseUp;

      gatelabel:=TLabel.create(paintContext.container);
      gatelabel.caption:=caption;
      gatelabel.AutoSize:=true;
      gatelabel.Font.size:=6;
      gatelabel.parent:=paintContext.container;
      gatelabel.OnMouseDown:=@mainShapeMouseDown;
      gatelabel.OnMouseMove:=@mainShapeMouseMove;
      gatelabel.OnMouseUp  :=@mainShapeMouseUp;

      for k:=0 to numberOfInputs-1 do begin
        shapes[shapeIndex]:=TShape.create(paintContext.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnClick:=@inputClick;
        shapes[shapeIndex].parent:=paintContext.container;
        inc(shapeIndex);
      end;

      for k:=0 to numberOfOutputs-1 do begin
        shapes[shapeIndex]:=TShape.create(paintContext.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].parent:=paintContext.container;
        inc(shapeIndex);
      end;
    end else setLength(shapes,0);
  end;

DESTRUCTOR T_abstractGate.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(shapes)-1 do shapes[i].free;
    gatelabel.free;
  end;

PROCEDURE T_abstractGate.setInput(CONST index: longint; CONST value: byte);
  begin
    if (length(shapes)>0) then begin
      if (value and 1)=1
      then shapes[1+index].Brush.color:=clGreen
      else shapes[1+index].Brush.color:=clWhite;
    end;
  end;

FUNCTION T_abstractGate.getInputPositionInZoom1(CONST index: longint): TPoint;
  begin
    result.X:=x0;
    result.Y:=(index*16-(numberOfInputs-1)*8)+zoom1Height div 2+y0;
  end;

FUNCTION T_abstractGate.getOutputPositionInZoom1(CONST index: longint): TPoint;
  begin
    result.X:=x0+zoom1Width;
    result.Y:=(index*16-(numberOfOutputs-1)*8)+zoom1Height div 2+y0;
  end;

PROCEDURE T_abstractGate.Repaint(CONST paintContext:T_paintContext);
  VAR shape:TShape;
      k,yCenter,
      newFontSize:longint;
      shapeIndex :longint=1;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=x0*paintContext.zoom;
    shapes[0].top   :=y0*paintContext.zoom;
    shapes[0].width :=zoom1Width*paintContext.zoom;
    shapes[0].height:=zoom1Height*paintContext.zoom;

    gatelabel.top :=shapes[0].top +(shapes[0].height-gateLabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gateLabel.width) div 2 ;
    newFontSize:=round(gatelabel.Font.size*shapes[0].width*0.75/gateLabel.width);
    if abs(newFontSize-gatelabel.Font.size)>1 then gatelabel.Font.size:=newFontSize;
    gatelabel.top :=shapes[0].top +(shapes[0].height-gateLabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gateLabel.width) div 2 ;

    for k:=0 to numberOfInputs-1 do begin
      shapes[shapeIndex].Left:=(x0-4)*paintContext.zoom;
      yCenter:=(k*16-(numberOfInputs-1)*8)*paintContext.zoom+shapes[0].height div 2+y0*paintContext.zoom;
      shapes[shapeIndex].top:=yCenter-4*paintContext.zoom;
      shapes[shapeIndex].width :=8*paintContext.zoom;
      shapes[shapeIndex].height:=8*paintContext.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      shapes[shapeIndex].Left:=shapes[0].width+(x0-4)*paintContext.zoom;
      yCenter:=(k*16-(numberOfOutputs-1)*8)*paintContext.zoom+shapes[0].height div 2+y0*paintContext.zoom;
      shapes[shapeIndex].top:=yCenter-4*paintContext.zoom;
      shapes[shapeIndex].width :=8*paintContext.zoom;
      shapes[shapeIndex].height:=8*paintContext.zoom;
      inc(shapeIndex);
    end;

    lastPaintContext:=paintContext;
  end;

PROCEDURE T_abstractGate.setOutput(CONST index: longint; CONST value: byte);
  begin
    if length(shapes)>0 then begin
      if odd(value)
      then shapes[1+numberOfInputs+index].Brush.color:=clGreen
      else shapes[1+numberOfInputs+index].Brush.color:=clWhite;
    end;
  end;

PROCEDURE T_abstractGate.inputClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=TShape(Sender).Tag;
    setInput(k,not getInput(k));
  end;

PROCEDURE T_abstractGate.mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=true;
      marked:=true;
      if lastPaintContext.gateMarked<>nil then lastPaintContext.gateMarked(@self);
      shapes[0].Pen.style:=psDash;
      dragX:=x;
      dragY:=y;
    end;
  end;

PROCEDURE T_abstractGate.mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR newX0,newY0:longint;
      dx,dy:longint;
  begin
    if dragging then begin
      dx:=x-dragX;
      dy:=y-dragY;
      newX0:=x0+round(dx/(8*lastPaintContext.zoom))*8; if newX0<0 then newX0:=0;
      newY0:=y0+round(dy/(8*lastPaintContext.zoom))*8; if newY0<0 then newY0:=0;
      if (newX0<>x0) or (newY0<>y0)
      then begin
        x0:=newX0;
        y0:=newY0;
        Repaint(lastPaintContext);
      end;
    end;
  end;

PROCEDURE T_abstractGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=false;
      shapes[0].Pen.style:=psSolid;
      Repaint(lastPaintContext);
    end;
  end;

end.

