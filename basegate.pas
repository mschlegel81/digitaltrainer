UNIT baseGate;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls,Classes,Controls,StdCtrls;
CONST GRID_SIZE=8;
      BOARD_MAX_SIZE_IN_GRID_ENTRIES=2000;
TYPE
  T_gateType=(gt_notGate,
              gt_andGate,
              gt_orGate,
              gt_xorGate,
              gt_nandGate,
              gt_norGate,
              gt_nxorGate,
              gt_input,
              gt_output,
              gt_junctionDot,
              gt_compound);
  T_valueArray=array of byte;

  P_abstractGate=^T_abstractGate;
  P_circuitBoard=^T_circuitBoard;
  { T_abstractGate }

  T_abstractGate=object
  private
    x0,y0:longint;
    zoom1Width,zoom1Height:longint;

    board:P_circuitBoard;
    //visual
    gatelabel:TLabel;
    shapes:array of TShape;
    //mouse interaction
    dragX,dragY:longint;
    dragging,marked_:boolean;
    PROCEDURE setMarked(CONST value:boolean);

  public

    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    DESTRUCTOR destroy; virtual;

    FUNCTION  caption:string;          virtual; abstract;
    FUNCTION  numberOfInputs :longint; virtual; abstract;
    FUNCTION  numberOfOutputs:longint; virtual; abstract;
    FUNCTION  gateType:T_gateType; virtual; abstract;

    PROCEDURE simulateStep;                                    virtual; abstract;
    FUNCTION  getOutput(CONST index:longint):byte;             virtual; abstract;
    PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
    FUNCTION  getInput(CONST index:longint):byte;              virtual; abstract;

    FUNCTION getInputPositionInZoom1(CONST index:longint):TPoint;
    FUNCTION getOutputPositionInZoom1(CONST index:longint):TPoint;
    PROCEDURE Repaint;

    PROPERTY marked:boolean read marked_ write setMarked;
  protected
    PROCEDURE setOutput(CONST index:longint; CONST value:byte);

  private
    PROCEDURE inputClick(Sender: TObject);
    PROCEDURE mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  end;

  { T_palette }

  { T_workspace }

  T_workspace=object
    paletteEntries:array of P_circuitBoard;
    currentBoard:P_circuitBoard;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
    PROCEDURE addCustomGate(CONST index:longint; CONST x0,y0:longint);
  end;

  { T_circuitBoard }

  T_circuitBoard=object
    GUI:record
      zoom:longint;
      container:TWinControl;
      wireImage:TImage;
    end;

    name:string;
    description:string;
    gates:array of P_abstractGate;
    logicWires:array of record
      source,sink:record
        gate:P_abstractGate;
        index:longint;
      end;
      visual:array of array of record x,y:longint; end;
    end;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION cloneAsGate(CONST x0_,y0_:longint; CONST targetBoard:P_circuitBoard):P_circuitBoard;

    PROCEDURE attachGUI(CONST zoom:longint; CONST container:TWinControl; CONST wireImage:TImage);
    PROCEDURE gateMarked(CONST marked:P_abstractGate);
    FUNCTION positionNewGate(CONST gateToAdd:P_abstractGate):boolean;
    PROCEDURE deleteMarkedGate;
  end;

  { T_notGate }

  P_notGate=^T_notGate;
  T_notGate=object(T_abstractGate)
    private
      inputState:byte;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);

      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType; virtual;

      PROCEDURE simulateStep;                                    virtual;
      FUNCTION  getOutput(CONST index:longint):byte;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
      FUNCTION  getInput(CONST index:longint):byte;              virtual;
  end;
//
//  P_inputConnector=^T_inputConnector;
//  T_inputConnector=object(T_abstractGate)
//    private
//      inputIndex:longint;
//      inputState:byte;
//    public
//      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST paintContext:T_paintContext);
//
//      FUNCTION  caption:string;          virtual;
//      FUNCTION  numberOfInputs :longint; virtual;
//      FUNCTION  numberOfOutputs:longint; virtual;
//
//      PROCEDURE simulateStep;                                    virtual;
//      FUNCTION  getOutput(CONST index:longint):byte;             virtual;
//      PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
//      FUNCTION  getInput(CONST index:longint):byte;              virtual;
//  end;
//

  { T_binaryBaseGate }

  T_binaryBaseGate=object(T_abstractGate)
    private
      inputState:array[0..1] of byte;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      PROCEDURE simulateStep; virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:byte); virtual;
      FUNCTION  getInput(CONST index:longint):byte;              virtual;
  end;

  { T_andGate }
  P_andGate=^T_andGate;
  T_andGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_orGate }
  P_orGate=^T_orGate;
  T_orGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_xorGate }
  P_xorGate=^T_xorGate;
  T_xorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_nandGate }
  P_nandGate=^T_nandGate;
  T_nandGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_norGate }
  P_norGate=^T_norGate;
  T_norGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_nxorGate }
  P_nxorGate=^T_nxorGate;
  T_nxorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    FUNCTION  getOutput(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

IMPLEMENTATION
USES math,Graphics;

{ T_workspace }

CONSTRUCTOR T_workspace.create;
  begin
    setLength(paletteEntries,0);
    new(currentBoard,create);
  end;

DESTRUCTOR T_workspace.destroy;
  begin

  end;

PROCEDURE T_workspace.addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
  VAR gateToAdd:P_abstractGate=nil;
  begin

    case gateType of
      gt_notGate : new(P_notGate (gateToAdd),create(x0,y0,@self));
      gt_andGate : new(P_andGate (gateToAdd),create(x0,y0,@self));
      gt_orGate  : new(P_orGate  (gateToAdd),create(x0,y0,@self));
      gt_xorGate : new(P_xorGate (gateToAdd),create(x0,y0,@self));
      gt_nandGate: new(P_nandGate(gateToAdd),create(x0,y0,@self));
      gt_norGate : new(P_norGate (gateToAdd),create(x0,y0,@self));
      gt_nxorGate: new(P_nxorGate(gateToAdd),create(x0,y0,@self));
      //TODO:
      //  gt_input
      //  gt_output
      //  gt_junctionDot
      //  gt_compound
    end;

    if gateToAdd<>nil then currentBoard^.positionNewGate(gateToAdd);
  end;

PROCEDURE T_workspace.addCustomGate(CONST index: longint; CONST x0, y0: longint);
  begin

  end;

{ T_circuitBoard }

CONSTRUCTOR T_circuitBoard.create;
  begin

  end;

DESTRUCTOR T_circuitBoard.destroy;
  begin

  end;

FUNCTION T_circuitBoard.cloneAsGate(CONST x0_, y0_: longint; CONST targetBoard: P_circuitBoard): P_circuitBoard;
  begin

  end;

PROCEDURE T_circuitBoard.attachGUI(CONST zoom: longint; CONST container: TWinControl; CONST wireImage: TImage);
  begin
    GUI.zoom:=zoom;
    GUI.container:=container;
    GUI.wireImage:=wireImage;
  end;

PROCEDURE T_circuitBoard.gateMarked(CONST marked: P_abstractGate);
  begin

  end;

FUNCTION T_circuitBoard.positionNewGate(CONST gateToAdd: P_abstractGate):boolean;
  FUNCTION isBoxFree(CONST x0,y0,x1,y1:longint):boolean;
    VAR i:longint;
        gate:P_abstractGate;
    begin
      result:=true;
      for gate in gates do
      if (x1>=gate^.x0) and (x0<=gate^.x0+gate^.zoom1Width) and
         (y1>=gate^.y0) and (y0<=gate^.y0+gate^.zoom1Height)
      then exit(false);
      //TODO: Check wires!
    end;

  VAR px,py,width,height,x0,y0:longint;
      radius:longint=0;
  begin
    x0:=round(gateToAdd^.x0/GRID_SIZE);
    y0:=round(gateToAdd^.y0/GRID_SIZE);
    width :=gateToAdd^.zoom1Width div GRID_SIZE;
    height:=gateToAdd^.zoom1Height div GRID_SIZE;

    repeat
      for px:=max(GRID_SIZE,x0-radius) to min(x0+radius,BOARD_MAX_SIZE_IN_GRID_ENTRIES-width-2) do
      for py:=max(GRID_SIZE,y0-radius) to min(y0+radius,BOARD_MAX_SIZE_IN_GRID_ENTRIES-height-2) do
      if max(abs(px),abs(py))=radius then begin
        if isBoxFree((px-1)*GRID_SIZE,
                     (py-1)*GRID_SIZE,
                     (px+width)*GRID_SIZE,
                     (py+height)*GRID_SIZE)
        then begin
          gateToAdd^.x0:=px*GRID_SIZE;
          gateToAdd^.y0:=py*GRID_SIZE;
          setLength(gates,length(gates)+1);
          gates[length(gates)-1]:=gateToAdd;
          exit(true);
        end;
      end;
      inc(radius);
    until radius>BOARD_MAX_SIZE_IN_GRID_ENTRIES;
    result:=false;
  end;

PROCEDURE T_circuitBoard.deleteMarkedGate;
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

{ T_nxorGate }

CONSTRUCTOR T_nxorGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_nxorGate.gateType: T_gateType;
  begin
    result:=gt_nxorGate;
  end;

{ T_norGate }

CONSTRUCTOR T_norGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_norGate.gateType: T_gateType;
  begin
    result:=gt_norGate;
  end;

{ T_nandGate }

CONSTRUCTOR T_nandGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_nandGate.gateType: T_gateType;
  begin
    result:=gt_nandGate;
  end;

{ T_xorGate }

CONSTRUCTOR T_xorGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_xorGate.gateType: T_gateType;
  begin
    result:=gt_xorGate;
  end;

{ T_orGate }

CONSTRUCTOR T_orGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_orGate.gateType: T_gateType;
  begin
    result:=gt_orGate;
  end;

{ T_andGate }

CONSTRUCTOR T_andGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
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

FUNCTION T_andGate.gateType: T_gateType;
  begin
    result:=gt_andGate;
  end;

{ T_binaryBaseGate }

CONSTRUCTOR T_binaryBaseGate.create(CONST x0_, y0_: longint; CONST board_:P_circuitBoard);
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

CONSTRUCTOR T_notGate.create(CONST x0_, y0_: longint;
  CONST board_: P_circuitBoard);
  begin
    inherited;
  end;

FUNCTION T_notGate.caption: string;
  begin
    result:='NOT';
  end;

FUNCTION T_notGate.numberOfInputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_notGate.numberOfOutputs: longint;
  begin
    result:=1;
  end;

FUNCTION T_notGate.gateType: T_gateType;
  begin
    result:=gt_notGate;
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

CONSTRUCTOR T_abstractGate.create(CONST x0_, y0_: longint; CONST board_:P_circuitBoard);
  VAR shapeIndex:longint=1;
      k,yCenter:longint;
  begin
    x0:=x0_;
    y0:=y0_;
    zoom1Width:=32;
    zoom1Height:=max(32,16*max(numberOfInputs,numberOfInputs));

    dragging:=false;
    marked  :=false;
    board   :=board_;

    if board^.GUI.container<>nil then begin
      setLength(shapes,1+numberOfInputs+numberOfOutputs);
      shapes[0]:=TShape.create(board^.GUI.container);
      shapes[0].parent:=board^.GUI.container;
      shapes[0].Shape :=stRectangle;
      shapes[0].OnMouseDown:=@mainShapeMouseDown;
      shapes[0].OnMouseMove:=@mainShapeMouseMove;
      shapes[0].OnMouseUp  :=@mainShapeMouseUp;

      gatelabel:=TLabel.create(board^.GUI.container);
      gatelabel.caption:=caption;
      gatelabel.AutoSize:=true;
      gatelabel.Font.size:=6;
      gatelabel.parent:=board^.GUI.container;
      gatelabel.OnMouseDown:=@mainShapeMouseDown;
      gatelabel.OnMouseMove:=@mainShapeMouseMove;
      gatelabel.OnMouseUp  :=@mainShapeMouseUp;

      for k:=0 to numberOfInputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnClick:=@inputClick;
        shapes[shapeIndex].parent:=board^.GUI.container;
        inc(shapeIndex);
      end;

      for k:=0 to numberOfOutputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].parent:=board^.GUI.container;
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
    result.Y:=(index*GRID_SIZE*2-(numberOfInputs-1)*GRID_SIZE)+zoom1Height div 2+y0;
  end;

FUNCTION T_abstractGate.getOutputPositionInZoom1(CONST index: longint): TPoint;
  begin
    result.X:=x0+zoom1Width;
    result.Y:=(index*GRID_SIZE*2-(numberOfOutputs-1)*GRID_SIZE)+zoom1Height div 2+y0;
  end;

PROCEDURE T_abstractGate.Repaint;
  VAR shape:TShape;
      k,yCenter,
      newFontSize:longint;
      shapeIndex :longint=1;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=x0*board^.GUI.zoom;
    shapes[0].top   :=y0*board^.GUI.zoom;
    shapes[0].width :=zoom1Width*board^.GUI.zoom;
    shapes[0].height:=zoom1Height*board^.GUI.zoom;

    gatelabel.top :=shapes[0].top +(shapes[0].height-gateLabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gateLabel.width) div 2 ;
    newFontSize:=round(gatelabel.Font.size*shapes[0].width*0.75/gateLabel.width);
    if abs(newFontSize-gatelabel.Font.size)>1 then gatelabel.Font.size:=newFontSize;
    gatelabel.top :=shapes[0].top +(shapes[0].height-gateLabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gateLabel.width) div 2 ;

    for k:=0 to numberOfInputs-1 do begin
      shapes[shapeIndex].Left:=(x0-(GRID_SIZE shr 1))*board^.GUI.zoom;
      yCenter:=(k*GRID_SIZE*2-(numberOfInputs-1)*GRID_SIZE)*board^.GUI.zoom+shapes[0].height div 2+y0*board^.GUI.zoom;
      shapes[shapeIndex].top:=yCenter-(GRID_SIZE shr 1)*board^.GUI.zoom;
      shapes[shapeIndex].width :=GRID_SIZE*board^.GUI.zoom;
      shapes[shapeIndex].height:=GRID_SIZE*board^.GUI.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      shapes[shapeIndex].Left:=shapes[0].width+(x0-(GRID_SIZE shr 1))*board^.GUI.zoom;
      yCenter:=(k*GRID_SIZE*2-(numberOfOutputs-1)*GRID_SIZE)*board^.GUI.zoom+shapes[0].height div 2+y0*board^.GUI.zoom;
      shapes[shapeIndex].top:=yCenter-(GRID_SIZE shr 1)*board^.GUI.zoom;
      shapes[shapeIndex].width :=GRID_SIZE*board^.GUI.zoom;
      shapes[shapeIndex].height:=GRID_SIZE*board^.GUI.zoom;
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_abstractGate.setOutput(CONST index: longint; CONST value: byte);
  begin
    if length(shapes)>0 then begin
      if odd(value)
      then shapes[1+numberOfInputs+index].Brush.color:=clGreen
      else shapes[1+numberOfInputs+index].Brush.color:=clWhite;
    end;
  end;

PROCEDURE T_abstractGate.setMarked(CONST value:boolean);
  begin
    if marked_=value then exit;
    marked_:=value;
    if length(shapes)=0 then exit;
    if marked
    then shapes[0].Brush.color:=clYellow
    else shapes[0].Brush.color:=clWhite;
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
      board^.gateMarked(@self);
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
      newX0:=x0+round(dx/(GRID_SIZE*board^.GUI.zoom))*GRID_SIZE; if newX0<0 then newX0:=0;
      newY0:=y0+round(dy/(GRID_SIZE*board^.GUI.zoom))*GRID_SIZE; if newY0<0 then newY0:=0;
      if (newX0<>x0) or (newY0<>y0)
      then begin
        x0:=newX0;
        y0:=newY0;
        Repaint;
      end;
    end;
  end;

PROCEDURE T_abstractGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=false;
      shapes[0].Pen.style:=psSolid;
      Repaint;
    end;
  end;

end.

