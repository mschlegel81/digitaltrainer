UNIT baseGate;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls,Classes,Controls,StdCtrls,wiringUtil,serializationUtil;

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
              gt_compound);
  T_valueArray=array of byte;

  P_abstractGate=^T_abstractGate;
  P_circuitBoard=^T_circuitBoard;
  { T_abstractGate }

  T_abstractGate=object(T_serializable)
  private
    origin,size:T_point;
    board:P_circuitBoard;
    //visual
    gatelabel:TLabel;
    shapes:array of TShape;
    //mouse interaction
    dragX,dragY:longint;
    dragging:boolean;
    wireDragOutputIndex:longint;
    marked_:boolean;
    PROCEDURE setMarked(CONST value:boolean);

  public

    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    DESTRUCTOR destroy; virtual;

    FUNCTION  caption:string;          virtual; abstract;
    FUNCTION  numberOfInputs :longint; virtual; abstract;
    FUNCTION  numberOfOutputs:longint; virtual; abstract;
    FUNCTION  gateType:T_gateType; virtual; abstract;

    PROCEDURE simulateStep;                                       virtual; abstract;
    FUNCTION  getOutput(CONST index:longint):boolean;             virtual; abstract;
    PROCEDURE setInput(CONST index:longint; CONST value:boolean); virtual;
    FUNCTION  getInput(CONST index:longint):boolean;              virtual; abstract;

    FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
    FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;
    PROCEDURE Repaint; virtual;

    PROPERTY marked:boolean read marked_ write setMarked;
  protected
    PROCEDURE setOutput(CONST index:longint; CONST value:boolean);
  private
    PROCEDURE inputClick(Sender: TObject);
    PROCEDURE mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE outputMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  end;

  { T_workspace }

  T_workspace=object
    paletteEntries:array of P_circuitBoard;
    currentBoard:P_circuitBoard;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
    PROCEDURE addCustomGate(CONST index:longint; CONST x0,y0:longint);
  end;

  T_repositionOutput=(ro_positionUnchanged,ro_positionFound,ro_noPositionFound);

  T_gateConnector=record
    gate:P_abstractGate;
    index:longint;
  end;

  T_circuitBoard=object(T_abstractGate)
    private
      GUI:record
        zoom:longint;
        container:TWinControl;
        wireImage:TImage;
      end;

      name       :string;
      description:string;
      gates      :array of P_abstractGate;

      //One source can be associated with many sinks
      logicWires:array of record
        source:T_gateConnector;
        wires:array of record
          sink:T_gateConnector;
          visual: T_wirePath; //Nonpersistent!
        end;
      end;

      prototype:P_circuitBoard;
      incompleteWire:record
        dragging:boolean;
        source:T_gateConnector;
        sourcePoint:T_point;
      end;
      wireGraph:P_wireGraph;
      FUNCTION repositionGate(CONST gateToCheck:P_abstractGate):T_repositionOutput;
      FUNCTION positionNewGate(CONST gateToAdd:P_abstractGate):boolean;
      FUNCTION isInputConnected(CONST gate:P_abstractGate; CONST inputIndex:longint):boolean;
      PROCEDURE initWireGraph(CONST start: T_gateConnector; CONST includeWires:boolean=true);
      PROCEDURE drawAllWires;
      FUNCTION findWirePath(CONST start:T_gateConnector; CONST endPoint:T_point):T_wirePath;
      PROCEDURE drawTempWire(CONST targetPoint:T_point);
      PROCEDURE finishWireDrag(CONST targetPoint:T_point);
      PROCEDURE rewire;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;  virtual;
      FUNCTION cloneAsGate(CONST x0_,y0_:longint; CONST targetBoard:P_circuitBoard):P_circuitBoard;
      //Base gate behavior:
      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      PROCEDURE simulateStep;                                       virtual;
      FUNCTION  getOutput(CONST index:longint):boolean;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:boolean); virtual;
      FUNCTION  getInput(CONST index:longint):boolean;              virtual;
      PROCEDURE Repaint; virtual;
      //GUI interaction
      PROCEDURE attachGUI(CONST zoom:longint; CONST container:TWinControl; CONST wireImage:TImage);
      PROCEDURE gateMoved(CONST gate:P_abstractGate);
      PROCEDURE gateMarked(CONST markedGate:P_abstractGate);
      PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE setZoom(CONST zoom:longint);
      PROCEDURE deleteMarkedGate;
  end;

  { T_notGate }

  P_notGate=^T_notGate;
  T_notGate=object(T_abstractGate)
    private
      input,output:boolean;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);

      FUNCTION  caption:string;          virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  gateType:T_gateType; virtual;

      PROCEDURE simulateStep;                                       virtual;
      FUNCTION  getOutput(CONST index:longint):boolean;             virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:boolean); virtual;
      FUNCTION  getInput(CONST index:longint):boolean;              virtual;
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
      input:array[0..1] of boolean;
      output:boolean;
    public
      CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  getOutput(CONST index:longint):boolean; virtual;
      PROCEDURE setInput(CONST index:longint; CONST value:boolean); virtual;
      FUNCTION  getInput(CONST index:longint):boolean;              virtual;
  end;

  { T_andGate }
  P_andGate=^T_andGate;
  T_andGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_orGate }
  P_orGate=^T_orGate;
  T_orGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_xorGate }
  P_xorGate=^T_xorGate;
  T_xorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_nandGate }
  P_nandGate=^T_nandGate;
  T_nandGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_norGate }
  P_norGate=^T_norGate;
  T_norGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

  { T_nxorGate }
  P_nxorGate=^T_nxorGate;
  T_nxorGate=object(T_binaryBaseGate)
    CONSTRUCTOR create(CONST x0_,y0_:longint; CONST board_:P_circuitBoard);
    FUNCTION  caption:string; virtual;
    PROCEDURE simulateStep; virtual;
    FUNCTION  gateType:T_gateType; virtual;
  end;

IMPLEMENTATION
USES math,Graphics,myGenerics;
OPERATOR =(CONST x,y:T_gateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

{ T_workspace }

CONSTRUCTOR T_workspace.create;
  begin
    setLength(paletteEntries,0);
    new(currentBoard,create);
  end;

DESTRUCTOR T_workspace.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do dispose(paletteEntries[i],destroy);
    dispose(currentBoard,destroy);
  end;

PROCEDURE T_workspace.addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
  VAR gateToAdd:P_abstractGate=nil;
  begin
    case gateType of
      gt_notGate : new(P_notGate (gateToAdd),create(x0,y0,currentBoard));
      gt_andGate : new(P_andGate (gateToAdd),create(x0,y0,currentBoard));
      gt_orGate  : new(P_orGate  (gateToAdd),create(x0,y0,currentBoard));
      gt_xorGate : new(P_xorGate (gateToAdd),create(x0,y0,currentBoard));
      gt_nandGate: new(P_nandGate(gateToAdd),create(x0,y0,currentBoard));
      gt_norGate : new(P_norGate (gateToAdd),create(x0,y0,currentBoard));
      gt_nxorGate: new(P_nxorGate(gateToAdd),create(x0,y0,currentBoard));
      //TODO:
      //  gt_input
      //  gt_output
      //  gt_junctionDot
    end;

    if gateToAdd<>nil then begin
      if not currentBoard^.positionNewGate(gateToAdd)
      then dispose(gateToAdd,destroy);
      //TODO: Issue error message
    end;
  end;

PROCEDURE T_workspace.addCustomGate(CONST index: longint; CONST x0, y0: longint);
  begin

  end;

{ T_circuitBoard }

CONSTRUCTOR T_circuitBoard.create;
  begin
    inherited create(0,0,nil);
    incompleteWire.dragging:=false;
    wireGraph:=nil;
    prototype:=nil;
  end;

DESTRUCTOR T_circuitBoard.destroy;
  VAR i:longint;
  begin
    inherited;
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    setLength(logicWires,0);
    if wireGraph<>nil then dispose(wireGraph,destroy);
    wireGraph:=nil;
  end;

FUNCTION T_circuitBoard.cloneAsGate(CONST x0_, y0_: longint; CONST targetBoard: P_circuitBoard): P_circuitBoard;
  begin
    new(result,create);
    result^.origin   :=pointOf(x0_,y0_);
    result^.board    :=targetBoard;
    result^.prototype:=@self;

    //TODO: Implement me!

  end;

PROCEDURE T_circuitBoard.attachGUI(CONST zoom: longint;
  CONST container: TWinControl; CONST wireImage: TImage);
  begin
    GUI.zoom:=zoom;
    GUI.container:=container;
    GUI.wireImage:=wireImage;
  end;

PROCEDURE T_circuitBoard.gateMarked(CONST markedGate: P_abstractGate);
  VAR gate:P_abstractGate;
  begin
    for gate in gates do if (gate<>markedGate) then gate^.setMarked(false);
  end;

FUNCTION T_circuitBoard.repositionGate(CONST gateToCheck:P_abstractGate):T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_abstractGate;
    begin
      if (o[0]<5) or (o[0]+gateToCheck^.size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
         (o[1]<5) or (o[1]+gateToCheck^.size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
      then exit(false);
      result:=true;
      for gate in gates do
      if (gate<>gateToCheck) and
         (o[0]+gateToCheck^.size[0]>=gate^.origin[0]-1) and (o[0]<gate^.origin[0]+gate^.size[0]+1) and
         (o[1]+gateToCheck^.size[1]>=gate^.origin[1]-1) and (o[1]<gate^.origin[1]+gate^.size[1]+1)
      then exit(false);
    end;

  VAR newOrigin:T_point;
  FUNCTION applyOrigin:T_repositionOutput;
    begin
      gateToCheck^.origin:=newOrigin;
      result:=ro_positionFound;
    end;

  VAR range:longint=1;
      i:longint;
  begin
    newOrigin:=gateToCheck^.origin;
    if isOriginValid(newOrigin)
    then exit(ro_positionUnchanged)
    else repeat
      for i:=1 to range do begin newOrigin+=wd_right; if isOriginValid(newOrigin) then exit(applyOrigin); end;
      for i:=1 to range do begin newOrigin+=wd_down;  if isOriginValid(newOrigin) then exit(applyOrigin); end;
      range+=1;
      for i:=1 to range do begin newOrigin+=wd_left;  if isOriginValid(newOrigin) then exit(applyOrigin); end;
      for i:=1 to range do begin newOrigin+=wd_up;    if isOriginValid(newOrigin) then exit(applyOrigin); end;
      range+=1;
    until range>BOARD_MAX_SIZE_IN_GRID_ENTRIES;
    result:=ro_noPositionFound;
  end;

FUNCTION T_circuitBoard.positionNewGate(CONST gateToAdd: P_abstractGate): boolean;
  begin
    if repositionGate(gateToAdd)<>ro_noPositionFound then begin
      setLength(gates,length(gates)+1);
      gates[length(gates)-1]:=gateToAdd;
      gateToAdd^.Repaint;
      gateMoved(gateToAdd);
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_circuitBoard.deleteMarkedGate;
  PROCEDURE removeAssociatedWires(CONST gateToDelete:P_abstractGate);
    VAR i:longint;
        j:longint=0;
        i_,j_:longint;
    begin
      for i:=0 to length(logicWires)-1 do begin
        if logicWires[i].source.gate<>gateToDelete then begin
          logicWires[j]:=logicWires[i];
          with logicWires[i] do begin
            j_:=0;
            for i_:=0 to length(wires)-1 do begin
              if wires[i_].sink.gate<>gateToDelete then begin
                wires[j_]:=wires[i_];
                inc(j_);
              end;
            end;
            setLength(wires,j_);
          end;
          inc(j);
        end;
      end;
      setLength(logicWires,j);
    end;

  VAR i:longint;
      j:longint=0;
  begin
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.marked
      then begin
        removeAssociatedWires(gates[i]);
        dispose(gates[i],destroy)
      end
      else begin
        gates[j]:=gates[i];
        inc(j);
      end;
    end;
    setLength(gates,j);
    Repaint;
  end;

PROCEDURE T_circuitBoard.setZoom(CONST zoom: longint);
  VAR width :longint=0;
      height:longint=0;
      gate:P_abstractGate;
      p:T_point;
      i,j:longint;
  begin
    GUI.zoom:=zoom;
    if GUI.wireImage<>nil then begin
      for gate in gates do begin
        p:=gate^.origin+gate^.size;
        if p[0]>width  then width :=p[0];
        if p[1]>height then height:=p[1];
      end;
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      for p in logicWires[i].wires[j].visual do begin
        if p[0]>width  then width :=p[0];
        if p[1]>height then height:=p[1];
      end;
      width +=1; width *=zoom; width +=max(1,round(GUI.zoom*0.15));
      height+=1; height*=zoom; height+=max(1,round(GUI.zoom*0.15));
      GUI.wireImage.SetBounds(0,0,width,height);
      GUI.wireImage.picture.Bitmap.Canvas.Brush.color:=clBtnFace;
      GUI.wireImage.picture.Bitmap.setSize(width,height);
      GUI.wireImage.picture.Bitmap.Canvas.clear;
    end;
    Repaint;
  end;

FUNCTION T_circuitBoard.caption: string;
  begin
    result:=name;
  end;

FUNCTION T_circuitBoard.numberOfInputs: longint;
  VAR gate:P_abstractGate;
  begin
    result:=0;
    for gate in gates do if gate^.gateType=gt_input then inc(result);
  end;

FUNCTION T_circuitBoard.numberOfOutputs: longint;
  VAR gate:P_abstractGate;
  begin
    result:=0;
    for gate in gates do if gate^.gateType=gt_output then inc(result);
  end;

FUNCTION T_circuitBoard.gateType: T_gateType;
  begin
    result:=gt_compound;
  end;

PROCEDURE T_circuitBoard.simulateStep;
  VAR gate:P_abstractGate;
      i,j:longint;
      output:boolean;
  begin
    for gate in gates do gate^.simulateStep;
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      output:=source.gate^.getOutput(source.index);
      for j:=0 to length(wires)-1 do
        wires[j].sink.gate^.setInput(wires[j].sink.index,output);
    end;
  end;

FUNCTION T_circuitBoard.getOutput(CONST index: longint): boolean;
  //VAR gate:P_abstractGate;
  begin
    //for gate in gates do if (gate^.gateType=gt_output) and ...
    result:=false;
  end;

PROCEDURE T_circuitBoard.setInput(CONST index: longint; CONST value: boolean);
  begin
    inherited;
    //TODO: ...
  end;

FUNCTION T_circuitBoard.getInput(CONST index: longint): boolean;
  begin
    //TODO...
  end;

PROCEDURE T_circuitBoard.Repaint;
  VAR gate:P_abstractGate;
  begin
    if (GUI.container=nil) then inherited else begin
      for gate in gates do gate^.Repaint;
      drawAllWires;
    end;
  end;

FUNCTION T_circuitBoard.isInputConnected(CONST gate: P_abstractGate; CONST inputIndex: longint): boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
        for j:=0 to length(wires)-1 do
          if (wires[j].sink.gate=gate) and
             (wires[j].sink.index=inputIndex)
          then exit(true);
  end;

PROCEDURE T_circuitBoard.anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    dispose(wireGraph,destroy);
    incompleteWire.dragging:=false;
    drawAllWires;
  end;

PROCEDURE T_circuitBoard.drawAllWires;
  PROCEDURE drawWires(CONST index:longint);
    VAR j,k:longint;
    begin for j:=0 to length(logicWires[index].wires)-1 do with logicWires[index].wires[j] do if length(visual)>1 then begin
      GUI.wireImage.Canvas.MoveTo(visual[0,0]*GUI.zoom,visual[0,1]*GUI.zoom);
      for k:=1 to length(visual)-1 do
      GUI.wireImage.Canvas.LineTo(visual[k,0]*GUI.zoom,visual[k,1]*GUI.zoom);
    end; end;

  VAR i:longint;
  begin
    if GUI.wireImage=nil then exit;
    with GUI.wireImage.Canvas do begin
      Brush.color:=clBtnFace;
      clear;
      for i:=0 to length(logicWires)-1 do begin
        Pen.color:=clBtnFace; Pen.width:=max(1,round(GUI.zoom*0.45));
        drawWires(i);
        Pen.color:=clBlack; Pen.width:=max(1,round(GUI.zoom*0.15));
        drawWires(i);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.initWireGraph(CONST start: T_gateConnector; CONST includeWires:boolean=true);
  VAR gate:P_abstractGate;
      x,y,i,j:longint;
  begin
    new(wireGraph,create);
    for gate in gates do begin
      for x:=gate^.origin[0] to gate^.origin[0]+gate^.size[0] do
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do
      wireGraph^.dropNode(pointOf(x,y));

      for i:=0 to gate^.numberOfInputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getInputPositionInGridSize(i)+wd_left,wd_right);

      for i:=0 to gate^.numberOfOutputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getOutputPositionInGridSize(i),wd_right);
    end;
    if includeWires then
    for i:=0 to length(logicWires)-1 do
    if (logicWires[i].source<>start) then with logicWires[i] do
      for j:=0 to length(wires)-1 do wireGraph^.dropWire(wires[j].visual);
  end;

FUNCTION T_circuitBoard.findWirePath(CONST start: T_gateConnector; CONST endPoint: T_point): T_wirePath;
  begin
    if wireGraph=nil then initWireGraph(start,true);
    if wireGraph^.anyEdgeLeadsTo(endPoint)
    then result:=wireGraph^.findPath(start.gate^.getOutputPositionInGridSize(start.index),endPoint)
    else setLength(result,0);
  end;

PROCEDURE T_circuitBoard.drawTempWire(CONST targetPoint: T_point);
  VAR wire:T_wirePath;
      i:longint;
  begin
    if GUI.wireImage=nil then exit;
    wire:=findWirePath(incompleteWire.source,targetPoint);
    if length(wire)<=0 then exit;
    drawAllWires;
    with GUI.wireImage.Canvas do begin
      Pen.color:=clRed;
      Pen.width:=max(1,round(GUI.zoom*0.15));
      MoveTo(wire[0,0]*GUI.zoom,wire[0,1]*GUI.zoom);
      for i:=1 to length(wire)-1 do LineTo(wire[i,0]*GUI.zoom,wire[i,1]*GUI.zoom);
    end;
  end;

PROCEDURE T_circuitBoard.gateMoved(CONST gate: P_abstractGate);
  begin
    repositionGate(gate);
    rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.finishWireDrag(CONST targetPoint:T_point);
  VAR i:longint=0;
      j:longint;
      gate:P_abstractGate;
      connector:T_gateConnector;
  begin
    if not(incompleteWire.dragging) then exit;
    connector.gate:=nil;
    for gate in gates do
    for j:=0 to gate^.numberOfInputs-1 do
    if gate^.getInputPositionInGridSize(j)=targetPoint then begin
      connector.gate:=gate;
      connector.index:=j;
      break;
    end;
    if connector.gate=nil then exit;
    if not(isInputConnected(connector.gate,connector.index)) then begin
      i:=0;
      while (i<length(logicWires)) and (logicWires[i].source<>incompleteWire.source) do inc(i);
      if i>=length(logicWires) then setLength(logicWires,i+1);
      with logicWires[i] do begin
        source:=incompleteWire.source;
        j:=length(wires);
        setLength(wires,j+1);
        wires[j].sink:=connector;
        wires[j].visual:=findWirePath(incompleteWire.source,targetPoint);
      end;
    end;
    drawAllWires;
    dispose(wireGraph,destroy);
    wireGraph:=nil;
    incompleteWire.dragging:=false;
  end;

PROCEDURE T_circuitBoard.rewire;
  VAR connector:T_gateConnector;
      i,j:longint;
  begin
    if wireGraph<>nil then dispose(wireGraph,destroy);
    connector.gate:=nil;
    connector.index:=0;
    initWireGraph(connector,false);
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      for j:=0 to length(wires)-1 do
        wires[j].visual:=
          wireGraph^.findPath(source       .gate^.getOutputPositionInGridSize(source       .index),
                              wires[j].sink.gate^.getInputPositionInGridSize (wires[j].sink.index));
      for j:=0 to length(wires)-1 do wireGraph^.dropWire(wires[j].visual);
    end;
  end;

CONSTRUCTOR T_nxorGate.create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;
CONSTRUCTOR T_norGate .create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;
CONSTRUCTOR T_nandGate.create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;
CONSTRUCTOR T_xorGate .create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;
CONSTRUCTOR T_orGate  .create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;
CONSTRUCTOR T_andGate .create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard); begin inherited; end;

FUNCTION T_nxorGate.caption: string; begin result:='NXOR'; end;
FUNCTION T_norGate .caption: string; begin result:='NOR';  end;
FUNCTION T_nandGate.caption: string; begin result:='NAND'; end;
FUNCTION T_xorGate .caption: string; begin result:='XOR';  end;
FUNCTION T_orGate  .caption: string; begin result:='OR';   end;
FUNCTION T_andGate .caption: string; begin result:='AND';  end;

PROCEDURE T_nxorGate.simulateStep; begin output:=not(input[0] xor input[1]); setOutput(0,output); end;
PROCEDURE T_norGate .simulateStep; begin output:=not(input[0]  or input[1]); setOutput(0,output); end;
PROCEDURE T_nandGate.simulateStep; begin output:=not(input[0] and input[1]); setOutput(0,output); end;
PROCEDURE T_xorGate .simulateStep; begin output:=    input[0] xor input[1] ; setOutput(0,output); end;
PROCEDURE T_orGate  .simulateStep; begin output:=    input[0]  or input[1] ; setOutput(0,output); end;
PROCEDURE T_andGate .simulateStep; begin output:=    input[0] and input[1] ; setOutput(0,output); end;

FUNCTION T_nxorGate.gateType: T_gateType; begin result:=gt_nxorGate; end;
FUNCTION T_norGate .gateType: T_gateType; begin result:=gt_norGate;  end;
FUNCTION T_nandGate.gateType: T_gateType; begin result:=gt_nandGate; end;
FUNCTION T_xorGate .gateType: T_gateType; begin result:=gt_xorGate;  end;
FUNCTION T_orGate  .gateType: T_gateType; begin result:=gt_orGate;   end;
FUNCTION T_andGate .gateType: T_gateType; begin result:=gt_andGate;  end;

{ T_binaryBaseGate }

CONSTRUCTOR T_binaryBaseGate.create(CONST x0_, y0_: longint; CONST board_:P_circuitBoard);
  begin
    inherited;
  end;

FUNCTION T_binaryBaseGate.numberOfInputs: longint;
  begin result:=2; end;

FUNCTION T_binaryBaseGate.numberOfOutputs: longint;
  begin result:=1; end;

FUNCTION T_binaryBaseGate.getOutput(CONST index:longint):boolean;
  begin result:=output; end;

PROCEDURE T_binaryBaseGate.setInput(CONST index: longint; CONST value: boolean);
  begin inherited; input[index]:=value; end;

FUNCTION T_binaryBaseGate.getInput(CONST index: longint): boolean;
  begin result:=input[index]; end;

{ T_notGate }

CONSTRUCTOR T_notGate.create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard);
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
    output:=not(input);
    setOutput(0,output);
  end;

FUNCTION T_notGate.getOutput(CONST index: longint): boolean;
  begin
    result:=output;
  end;

PROCEDURE T_notGate.setInput(CONST index: longint; CONST value: boolean);
  begin
    inherited;
    input:=value;
  end;

FUNCTION T_notGate.getInput(CONST index: longint): boolean;
  begin
    result:=input;
  end;

{ T_abstractGate }

CONSTRUCTOR T_abstractGate.create(CONST x0_, y0_: longint; CONST board_: P_circuitBoard);
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    origin:=pointOf(x0_,y0_);
    size:=pointOf(4,max(2,2*max(numberOfInputs,numberOfInputs)));

    dragging:=false;
    marked  :=false;
    board   :=board_;

    if (board<>nil) and (board^.GUI.container<>nil) then begin
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
        shapes[shapeIndex].OnClick  :=@inputClick;
        shapes[shapeIndex].parent:=board^.GUI.container;
        inc(shapeIndex);
      end;

      for k:=0 to numberOfOutputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnMouseDown:=@outputMouseDown;
        shapes[shapeIndex].OnMouseMove:=@outputMouseMove;
        shapes[shapeIndex].OnMouseUp  :=@outputMouseUp;
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

PROCEDURE T_abstractGate.setInput(CONST index: longint; CONST value: boolean);
  begin
    if (length(shapes)>0) then begin
      if value
      then shapes[1+index].Brush.color:=clGreen
      else shapes[1+index].Brush.color:=clWhite;
    end;
  end;

FUNCTION T_abstractGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0];
    result[1]:=origin[1]+(index*2-(numberOfInputs-1))+size[1] div 2;
  end;

FUNCTION T_abstractGate.getOutputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0]+size[0];
    result[1]:=origin[1]+(index*2-(numberOfOutputs-1))+size[1] div 2;
  end;

PROCEDURE T_abstractGate.Repaint;
  VAR k,yCenter,
      newFontSize:longint;
      shapeIndex :longint=1;
      p:T_point;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=origin[0]*board^.GUI.zoom;
    shapes[0].top   :=origin[1]*board^.GUI.zoom;
    shapes[0].width :=size  [0]*board^.GUI.zoom;
    shapes[0].height:=size  [1]*board^.GUI.zoom;

    gatelabel.top :=shapes[0].top +(shapes[0].height-gatelabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gatelabel.width) div 2 ;
    newFontSize:=round(gatelabel.Font.size*shapes[0].width*0.75/gatelabel.width);
    if abs(newFontSize-gatelabel.Font.size)>1 then gatelabel.Font.size:=newFontSize;
    gatelabel.top :=shapes[0].top +(shapes[0].height-gatelabel.height) div 2;
    gatelabel.Left:=shapes[0].Left+(shapes[0].width -gatelabel.width) div 2 ;

    for k:=0 to numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_abstractGate.setOutput(CONST index: longint; CONST value: boolean);
  begin
    if length(shapes)>0 then begin
      if value
      then shapes[1+numberOfInputs+index].Brush.color:=clGreen
      else shapes[1+numberOfInputs+index].Brush.color:=clWhite;
    end;
  end;

PROCEDURE T_abstractGate.setMarked(CONST value: boolean);
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

//PROCEDURE T_abstractGate.inputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  begin
//    if board=nil then exit;
//    board^.finishWireDrag(@self,TShape(Sender).Tag);
//  end;

PROCEDURE T_abstractGate.outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if board=nil then exit;
    board^.finishWireDrag(pointOf(dragX+floor(x/board^.GUI.zoom),
                                  dragY+floor(y/board^.GUI.zoom)));
  end;

PROCEDURE T_abstractGate.outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    if (button=mbLeft) then begin
      wireDragOutputIndex:=TShape(Sender).Tag;
      p:=getOutputPositionInGridSize(wireDragOutputIndex);
      with board^.incompleteWire do begin
        dragging:=true;
        source.gate:=@self;
        source.index:=wireDragOutputIndex;
        sourcePoint:=p;
      end;
      dragX:=p[0];
      dragY:=p[1];
    end;
  end;

PROCEDURE T_abstractGate.outputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if board^.incompleteWire.dragging
    then board^.drawTempWire(pointOf(dragX+floor(x/board^.GUI.zoom),
                                     dragY+floor(y/board^.GUI.zoom)));
  end;

PROCEDURE T_abstractGate.mainShapeMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
  VAR newOrigin:T_point;
      dx,dy:longint;
  begin
    if dragging then begin
      dx:=x-dragX;
      dy:=y-dragY;
      newOrigin:=origin+pointOf(round(dx/board^.GUI.zoom),round(dy/board^.GUI.zoom));
      if newOrigin<>origin then begin
        origin:=newOrigin;
        board^.gateMoved(@self);
        Repaint;
      end;
    end;
  end;

PROCEDURE T_abstractGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=false;
      board^.incompleteWire.dragging:=false;
      shapes[0].Pen.style:=psSolid;
      Repaint;
    end;
  end;

end.

