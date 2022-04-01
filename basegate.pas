UNIT baseGate;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls,Classes,Controls,StdCtrls,wiringUtil,serializationUtil,logicGates;

TYPE
  P_abstractGate=^T_abstractGate;
  P_circuitBoard=^T_circuitBoard;
  { T_abstractGate }
  P_visualGate=^T_visualGate;

  { T_visualGate }

  T_visualGate=object
    private
      behavior:P_abstractGate;

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
    protected
      CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
      DESTRUCTOR destroy;
    private
      PROCEDURE setMarked(CONST value:boolean);
      PROCEDURE inputClick(Sender: TObject);
      PROCEDURE mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE outputMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;
      PROCEDURE Repaint;
      PROPERTY marked:boolean read marked_ write setMarked;
      PROCEDURE updateIoVisuals;
      FUNCTION numberOfInputs:longint;
      FUNCTION numberOfOutputs:longint;
  end;

  T_visualGateConnector=object
    gate:P_visualGate;
    index:longint;
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

  { T_gateConnector }

  { T_customGate }
  P_customGate=^T_customGate;
  T_customGate=object(T_abstractGate)
    prototype  :P_circuitBoard;
    gates      :array of P_abstractGate;
    name       :string;
    connections:array of record source,sink:T_gateConnector end;
    inputConnections :array of record
                        value:boolean;
                        goesTo:array of T_gateConnector;
                      end;
    outputConnections:array of T_gateConnector;

    CONSTRUCTOR create(CONST origin:P_circuitBoard);
    DESTRUCTOR destroy; virtual;
    FUNCTION  caption:string;          virtual;
    FUNCTION  numberOfInputs :longint; virtual;
    FUNCTION  numberOfOutputs:longint; virtual;
    FUNCTION  gateType:T_gateType; virtual;
    PROCEDURE simulateStep;                                       virtual;
    FUNCTION  getOutput(CONST index:longint):boolean;             virtual;
    PROCEDURE setInput(CONST index:longint; CONST value:boolean); virtual;
    FUNCTION  getInput(CONST index:longint):boolean;              virtual;
  end;

  T_circuitBoard=object
    private
      GUI:record
        zoom:longint;
        container:TWinControl;
        wireImage:TImage;
      end;

      name       :string;
      description:string;
      gates      :array of P_visualGate;

      //One source can be associated with many sinks
      logicWires:array of record
        source:   T_visualGateConnector;
        wires:array of record
          sink:   T_visualGateConnector;
          visual: T_wirePath; //Nonpersistent!
        end;
      end;

      incompleteWire:record
        dragging:boolean;
        source:T_visualGateConnector;
        sourcePoint:T_point;
      end;
      wireGraph:P_wireGraph;
      FUNCTION repositionGate(CONST gateToCheck:P_visualGate):T_repositionOutput;
      FUNCTION positionNewGate(CONST gateToAdd:P_visualGate):boolean;
      FUNCTION isInputConnected(CONST gate:P_visualGate; CONST inputIndex:longint):boolean;
      PROCEDURE initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
      PROCEDURE drawAllWires;
      FUNCTION findWirePath(CONST start:T_visualGateConnector; CONST endPoint:T_point):T_wirePath;
      PROCEDURE drawTempWire(CONST targetPoint:T_point);
      PROCEDURE finishWireDrag(CONST targetPoint:T_point);
      PROCEDURE rewire;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;  virtual;
      FUNCTION cloneAsGate(CONST x0_,y0_:longint; CONST targetBoard:P_circuitBoard):P_customGate;

      PROCEDURE attachGUI(CONST zoom:longint; CONST container:TWinControl; CONST wireImage:TImage);
      PROCEDURE gateMoved(CONST gate:P_visualGate);
      PROCEDURE gateMarked(CONST markedGate: P_visualGate);
      PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE setZoom(CONST zoom:longint);
      PROCEDURE deleteMarkedGate;
      PROCEDURE Repaint;
      PROCEDURE simulateStep;
  end;

  { T_notGate }

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

IMPLEMENTATION
USES sysutils,math,Graphics,myGenerics;

OPERATOR =(CONST x,y:T_visualGateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

{ T_visualGate }

CONSTRUCTOR T_visualGate.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    origin:=origin_;
    behavior:=gateToWrap;
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
      gatelabel.caption:=behavior^.caption;
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

DESTRUCTOR T_visualGate.destroy;
  VAR i:longint;
  begin
    dispose(behavior,destroy);
    for i:=0 to length(shapes)-1 do shapes[i].free;
    gatelabel.free;
  end;

PROCEDURE T_visualGate.setMarked(CONST value: boolean);
  begin
    if marked_=value then exit;
    marked_:=value;
    if length(shapes)=0 then exit;
    if marked
    then shapes[0].Brush.color:=clYellow
    else shapes[0].Brush.color:=clWhite;
  end;

PROCEDURE T_visualGate.inputClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=TShape(Sender).Tag;
    behavior^.setInput(k,not behavior^.getInput(k));
    updateIoVisuals;
  end;

PROCEDURE T_visualGate.mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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

PROCEDURE T_visualGate.mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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

PROCEDURE T_visualGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=false;
      board^.incompleteWire.dragging:=false;
      shapes[0].Pen.style:=psSolid;
      Repaint;
    end;
  end;

PROCEDURE T_visualGate.outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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

PROCEDURE T_visualGate.outputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if board^.incompleteWire.dragging
    then board^.drawTempWire(pointOf(dragX+floor(x/board^.GUI.zoom),
                                     dragY+floor(y/board^.GUI.zoom)));
  end;

PROCEDURE T_visualGate.outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if board=nil then exit;
    board^.finishWireDrag(pointOf(dragX+floor(x/board^.GUI.zoom),
                                  dragY+floor(y/board^.GUI.zoom)));
  end;

FUNCTION T_visualGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0];
    result[1]:=origin[1]+(index*2-(numberOfInputs-1))+size[1] div 2;
  end;

FUNCTION T_visualGate.getOutputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0]+size[0];
    result[1]:=origin[1]+(index*2-(numberOfOutputs-1))+size[1] div 2;
  end;

PROCEDURE T_visualGate.Repaint;
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

PROCEDURE T_visualGate.updateIoVisuals;
  VAR inputs,outputs,i:longint;

  begin
    inputs :=numberOfOutputs;
    outputs:=numberOfOutputs;
    for i:=0 to inputs-1 do if behavior^.getInput(i)
    then shapes[i+1].Brush.color:=clLime
    else shapes[i+1].Brush.color:=clGray;

    for i:=0 to outputs-1 do if behavior^.getOutput(i)
    then shapes[i+inputs+1].Brush.color:=clLime
    else shapes[i+inputs+1].Brush.color:=clGray;

  end;

FUNCTION T_visualGate.numberOfInputs: longint;
  begin
    result:=behavior^.numberOfInputs;
  end;

FUNCTION T_visualGate.numberOfOutputs: longint;
  begin
    result:=behavior^.numberOfOutputs;
  end;

{ T_customGate }

CONSTRUCTOR T_customGate.create(CONST origin: P_circuitBoard);
  FUNCTION myCorrespondingGate(CONST original:T_visualGateConnector):T_gateConnector;
    VAR i:longint;
    begin
      for i:=0 to length(origin^.gates)-1 do begin
        if origin^.gates[i]=original.gate
        then begin
          result.gate:=gates[i];
          result.index:=original.index;
        end;
      end;
      raise Exception.create('Connection cannot be reproduced!');
    end;

  PROCEDURE addConnection(CONST src,tgt:T_gateConnector);
    VAR k,ioIdx:longint;
    begin
      if src.gate^.gateType=gt_input then begin
        ioIdx:=P_inputGate(src.gate)^.ioIndex;
        if length(inputConnections)<=ioIdx then begin
          k:=length(inputConnections);
          setLength(inputConnections,ioIdx+1);
          while k<length(inputConnections) do begin
            setLength(inputConnections[k].goesTo,0);
            inputConnections[k].value:=random>0.5;
            inc(k);
          end;
        end;
        k:=length(inputConnections[ioIdx].goesTo);
        setLength(inputConnections[ioIdx].goesTo,k+1);
        inputConnections[ioIdx].goesTo[k]:=tgt;
      end else if tgt.gate^.gateType=gt_output then begin
        ioIdx:=P_outputGate(tgt.gate)^.ioIndex;
        if length(outputConnections)<=ioIdx then begin
          k:=length(outputConnections);
          setLength(outputConnections,ioIdx+1);
          while k<length(outputConnections) do begin
            outputConnections[k].gate:=nil;
            inc(k);
          end;
        end;
        outputConnections[ioIdx]:=src;
      end else begin
        k:=length(connections);
        setLength(connections,k+1);
        connections[k].source:=src;
        connections[k].sink  :=tgt;
      end;
    end;

  VAR i,j:longint;
      src,tgt:T_gateConnector;
  begin
    inherited create;
    prototype:=origin;

    setLength(connections      ,0);
    setLength(inputConnections ,0);
    setLength(outputConnections,0);
    setLength(gates,length(origin^.gates));
    for i:=0 to length(gates)-1 do gates[i]:=origin^.gates[i]^.behavior^.clone;
    name:=origin^.name;

    for i:=0 to length(origin^.logicWires)-1 do begin
      src:=myCorrespondingGate(origin^.logicWires[i].source);
      for j:=0 to length(origin^.logicWires[i].wires)-1 do begin
        tgt:=myCorrespondingGate(origin^.logicWires[i].wires[j].sink);
        addConnection(src,tgt);
      end;
    end;

    j:=0;
    //Actually we do not need input and output gates, so we drop them again...
    for i:=0 to length(gates)-1 do
    if gates[i]^.gateType in [gt_input,gt_output]
    then dispose(gates[i],destroy)
    else begin
      gates[j]:=gates[i];
      inc(j);
    end;
    setLength(gates,j);
  end;

DESTRUCTOR T_customGate.destroy;
  VAR i:longint;
  begin
    inherited;
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy); setLength(gates,0);
    for i:=0 to length(inputConnections)-1 do setLength(inputConnections[i].goesTo,0); setLength(inputConnections,0);
    setLength(outputConnections,0);
    setLength(connections,0);
  end;

FUNCTION T_customGate.caption: string;
  begin
    result:=name;
  end;

FUNCTION T_customGate.numberOfInputs: longint;
  begin
    result:=length(inputConnections);
  end;

FUNCTION T_customGate.numberOfOutputs: longint;
  begin
    result:=length(outputConnections);
  end;

FUNCTION T_customGate.gateType: T_gateType;
  begin
    result:=gt_compound;
  end;

PROCEDURE T_customGate.simulateStep;
  VAR gate:P_abstractGate;
      i:longint;
      tgt:T_gateConnector;
  begin
    for i:=0 to length(inputConnections)-1 do for tgt in inputConnections[i].goesTo do tgt.setInputValue(inputConnections[i].value);
    for gate in gates do simulateStep;
    for i:=0 to length(connections)-1 do with connections[i] do sink.setInputValue(source.getOutputValue);
  end;

FUNCTION T_customGate.getOutput(CONST index: longint): boolean;
  begin
    result:=outputConnections[index].getOutputValue;
  end;

PROCEDURE T_customGate.setInput(CONST index: longint; CONST value: boolean);
  begin
    inputConnections[index].value:=value;
  end;

FUNCTION T_customGate.getInput(CONST index: longint): boolean;
  begin
    result:=inputConnections[index].value;
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
      visual:P_visualGate;
  begin
    gateToAdd:=newBaseGate(gateType);
    if gateToAdd<>nil then begin
      new(visual,create(pointOf(x0,y0),gateToAdd,currentBoard));
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

PROCEDURE T_workspace.addCustomGate(CONST index: longint; CONST x0, y0: longint);
  begin

  end;

{ T_circuitBoard }

CONSTRUCTOR T_circuitBoard.create;
  begin
    incompleteWire.dragging:=false;
    wireGraph:=nil;
  end;

DESTRUCTOR T_circuitBoard.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    setLength(logicWires,0);
    if wireGraph<>nil then dispose(wireGraph,destroy);
    wireGraph:=nil;
  end;

FUNCTION T_circuitBoard.cloneAsGate(CONST x0_, y0_: longint; CONST targetBoard: P_circuitBoard): P_customGate;
  begin
    //new(result,create...);
    //TODO: Implement me!

  end;

PROCEDURE T_circuitBoard.attachGUI(CONST zoom: longint;
  CONST container: TWinControl; CONST wireImage: TImage);
  begin
    GUI.zoom:=zoom;
    GUI.container:=container;
    GUI.wireImage:=wireImage;
  end;

PROCEDURE T_circuitBoard.gateMarked(CONST markedGate: P_visualGate);
  VAR gate:P_visualGate;
  begin
    for gate in gates do if (gate<>markedGate) then gate^.setMarked(false);
  end;

FUNCTION T_circuitBoard.repositionGate(CONST gateToCheck:P_visualGate):T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_visualGate;
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

FUNCTION T_circuitBoard.positionNewGate(CONST gateToAdd: P_visualGate): boolean;
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
  PROCEDURE removeAssociatedWires(CONST gateToDelete:P_visualGate);
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
      gate:P_visualGate;
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

PROCEDURE T_circuitBoard.simulateStep;
  VAR gate:P_visualGate;
      i,j:longint;
      output:boolean;
  begin
    for gate in gates do gate^.behavior^.simulateStep;
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      output:=source.gate^.behavior^.getOutput(source.index);
      for j:=0 to length(wires)-1 do
        wires[j].sink.gate^.behavior^.setInput(wires[j].sink.index,output);
    end;
    for gate in gates do gate^.updateIoVisuals;
  end;

PROCEDURE T_circuitBoard.Repaint;
  VAR gate:P_visualGate;
  begin
    if (GUI.container=nil) then inherited else begin
      for gate in gates do gate^.Repaint;
      drawAllWires;
    end;
  end;

FUNCTION T_circuitBoard.isInputConnected(CONST gate: P_visualGate; CONST inputIndex: longint): boolean;
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

PROCEDURE T_circuitBoard.initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
  VAR gate:P_visualGate;
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

FUNCTION T_circuitBoard.findWirePath(CONST start: T_visualGateConnector; CONST endPoint: T_point): T_wirePath;
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

PROCEDURE T_circuitBoard.gateMoved(CONST gate: P_visualGate);
  begin
    repositionGate(gate);
    rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.finishWireDrag(CONST targetPoint:T_point);
  VAR i:longint=0;
      j:longint;
      gate:P_visualGate;
      connector:T_visualGateConnector;
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
  VAR connector:T_visualGateConnector;
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

//PROCEDURE T_abstractGate.setInput(CONST index: longint; CONST value: boolean);
//  begin
//    if (length(shapes)>0) then begin
//      if value
//      then shapes[1+index].Brush.color:=clGreen
//      else shapes[1+index].Brush.color:=clWhite;
//    end;
//  end;

//PROCEDURE T_abstractGate.setOutput(CONST index: longint; CONST value: boolean);
//  begin
//    if length(shapes)>0 then begin
//      if value
//      then shapes[1+numberOfInputs+index].Brush.color:=clGreen
//      else shapes[1+numberOfInputs+index].Brush.color:=clWhite;
//    end;
//  end;

//PROCEDURE T_abstractGate.inputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
//  begin
//    if board=nil then exit;
//    board^.finishWireDrag(@self,TShape(Sender).Tag);
//  end;

end.

