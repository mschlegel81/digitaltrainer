UNIT visualGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, logicalGates, compoundGates, StdCtrls, ExtCtrls,serializationUtil,Forms,Controls,wiringUtil,math;
TYPE
  P_uiAdapter=^T_uiAdapter;
  P_visualGate=^T_visualGate;
  P_visualBoard=^T_visualBoard;

  { T_visualWire }

  T_visualWire=object
    source:P_visualGate;
    sourceOutputIndex:longint;

    sink:array of record
      gate:P_visualGate;
      gateInputIndex:longint;
      path:T_wirePath;
      Selected:boolean;
    end;

    PROCEDURE dropWiresAssociatedWith(CONST gate:P_visualGate);
    FUNCTION simulateStep:boolean;
    PROCEDURE paint(CONST x0,y0:longint; CONST zoom:longint; CONST image:TImage);
  end;

  { T_visualGate }

  T_visualGate=object(T_serializable)
    private
      gridPos:T_point;
      gridWidth,gridHeight:longint;
      behavior:P_abstractGate;

      ioLocations: T_ioLocations;

      shapes:array of TShape;
      labels:array of TLabel;
      ioEdit:TEdit;
      ioMode:T_multibitWireRepresentation;

      marked:boolean;
      PROCEDURE ioEditEditingDone(Sender: TObject);
      PROCEDURE ioEditKeyPress(Sender: TObject; VAR key: char);
      PROCEDURE paletteEntryMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementOutputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE ioModeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    public
      uiAdapter:P_uiAdapter;

      CONSTRUCTOR create(CONST behavior_:P_abstractGate);
      DESTRUCTOR destroy;

      PROCEDURE ensureGuiElements(CONST container:TWinControl); virtual;
      PROCEDURE disposeGuiElements; virtual;

      FUNCTION simulateStep:boolean;
      PROCEDURE updateVisuals;
      PROCEDURE paintAll(CONST x,y:longint; CONST zoom:longint); virtual;
      FUNCTION  clone:P_visualGate;
      PROCEDURE propertyEditDone(CONST paletteElement:boolean);
      FUNCTION visualHeight:longint;
      FUNCTION visualWidth:longint;

      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;

      FUNCTION overlaps(CONST other:P_visualGate):boolean;

      PROCEDURE setPaletteEntryMouseActions();
      PROCEDURE setBoardElementMouseActions;
      PROPERTY  getBehavior:P_abstractGate read behavior;

  end;

  { T_UIAdapter }
  T_uiAdapterState=(uas_initial,uas_draggingFromPalette,uas_draggingFromBoard,uas_draggingWire,uas_propertyEditFromBoard,uas_propertyEditFromPalette);

  F_showPropertyEditorCallback=PROCEDURE (CONST gate:P_visualGate; CONST fromBoard:boolean; CONST mouseX,mouseY:longint) of object;

  T_uiAdapter=object
    private
      zoom:longint;
      mainForm:TForm;
      showPropertyEditorCallback:F_showPropertyEditorCallback;

      state:T_uiAdapterState;
      dragData:record
        startX,startY:longint;
        draggedGate:P_visualGate;
        outputIndex:longint;

        dragTarget:P_visualGate;
        inputIndex:longint;
      end;

      activeBoard:P_visualBoard;

//    wireImage:TImage;
//    //gateContextMenu:TPopupMenu;
//    //anyChangeCallback:F_changeCallback;
//    lastClickedGate:P_visualGate;
//    selectionFrame:TShape;
//    selectionStart:T_point;
//    Clipboard:P_circuitBoard;
//    undoList,redoList:array of P_circuitBoard;
//
//    incompleteWire:record
//      dragging:boolean;
//      width:byte;
//      source:T_visualGateConnector;
//      sourcePoint:T_point;
//    end;
//    wireGraph:T_wireGraph;
//
//    wiringInputCs,wiringOutputCs:TRTLCriticalSection;
//
//    currentBoard:P_circuitBoard;
//
//    peekPanels:array of record
//      panel:TPanel;
//      peekLabel:TLabel;
//
//      connector:T_visualGateConnector;
//    end;
//    PROCEDURE peekLabelClick(Sender: TObject);
//    PROCEDURE fixWireImageSize;
//    PROCEDURE initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
//    FUNCTION findWirePath(CONST start:T_visualGateConnector; CONST endPoint:T_point):T_wirePath;
    public
      CONSTRUCTOR create(CONST mainForm_:TForm; CONST showPropertyEditorCallback_:F_showPropertyEditorCallback);

      PROCEDURE paletteEntryMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
      PROCEDURE paletteEntryMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardElementMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardOutputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
      PROCEDURE boardOutputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

      PROCEDURE paletteSizeUpdated(CONST Right:longint);
      PROCEDURE propertyEditorShown(CONST gate:P_visualGate; CONST fromBoard:boolean);
      PROPERTY draggedGate:P_visualGate read dragData.draggedGate;

//    DESTRUCTOR destroy;
//    PROCEDURE clearUndoLists;
//    PROCEDURE newBoardAttached(CONST board:P_circuitBoard);
//    PROPERTY getLastClickedGate:P_visualGate read lastClickedGate;
//
//    PROCEDURE addPeekPanel(CONST panel:TPanel; CONST label_:TLabel);
//    PROCEDURE gateDeleted(CONST gate:P_visualGate);
//    PROCEDURE simStepDone;
//    PROCEDURE startPeeking(CONST connector:T_visualGateConnector);
//    PROCEDURE hideAllPeekPanels;
//    PROCEDURE repaint;
      PROCEDURE zoomIn;
      PROCEDURE zoomOut;
      PROPERTY getZoom:longint read zoom;

//    FUNCTION positionForNextGate(CONST gateExtend:T_point):T_point;
//    PROCEDURE recenter;
//    PROCEDURE saveStateToUndoList;
//    PROCEDURE performUndo;
//    PROCEDURE performRedo;
//    PROCEDURE fixAfterGatePropertiesEdited(CONST gate:P_abstractGate);
//
//    PROCEDURE drawAllWires;
//    PROCEDURE finishWireDrag(CONST targetPoint:T_point; CONST previewDuringDrag:boolean=false);
//    PROCEDURE WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
//    PROCEDURE WireImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
//    PROCEDURE WireImageMouseUp  (Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
//    PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
//    PROCEDURE pasteFromClipboard;
  end;

  { T_visualBoard }

  T_visualBoard=object(T_captionedAndIndexed)
    private
      associatedPalette:P_abstractPrototypeSource;
      myIndex:longint;

      captionString:string;
      descriptionString:string;
      inputs : array of P_visualGate;
      outputs: array of P_visualGate;
      gates  : array of P_visualGate;
      wires  : array of T_visualWire;

      ui:record
        wireImage:TImage;
        horizontalScrollBar,verticalScrollBar:TScrollBar;
        uiAdapter:P_uiAdapter;
      end;

      gridOutputX0,gridOutputY0:longint;
    public
      CONSTRUCTOR create(CONST palette:P_abstractPrototypeSource);
      DESTRUCTOR destroy;

      PROCEDURE checkSizes;
      PROCEDURE detachUI;
      PROCEDURE attachUI(CONST wireImage:TImage;
                         CONST horizontalScrollBar,verticalScrollBar:TScrollBar;
                         CONST uiAdapter:P_uiAdapter);
      PROCEDURE elementAdded(CONST newElement:P_visualGate; CONST screenX,screenY:longint);
      PROCEDURE repositionElement(CONST element: P_visualGate; CONST screenX, screenY: longint);

      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
      PROCEDURE savePaletteEntryToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST paletteIndex:longint);
      PROCEDURE loadPaletteEntryFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST paletteIndex:longint);
      FUNCTION extractBehavior:P_circuitBoard;

      FUNCTION getWireGraph(CONST dropExistingWires:boolean):P_wireGraph;
      PROCEDURE rewire;
      PROCEDURE paintWires;
      PROCEDURE paintWirePreview(CONST wireStart:T_point; CONST screenX,screenY:longint);

      FUNCTION getCaption: string; virtual;
      PROCEDURE setCaption(const s: string); virtual;
      FUNCTION getDescription: string; virtual;
      PROCEDURE setDescription(const s: string); virtual;
      FUNCTION getIndexInPalette: longint; virtual;

      PROCEDURE afterGatePropertiesEdited(CONST editedGate:P_visualGate);

      FUNCTION isInputConnected(CONST gate:P_visualGate; CONST gateInputIndex:longint):boolean;
      FUNCTION isOutputputConnected(CONST gate:P_visualGate; CONST gateOutputIndex:longint):boolean;
      PROCEDURE addWire(CONST sourceGate:P_visualGate; CONST sourceOutputIndex:longint;
                        CONST sinkGate:P_visualGate; CONST sinkInputIndex:longint);

      FUNCTION simulateSteps(CONST count:longint):longint;
  end;

IMPLEMENTATION

{ T_visualWire }

PROCEDURE T_visualWire.dropWiresAssociatedWith(CONST gate: P_visualGate);
  VAR i,j:longint;
  begin
    if source=gate then begin
      for i:=0 to length(sink)-1 do setLength(sink[i].path,0);
      setLength(sink,0);
    end else begin
      j:=0;
      for i:=0 to length(sink)-1 do if sink[i].gate=gate
      then setLength(sink[i].path,0)
      else begin
        sink[j]:=sink[i]; inc(j);
      end;
      setLength(sink,j);
    end;
  end;

FUNCTION T_visualWire.simulateStep: boolean;
  VAR value:T_wireValue;
      i:longint;
  begin
    value:=source^.behavior^.getOutput(sourceOutputIndex);
    result:=false;
    for i:=0 to length(sink)-1 do if sink[i].gate^.behavior^.setInput(sink[i].gateInputIndex,value) then result:=true;
  end;

PROCEDURE paintWire(CONST image:TImage; CONST bitWidth:byte; CONST path:T_wirePathArray; CONST preview:boolean=false);
  CONST surround=4;
  VAR w:longint=1;
      run:longint;
      i,j:longint;
  begin
    case bitWidth of
       1..3: w:=1+surround;
       4..7: w:=2+surround;
      8..15: w:=3+surround;
       else  w:=4+surround;
    end;
    for run:=0 to 1 do begin
      image.Canvas.Pen.width:=w;
      w-=surround;
      if run=0         then image.Canvas.Pen.color:=image.Canvas.Brush.color
      else if preview  then image.Canvas.Pen.color:=$000080ff
                       else image.Canvas.Pen.color:=$00FFFFFF;
      for i:=0 to length(path)-1 do if length(path[i])>1 then begin
                                         image.Canvas.MoveTo(path[i,0,0],path[i,0,1]);
        for j:=1 to length(path[i])-1 do image.Canvas.LineTo(path[i,j,0],path[i,j,1]);
      end;
    end;
  end;

FUNCTION wireToScreen(CONST wire:T_wirePathArray; CONST x0, y0: longint; CONST zoom: longint):T_wirePathArray;
  VAR i,j:longint;
      p:T_point;
  begin
    setLength(result,length(wire));
    for i:=0 to length(wire)-1 do begin
      setLength(result[i],length(wire[i]));
      for j:=0 to length(wire[i])-1 do begin
        p:=wire[i,j];
        p[0]:=x0+p[0]*zoom;
        p[1]:=y0+p[1]*zoom;
        result[i,j]:=p;
      end;
    end;
  end;

FUNCTION wireToScreen(CONST wire:T_wirePath; CONST x0, y0: longint; CONST zoom: longint):T_wirePathArray;
  VAR wrap:T_wirePathArray;
  begin
    setLength(wrap,1);
    wrap[0]:=wire;
    result:=wireToScreen(wrap,x0,y0,zoom);
    setLength(wrap,0);
  end;

PROCEDURE T_visualWire.paint(CONST x0, y0: longint; CONST zoom: longint; CONST image: TImage);
  VAR wrap:T_wirePathArray;
      i:longint;
  begin
    setLength(wrap,length(sink));
    for i:=0 to length(wrap)-1 do wrap[i]:=sink[i].path;
    paintWire(image,
              source^.behavior^.outputWidth(sourceOutputIndex),
              wireToScreen(wrap,x0,y0,zoom));
    setLength(wrap,0);
  end;

{ T_visualBoard }

constructor T_visualBoard.create(const palette: P_abstractPrototypeSource);
  begin
    associatedPalette:=palette;
    gridOutputX0:=8;
    gridOutputY0:=8;
  end;

destructor T_visualBoard.destroy;
begin

end;

procedure T_visualBoard.checkSizes;
  VAR e:P_visualGate;
      zoom:longint;
      x0,y0:longint;
      requiredWidth,requiredHeight:longint;

      minVisX,minVisY,maxVisX,maxVisY:longint;
  begin

    //TODO: Scroll bar handling
    ui.horizontalScrollBar.visible:=false;
    ui.verticalScrollBar.visible:=false;
    zoom:=ui.uiAdapter^.zoom;

    requiredWidth :=(gridOutputX0+4)*zoom;
    requiredHeight:=(gridOutputY0+4)*zoom;

    if requiredWidth>ui.wireImage.width then begin
      ui.horizontalScrollBar.visible:=true;
      ui.horizontalScrollBar.max:=requiredWidth-ui.wireImage.width;
      ui.horizontalScrollBar.visible:=true;
    end else begin
      ui.horizontalScrollBar.position:=0;
      ui.horizontalScrollBar.visible:=false;
    end;

    if requiredHeight>ui.wireImage.height then begin
      ui.verticalScrollBar.visible:=true;
      ui.verticalScrollBar.max:=requiredHeight-ui.wireImage.height;
      ui.verticalScrollBar.visible:=true;
    end else begin
      ui.verticalScrollBar.position:=0;
      ui.verticalScrollBar.visible:=false;
    end;

    x0:=ui.wireImage.Left-ui.horizontalScrollBar.position;
    y0:=ui.wireImage.top -ui.verticalScrollBar.position;

    writeln('repainting with origin ',x0,',',y0);
    for e in inputs  do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in outputs do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in gates   do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);

    paintWires;
  end;

procedure T_visualBoard.detachUI;
  begin
    ui.uiAdapter:=nil;
  end;

procedure T_visualBoard.attachUI(const wireImage: TImage;
  const horizontalScrollBar, verticalScrollBar: TScrollBar;
  const uiAdapter: P_uiAdapter);
  VAR e:P_visualGate;
  begin
    ui.wireImage:=wireImage;
    ui.horizontalScrollBar:=horizontalScrollBar;
    ui.verticalScrollBar :=verticalScrollBar;
    ui.uiAdapter:=uiAdapter;
    uiAdapter^.activeBoard:=@self;
    for e in inputs  do e^.uiAdapter:=uiAdapter;
    for e in outputs do e^.uiAdapter:=uiAdapter;
    for e in gates   do e^.uiAdapter:=uiAdapter;
  end;

procedure T_visualBoard.elementAdded(const newElement: P_visualGate;
  const screenX, screenY: longint);
  VAR element:P_visualGate;
  begin
    element:=newElement;
    if (screenX<ui.wireImage.Left) or (screenY<ui.wireImage.top)
    then dispose(element,destroy)
    else begin
      case element^.behavior^.gateType of
        gt_input:  begin setLength(inputs ,length(inputs )+1); inputs [length(inputs )-1]:=element; end;
        gt_output: begin setLength(outputs,length(outputs)+1); outputs[length(outputs)-1]:=element; end;
        else       begin setLength(gates  ,length(gates  )+1); gates  [length(gates  )-1]:=element; end;
      end;
      repositionElement(element, screenX, screenY);
      element^.setBoardElementMouseActions;
    end;
  end;

procedure T_visualBoard.repositionElement(const element: P_visualGate;
  const screenX, screenY: longint);
  VAR gridX, gridY: longint;
      i,j:longint;

  PROCEDURE reshapeGrid(CONST newGridOutputX0,newGridOutputY0:longint);
    VAR gate:P_visualGate;
    begin
      for gate in outputs do begin
        if gate^.gridPos[0]=gridOutputX0 then gate^.gridPos[0]:=newGridOutputX0;
        if gate^.gridPos[1]=gridOutputY0 then gate^.gridPos[1]:=newGridOutputY0;
      end;
      gridOutputX0:=newGridOutputX0;
      gridOutputY0:=newGridOutputY0;
      rewire;
      checkSizes;
    end;

  FUNCTION overlapsAnyOther:boolean;
    VAR other:P_visualGate;
    begin
      for other in inputs  do if (element<>other) and element^.overlaps(other) then exit(true);
      for other in outputs do if (element<>other) and element^.overlaps(other) then exit(true);
      for other in gates   do if (element<>other) and element^.overlaps(other) then exit(true);
      result:=false;
    end;

  PROCEDURE repositionVertically;
    VAR stepY:longint=1;
    begin
      if element^.gridPos[1]<4 then element^.gridPos[1]:=4;
      if element^.gridPos[1]+4>gridOutputY0 then reshapeGrid(gridOutputX0,element^.gridPos[1]+4);
      while overlapsAnyOther do begin
        element^.gridPos[1]+=stepY;
        if stepY<0 then stepY:=-1-stepY else stepY:=1-stepY;
        if element^.gridPos[1]<element^.gridHeight then element^.gridPos[1]:=element^.gridHeight;
        if element^.gridPos[1]>       gridOutputY0 then element^.gridPos[1]:=gridOutputY0;
      end;
    end;

  PROCEDURE repositionHorizontally;
    VAR stepX:longint=1;
    begin
      if element^.gridPos[0]<4 then element^.gridPos[0]:=4;
      if element^.gridPos[0]+4>gridOutputX0 then reshapeGrid(element^.gridPos[0]+4,gridOutputY0);
      while overlapsAnyOther do begin
        element^.gridPos[0]+=stepX;
        if stepX<0 then stepX:=-1-stepX else stepX:=1-stepX;
        if element^.gridPos[0]<element^.gridWidth then element^.gridPos[0]:=element^.gridWidth;
        if element^.gridPos[0]>gridOutputX0       then element^.gridPos[0]:=gridOutputX0;
      end;
    end;

  PROCEDURE reposition;
    VAR stepX:longint=1;
        stepY:longint=0;
        initialX,initialY:longint;
    begin

      if element^.gridPos[1]<4 then element^.gridPos[1]:=4;
      if element^.gridPos[0]<4 then element^.gridPos[0]:=4;
      if element^.gridPos[1]+4>gridOutputY0 then reshapeGrid(gridOutputX0,element^.gridPos[1]+4);
      if element^.gridPos[0]+4>gridOutputX0 then reshapeGrid(element^.gridPos[0]+4,gridOutputY0);
      initialX:=element^.gridPos[0];
      initialY:=element^.gridPos[1];
      while (overlapsAnyOther or
           (element^.gridPos[0]<4) or (element^.gridPos[0]+4>gridOutputX0) or
           (element^.gridPos[1]<4) or (element^.gridPos[1]+4>gridOutputY0)) and (stepX<100) do begin
        element^.gridPos[0]+=stepX;
        element^.gridPos[1]+=stepY;
        if      stepX>0 then begin stepY:=   stepX; stepX:=0; end
        else if stepY>0 then begin stepX:=-1-stepY; stepY:=0; end
        else if stepX<0 then begin stepY:=   stepX; stepX:=0; end
        else                 begin stepX:= 1-stepY; stepY:=0; end;
      end;
      if not(overlapsAnyOther) then exit;
      element^.gridPos[0]:=initialX;
      element^.gridPos[1]:=initialY;
      while (overlapsAnyOther or
           (element^.gridPos[0]<4) or
           (element^.gridPos[1]<4)) and (stepX<100) do begin
        element^.gridPos[0]+=stepX;
        element^.gridPos[1]+=stepY;
        if      stepX>0 then begin stepY:=   stepX; stepX:=0; end
        else if stepY>0 then begin stepX:=-1-stepY; stepY:=0; end
        else if stepX<0 then begin stepY:=   stepX; stepX:=0; end
        else                 begin stepX:= 1-stepY; stepY:=0; end;
        if element^.gridPos[1]+4>gridOutputY0 then reshapeGrid(gridOutputX0,element^.gridPos[1]+4);
        if element^.gridPos[0]+4>gridOutputX0 then reshapeGrid(element^.gridPos[0]+4,gridOutputY0);
      end;
    end;

  begin
    gridX:=round((screenX-ui.wireImage.Left+ui.horizontalScrollBar.position)/ui.uiAdapter^.getZoom);
    gridY:=round((screenY-ui.wireImage.top +ui.verticalScrollBar  .position)/ui.uiAdapter^.getZoom);

    if (gridX<0) or (gridY<0) then begin
      j:=0;
      for i:=0 to length(wires)-1 do begin
        wires[i].dropWiresAssociatedWith(element);
        if length(wires[i].sink)>0 then begin
          wires[j]:=wires[i];
          inc(j);
        end;
      end;
      setLength(wires,j);

      j:=0; for i:=0 to length(inputs )-1 do if inputs [i]=element then dispose(inputs [i],destroy) else begin inputs [j]:=inputs [i]; inc(j); end; setLength(inputs ,j);
      j:=0; for i:=0 to length(outputs)-1 do if outputs[i]=element then dispose(outputs[i],destroy) else begin outputs[j]:=outputs[i]; inc(j); end; setLength(outputs,j);
      j:=0; for i:=0 to length(gates  )-1 do if gates  [i]=element then dispose(gates  [i],destroy) else begin gates  [j]:=gates  [i]; inc(j); end; setLength(gates  ,j);

      rewire;
    end else begin
      element^.gridPos[0]:=gridX;
      element^.gridPos[1]:=gridY;
      case element^.behavior^.gateType of
        gt_input: begin
          if element^.gridPos[0]<=element^.gridPos[1] then begin
            element^.gridPos[0]:=0;
            repositionVertically;
            if not(P_inputGate(element^.behavior)^.onLeftOrRightSide) then begin
              P_inputGate(element^.behavior)^.onLeftOrRightSide:=true;
              element^.propertyEditDone(false);
            end;
          end else begin
            element^.gridPos[1]:=0;
            repositionHorizontally;
            if P_inputGate(element^.behavior)^.onLeftOrRightSide then begin
              P_inputGate(element^.behavior)^.onLeftOrRightSide:=false;
              element^.propertyEditDone(false);
            end;
          end;
        end;
        gt_output: begin
          if gridOutputX0-element^.gridPos[0]<gridOutputY0-element^.gridPos[1] then begin
            element^.gridPos[0]:=gridOutputX0;
            repositionVertically;
            if not(P_outputGate(element^.behavior)^.onLeftOrRightSide) then begin
              P_outputGate(element^.behavior)^.onLeftOrRightSide:=true;
              element^.propertyEditDone(false);
            end;
          end else begin
            element^.gridPos[1]:=gridOutputY0;
            repositionHorizontally;
            if P_outputGate(element^.behavior)^.onLeftOrRightSide then begin
              P_outputGate(element^.behavior)^.onLeftOrRightSide:=false;
              element^.propertyEditDone(false);
            end;
          end;
        end;
        else begin
          reposition;
        end;
      end;
      element^.paintAll(element^.gridPos[0]*ui.uiAdapter^.getZoom+ui.wireImage.Left-ui.horizontalScrollBar.position,
                        element^.gridPos[1]*ui.uiAdapter^.getZoom+ui.wireImage.top -ui.verticalScrollBar.position,
                                       ui.uiAdapter^.getZoom);
      rewire;
    end;
  end;

function T_visualBoard.loadFromStream(var stream: T_bufferedInputStreamWrapper
  ): boolean;
begin

end;

procedure T_visualBoard.saveToStream(var stream: T_bufferedOutputStreamWrapper);
begin

end;

procedure T_visualBoard.savePaletteEntryToStream(
  var stream: T_bufferedOutputStreamWrapper; const paletteIndex: longint);
  begin
    myIndex:=paletteIndex;
  end;

procedure T_visualBoard.loadPaletteEntryFromStream(
  var stream: T_bufferedInputStreamWrapper; const paletteIndex: longint);
  begin
    myIndex:=paletteIndex;
  end;

function T_visualBoard.extractBehavior: P_circuitBoard;
  VAR cloned:P_circuitBoard;
      i,j:longint;
  FUNCTION gateInClone(CONST gate:P_visualGate):P_abstractGate;
    VAR i:longint;
    begin
      result:=nil;
      for i:=0 to length(inputs )-1 do if inputs [i]=gate then exit(cloned^.inputs[i]);
      for i:=0 to length(outputs)-1 do if outputs[i]=gate then exit(cloned^.outputs[i]);
      for i:=0 to length(gates  )-1 do if gates  [i]=gate then exit(cloned^.gates[i]);
      assert(result<>nil,'Cloning of T_circuitBoard failed');
    end;

  begin
    new(cloned,create(associatedPalette));

    cloned^.prototype:=@self;

    cloned^.captionString:=captionString;
    cloned^.descriptionString:=descriptionString;
    setLength(cloned^.inputs ,length(inputs )); for i:=0 to length(inputs )-1 do cloned^.inputs [i]:=P_inputGate (inputs [i]^.behavior^.clone(false));
    setLength(cloned^.outputs,length(outputs)); for i:=0 to length(outputs)-1 do cloned^.outputs[i]:=P_outputGate(outputs[i]^.behavior^.clone(false));
    setLength(cloned^.gates  ,length(gates));   for i:=0 to length(gates  )-1 do cloned^.gates  [i]:=             gates  [i]^.behavior^.clone(false);
    setLength(cloned^.wires,length(wires));
    for i:=0 to length(wires)-1 do begin
      cloned^.wires[i].source:=gateInClone(wires[i].source);
      cloned^.wires[i].sourceOutputIndex:= wires[i].sourceOutputIndex;
      setLength(cloned^.wires[i].sink,length(wires[i].sink));
      for j:=0 to length(wires[i].sink)-1 do begin
        cloned^.wires[i].sink[j].gate:=gateInClone(wires[i].sink[j].gate);
        cloned^.wires[i].sink[j].gateInputIndex:=  wires[i].sink[j].gateInputIndex;
      end;
    end;
    result:=cloned;
  end;

function T_visualBoard.getWireGraph(const dropExistingWires: boolean
  ): P_wireGraph;
  VAR graph:P_wireGraph;
  PROCEDURE punchOutGate(CONST gate:P_visualGate);
    VAR x,y:longint;
        i:longint;
        p: T_point;
    begin
      for y:=gate^.gridPos[1] to gate^.gridPos[1]+gate^.gridHeight do
      for x:=gate^.gridPos[0] to gate^.gridPos[0]+gate^.gridWidth do graph^.dropNode(pointOf(x,y));

      for i:=0 to gate^.behavior^.numberOfOutputs-1 do begin
        p:=gate^.getOutputPositionInGridSize(i)+gate^.gridPos;
        if p[0]=gate^.gridPos[0]+gate^.gridWidth
        then graph^.addUnidirectionalEdge(p,wd_right)
        else graph^.addUnidirectionalEdge(p,wd_down);
      end;
      for i:=0 to gate^.behavior^.numberOfInputs-1 do begin
        p:=gate^.getInputPositionInGridSize(i)+gate^.gridPos;
        if p[0]=gate^.gridPos[0]
        then graph^.addUnidirectionalEdge(p+wd_left,wd_right)
        else graph^.addUnidirectionalEdge(p+wd_up  ,wd_down);
      end;
    end;

  VAR gate:P_visualGate;
      i,j:longint;
  begin
    new(graph,create(gridOutputX0+1,gridOutputY0+1));
    graph^.initDirections;
    for gate in inputs  do punchOutGate(gate);
    for gate in outputs do punchOutGate(gate);
    for gate in gates   do punchOutGate(gate);

    if not(dropExistingWires) then exit(graph);
    for i:=0 to length(wires)-1 do
    for j:=0 to length(wires[i].sink)-1 do
      graph^.dropWire(wires[i].sink[j].path);

    result:=graph;
  end;

procedure T_visualBoard.rewire;
  VAR graph:P_wireGraph;
  VAR i,j:longint;
      startPoint:T_point;
      endPoints: T_wirePath;
      paths: T_wirePathArray;
  begin
    graph:=getWireGraph(false);
    for i:=0 to length(wires)-1 do begin
      startPoint:=wires[i].source^.getOutputPositionInGridSize(wires[i].sourceOutputIndex)+wires[i].source^.gridPos;
      setLength(endPoints,length(wires[i].sink));
      for j:=0 to length(endPoints)-1 do endPoints[j]:=wires[i].sink[j].gate^.getInputPositionInGridSize(wires[i].sink[j].gateInputIndex)+wires[i].sink[j].gate^.gridPos;
      paths:=graph^.findPaths(startPoint,endPoints,true);
      for j:=0 to length(paths)-1 do begin
        wires[i].sink[j].path:=paths[j];
        graph^.dropWire(paths[j]);
      end;
    end;
    paintWires;
    dispose(graph,destroy);
  end;

procedure T_visualBoard.paintWires;
  VAR i:longint;
  begin
//    writeln('Painting image @',ui.wireImage.Width,',',ui.wireImage.Height,' (',ui.wireImage.Canvas.Width,',',ui.wireImage.Canvas.Height,') ',flip);
//    ui.wireImage.picture.Bitmap.setSize(ui.wireImage.width,ui.wireImage.height);
    ui.wireImage.visible:=true;
    ui.wireImage.Canvas.Brush.color:=$00804040;

    ui.wireImage.Canvas.FillRect(0,0,ui.wireImage.width,ui.wireImage.height);

    for i:=0 to length(wires)-1 do
      wires[i].paint(-ui.horizontalScrollBar.position,
                     -ui.verticalScrollBar  .position,
                      ui.uiAdapter^.zoom,
                      ui.wireImage);

  end;

procedure T_visualBoard.paintWirePreview(const wireStart: T_point;
  const screenX, screenY: longint);
  CONST MAX_INT64= 9223372036854775807;
  VAR tgt ,closestTarget:T_point;
      dist,distToClosest:int64;

      g:P_visualGate;
      i:longint;
      graph: P_wireGraph;
      wirePath: T_wirePath;

      connectible:array of P_visualGate;

  FUNCTION wiresStartingAt(CONST gate:P_visualGate; CONST outputIndex:longint):T_wirePathArray;
    VAR i,j:longint;
    begin
      for i:=0 to length(wires)-1 do with wires[i] do if (source=gate) and (sourceOutputIndex=outputIndex) then begin
        setLength(result,length(sink));
        for j:=0 to length(sink)-1 do result[j]:=sink[j].path;
        exit(result);
      end;
      setLength(result,0);
    end;

  begin
    distToClosest:=MAX_INT64;
    setLength(connectible,length(gates)+length(outputs));
    for i:=0 to length(gates)-1 do connectible[i]:=gates[i];
    for i:=0 to length(outputs)-1 do connectible[length(gates)+i]:=outputs[i];

    for g in connectible do for i:=0 to g^.behavior^.numberOfInputs-1 do if not(isInputConnected(g,i)) then begin
      tgt:=g^.getInputPositionInGridSize(i)+g^.gridPos;
      dist:=sqr(tgt[0]*ui.uiAdapter^.getZoom+ui.wireImage.Left-ui.horizontalScrollBar.position-screenX)
           +sqr(tgt[1]*ui.uiAdapter^.getZoom+ui.wireImage.top -ui.verticalScrollBar  .position-screenY);
      if dist<distToClosest then begin
        ui.uiAdapter^.dragData.dragTarget:=g;
        ui.uiAdapter^.dragData.inputIndex:=i;
        distToClosest:=dist;
        closestTarget:=tgt;
      end;
    end;

    if distToClosest=MAX_INT64 then exit;

    graph:=getWireGraph(true);

    wirePath:=graph^.findPath(wireStart,closestTarget,wiresStartingAt(ui.uiAdapter^.dragData.draggedGate,ui.uiAdapter^.dragData.outputIndex));
    paintWires;

    paintWire(ui.wireImage,
              ui.uiAdapter^.dragData.draggedGate^.behavior^.outputWidth(ui.uiAdapter^.dragData.outputIndex),
              wireToScreen(wirePath,-ui.horizontalScrollBar.position,
                                    -ui.verticalScrollBar  .position,
                                     ui.uiAdapter^.zoom),true);

  end;

function T_visualBoard.getCaption: string;
begin
  result:=captionString;
end;

procedure T_visualBoard.setCaption(const s: string);
begin
  captionString:=s;
end;

function T_visualBoard.getDescription: string;
begin
  result:=descriptionString;
end;

procedure T_visualBoard.setDescription(const s: string);
begin
  descriptionString:=s;
end;

function T_visualBoard.getIndexInPalette: longint;
begin
  result:=myIndex;
end;

procedure T_visualBoard.afterGatePropertiesEdited(const editedGate: P_visualGate
  );
  VAR g:P_visualGate;
  begin
    for g in gates do if g^.behavior^.equals(editedGate^.behavior) then g^.propertyEditDone(false);
    rewire;
  end;

function T_visualBoard.isInputConnected(const gate: P_visualGate;
  const gateInputIndex: longint): boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=0 to length(wires)-1 do for j:=0 to length(wires[i].sink)-1 do
      if (wires[i].sink[j].gate=gate) and
         (wires[i].sink[j].gateInputIndex=gateInputIndex) then exit(true);
  end;

function T_visualBoard.isOutputputConnected(const gate: P_visualGate;
  const gateOutputIndex: longint): boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(wires)-1 do with wires[i] do if (source=gate) and (sourceOutputIndex=gateOutputIndex) then exit(true);
  end;

procedure T_visualBoard.addWire(const sourceGate: P_visualGate;
  const sourceOutputIndex: longint; const sinkGate: P_visualGate;
  const sinkInputIndex: longint);
  VAR i:longint=0;
      j:longint;
  begin
    while (i<length(wires)) and ((wires[i].source<>sourceGate) or (wires[i].sourceOutputIndex<>sourceOutputIndex)) do inc(i);
    if i>=length(wires) then setLength(wires,i+1);
    wires[i].source:=sourceGate;
    wires[i].sourceOutputIndex:=sourceOutputIndex;
    j:=length(wires[i].sink);
    setLength(wires[i].sink,j+1);
    wires[i].sink[j].gate:=sinkGate;
    wires[i].sink[j].gateInputIndex:=sinkInputIndex;
    rewire;
    if wires[i].simulateStep then sinkGate^.updateVisuals;
  end;

function T_visualBoard.simulateSteps(const count: longint): longint;
  VAR changed:boolean;
      step:longint;
      g:P_visualGate;
      i:longint;
  begin
    result:=0;
    for step:=1 to count do begin
      changed:=false;
      for g in inputs             do if g^.simulateStep       then changed:=true;
      for i:=0 to length(wires)-1 do if wires[i].simulateStep then changed:=true;
      for g in gates              do if g^.simulateStep       then changed:=true;
      for g in outputs            do if g^.simulateStep       then changed:=true;
      if changed then inc(result);
    end;
    if (result>0) then begin
      for g in inputs  do g^.updateVisuals;
      for g in gates   do g^.updateVisuals;
      for g in outputs do g^.updateVisuals;
    end;
  end;

{ T_UIAdapter }

constructor T_uiAdapter.create(const mainForm_: TForm;
  const showPropertyEditorCallback_: F_showPropertyEditorCallback);
  begin
    mainForm :=mainForm_;
    zoom:=20;
    state:=uas_initial;
    showPropertyEditorCallback:=showPropertyEditorCallback_;
  end;

procedure T_uiAdapter.paletteEntryMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
  begin
    if state=uas_draggingFromPalette then with dragData do draggedGate^.paintAll(x+startX,y+startY,zoom);

  end;

procedure T_uiAdapter.paletteEntryMouseUp(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR screenX,screenY:longint;
  begin
    if state<>uas_draggingFromPalette then exit;
    state:=uas_initial;
    with dragData do begin
      screenX:=x+startX;
      screenY:=y+startY;
      if activeBoard=nil
      then dispose(draggedGate,destroy)
      else activeBoard^.elementAdded(draggedGate,screenX,screenY);
      draggedGate:=nil;
    end;
  end;

procedure T_uiAdapter.boardElementMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
  begin
    if state=uas_draggingFromBoard then with dragData do begin
      draggedGate^.paintAll(x+startX,y+startY,zoom);
      startX+=x;
      startY+=y;
    end;
  end;

procedure T_uiAdapter.boardElementMouseUp(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR screenX,screenY:longint;
  begin
    if state=uas_draggingFromBoard then with dragData do begin
      screenX:=x+startX;
      screenY:=y+startY;
      activeBoard^.repositionElement(draggedGate,screenX,screenY);
      draggedGate:=nil;
    end;
    state:=uas_initial;
  end;

procedure T_uiAdapter.boardOutputMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
  begin
    if state=uas_draggingWire then begin
      activeBoard^.paintWirePreview(dragData.draggedGate^.getOutputPositionInGridSize(dragData.outputIndex)
                                   +dragData.draggedGate^.gridPos,
                                   x+dragData.startX,
                                   y+dragData.startY);
    end;
  end;

procedure T_uiAdapter.boardOutputMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if state=uas_draggingWire then with dragData do
    if (activeBoard<>nil) and (dragTarget<>nil) then
      activeBoard^.addWire(draggedGate,outputIndex,dragTarget,inputIndex);
    state:=uas_initial;
  end;

procedure T_uiAdapter.paletteSizeUpdated(const Right: longint);
  begin
    if activeBoard<>nil then begin
      activeBoard^.ui.wireImage.Left:=Right;
      activeBoard^.checkSizes;
    end;
  end;

procedure T_uiAdapter.propertyEditorShown(const gate: P_visualGate;
  const fromBoard: boolean);
  begin
    if fromBoard
    then state:=uas_propertyEditFromBoard
    else state:=uas_propertyEditFromPalette;
    dragData.draggedGate:=gate;
  end;

procedure T_uiAdapter.zoomIn;
  begin
    zoom:=round(zoom*1.1);
  end;

procedure T_uiAdapter.zoomOut;
  begin
    zoom:=round(zoom/1.1);
  end;

PROCEDURE T_visualGate.ioEditKeyPress(Sender: TObject; VAR key: char);
  VAR AllowedKeys:array[T_multibitWireRepresentation] of set of char=
      (['0','1'     ,#8], //wr_binary,
       ['0'..'9'    ,#8], //wr_decimal,
       ['0'..'9','-',#8]);//wr_2complement
  begin
    if key=#13 then ioEditEditingDone(Sender)
    else if not(key in AllowedKeys[ioMode]) then key:=#0;
  end;

PROCEDURE T_visualGate.ioEditEditingDone(Sender: TObject);
  begin
    behavior^.setInput(0,parseWire(ioEdit.text,behavior^.inputWidth(0),ioMode));
    ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    updateVisuals;
  end;

CONSTRUCTOR T_visualGate.create(CONST behavior_: P_abstractGate);
  begin
    uiAdapter :=nil;
    gridPos[0]:=0;
    gridPos[1]:=0;
    gridWidth :=0;
    gridHeight:=0;
    behavior:=behavior_;

    setLength(shapes,0);
    setLength(labels,0);
  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    disposeGuiElements;
  end;

PROCEDURE T_visualGate.ensureGuiElements(CONST container: TWinControl);
  VAR gateType:T_gateType;
      i:longint;
      index:longint=1;
  begin
    if length(shapes)>0 then exit;
    ioLocations:=behavior^.getIoLocations;

    gridHeight:=1;
    if (length(ioLocations[gt_input])>0) and (length(ioLocations[gt_output])>0)
    then gridWidth:=2
    else gridWidth:=1;

    for gateType:=gt_input to gt_output do for i:=0 to length(ioLocations[gateType])-1 do begin
      if ioLocations[gateType,i].leftOrRight then begin
        if ioLocations[gateType,i].positionIndex>=gridHeight then gridHeight:=ioLocations[gateType,i].positionIndex+1;
      end else begin
        if ioLocations[gateType,i].positionIndex>=gridWidth  then gridWidth :=ioLocations[gateType,i].positionIndex+1;
      end;
    end;
    gridWidth*=2;
    gridHeight*=2;

    setLength(shapes,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
    setLength(labels,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
    shapes[0]:=TShape.create(nil);
    shapes[0].parent:=container;
    shapes[0].Shape :=stRectangle;
    shapes[0].Brush.color:=$00603030;

    labels[0]:=TLabel.create(nil);
    labels[0].caption:=behavior^.getCaption;
    labels[0].AutoSize:=true;
    labels[0].Font.size:=6;
    labels[0].Font.color:=$00FFFFFF;
    labels[0].parent:=container;
    labels[0].AnchorVerticalCenterTo  (shapes[0]);
    labels[0].AnchorHorizontalCenterTo(shapes[0]);

    for i:=0 to behavior^.numberOfInputs-1 do begin
      shapes[index]:=TShape.create(nil);
      shapes[index].Shape:=stCircle;
      shapes[index].Brush.color:=$00804040;
      shapes[index].Tag:=i;
      shapes[index].parent:=container;

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=ioLocations[gt_input,i].ioLabel;
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].AnchorVerticalCenterTo(shapes[index]);
      labels[index].AnchorHorizontalCenterTo(shapes[index]);
      inc(index);
    end;

    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[index]:=TShape.create(nil);
      shapes[index].Shape:=stCircle;
      shapes[index].Tag:=i;
      shapes[index].Brush.color:=$00804040;
      shapes[index].parent:=container;

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=ioLocations[gt_output,i].ioLabel;
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].AnchorVerticalCenterTo(shapes[index]);
      labels[index].AnchorHorizontalCenterTo(shapes[index]);
      inc(index);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      //TODO: Add custom behavior
      labels[0].AnchorParallel(akLeft,5,shapes[0]);
      labels[0].AnchorParallel(akTop ,5,shapes[0]);
      ioMode:=wr_binary;

      ioEdit:=TEdit.create(nil);
      ioEdit.parent:=container;
      ioEdit.Font.color:=$00FFFFFF;
      ioEdit.color:=$00A05050;
      ioEdit.readonly:=behavior^.gateType=gt_output;

      setLength(shapes,index+1);
      setLength(labels,index+1);
      shapes[index]:=TShape.create(nil);
      shapes[index].parent:=container;
      shapes[index].Shape :=stRoundRect;
      shapes[index].Brush.color:=$00A05050;
      gridWidth:=4;
      gridHeight:=4;

      labels[index]:=TLabel.create(nil);
      labels[index].caption:=C_multibitWireRepresentationName[wr_binary];
      labels[index].AutoSize:=true;
      labels[index].Font.size:=6;
      labels[index].Font.color:=$00FFFFFF;
      labels[index].parent:=container;
      labels[index].AnchorVerticalCenterTo(shapes[index]);
      labels[index].AnchorHorizontalCenterTo(shapes[index]);
    end;

  end;

PROCEDURE T_visualGate.disposeGuiElements;
  VAR i:longint;
  begin
    for i:=0 to length(labels)-1 do begin
      labels[i].Anchors:=[];
      FreeAndNil(labels[i]);
    end;
    setLength(labels,0);
    if ioEdit<>nil then FreeAndNil(ioEdit);
    for i:=length(shapes)-1 downto 0 do begin
      shapes[i].Anchors:=[];
      FreeAndNil(shapes[i]);
    end;
    setLength(shapes,0);
  end;

FUNCTION T_visualGate.simulateStep: boolean;
  begin
    result:=behavior^.simulateStep;
  end;

PROCEDURE T_visualGate.updateVisuals;
  FUNCTION colorOf(CONST w:T_wireValue):longint;
    begin
      if w.width>1 then begin
        if isFullyDefined(w)
        then result:=$00606060
        else result:=$00804040;
      end else case w.bit[0] of
        tsv_true        : result:=$0000AAFF;
        tsv_false       : result:=$00000000;
        tsv_undetermined: result:=$00804040;
      end;
    end;

  VAR Shape:TShape;
      shapeIndex:longint=1;
      i:longint;
  begin
    if length(shapes)=0 then exit;
    if marked
    then for Shape in shapes do Shape.Pen.color:=$00FFFFFF
    else for Shape in shapes do Shape.Pen.color:=$00000000;

    for i:=0 to behavior^.numberOfInputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getInput(i));
      inc(shapeIndex);
    end;
    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getOutput(i));
      inc(shapeIndex);
    end;

    if (ioEdit<>nil) and not(ioEdit.Focused) then ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
  end;

PROCEDURE T_visualGate.paintAll(CONST x, y: longint; CONST zoom: longint);
  VAR k: integer;
      p: T_point;
      shapeIndex:longint=1;
      zoomChanged:boolean=false;
  begin

    shapes[0].Left  :=x;
    shapes[0].top   :=y;
    zoomChanged:=shapes[0].width<>zoom*gridWidth;
    shapes[0].width :=zoom*gridWidth;
    shapes[0].height:=zoom*gridHeight;

    for k:=0 to behavior^.numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=x+p[0]*zoom-zoom div 2;
      shapes[shapeIndex].top   :=y+p[1]*zoom-zoom div 2;
      shapes[shapeIndex].width :=                   zoom;
      shapes[shapeIndex].height:=                   zoom;
      inc(shapeIndex);
    end;

    for k:=0 to behavior^.numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=x+p[0]*zoom-zoom div 2;
      shapes[shapeIndex].top   :=y+p[1]*zoom-zoom div 2;
      shapes[shapeIndex].width :=                   zoom;
      shapes[shapeIndex].height:=                   zoom;
      inc(shapeIndex);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      ioEdit.Left:=shapes[0].Left+5;
      ioEdit.top:=shapes[0].top+shapes[0].height-5-ioEdit.height;
      ioEdit.width:=shapes[0].width-10;

      shapes[2].Left  :=shapes[0].Left+shapes[0].width-zoom*2;
      shapes[2].width :=                               zoom*2;
      shapes[2].top   :=ioEdit.top-ioEdit.height-5;
      shapes[2].height:=ioEdit.height;
    end;

    if not(zoomChanged) then exit;

    for k:=0 to length(shapes)-1 do
      labels[k].Font.size:=min(round(labels[k].Font.size*shapes[k].width  *0.75/labels[k].width),
                               round(labels[k].Font.size*shapes[k].height *0.75/labels[k].height));

  end;

FUNCTION T_visualGate.clone: P_visualGate;
  begin
    new(result,create(behavior^.clone(false)));
    result^.uiAdapter:=uiAdapter;
  end;

PROCEDURE T_visualGate.propertyEditDone(CONST paletteElement: boolean);
  begin
    disposeGuiElements;
    ensureGuiElements(uiAdapter^.mainForm);
    paintall(gridPos[0]*uiAdapter^.getZoom,
             gridPos[1]*uiAdapter^.getZoom,uiAdapter^.getZoom);
    updateVisuals;
    if paletteElement
    then setPaletteEntryMouseActions
    else setBoardElementMouseActions;
  end;

FUNCTION T_visualGate.visualHeight: longint;
  VAR s:TShape;
      y0:longint= 1000000;
      y1:longint=-1000000;
      Bottom:longint;
  begin
    for s in shapes do begin
      Bottom:=s.top+s.height;
      if Bottom>y1 then y1:=Bottom;
      if s. top<y0 then y0:=s.top;
    end;
    result:=y1-y0;
    if result<0 then result:=0;
  end;

FUNCTION T_visualGate.visualWidth: longint;
  VAR s:TShape;
      x0:longint= 1000000;
      x1:longint=-1000000;
      Right:longint;
  begin
    for s in shapes do begin
      Right:=s.Left+s.width;
      if Right >x1 then x1:=Right;
      if s.Left<x0 then x0:=s.Left;
    end;
    result:=x1-x0;
    if result<0 then result:=0;
  end;

FUNCTION T_visualGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    if ioLocations[gt_input,index].leftOrRight
    then begin
      result[0]:=0;
      result[1]:=(ioLocations[gt_input,index].positionIndex*2-(length(ioLocations[gt_input])-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations[gt_input,index].positionIndex*2-(length(ioLocations[gt_input])-1))+gridWidth div 2;
      result[1]:=0;
    end;
  end;

FUNCTION T_visualGate.getOutputPositionInGridSize(CONST index: longint): T_point;
  begin
    if ioLocations[gt_output,index].leftOrRight
    then begin
      result[0]:=gridWidth;
      result[1]:=(ioLocations[gt_output,index].positionIndex*2-(length(ioLocations[gt_output])-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations[gt_output,index].positionIndex*2-(length(ioLocations[gt_output])-1))+gridWidth div 2;
      result[1]:=gridHeight;
    end;
  end;

FUNCTION T_visualGate.overlaps(CONST other: P_visualGate): boolean;
  begin
    result:=(max(gridPos[0],other^.gridPos[0])<min(gridPos[0]+gridWidth ,other^.gridPos[0]+other^.gridWidth ))
        and (max(gridPos[1],other^.gridPos[1])<min(gridPos[1]+gridHeight,other^.gridPos[1]+other^.gridHeight));
  end;

PROCEDURE T_visualGate.setPaletteEntryMouseActions;
  begin
    if length(shapes)=0 then exit;
    shapes[0].OnMouseDown:=@paletteEntryMouseDown;
    labels[0].OnMouseDown:=@paletteEntryMouseDown;
    shapes[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
    labels[0].OnMouseMove:=@uiAdapter^.paletteEntryMouseMove;
    shapes[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
    labels[0].OnMouseUp:=@uiAdapter^.paletteEntryMouseUp;
  end;

PROCEDURE T_visualGate.paletteEntryMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;

    if button=mbLeft then begin
      uiAdapter^.state:=uas_draggingFromPalette;
      with uiAdapter^.dragData do begin
        draggedGate:=clone;
        startX:=shapes[0].Left;
        startY:=shapes[0].top;
        draggedGate^.ensureGuiElements(uiAdapter^.mainForm);
        draggedGate^.paintAll(x+startX,y+startY,uiAdapter^.zoom);
      end;
    end else if button=mbRight then begin
      uiAdapter^.showPropertyEditorCallback(@self,false,shapes[0].left+shapes[0].Width,shapes[0].Top);
    end
  end;

PROCEDURE T_visualGate.boardElementMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;

    if button=mbLeft then begin
      uiAdapter^.state:=uas_draggingFromBoard;
      with uiAdapter^.dragData do begin
        draggedGate:=@self;
        startX:=shapes[0].Left;
        startY:=shapes[0].top;
      end;
    end else if button=mbRight then begin
      uiAdapter^.showPropertyEditorCallback(@self,true,shapes[0].left+shapes[0].Width,shapes[0].Top);
    end;
  end;

PROCEDURE T_visualGate.boardElementOutputMouseDown(Sender: TObject;  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if uiAdapter=nil then exit;
    if uiAdapter^.state<>uas_initial then exit;

    if button=mbLeft then begin
      uiAdapter^.state:=uas_draggingWire;
      with uiAdapter^.dragData do begin
        startX:=TGraphicControl(Sender).Left+x;
        startY:=TGraphicControl(Sender).top +y;
        draggedGate:=@self;
        outputIndex:=TGraphicControl(Sender).Tag;
        dragTarget:=nil;
        inputIndex:=-1;
      end;
    end;
  end;

PROCEDURE T_visualGate.ioModeShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  CONST next:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
        prev:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_2complement,wr_binary,wr_decimal);

  begin
    if button=mbLeft
    then ioMode:=next[ioMode]
    else ioMode:=prev[ioMode];
    labels[length(labels)-1].caption:=C_multibitWireRepresentationName[ioMode];
    ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    updateVisuals;
  end;

PROCEDURE T_visualGate.setBoardElementMouseActions;
  VAR index,i:longint;

  begin
    if length(shapes)=0 then exit;
    shapes[0].OnMouseDown:=@boardElementMouseDown;
    labels[0].OnMouseDown:=@boardElementMouseDown;
    shapes[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
    labels[0].OnMouseMove:=@uiAdapter^.boardElementMouseMove;
    shapes[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
    labels[0].OnMouseUp:=@uiAdapter^.boardElementMouseUp;
    index:=1+behavior^.numberOfInputs;
    for i:=0 to behavior^.numberOfOutputs-1 do begin
      shapes[index].Tag:=i;
      shapes[index].OnMouseDown:=@boardElementOutputMouseDown;
      shapes[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
      shapes[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
      labels[index].Tag:=i;
      labels[index].OnMouseDown:=@boardElementOutputMouseDown;
      labels[index].OnMouseMove:=@uiAdapter^.boardOutputMouseMove;
      labels[index].OnMouseUp  :=@uiAdapter^.boardOutputMouseUp;
      inc(index);
    end;

    if behavior^.gateType in [gt_input,gt_output] then begin
      index:=length(shapes)-1;
      shapes[index].OnMouseDown:=@ioModeShapeMouseDown;
      labels[index].OnMouseDown:=@ioModeShapeMouseDown;
    end;

    if behavior^.gateType=gt_input then begin
      ioEdit.OnKeyPress:=@ioEditKeyPress;
      ioEdit.OnEditingDone:=@ioEditEditingDone;
    end;

  end;

end.

