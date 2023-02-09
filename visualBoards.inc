{$ifdef includeInterface}

{ T_visualBoard }

T_visualBoard=object(T_captionedAndIndexed)
  private
    associatedPalette:P_abstractPrototypeSource;
    myIndex:longint;

    captionString:string;
    descriptionString:string;
    inputs : T_visualGateArray;
    outputs: T_visualGateArray;
    gates  : T_visualGateArray;
    wires  : array of T_visualWire;

    ui:record
      wireImage:TImage;
      horizontalScrollBar,verticalScrollBar:TScrollBar;
      uiAdapter:P_uiAdapter;
    end;

    gridOutputX0,gridOutputY0:longint;
    PROCEDURE boardImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE boardImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE boardImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
    PROCEDURE reshapeGrid(CONST newGridOutputX0,newGridOutputY0:longint; CONST byMouseEvent:boolean);
  public
    CONSTRUCTOR create(CONST palette:P_abstractPrototypeSource);
    PROCEDURE clear;
    DESTRUCTOR destroy;

    PROCEDURE setIndexInPalette(CONST index:longint);

    PROCEDURE checkSizes;
    PROCEDURE checkBoardExtend(CONST movingLeft,movingBottom:boolean);

    PROCEDURE detachUI;
    PROCEDURE attachUI(CONST uiAdapter:P_uiAdapter);
    PROCEDURE elementAdded(CONST newElement:P_visualGate; CONST screenX,screenY:longint);
    PROCEDURE repositionElement(CONST element:P_visualGate       ; CONST skipSaveUndo:boolean);
    PROCEDURE repositionElement(CONST elements: T_visualGateArray; CONST skipSaveUndo:boolean);
    PROCEDURE remove(CONST element:P_visualGate       ; CONST skipSaveUndo:boolean);
    PROCEDURE remove(CONST elements: T_visualGateArray; CONST skipSaveUndo:boolean);

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
    PROCEDURE savePaletteEntryToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST paletteIndex:longint);
    PROCEDURE loadPaletteEntryFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST paletteIndex:longint);
    FUNCTION extractBehavior:P_compoundGate;
    FUNCTION clone:P_visualBoard;

    PROCEDURE enumerateIo;

    FUNCTION getWireGraph(CONST dropExistingWires:boolean):P_wireGraph;
    PROCEDURE rewire(CONST forced:boolean);
    PROCEDURE paintWires;
    PROCEDURE paintWirePreview(CONST wireStart:T_point; CONST screenX,screenY:longint);

    FUNCTION getCaption: string; virtual;
    PROCEDURE setCaption(CONST s: string); virtual;
    FUNCTION getDescription: string; virtual;
    PROCEDURE setDescription(CONST s: string); virtual;
    FUNCTION getIndexInPalette: longint; virtual;

    PROCEDURE afterGatePropertiesEdited(CONST editedGate:P_visualGate);

    FUNCTION isInputConnected(CONST gate:P_visualGate; CONST gateInputIndex:longint):boolean;
    FUNCTION isOutputputConnected(CONST gate:P_visualGate; CONST gateOutputIndex:longint):boolean;
    PROCEDURE addWire(CONST sourceGate:P_visualGate; CONST sourceOutputIndex:longint;
                      CONST sinkGate:P_visualGate; CONST sinkInputIndex:longint);

    PROCEDURE reset;
    FUNCTION simulateSteps(CONST count:longint):longint;
    FUNCTION startMultiDrag(CONST primaryGate:P_visualGate):boolean;
    FUNCTION isVisualBoard: boolean; virtual;
    FUNCTION usesPrototype(CONST p:P_captionedAndIndexed):boolean;

    PROCEDURE copySelectionToClipboard;
    PROCEDURE pasteFromClipboard(CONST tgtX,tgtY:longint);
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_visualBoard.create(CONST palette: P_abstractPrototypeSource);
  begin
    associatedPalette:=palette;
    clear;
  end;

PROCEDURE T_visualBoard.clear;
  VAR i:longint;
  begin
    myIndex:=-1;
    gridOutputX0:=8;
    gridOutputY0:=8;
    captionString:='';
    descriptionString:='';
    for i:=0 to length(inputs)-1 do dispose(inputs[i],destroy);
    setLength(inputs,0);
    for i:=0 to length(outputs)-1 do dispose(outputs[i],destroy);
    setLength(outputs,0);
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    setLength(wires,0);
  end;

DESTRUCTOR T_visualBoard.destroy;
  begin
    clear;
    detachUI;
  end;

PROCEDURE T_visualBoard.setIndexInPalette(CONST index: longint);
  begin
    myIndex:=index;
  end;

PROCEDURE T_visualBoard.checkSizes;
  VAR e:P_visualGate;
      zoom:longint;
      x0,y0:longint;
      requiredWidth,requiredHeight:longint;

  begin
    ui.uiAdapter^.BeginFormUpdate();
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

    for e in inputs  do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in outputs do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in gates   do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);

    paintWires;
    ui.uiAdapter^.EndFormUpdate();
  end;

PROCEDURE T_visualBoard.checkBoardExtend(CONST movingLeft, movingBottom: boolean
  );
  PROCEDURE growIfNecessary(CONST gate:P_visualGate);
    VAR corner:T_point;
    begin
      corner:=gate^.gridPos;
      if gate^.behavior^.gateType=gt_output then begin
        if gate^.ioLocations.numberOfTopInputs>0
        then begin corner[0]+=gate^.gridWidth;  if movingBottom then corner[1]:=0; end
        else begin corner[1]+=gate^.gridHeight; if movingLeft   then corner[0]:=0; end;
      end else begin
        corner[0]+=gate^.gridWidth;  if gate^.ioLocations.numberOfRightOutputs >0 then inc(corner[0]);
        corner[1]+=gate^.gridHeight; if gate^.ioLocations.numberOfBottomOutputs>0 then inc(corner[1]);
      end;
      if (corner[0]>gridOutputX0) then gridOutputX0:=corner[0];
      if (corner[1]>gridOutputY0) then gridOutputY0:=corner[1];
    end;

  VAR gate:P_visualGate;
  begin
    writeln('checkBoardExtend@pre  ',gridOutputX0,'x',gridOutputY0);
    for gate in inputs  do growIfNecessary(gate);
    for gate in outputs do growIfNecessary(gate);
    for gate in gates   do growIfNecessary(gate);
    writeln('checkBoardExtend@post ',gridOutputX0,'x',gridOutputY0);
  end;

PROCEDURE T_visualBoard.detachUI;
  VAR e:P_visualGate;
  begin
    for e in inputs  do begin e^.uiAdapter:=nil; e^.disposeGuiElements; end;
    for e in outputs do begin e^.uiAdapter:=nil; e^.disposeGuiElements; end;
    for e in gates   do begin e^.uiAdapter:=nil; e^.disposeGuiElements; end;
    if ui.uiAdapter=nil then exit;
    ui.wireImage.OnMouseMove:=nil;
    ui.wireImage.OnMouseDown:=nil;
    ui.wireImage.OnMouseUp  :=nil;
    ui.uiAdapter^.activeBoard:=nil;
    ui.uiAdapter:=nil;
  end;

PROCEDURE T_visualBoard.attachUI(CONST uiAdapter: P_uiAdapter);
  VAR e:P_visualGate;
  begin
    ui.wireImage:=uiAdapter^.boardUiElements.wireImage;
    ui.wireImage.OnMouseMove:=@boardImageMouseMove;
    ui.wireImage.OnMouseDown:=@boardImageMouseDown;
    ui.wireImage.OnMouseUp  :=@boardImageMouseUp;
    ui.horizontalScrollBar:=uiAdapter^.boardUiElements.horizontalScrollBar;
    ui.verticalScrollBar :=uiAdapter^.boardUiElements.verticalScrollBar;
    ui.horizontalScrollBar.OnScroll:=@ScrollbarScroll;
    ui.verticalScrollBar.OnScroll:=@ScrollbarScroll;
    ui.uiAdapter:=uiAdapter;
    uiAdapter^.activeBoard:=@self;
    for e in inputs  do begin e^.uiAdapter:=uiAdapter; e^.ensureGuiElements(uiAdapter^.mainForm); e^.setBoardElementMouseActions; end;
    for e in outputs do begin e^.uiAdapter:=uiAdapter; e^.ensureGuiElements(uiAdapter^.mainForm); e^.setBoardElementMouseActions; end;
    for e in gates   do begin e^.uiAdapter:=uiAdapter; e^.ensureGuiElements(uiAdapter^.mainForm); e^.setBoardElementMouseActions; end;
    checkBoardExtend(false,false);
    rewire(true);
  end;

PROCEDURE T_visualBoard.elementAdded(CONST newElement: P_visualGate;
  CONST screenX, screenY: longint);
  VAR element:P_visualGate;
  begin
    element:=newElement;
    if (screenX<ui.wireImage.Left) or (screenY<ui.wireImage.top)
    then dispose(element,destroy)
    else begin
      ui.uiAdapter^.saveStateToUndoList;
      case element^.behavior^.gateType of
        gt_input:  begin setLength(inputs ,length(inputs )+1); inputs [length(inputs )-1]:=element; end;
        gt_output: begin setLength(outputs,length(outputs)+1); outputs[length(outputs)-1]:=element; end;
        else       begin setLength(gates  ,length(gates  )+1); gates  [length(gates  )-1]:=element; end;
      end;
      if element^.behavior^.gateType in [gt_input,gt_output] then begin
        enumerateIo;
        element^.updateCaption;
      end;
      repositionElement(element,true);
      element^.setBoardElementMouseActions;
    end;
  end;

PROCEDURE T_visualBoard.repositionElement(CONST element: P_visualGate;
  CONST skipSaveUndo: boolean);
  VAR wrap:T_visualGateArray;
  begin
    setLength(wrap,1);
    wrap[0]:=element;
    repositionElement(wrap,skipSaveUndo);
  end;

PROCEDURE T_visualBoard.repositionElement(CONST elements: T_visualGateArray;
  CONST skipSaveUndo: boolean);
  VAR gridX, gridY: longint;
      element:P_visualGate;
      elementIndex:longint=0;

  FUNCTION overlapsAnyOther:boolean;
    FUNCTION isInElementsStillInNeedOfPositioning(CONST g:P_visualGate):boolean;
      VAR i:longint;
      begin
        for i:=elementIndex to length(elements)-1 do if elements[i]=g then exit(true);
        result:=false;
      end;

    VAR other:P_visualGate;
    begin
      for other in inputs  do if not(isInElementsStillInNeedOfPositioning(other)) and element^.overlaps(other) then exit(true);
      for other in outputs do if not(isInElementsStillInNeedOfPositioning(other)) and element^.overlaps(other) then exit(true);
      for other in gates   do if not(isInElementsStillInNeedOfPositioning(other)) and element^.overlaps(other) then exit(true);
      result:=false;
    end;

  PROCEDURE repositionVertically;
    VAR stepY:longint=1;
    begin
      if element^.gridPos[1]<4 then element^.gridPos[1]:=4;
      if element^.gridPos[1]+4>gridOutputY0 then reshapeGrid(gridOutputX0,element^.gridPos[1]+4,false);
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
      if element^.gridPos[0]+4>gridOutputX0 then reshapeGrid(element^.gridPos[0]+4,gridOutputY0,false);
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
      if element^.gridPos[1]+4>gridOutputY0 then reshapeGrid(gridOutputX0,element^.gridPos[1]+4,false);
      if element^.gridPos[0]+4>gridOutputX0 then reshapeGrid(element^.gridPos[0]+4,gridOutputY0,false);
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
      end;
    end;

  VAR boardOriginX,
      boardOriginY:longint;
      needToRemove:boolean=false;
  begin
    if not skipSaveUndo then ui.uiAdapter^.saveStateToUndoList;
    boardOriginX:=ui.wireImage.Left-ui.horizontalScrollBar.position;
    boardOriginY:=ui.wireImage.top -ui.verticalScrollBar  .position;

    for element in elements do begin
      gridX:=round((element^.shapes[0].Left-boardOriginX)/ui.uiAdapter^.getZoom);
      gridY:=round((element^.shapes[0].top -boardOriginY)/ui.uiAdapter^.getZoom);
      needToRemove:=needToRemove or (gridX<0) or (gridY<0);
    end;

    if needToRemove then begin
      remove(elements,true);
      exit;
    end;

    for element in elements do begin
      gridX:=round((element^.shapes[0].Left-boardOriginX)/ui.uiAdapter^.getZoom);
      gridY:=round((element^.shapes[0].top -boardOriginY)/ui.uiAdapter^.getZoom);

      element^.gridPos[0]:=gridX;
      element^.gridPos[1]:=gridY;
      case element^.behavior^.gateType of
        gt_input: begin
          if element^.gridPos[0]<=element^.gridPos[1] then begin
            element^.gridPos[0]:=0;
            repositionVertically;
            if not(P_inputGate(element^.behavior)^.onLeftOrRightSide) then begin
              P_inputGate(element^.behavior)^.onLeftOrRightSide:=true;
              element^.propertyEditDone(false,boardOriginX,boardOriginY);
            end;
          end else begin
            element^.gridPos[1]:=0;
            repositionHorizontally;
            if P_inputGate(element^.behavior)^.onLeftOrRightSide then begin
              P_inputGate(element^.behavior)^.onLeftOrRightSide:=false;
              element^.propertyEditDone(false,boardOriginX,boardOriginY);
            end;
          end;
        end;
        gt_output: begin
          if gridOutputX0-element^.gridPos[0]<gridOutputY0-element^.gridPos[1] then begin
            element^.gridPos[0]:=gridOutputX0;
            repositionVertically;
            if not(P_outputGate(element^.behavior)^.onLeftOrRightSide) then begin
              P_outputGate(element^.behavior)^.onLeftOrRightSide:=true;
              element^.propertyEditDone(false,boardOriginX,boardOriginY);
            end;
          end else begin
            element^.gridPos[1]:=gridOutputY0;
            repositionHorizontally;
            if P_outputGate(element^.behavior)^.onLeftOrRightSide then begin
              P_outputGate(element^.behavior)^.onLeftOrRightSide:=false;
              element^.propertyEditDone(false,boardOriginX,boardOriginY);
            end;
          end;
        end;
        else reposition;
      end;
      inc(elementIndex);
      element^.paintAll(element^.gridPos[0]*ui.uiAdapter^.getZoom+boardOriginX,
                        element^.gridPos[1]*ui.uiAdapter^.getZoom+boardOriginY,
                                       ui.uiAdapter^.getZoom);
    end;
    checkBoardExtend(false,false);
    rewire(false);
    enumerateIo;
  end;

PROCEDURE T_visualBoard.remove(CONST element: P_visualGate;
  CONST skipSaveUndo: boolean);
  VAR wrap:T_visualGateArray;
  begin
    setLength(wrap,1);
    wrap[0]:=element;
    remove(wrap,skipSaveUndo);
  end;

PROCEDURE T_visualBoard.remove(CONST elements: T_visualGateArray;
  CONST skipSaveUndo: boolean);
  VAR
    element: P_visualGate;
    j, i: integer;
  begin
    if not(skipSaveUndo) then ui.uiAdapter^.saveStateToUndoList;
    for element in elements do begin
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
    end;
    enumerateIo;
    rewire(true);
  end;

FUNCTION T_visualBoard.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
  FUNCTION gateByIndex(index:longint):P_visualGate;
    begin
      if index<length(inputs) then exit(inputs[index]); dec(index,length(inputs));
      if index<length(gates)  then exit(gates [index]); dec(index,length(gates));
      result:=outputs[index];
    end;

  VAR i,j:longint;
      inputGate :P_inputGate;
      outputGate:P_outputGate;
      behavior  :P_abstractGate;
  begin
    result:=true;
    captionString:=stream.readAnsiString;
    descriptionString:=stream.readAnsiString;
    myIndex:=stream.readInteger;

    if not(stream.allOkay) then exit(false);
    gridOutputX0:=0;
    gridOutputY0:=0;
    setLength(inputs,stream.readNaturalNumber);
    for i:=0 to length(inputs)-1 do begin
      new(inputGate,create);
      inputGate^.readMetaDataFromStream(stream);
      new(inputs[i],create(inputGate));
      inputs[i]^.gridPos:=readPoint(stream);
    end;
    if not(stream.allOkay) then exit(false);

    setLength(gates,stream.readNaturalNumber);
    for i:=0 to length(gates)-1 do begin
      behavior:=associatedPalette^.readGate(stream);
      new(gates[i],create(behavior));
      gates[i]^.gridPos:=readPoint(stream);
    end;
    if not(stream.allOkay) then exit(false);

    setLength(outputs,stream.readNaturalNumber);
    for i:=0 to length(outputs)-1 do begin
      new(outputGate,create);
      outputGate^.readMetaDataFromStream(stream);
      new(outputs[i],create(outputGate));
      outputs[i]^.gridPos:=readPoint(stream);
    end;
    if not(stream.allOkay) then exit(false);

    setLength(wires,stream.readNaturalNumber);
    for i:=0 to length(wires)-1 do with wires[i] do begin
      source:=gateByIndex(stream.readNaturalNumber);
      sourceOutputIndex:= stream.readNaturalNumber;
      setLength(sink,stream.readNaturalNumber);
      for j:=0 to length(sink)-1 do with sink[j] do begin
        gate:=gateByIndex(stream.readNaturalNumber);
        gateInputIndex:=  stream.readNaturalNumber;
        Selected:=false;
      end;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_visualBoard.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  FUNCTION gateToIndex(CONST g:P_visualGate):longint;
    VAR i:longint;
        offset:longint=0;
    begin
      result:=0;
      for i:=0 to length(inputs )-1 do if inputs [i]=g then exit(offset+i); inc(offset,length(inputs));
      for i:=0 to length(gates  )-1 do if gates  [i]=g then exit(offset+i); inc(offset,length(gates));
      for i:=0 to length(outputs)-1 do if outputs[i]=g then exit(offset+i);
    end;

  VAR i,j:longint;
  begin
    stream.writeAnsiString(captionString);
    stream.writeAnsiString(descriptionString);
    stream.writeInteger(myIndex);

    stream.writeNaturalNumber(length(inputs));
    for i:=0 to length(inputs)-1 do begin
      inputs[i]^.behavior^.writeToStream(stream,true);
      writePointToStream(stream,inputs[i]^.gridPos);
    end;

    stream.writeNaturalNumber(length(gates));
    for i:=0 to length(gates)-1 do begin
      gates[i]^.behavior^.writeToStream(stream);
      writePointToStream(stream,gates[i]^.gridPos);
    end;

    stream.writeNaturalNumber(length(outputs));
    for i:=0 to length(outputs)-1 do begin
      outputs[i]^.behavior^.writeToStream(stream,true);
      writePointToStream(stream,outputs[i]^.gridPos);
    end;

    stream.writeNaturalNumber(length(wires));
    for i:=0 to length(wires)-1 do with wires[i] do begin
      stream.writeNaturalNumber(gateToIndex(source));
      stream.writeNaturalNumber(sourceOutputIndex);
      stream.writeNaturalNumber(length(sink));
      for j:=0 to length(sink)-1 do with sink[j] do begin
        stream.writeNaturalNumber(gateToIndex(gate));
        stream.writeNaturalNumber(gateInputIndex);
      end;
    end;
  end;

PROCEDURE T_visualBoard.savePaletteEntryToStream(
  VAR stream: T_bufferedOutputStreamWrapper; CONST paletteIndex: longint);
  begin
    myIndex:=paletteIndex;
    saveToStream(stream);
  end;

PROCEDURE T_visualBoard.loadPaletteEntryFromStream(
  VAR stream: T_bufferedInputStreamWrapper; CONST paletteIndex: longint);
  begin
    loadFromStream(stream);
    myIndex:=paletteIndex;
  end;

FUNCTION T_visualBoard.extractBehavior: P_compoundGate;
  VAR cloned:P_compoundGate;
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
    cloned^.captionString:='';
    cloned^.descriptionString:='';
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

FUNCTION T_visualBoard.clone: P_visualBoard;
  VAR cloned:P_visualBoard;
      i,j:longint;
  FUNCTION gateInClone(CONST gate:P_visualGate):P_visualGate;
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
    cloned^.captionString:=captionString;
    cloned^.descriptionString:=descriptionString;
    cloned^.myIndex:=myIndex;
    cloned^.gridOutputX0:=gridOutputX0;
    cloned^.gridOutputY0:=gridOutputY0;

    setLength(cloned^.inputs ,length(inputs )); for i:=0 to length(inputs )-1 do cloned^.inputs [i]:=inputs [i]^.clone;
    setLength(cloned^.outputs,length(outputs)); for i:=0 to length(outputs)-1 do cloned^.outputs[i]:=outputs[i]^.clone;
    setLength(cloned^.gates  ,length(gates));   for i:=0 to length(gates  )-1 do cloned^.gates  [i]:=gates  [i]^.clone;
    setLength(cloned^.wires  ,length(wires));
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

PROCEDURE T_visualBoard.enumerateIo;
  VAR i:longint;
      axis:longint;
      gateSet:T_visualGateArray;
      startTicks: qword;

  PROCEDURE initGateSet(CONST axis:longint; CONST baseSet:T_visualGateArray);
    VAR k:longint;
        ko:longint=0;
    begin
      setLength(gateSet,length(baseSet));
      for k:=0 to length(baseSet)-1 do if P_inputGate(baseSet[k]^.behavior)^.onLeftOrRightSide=(axis=1)
      then begin
        gateSet[ko]:=baseSet[k];
        inc(ko);
      end;
      setLength(gateSet,ko);
    end;

  FUNCTION assignMinInGateSetAndRemove(CONST axis,ioIndexToAssign:longint):boolean;
    VAR i:longint;
        iMin:longint=0;
        minCoord:longint=maxLongint;
    begin
      if length(gateSet)=0 then exit(false);
      for i:=0 to length(gateSet)-1 do if gateSet[i]^.gridPos[axis]<minCoord then begin
        iMin:=i;
        minCoord:=gateSet[i]^.gridPos[axis];
      end;
      P_inputGate(gateSet[iMin]^.behavior)^.positionIndex:=ioIndexToAssign;
      for i:=iMin to length(gateSet)-2 do gateSet[i]:=gateSet[i+1];
      setLength(gateSet,length(gateSet)-1);
      result:=true;
    end;

  VAR changed:boolean;
  begin
    startTicks:=GetTickCount64;
    for i:=0 to length(inputs )-1 do begin
      changed:=P_inputGate (inputs [i]^.behavior)^.ioIndex<>i;
      P_inputGate (inputs[i]^.behavior)^.ioIndex:=i;
      if changed then inputs[i]^.labels[0].caption:=inputs[i]^.behavior^.getCaption;
    end;
    for i:=0 to length(outputs)-1 do begin
      changed:=P_outputGate(outputs[i]^.behavior)^.ioIndex<>i;
      P_outputGate(outputs[i]^.behavior)^.ioIndex:=i;
      if changed then outputs[i]^.labels[0].caption:=outputs[i]^.behavior^.getCaption;
    end;

    for axis:=0 to 1 do begin
      initGateSet(axis,inputs);
      i:=0;
      while assignMinInGateSetAndRemove(axis,i) do inc(i);
    end;
    for axis:=0 to 1 do begin
      initGateSet(axis,outputs);
      i:=0;
      while assignMinInGateSetAndRemove(axis,i) do inc(i);
    end;
    writeln('Enumerate I/O took ',GetTickCount64-startTicks,' ticks');
  end;

FUNCTION T_visualBoard.getWireGraph(CONST dropExistingWires: boolean
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

PROCEDURE T_visualBoard.rewire(CONST forced: boolean);
  VAR graph:P_wireGraph;
  VAR i,j,io,jo:longint;
      startPoint:T_point;
      endPoints: T_wirePath;
      paths: T_wirePathArray;
      startTicks: qword;
      wireNeedsUpdate:boolean;
  begin
    startTicks:=GetTickCount64;

    //Drop wires referrring to nonexistent ports
    io:=0;
    for i:=0 to length(wires)-1 do begin
      with wires[i] do begin
        if sourceOutputIndex>=source^.behavior^.numberOfOutputs then setLength(sink,0);
        jo:=0;
        for j:=0 to length(sink)-1 do
        if (sink[j].gateInputIndex<sink[j].gate^.behavior^.numberOfInputs) and
           (sink[j].gate^.behavior^.inputWidth(sink[j].gateInputIndex)=source^.behavior^.outputWidth(sourceOutputIndex))
        then begin
          if j<>jo then sink[jo]:=sink[j];
          inc(jo);
        end;
        setLength(sink,jo);
      end;
      if jo>0 then begin
        if i<>io then wires[io]:=wires[i];
        inc(io);
      end;
    end;
    setLength(wires,io);

    graph:=getWireGraph(false);
    for i:=0 to length(wires)-1 do begin
      startPoint:=wires[i].source^.getOutputPositionInGridSize(wires[i].sourceOutputIndex)+wires[i].source^.gridPos;
      setLength(endPoints,length(wires[i].sink));

      for j:=0 to length(endPoints)-1 do endPoints[j]:=wires[i].sink[j].gate^.getInputPositionInGridSize(wires[i].sink[j].gateInputIndex)+wires[i].sink[j].gate^.gridPos;
      wireNeedsUpdate:=forced;
      if not(forced) then for j:=0 to length(wires[i].sink)-1 do
        wireNeedsUpdate:=wireNeedsUpdate
          or (length(wires[i].sink[j].path)<1)
          or (wires[i].sink[j].path[0]<>startPoint)
          or (wires[i].sink[j].path[length(wires[i].sink[j].path)-1]<>endPoints[j]);

      if wireNeedsUpdate then begin
        paths:=graph^.findPaths(startPoint,endPoints,true);
        for j:=0 to length(paths)-1 do begin
          wires[i].sink[j].path:=paths[j];
          graph^.dropWire(paths[j]);
        end;
      end else for j:=0 to length(wires[i].sink)-1 do graph^.dropWire(wires[i].sink[j].path);
    end;

    writeln(forced,' rewiring took ',GetTickCount64-startTicks,' ticks');
    startTicks:=GetTickCount64;
    paintWires;
    dispose(graph,destroy);
  end;

PROCEDURE T_visualBoard.paintWires;
  VAR i:longint;
    startTicks: qword;
  begin
    ui.uiAdapter^.BeginFormUpdate();
    ui.wireImage.visible:=true;
    startTicks:=GetTickCount64;

    if (ui.wireImage.picture.Bitmap.width<>ui.wireImage.width)
    or (ui.wireImage.picture.Bitmap.height<>ui.wireImage.height)
    then ui.wireImage.picture.Bitmap.setSize(ui.wireImage.width,ui.wireImage.height);

    ui.wireImage.Canvas.lock;
    ui.wireImage.Canvas.Brush.color:=$00804040;
    ui.wireImage.Canvas.FillRect(0,0,ui.wireImage.width,ui.wireImage.height);

    ui.wireImage.Canvas.Pen.color:=$00603030;
    ui.wireImage.Canvas.Pen.width:=1;

    i:=-ui.horizontalScrollBar.position+ui.uiAdapter^.zoom*gridOutputX0;
    ui.wireImage.Canvas.line(i,0,i,ui.wireImage.height);

    i:=-ui.horizontalScrollBar.position+ui.uiAdapter^.zoom*4;
    ui.wireImage.Canvas.line(i,0,i,ui.wireImage.height);

    i:=-ui.verticalScrollBar.position+ui.uiAdapter^.zoom*gridOutputY0;
    ui.wireImage.Canvas.line(0,i,ui.wireImage.width,i);

    i:=-ui.verticalScrollBar.position+ui.uiAdapter^.zoom*4;
    ui.wireImage.Canvas.line(0,i,ui.wireImage.width,i);

    for i:=0 to length(wires)-1 do
      wires[i].paint(-ui.horizontalScrollBar.position,
                     -ui.verticalScrollBar  .position,
                      ui.uiAdapter^.zoom,
                      ui.wireImage);
    ui.wireImage.Canvas.unlock;

    ui.uiAdapter^.EndFormUpdate();
    writeln('Painting wires @',ui.wireImage.width,'x',ui.wireImage.height,' took ',GetTickCount64-startTicks,' ticks');

  end;

PROCEDURE T_visualBoard.boardImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
  VAR gridX,gridY:longint;
      i:longint;
      horizontal: boolean;
  begin
    gridX:=round((x+ui.horizontalScrollBar.position)/ui.uiAdapter^.zoom);
    gridY:=round((y+ui.verticalScrollBar  .position)/ui.uiAdapter^.zoom);

    if ui.uiAdapter^.state=uas_draggingGridOutputX0 then begin
      ui.wireImage.Cursor:=crSizeWE;
      if gridX<>gridOutputX0 then reshapeGrid(gridX,gridOutputY0,true);
      exit;
    end;
    if ui.uiAdapter^.state=uas_draggingGridOutputY0 then begin
      ui.wireImage.Cursor:=crSizeNS;
      if gridY<>gridOutputY0 then reshapeGrid(gridOutputX0,gridY,true);
      exit;
    end;

    if ui.uiAdapter^.state=uas_draggingSelectionFrame then begin
      ui.uiAdapter^.selectionShape.Left  :=min(x+ui.wireImage.Left,ui.uiAdapter^.dragData.startX);
      ui.uiAdapter^.selectionShape.width :=abs(x+ui.wireImage.Left-ui.uiAdapter^.dragData.startX);
      ui.uiAdapter^.selectionShape.top   :=min(y+ui.wireImage.top ,ui.uiAdapter^.dragData.startY);
      ui.uiAdapter^.selectionShape.height:=abs(y+ui.wireImage.top -ui.uiAdapter^.dragData.startY);
    end;

    if gridX=gridOutputX0 then ui.wireImage.Cursor:=crSizeWE else
    if gridY=gridOutputY0 then ui.wireImage.Cursor:=crSizeNS else begin
      for i:=0 to length(wires)-1 do if wires[i].isWirePosition(gridX,gridY,horizontal) then begin
        if horizontal then ui.wireImage.Cursor:=crHSplit
                      else ui.wireImage.Cursor:=crVSplit;
        exit;
      end;
      ui.wireImage.Cursor:=crDefault;
    end;

  end;

PROCEDURE T_visualBoard.boardImageMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR gridX,gridY:longint;
      i:longint=0;
      j:longint=0;
      horizontal: boolean;
      needToDropWire: boolean=false;
      g:P_visualGate;
  begin
    gridX:=round((x+ui.horizontalScrollBar.position)/ui.uiAdapter^.zoom);
    gridY:=round((y+ui.verticalScrollBar  .position)/ui.uiAdapter^.zoom);
    if gridX=gridOutputX0 then begin
      ui.wireImage.Cursor:=crSizeWE;
      ui.uiAdapter^.state:=uas_draggingGridOutputX0;
      exit;
    end;

    if gridY=gridOutputY0 then begin
      ui.wireImage.Cursor:=crSizeNS;
      ui.uiAdapter^.state:=uas_draggingGridOutputY0;
      exit;
    end;

    for i:=0 to length(wires)-1 do needToDropWire:=needToDropWire or wires[i].isWirePosition(gridX,gridY,horizontal);
    if needToDropWire then begin
      ui.uiAdapter^.saveStateToUndoList;
      for i:=0 to length(wires)-1 do begin
        wires[i].dropWiresTouchingPosition(gridX,gridY,horizontal);
        if length(wires[i].sink)>0 then begin
          if i<>j then wires[j]:=wires[i];
          inc(j);
        end;
      end;
      setLength(wires,j);
      rewire(false);
      exit;
    end;

    if not(ssShift in Shift) then begin
      for g in inputs  do begin g^.marked:=false; g^.updateVisuals; end;
      for g in outputs do begin g^.marked:=false; g^.updateVisuals; end;
      for g in gates   do begin g^.marked:=false; g^.updateVisuals; end;
    end;
    ui.uiAdapter^.startDragSelectionFrame(X+ui.wireImage.Left,Y+ui.wireImage.top);
  end;

PROCEDURE T_visualBoard.boardImageMouseUp(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR x0,x1,y0,y1,i:longint;
  PROCEDURE mark(CONST gate:P_visualGate);
    begin
      if (gate^.shapes[0].Left>=x0) and (gate^.shapes[0].Left+gate^.shapes[0].width<=x1) and
         (gate^.shapes[0].top >=y0) and (gate^.shapes[0].top+gate^.shapes[0].height<=y1) then begin
        gate^.marked:=true;
        gate^.updateVisuals;
      end;
    end;

  VAR g:P_visualGate;
  begin
    case ui.uiAdapter^.state of
      uas_draggingGridOutputY0,uas_draggingGridOutputX0: ui.uiAdapter^.state:=uas_initial;
      uas_draggingSelectionFrame: begin
        x0:=ui.uiAdapter^.dragData.startX;
        x1:=x+ui.wireImage.Left;
        y0:=ui.uiAdapter^.dragData.startY;
        y1:=y+ui.wireImage.top;
        if x1<x0 then begin i:=x0; x0:=x1; x1:=i; end;
        if y1<y0 then begin i:=y0; y0:=y1; y1:=i; end;

        for g in inputs  do mark(g);
        for g in outputs do mark(g);
        for g in gates   do mark(g);

        ui.uiAdapter^.endSelectionDrag;
      end
    end;
  end;

PROCEDURE T_visualBoard.ScrollbarScroll(Sender: TObject;
  ScrollCode: TScrollCode; VAR ScrollPos: integer);
  VAR x0, y0: integer;
    e: P_visualGate;
    zoom: longint;
  begin
    ui.uiAdapter^.BeginFormUpdate();
    zoom:=ui.uiAdapter^.zoom;
    x0:=ui.wireImage.Left-ui.horizontalScrollBar.position;
    y0:=ui.wireImage.top -ui.verticalScrollBar.position;

    for e in inputs  do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in outputs do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);
    for e in gates   do e^.paintAll(x0+e^.gridPos[0]*zoom,y0+e^.gridPos[1]*zoom,zoom);

    paintWires;
    ui.uiAdapter^.EndFormUpdate();
    if ui.horizontalScrollBar.position>0 then associatedPalette^.BringToFront;
  end;

PROCEDURE T_visualBoard.reshapeGrid(CONST newGridOutputX0,
  newGridOutputY0: longint; CONST byMouseEvent: boolean);
  VAR gate:P_visualGate;
      movingBottom,
      movingLeft: boolean;

  begin
    movingLeft  :=(newGridOutputX0<>gridOutputX0) and byMouseEvent;
    movingBottom:=(newGridOutputY0<>gridOutputY0) and byMouseEvent;
    gridOutputX0:=newGridOutputX0;
    gridOutputY0:=newGridOutputY0;
    checkBoardExtend(movingLeft,movingBottom);
    for gate in outputs do begin
      if P_outputGate(gate^.behavior)^.onLeftOrRightSide
      then gate^.gridPos[0]:=gridOutputX0
      else gate^.gridPos[1]:=gridOutputY0;
    end;
    rewire(false);
    checkSizes;
  end;

PROCEDURE T_visualBoard.paintWirePreview(CONST wireStart: T_point;
  CONST screenX, screenY: longint);
  CONST MAX_INT64= 9223372036854775807;
  VAR tgt ,closestTarget:T_point;
      dist,distToClosest:int64;

      g:P_visualGate;
      i:longint;
      graph: P_wireGraph;
      wirePath, newWirePath: T_wirePath;
      wireWidth:byte;

      connectible:T_visualGateArray;

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
    wireWidth:=ui.uiAdapter^.draggedGate^.behavior^.outputWidth(ui.uiAdapter^.dragData.outputIndex);
    graph:=getWireGraph(true);

    distToClosest:=MAX_INT64;
    setLength(connectible,length(gates)+length(outputs));
    for i:=0 to length(gates)-1 do connectible[i]:=gates[i];
    for i:=0 to length(outputs)-1 do connectible[length(gates)+i]:=outputs[i];

    for g in connectible do for i:=0 to g^.behavior^.numberOfInputs-1 do
    if (g^.behavior^.inputWidth(i)=wireWidth) and not(isInputConnected(g,i)) then begin
      tgt:=g^.getInputPositionInGridSize(i)+g^.gridPos;
      dist:=sqr(tgt[0]*ui.uiAdapter^.getZoom+ui.wireImage.Left-ui.horizontalScrollBar.position-screenX)
           +sqr(tgt[1]*ui.uiAdapter^.getZoom+ui.wireImage.top -ui.verticalScrollBar  .position-screenY);
      if dist<distToClosest then begin
        newWirePath:=graph^.findPath(wireStart,tgt,wiresStartingAt(ui.uiAdapter^.draggedGate,ui.uiAdapter^.dragData.outputIndex));
        if length(newWirePath)>0 then begin
          wirePath:=newWirePath;
          ui.uiAdapter^.dragData.dragTarget:=g;
          ui.uiAdapter^.dragData.inputIndex:=i;
          distToClosest:=dist;
          closestTarget:=tgt;
        end;
      end;
    end;
    dispose(graph,destroy);

    if distToClosest=MAX_INT64 then exit;

    paintWires;
    paintWire(ui.wireImage,
              ui.uiAdapter^.zoom,
              wireWidth,
              wireToScreen(wirePath,-ui.horizontalScrollBar.position,
                                    -ui.verticalScrollBar  .position,
                                     ui.uiAdapter^.zoom),true);

  end;

FUNCTION T_visualBoard.getCaption: string;
begin
  result:=captionString;
end;

PROCEDURE T_visualBoard.setCaption(CONST s: string);
begin
  captionString:=s;
end;

FUNCTION T_visualBoard.getDescription: string;
begin
  result:=descriptionString;
end;

PROCEDURE T_visualBoard.setDescription(CONST s: string);
begin
  descriptionString:=s;
end;

FUNCTION T_visualBoard.getIndexInPalette: longint;
  begin
    result:=myIndex;
  end;

PROCEDURE T_visualBoard.afterGatePropertiesEdited(CONST editedGate: P_visualGate
  );
  VAR g:P_visualGate;
  begin
    for g in gates do if g^.behavior^.equals(editedGate^.behavior) then g^.propertyEditDone(false,
      ui.wireImage.Left-ui.horizontalScrollBar.position,
      ui.wireImage.top -ui.verticalScrollBar.position);
    rewire(false);
  end;

FUNCTION T_visualBoard.isInputConnected(CONST gate: P_visualGate;
  CONST gateInputIndex: longint): boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=0 to length(wires)-1 do for j:=0 to length(wires[i].sink)-1 do
      if (wires[i].sink[j].gate=gate) and
         (wires[i].sink[j].gateInputIndex=gateInputIndex) then exit(true);
  end;

FUNCTION T_visualBoard.isOutputputConnected(CONST gate: P_visualGate;
  CONST gateOutputIndex: longint): boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(wires)-1 do with wires[i] do if (source=gate) and (sourceOutputIndex=gateOutputIndex) then exit(true);
  end;

PROCEDURE T_visualBoard.addWire(CONST sourceGate: P_visualGate;
  CONST sourceOutputIndex: longint; CONST sinkGate: P_visualGate;
  CONST sinkInputIndex: longint);
  VAR i:longint=0;
      j:longint;
  begin
    ui.uiAdapter^.saveStateToUndoList;
    while (i<length(wires)) and ((wires[i].source<>sourceGate) or (wires[i].sourceOutputIndex<>sourceOutputIndex)) do inc(i);
    if i>=length(wires) then setLength(wires,i+1);
    wires[i].source:=sourceGate;
    wires[i].sourceOutputIndex:=sourceOutputIndex;
    j:=length(wires[i].sink);
    setLength(wires[i].sink,j+1);
    wires[i].sink[j].gate:=sinkGate;
    wires[i].sink[j].gateInputIndex:=sinkInputIndex;
    rewire(false);
    if wires[i].simulateStep then sinkGate^.updateVisuals;
  end;

PROCEDURE T_visualBoard.reset;
  VAR g:P_visualGate;
  begin
    for g in outputs do begin
      g^.getBehavior^.reset;
      g^.updateVisuals;
    end;
    for g in gates do begin
      g^.getBehavior^.reset;
      g^.updateVisuals;
    end;
  end;

FUNCTION T_visualBoard.simulateSteps(CONST count: longint): longint;
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

FUNCTION T_visualBoard.startMultiDrag(CONST primaryGate: P_visualGate): boolean;
  VAR marked:T_visualGateArray;
      g:P_visualGate;
      i:longint=1;
  begin
    if not(primaryGate^.marked) then exit(false);
    setLength(marked,length(inputs)+length(outputs)+length(gates));
    marked[0]:=primaryGate;
    for g in inputs  do if (g^.marked) and (g<>primaryGate) then begin marked[i]:=g; inc(i); end;
    for g in outputs do if (g^.marked) and (g<>primaryGate) then begin marked[i]:=g; inc(i); end;
    for g in gates   do if (g^.marked) and (g<>primaryGate) then begin marked[i]:=g; inc(i); end;
    if i>1 then begin
      setLength(marked,i);
      ui.uiAdapter^.dragData.draggedGates:=marked;
      result:=true;
    end else begin
      setLength(marked,0);
      result:=false;
    end;
  end;

FUNCTION T_visualBoard.isVisualBoard: boolean;
  begin result:=true; end;

FUNCTION T_visualBoard.usesPrototype(CONST p: P_captionedAndIndexed): boolean;
  VAR g:P_visualGate;
  begin
    result:=false;
    for g in gates do if (g^.behavior^.gateType=gt_compound) and P_compoundGate(g^.behavior)^.usesPrototype(p) then result:=true;
  end;

PROCEDURE addGate(VAR arr:T_visualGateArray; CONST toAdd:P_visualGate);
  begin
    setLength(arr,length(arr)+1);
    arr[length(arr)-1]:=toAdd;
  end;

PROCEDURE T_visualBoard.copySelectionToClipboard;
  VAR Clipboard:P_visualBoard;
  FUNCTION gateInClone(CONST gate:P_visualGate):P_visualGate;
    VAR i,ic:longint;
    begin
      if not(gate^.marked) then exit(nil);
      result:=nil;
      ic:=0;
      for i:=0 to length(inputs)-1 do if inputs[i]=gate then exit(Clipboard^.inputs[ic]) else if inputs[i]^.marked then inc(ic);
      ic:=0;
      for i:=0 to length(outputs)-1 do if outputs[i]=gate then exit(Clipboard^.outputs[ic]) else if outputs[i]^.marked then inc(ic);
      ic:=0;
      for i:=0 to length(gates)-1 do if gates[i]=gate then exit(Clipboard^.gates[ic]) else if gates[i]^.marked then inc(ic);
    end;

  VAR g:P_visualGate;
      anyMarked:boolean=false;
      i,ic,j,jc:longint;
  begin
    for g in inputs  do anyMarked:=anyMarked or g^.marked;
    for g in outputs do anyMarked:=anyMarked or g^.marked;
    for g in gates   do anyMarked:=anyMarked or g^.marked;
    if not(anyMarked) then exit;

    Clipboard:=ui.uiAdapter^.helperState.Clipboard;
    if Clipboard=nil
    then new(Clipboard,create(associatedPalette))
    else Clipboard^.clear;
    ui.uiAdapter^.helperState.Clipboard:=Clipboard;

    for g in inputs  do if g^.marked then addGate(Clipboard^.inputs ,g^.clone);
    for g in outputs do if g^.marked then addGate(Clipboard^.outputs,g^.clone);
    for g in gates   do if g^.marked then addGate(Clipboard^.gates ,g^.clone);

    ic:=0;
    setLength(Clipboard^.wires,length(wires));
    for i:=0 to length(wires)-1 do begin
      g:=gateInClone(wires[i].source);
      if g<>nil then begin
        Clipboard^.wires[ic].source:=g;
        Clipboard^.wires[ic].sourceOutputIndex:=
                   wires[i ].sourceOutputIndex;
        jc:=0;
        setLength(Clipboard^.wires[ic].sink,length(wires[i].sink));
        for j:=0 to length(wires[i].sink)-1 do begin
          g:=gateInClone(wires[i].sink[j].gate);
          if g<>nil then begin
            Clipboard^.wires[ic].sink[jc].gate:=g;
            Clipboard^.wires[ic].sink[jc].gateInputIndex:=
                       wires[i ].sink[j ].gateInputIndex;
            inc(jc);
          end;
        end;
        setLength(Clipboard^.wires[ic].sink,jc);
        if jc>0 then inc(ic);
      end;
    end;
    setLength(Clipboard^.wires,ic);
  end;

PROCEDURE T_visualBoard.pasteFromClipboard(CONST tgtX, tgtY: longint);
  VAR Clipboard: P_visualBoard;
      oldInputsCount :longint;
      oldOutputsCount:longint;
      oldGatesCount  :longint;
      newElements:T_visualGateArray;

  FUNCTION gateInSelf(CONST gate:P_visualGate):P_visualGate;
    VAR i:longint;
    begin
      result:=nil;
      for i:=0 to length(Clipboard^.inputs )-1 do if Clipboard^.inputs [i]=gate then exit(inputs [oldInputsCount +i]);
      for i:=0 to length(Clipboard^.outputs)-1 do if Clipboard^.outputs[i]=gate then exit(outputs[oldOutputsCount+i]);
      for i:=0 to length(Clipboard^.gates  )-1 do if Clipboard^.gates  [i]=gate then exit(gates  [oldGatesCount  +i]);
    end;

  VAR g,gCopy:P_visualGate;
      i0,i,j:longint;
      posDelta:T_point;
  begin
    ui.uiAdapter^.saveStateToUndoList;
    Clipboard:=ui.uiAdapter^.helperState.Clipboard;
    if Clipboard=nil then exit;
    setLength(newElements,0);
    oldInputsCount :=length(inputs );
    oldOutputsCount:=length(outputs);
    oldGatesCount  :=length(gates  );
    for g in Clipboard^.inputs  do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(inputs ,gCopy); end;
    for g in Clipboard^.outputs do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(outputs,gCopy); end;
    for g in Clipboard^.gates   do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(gates  ,gCopy); end;
    i0:=length(wires);
    setLength(wires,i0+length(Clipboard^.wires));
    for i:=0 to length(Clipboard^.wires)-1 do begin
      wires[i+i0].source           :=gateInSelf(Clipboard^.wires[i].source);
      wires[i+i0].sourceOutputIndex:=           Clipboard^.wires[i].sourceOutputIndex;
      setLength(wires[i+i0].sink,length(Clipboard^.wires[i].sink));
      for j:=0 to length(Clipboard^.wires[i].sink)-1 do begin
        wires[i+i0].sink[j].gate:=gateInSelf(Clipboard^.wires[i].sink[j].gate);
        wires[i+i0].sink[j].gateInputIndex:= Clipboard^.wires[i].sink[j].gateInputIndex;
      end;
    end;

    posDelta[0]:=maxLongint;
    posDelta[1]:=maxLongint;
    for g in newElements do begin
      posDelta[0]:=min(posDelta[0],g^.gridPos[0]);
      posDelta[1]:=min(posDelta[1],g^.gridPos[1]);
    end;
    posDelta[0]:=tgtX-posDelta[0];
    posDelta[1]:=tgtY-posDelta[1];

    for g in newElements do begin
      g^.ensureGuiElements(ui.wireImage.parent);
      g^.gridPos+=posDelta;
      g^.paintAll(g^.gridPos[0]*ui.uiAdapter^.getZoom+ui.wireImage.Left-ui.horizontalScrollBar.position,
                  g^.gridPos[1]*ui.uiAdapter^.getZoom+ui.wireImage.top -ui.verticalScrollBar  .position,
                                ui.uiAdapter^.getZoom);
      g^.setBoardElementMouseActions;
      g^.marked:=true;
      g^.updateVisuals;
    end;

    repositionElement(newElements,true);
  end;

{$endif}

