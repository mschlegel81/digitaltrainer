{$ifdef includeInterface}

{ T_visualBoard }
P_watcher=^T_watcher;

{ T_watcher }

T_watcher=object
  gate:P_visualGate;
  input:boolean;
  ioIndex:longint;

  CONSTRUCTOR create(CONST gate_:P_visualGate; CONST input_:boolean; CONST ioIndex_:longint);
  DESTRUCTOR destroy;
  FUNCTION getCanvasPos(CONST uiAdapter:P_uiAdapter):T_point;
  PROCEDURE paint(CONST uiAdapter:P_uiAdapter; CONST Canvas:TCanvas);
  FUNCTION isAtCanvasPos(CONST uiAdapter:P_uiAdapter; CONST x,y:longint):boolean;
end;

T_visualBoard=object(T_captionedAndIndexed)
  private
    wiresCs:TRTLCriticalSection;
    associatedPalette:P_abstractPrototypeSource;
    myIndex:longint;

    captionString:shortstring;
    descriptionString:string;

    simStepCounter:longint;
    wiresUpToDate:boolean;
    ui:P_uiAdapter;

    wiringTask:P_wiringTask;

    gridX0,gridX1,gridY0,gridY1:longint;

    PROCEDURE ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);

    PROCEDURE boardMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE boardMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE boardMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE reshapeGrid(CONST newGridX1,newGridY1:longint; CONST byMouseEvent:boolean);
    PROCEDURE dropWatchersAssociatedWith(CONST gate:P_visualGate);
    PROCEDURE deleteMarkedGates;
    PROCEDURE unhoverAllWires;
  public
    underlyingPrototype:P_visualBoard;
    modified:boolean;

    inputs : T_visualGateArray;
    outputs: T_visualGateArray;
    gates  : T_visualGateArray;
    wires  : array of T_visualWire;
    watchers:array of P_watcher;

    wiresHaveChangedInBackground:boolean;
    CONSTRUCTOR create(CONST palette:P_abstractPrototypeSource);
    PROCEDURE clear;
    DESTRUCTOR destroy;

    PROPERTY isModified:boolean read modified;

    PROCEDURE handleInputKey(CONST key:word; CONST shiftPressed:boolean);
    PROCEDURE setIndexInPalette(CONST index:longint; CONST dropBackReference:boolean);
    PROCEDURE checkBoardExtend(CONST movingLeft,movingBottom:boolean; CONST autoshrink:boolean=false);

    PROCEDURE detachUI;
    PROCEDURE attachUI(CONST uiAdapter:P_uiAdapter);
    PROCEDURE elementAdded(CONST newElement:P_visualGate; CONST mouseIsInBoard:boolean);
    PROCEDURE repositionElement(CONST element:P_visualGate       ; CONST skipSaveUndo,mouseIsInBoard:boolean);
    PROCEDURE repositionElement(CONST elements: T_visualGateArray; CONST skipSaveUndo,mouseIsInBoard:boolean);
    PROCEDURE remove(CONST element:P_visualGate       ; CONST skipSaveUndo:boolean);
    PROCEDURE remove(CONST elements: T_visualGateArray; CONST skipSaveUndo:boolean);

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST includeFixing:boolean):boolean;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST includeFixing:boolean);
    PROCEDURE savePaletteEntryToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST paletteIndex:longint);
    PROCEDURE loadPaletteEntryFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST paletteIndex:longint);
    FUNCTION extractBehavior:P_compoundGate;
    PROCEDURE extractChallenge(CONST challengePalette: P_abstractPrototypeSource; OUT expectedBehavior: P_visualBoard);
    FUNCTION getInterfaces:T_gateInterfaces;

    FUNCTION cloneAsTrueCopy(CONST fixProperties:boolean=false):P_visualBoard;
    FUNCTION cloneWithBackReference:P_visualBoard;
    PROCEDURE enumerateIo;

    FUNCTION getWireGraph(CONST dropExistingWires:boolean):P_wireGraph;
    PROCEDURE rewire(CONST forced:boolean);
    PROCEDURE updateWiresFromTask;
    PROCEDURE paintWires(CONST init:boolean=true; CONST draw:boolean=true);
    PROCEDURE forceWireRepaint;
    PROCEDURE paintWirePreview(CONST wireStart:T_point; CONST screenX,screenY:longint);

    FUNCTION getCaption: shortstring; virtual;
    PROCEDURE setCaption(CONST s: string); virtual;
    FUNCTION getDescription: string; virtual;
    PROCEDURE setDescription(CONST s: string); virtual;
    FUNCTION getIndexInPalette: longint; virtual;

    PROCEDURE afterGatePropertiesEdited(CONST editedGate:P_visualGate; CONST editedIsBoardElement:boolean);

    FUNCTION isInputConnected(CONST gate:P_visualGate; CONST gateInputIndex:longint):boolean;
    FUNCTION isOutputputConnected(CONST gate:P_visualGate; CONST gateOutputIndex:longint):boolean;
    PROCEDURE addWire(CONST sourceGate:P_visualGate; CONST sourceOutputIndex:longint;
                      CONST sinkGate:P_visualGate; CONST sinkInputIndex:longint);

    PROCEDURE reset(CONST includeInputs:boolean=false);
    FUNCTION coSimulateSteps(CONST count,timeOutInTicks: longint; CONST expectedBehavior:P_abstractGate):longint;
    FUNCTION simulateSteps(CONST count,timeOutInTicks: longint):longint;
    FUNCTION outputsMatch(CONST expected:P_compoundGate):boolean;
    FUNCTION interfacesMatch(CONST expected:P_compoundGate):boolean;
    PROCEDURE setInputs(CONST inputWires:T_wireValueArray);

    FUNCTION startMultiDrag(CONST primaryGate:P_visualGate):boolean;
    FUNCTION isVisualBoard: boolean; virtual;
    FUNCTION usesPrototype(CONST p:P_captionedAndIndexed; CONST transitiveScan:boolean):boolean;
    PROCEDURE prototypeUpdated(CONST oldPrototype,newPrototype:P_captionedAndIndexed);

    PROCEDURE copySelectionToClipboard;
    PROCEDURE pasteFromClipboard(CONST tgtX,tgtY:longint);
    PROCEDURE paintBoard(CONST exludeDragged:boolean=false);

    FUNCTION getGateCount:T_gateCount;
    FUNCTION getInfoLabelText:string;

    PROCEDURE moveToPalette(CONST newPalette:P_abstractPrototypeSource);
    FUNCTION equals(CONST other:P_visualBoard):boolean;
    FUNCTION hash:word;
    FUNCTION interfaceHash:word;

    FUNCTION newGateFromPrototype: P_abstractGate; virtual;
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_visualBoard.create(CONST palette: P_abstractPrototypeSource);
  begin
    initCriticalSection(wiresCs);
    associatedPalette:=palette;
    clear;
    wiringTask:=nil;
  end;

PROCEDURE T_visualBoard.clear;
  VAR i:longint;
  begin
    myIndex:=-1;
    underlyingPrototype:=nil;
    gridX0:=0;
    gridX1:=32;
    gridY0:=0;
    gridY1:=32;
    captionString:='';
    descriptionString:='';
    for i:=0 to length(inputs)-1 do dispose(inputs[i],destroy);
    setLength(inputs,0);
    for i:=0 to length(outputs)-1 do dispose(outputs[i],destroy);
    setLength(outputs,0);
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    setLength(wires,0);
    wiresUpToDate:=false;
    if ui<>nil then begin
      ui^.hideIoEdit;
      ui^.updateTitleLayer;
    end;
    setLength(watchers,0);
    simStepCounter:=0;
    modified:=false;
    disposeTask(wiringTask);
  end;

DESTRUCTOR T_visualBoard.destroy;
  begin
    detachUI;
    clear;
    enterCriticalSection(wiresCs);
    leaveCriticalSection(wiresCs);
    doneCriticalSection(wiresCs);
  end;

PROCEDURE T_visualBoard.handleInputKey(CONST key: word;
  CONST shiftPressed: boolean);
  CONST DELETE_KEY=46;
  VAR i  :longint;
      idx:longint=-1;
      inputGate:P_visualGate;
  begin
    if key=DELETE_KEY then begin
      deleteMarkedGates;
      exit;
    end;

    if key>ord('Z') then exit;

    for i:=0 to min(length(C_inputKey),length(inputs))-1 do if ord(C_inputKey[i])=key then idx:=i;
    if (idx<0) then exit;
    inputGate:=inputs[idx];

    if shiftPressed
    then ui^.showIoEdit(inputGate)
    else inputGate^.flipInputBits;
  end;

PROCEDURE T_visualBoard.setIndexInPalette(CONST index: longint; CONST dropBackReference:boolean);
  begin
    if dropBackReference then underlyingPrototype:=nil;
    myIndex:=index;
  end;

PROCEDURE T_visualBoard.checkBoardExtend(CONST movingLeft,
  movingBottom: boolean; CONST autoshrink: boolean);
  VAR nonInX0,nonInY0:longint;
  PROCEDURE growIfNecessary(CONST gate:P_visualGate);
    VAR corner:T_point;
    begin
      if gate^.gridPos[0]<0 then gate^.gridPos[0]:=0;
      if gate^.gridPos[1]<0 then gate^.gridPos[1]:=0;

      corner:=gate^.gridPos;
      if gate^.behavior^.gateType<>gt_input then begin
        if corner[0]<nonInX0 then nonInX0:=corner[0];
        if corner[1]<nonInY0 then nonInY0:=corner[1];
      end;
      if gate^.behavior^.gateType=gt_output then begin
        if gate^.ioLocations.numberOfTopInputs>0
        then begin corner[0]+=gate^.gridWidth;  if movingBottom or autoshrink then corner[1]:=0; end
        else begin corner[1]+=gate^.gridHeight; if movingLeft   or autoshrink then corner[0]:=0; end;
      end else begin
        corner[0]+=gate^.gridWidth;  if gate^.ioLocations.numberOfRightOutputs >0 then inc(corner[0]);
        corner[1]+=gate^.gridHeight; if gate^.ioLocations.numberOfBottomOutputs>0 then inc(corner[1]);
      end;
      if gate^.behavior^.gateType=gt_input then begin
        if gate^.ioLocations.numberOfRightOutputs>0
        then gridX0:=4
        else gridY0:=4;
      end;
      if (corner[0]>gridX1) then gridX1:=corner[0];
      if (corner[1]>gridY1) then gridY1:=corner[1];
    end;

  VAR gate:P_visualGate;
      oldGridX1,oldGridY1,
      oldGridX0,oldGridY0:longint;
      delta: T_point;
  begin
    nonInX0:=maxLongint;
    nonInY0:=maxLongint;
    oldGridX0:=gridX0; gridX0:=0;
    oldGridY0:=gridY0; gridY0:=0;
    oldGridX1:=gridX1;
    oldGridY1:=gridY1;
    if autoshrink then begin
      gridX1:=gridX0;
      gridY1:=gridY0;
    end;
    for gate in inputs  do growIfNecessary(gate);
    for gate in outputs do growIfNecessary(gate);
    for gate in gates   do growIfNecessary(gate);
    if autoshrink then begin
      gridX1+=1;
      gridY1+=1;
    end;
    if (gridX0<>oldGridX0) or (gridY0<>oldGridY0) then begin
      delta:=pointOf(gridX0-oldGridX0,gridY0-oldGridY0);
      if nonInX0>=gridX0 then delta[0]:=0;
      if nonInY0>=gridY0 then delta[1]:=0;
      for gate in gates   do gate^.gridPos+=delta;
      for gate in outputs do gate^.gridPos+=delta;
      gridX1+=delta[0];
      gridY1+=delta[1];
    end;
    for gate in outputs do begin
      if P_outputGate(gate^.behavior)^.onLeftOrRightSide
      then gate^.gridPos[0]:=gridX1
      else gate^.gridPos[1]:=gridY1;
    end;
    if ((gridX1<>oldGridX1) or (gridY1<>oldGridY1)) and (ui<>nil) then begin
      ui^.updateBoardScrollbars;
      rewire(false);
    end;
    wiresUpToDate:=false;
  end;

PROCEDURE T_visualBoard.detachUI;
  VAR e:P_visualGate;
  begin
    if ui<>nil then begin
      ui^.activeBoard:=nil;
      ui^.hideIoEdit;
    end;
    for e in inputs  do begin e^.uiAdapter:=nil; end;
    for e in outputs do begin e^.uiAdapter:=nil; end;
    for e in gates   do begin e^.uiAdapter:=nil; end;
    ui:=nil;
  end;

PROCEDURE T_visualBoard.attachUI(CONST uiAdapter: P_uiAdapter);
  VAR e:P_visualGate;
  begin
    if uiAdapter^.activeBoard<>nil then uiAdapter^.activeBoard^.detachUI;
    wiresUpToDate:=false;
    ui:=uiAdapter;
    uiAdapter^.activeBoard:=@self;
    for e in inputs  do begin e^.uiAdapter:=uiAdapter; end;
    for e in outputs do begin e^.uiAdapter:=uiAdapter; end;
    for e in gates   do begin e^.uiAdapter:=uiAdapter; end;
    checkBoardExtend(false,false);
    rewire(false); //only missing wires...
    ui^.resetState;
    ui^.uiElement.boardImage.OnMouseMove:=@boardMouseMove;
    ui^.uiElement.boardImage.OnMouseDown:=@boardMouseDown;
    ui^.uiElement.boardImage.OnMouseUp  :=@boardMouseUp;
    ui^.uiElement.boardHorizontalScrollBar.OnScroll:=@ScrollbarScroll;
    ui^.uiElement.boardVerticalScrollBar  .OnScroll:=@ScrollbarScroll;
    ui^.updateTitleLayer;
  end;

PROCEDURE T_visualBoard.elementAdded(CONST newElement: P_visualGate;  CONST mouseIsInBoard: boolean);
  VAR element:P_visualGate;
  begin
    modified:=true;
    element:=newElement;
    if not(mouseIsInBoard)
    then dispose(element,destroy)
    else begin
      if ui<>nil then ui^.saveStateToUndoList;
      case element^.behavior^.gateType of
        gt_input:  begin setLength(inputs ,length(inputs )+1); inputs [length(inputs )-1]:=element; end;
        gt_output: begin setLength(outputs,length(outputs)+1); outputs[length(outputs)-1]:=element; end;
        else       begin setLength(gates  ,length(gates  )+1); gates  [length(gates  )-1]:=element; end;
      end;
      if element^.behavior^.gateType in [gt_input,gt_output] then enumerateIo;
      if ui<>nil then repositionElement(element,true,true);
    end;
    if ui<>nil then ui^.callback.boardModifiedCallback();
  end;

PROCEDURE T_visualBoard.repositionElement(CONST element: P_visualGate;
  CONST skipSaveUndo, mouseIsInBoard: boolean);
  VAR wrap:T_visualGateArray;
  begin
    setLength(wrap,1);
    wrap[0]:=element;
    repositionElement(wrap,skipSaveUndo,mouseIsInBoard);
  end;

PROCEDURE T_visualBoard.repositionElement(CONST elements: T_visualGateArray;
  CONST skipSaveUndo, mouseIsInBoard: boolean);
  VAR element:P_visualGate;
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
      if element^.gridPos[1]+4>gridY1 then reshapeGrid(gridX1,element^.gridPos[1]+4,false);
      while overlapsAnyOther do begin
        element^.gridPos[1]+=stepY;
        if stepY<0 then stepY:=-1-stepY else stepY:=1-stepY;
        if element^.gridPos[1]<element^.gridHeight then element^.gridPos[1]:=element^.gridHeight;
        if element^.gridPos[1]>             gridY1 then element^.gridPos[1]:=gridY1;
      end;
    end;

  PROCEDURE repositionHorizontally;
    VAR stepX:longint=1;
    begin
      if element^.gridPos[0]<4 then element^.gridPos[0]:=4;
      if element^.gridPos[0]+4>gridX1 then reshapeGrid(element^.gridPos[0]+4,gridY1,false);
      while overlapsAnyOther do begin
        element^.gridPos[0]+=stepX;
        if stepX<0 then stepX:=-1-stepX else stepX:=1-stepX;
        if element^.gridPos[0]<element^.gridWidth then element^.gridPos[0]:=element^.gridWidth;
        if element^.gridPos[0]>gridX1             then element^.gridPos[0]:=gridX1;
      end;
    end;

  PROCEDURE reposition;
    VAR stepX:longint=1;
        stepY:longint=0;
        initialX,initialY:longint;
    begin

      if element^.gridPos[1]<gridY0 then element^.gridPos[1]:=gridY0;
      if element^.gridPos[0]<gridX0 then element^.gridPos[0]:=gridX0;
      if element^.gridPos[1]+4>gridY1 then reshapeGrid(gridX1,element^.gridPos[1]+4,false);
      if element^.gridPos[0]+4>gridX1 then reshapeGrid(element^.gridPos[0]+4,gridY1,false);
      initialX:=element^.gridPos[0];
      initialY:=element^.gridPos[1];
      while (overlapsAnyOther or
           (element^.gridPos[0]<gridX0) or (element^.gridPos[0]+element^.gridWidth>gridX1) or
           (element^.gridPos[1]<gridY0) or (element^.gridPos[1]+element^.gridHeight>gridY1)) and (stepX<100) do begin
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
           (element^.gridPos[0]<gridX0) or
           (element^.gridPos[1]<gridY0)) and (stepX<100) do begin
        element^.gridPos[0]+=stepX;
        element^.gridPos[1]+=stepY;
        if      stepX>0 then begin stepY:=   stepX; stepX:=0; end
        else if stepY>0 then begin stepX:=-1-stepY; stepY:=0; end
        else if stepX<0 then begin stepY:=   stepX; stepX:=0; end
        else                 begin stepX:= 1-stepY; stepY:=0; end;
      end;
    end;

  begin
    if not skipSaveUndo then ui^.saveStateToUndoList;

    if not(mouseIsInBoard) then begin
      remove(elements,true);
      exit;
    end;

    for element in elements do begin
      element^.gridPos:=ui^.canvasToGrid(element^.canvasPos);
      case element^.behavior^.gateType of
        gt_input: begin
          if element^.gridPos[0]-gridX0<=element^.gridPos[1]-gridY0 then begin
            element^.gridPos[0]:=0;
            repositionVertically;
            P_inputGate(element^.behavior)^.onLeftOrRightSide:=true;
          end else begin
            element^.gridPos[1]:=0;
            repositionHorizontally;
            P_inputGate(element^.behavior)^.onLeftOrRightSide:=false;
          end;
          element^.setupVisuals;
        end;
        gt_output: begin
          if gridX1-element^.gridPos[0]<gridY1-element^.gridPos[1] then begin
            element^.gridPos[0]:=gridX1;
            repositionVertically;
            P_outputGate(element^.behavior)^.onLeftOrRightSide:=true;
          end else begin
            element^.gridPos[1]:=gridY1;
            repositionHorizontally;
            P_outputGate(element^.behavior)^.onLeftOrRightSide:=false;
          end;
          element^.setupVisuals;
        end;
        else begin
          checkBoardExtend(false,false);
          reposition;
        end;
      end;
      inc(elementIndex);
    end;
    checkBoardExtend(false,false);
    rewire(true);
    enumerateIo;
    ui^.paintAll;
    ui^.uiElement.boardImage.repaint;
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
    if not(skipSaveUndo) then ui^.saveStateToUndoList;
    for element in elements do if not element^.fixedPosition then begin
      //For challenges with fixed number of gates, this has to be added to the palette again.
      associatedPalette^.countUpGate(element^.behavior);
      j:=0;
      enterCriticalSection(wiresCs);
      for i:=0 to length(wires)-1 do begin
        wires[i].dropWiresAssociatedWith(element);
        if length(wires[i].sink)>0 then begin
          wires[j]:=wires[i];
          inc(j);
        end;
      end;
      setLength(wires,j);
      leaveCriticalSection(wiresCs);

      dropWatchersAssociatedWith(element);

      j:=0; for i:=0 to length(inputs )-1 do if inputs [i]=element then dispose(inputs [i],destroy) else begin inputs [j]:=inputs [i]; inc(j); end; setLength(inputs ,j);
      j:=0; for i:=0 to length(outputs)-1 do if outputs[i]=element then dispose(outputs[i],destroy) else begin outputs[j]:=outputs[i]; inc(j); end; setLength(outputs,j);
      j:=0; for i:=0 to length(gates  )-1 do if gates  [i]=element then dispose(gates  [i],destroy) else begin gates  [j]:=gates  [i]; inc(j); end; setLength(gates  ,j);
    end;
    enumerateIo;
    rewire(true);
    ui^.callback.boardModifiedCallback();
  end;

FUNCTION T_visualBoard.loadFromStream(VAR stream: T_bufferedInputStreamWrapper; CONST includeFixing: boolean): boolean;
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
    try
      result:=true;
      captionString:=stream.readAnsiString;
      descriptionString:=stream.readAnsiString;
      myIndex:=stream.readInteger;

      if not(stream.allOkay) then exit(false);
      gridX1:=0;
      gridY1:=0;
      setLength(inputs,stream.readNaturalNumber);
      for i:=0 to length(inputs)-1 do begin
        new(inputGate,create);
        inputGate^.readMetaDataFromStream(stream);
        new(inputs[i],create(inputGate));
        inputs[i]^.gridPos:=readPoint(stream);
        if includeFixing then begin
          inputs[i]^.fixedPosition:=stream.readBoolean;
          inputs[i]^.fixedProperties:=stream.readBoolean;
        end;
      end;
      if not(stream.allOkay) then exit(false);

      setLength(gates,stream.readNaturalNumber);
      for i:=0 to length(gates)-1 do begin
        behavior:=associatedPalette^.readGate(stream);
        new(gates[i],create(behavior));
        gates[i]^.gridPos:=readPoint(stream);
        if includeFixing then begin
          gates[i]^.fixedPosition:=stream.readBoolean;
          gates[i]^.fixedProperties:=stream.readBoolean;
        end;
      end;
      if not(stream.allOkay) then exit(false);

      setLength(outputs,stream.readNaturalNumber);
      for i:=0 to length(outputs)-1 do begin
        new(outputGate,create);
        outputGate^.readMetaDataFromStream(stream);
        new(outputs[i],create(outputGate));
        outputs[i]^.gridPos:=readPoint(stream);
        if includeFixing then begin
          outputs[i]^.fixedPosition:=stream.readBoolean;
          outputs[i]^.fixedProperties:=stream.readBoolean;
        end;
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
        end;
      end;
    except
      stream.logWrongTypeError;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_visualBoard.saveToStream(VAR stream: T_bufferedOutputStreamWrapper;
  CONST includeFixing: boolean);
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
    stream.writeInteger(getIndexInPalette);

    stream.writeNaturalNumber(length(inputs));
    for i:=0 to length(inputs)-1 do begin
      inputs[i]^.behavior^.writeToStream(stream,true);
      writePointToStream(stream,inputs[i]^.gridPos);
      if includeFixing then begin
        stream.writeBoolean(inputs[i]^.fixedPosition);
        stream.writeBoolean(inputs[i]^.fixedProperties);
      end;
    end;

    stream.writeNaturalNumber(length(gates));
    for i:=0 to length(gates)-1 do begin
      gates[i]^.behavior^.writeToStream(stream);
      writePointToStream(stream,gates[i]^.gridPos);
      if includeFixing then begin
        stream.writeBoolean(gates[i]^.fixedPosition);
        stream.writeBoolean(gates[i]^.fixedProperties);
      end;
    end;

    stream.writeNaturalNumber(length(outputs));
    for i:=0 to length(outputs)-1 do begin
      outputs[i]^.behavior^.writeToStream(stream,true);
      writePointToStream(stream,outputs[i]^.gridPos);
      if includeFixing then begin
        stream.writeBoolean(outputs[i]^.fixedPosition);
        stream.writeBoolean(outputs[i]^.fixedProperties);
      end;
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

PROCEDURE T_visualBoard.savePaletteEntryToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST paletteIndex: longint);
  begin
    underlyingPrototype:=nil;
    myIndex:=paletteIndex;
    saveToStream(stream,false);
  end;

PROCEDURE T_visualBoard.loadPaletteEntryFromStream(
  VAR stream: T_bufferedInputStreamWrapper; CONST paletteIndex: longint);
  begin
    loadFromStream(stream,false);
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

FUNCTION T_visualBoard.newGateFromPrototype: P_abstractGate;
  begin
    result:=extractBehavior;
  end;

PROCEDURE T_visualBoard.extractChallenge(CONST challengePalette: P_abstractPrototypeSource; OUT expectedBehavior: P_visualBoard);
  FUNCTION gateInClone(CONST gate:P_visualGate):P_visualGate;
    VAR i:longint;
    begin
      result:=nil;
      for i:=0 to length(inputs )-1 do if inputs [i]=gate then exit(expectedBehavior^.inputs[i]);
      for i:=0 to length(outputs)-1 do if outputs[i]=gate then exit(expectedBehavior^.outputs[i]);
      for i:=0 to length(gates  )-1 do if gates  [i]=gate then exit(expectedBehavior^.gates[i]);
      assert(result<>nil,'extractChallenge of T_circuitBoard failed');
    end;
  VAR i,j:longint;
      behavior: P_abstractGate;
      visual  : P_visualGate;
  begin
    new(expectedBehavior,create(challengePalette));

    expectedBehavior^.myIndex:=-1;
    expectedBehavior^.captionString:=captionString;
    expectedBehavior^.descriptionString:=descriptionString;
    setLength(expectedBehavior^.inputs ,length(inputs ));
    for i:=0 to length(inputs )-1 do begin
      expectedBehavior^.inputs [i]:=inputs[i]^.clone;
    end;
    setLength(expectedBehavior^.outputs,length(outputs));
    for i:=0 to length(outputs)-1 do begin
      expectedBehavior^.outputs[i]:=outputs[i]^.clone;
    end;
    setLength(expectedBehavior^.gates,length(gates));
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.behavior^.gateType=gt_compound
      then begin
        behavior:=P_compoundGate(gates[i]^.behavior)^.exportForChallenge(challengePalette);
        new(visual,create(behavior));
        visual^.gridPos        :=gates[i]^.gridPos;
        visual^.uiAdapter      :=gates[i]^.uiAdapter;
        visual^.canvasPos      :=gates[i]^.canvasPos;
        visual^.ioMode         :=gates[i]^.ioMode;
        visual^.fixedProperties:=false;
        visual^.fixedPosition  :=false;
        expectedBehavior^.gates[i]:=visual;
      end
      else begin
        expectedBehavior^.gates[i]:=gates[i]^.clone;
      end
    end;
    setLength(expectedBehavior^.wires,length(wires));
    for i:=0 to length(wires)-1 do begin
      expectedBehavior^.wires[i].source:=gateInClone(wires[i].source);
      expectedBehavior^.wires[i].sourceOutputIndex:= wires[i].sourceOutputIndex;
      setLength(expectedBehavior^.wires[i].sink,length(wires[i].sink));
      for j:=0 to length(wires[i].sink)-1 do begin
        expectedBehavior^.wires[i].sink[j].gate:=gateInClone(wires[i].sink[j].gate);
        expectedBehavior^.wires[i].sink[j].gateInputIndex:=  wires[i].sink[j].gateInputIndex;
      end;
    end;
  end;

FUNCTION T_visualBoard.getInterfaces: T_gateInterfaces;
  VAR i:longint;
  begin
    initialize(result);
    setLength(result.inputs,length(inputs));
    for i:=0 to length(inputs)-1 do begin
      result.inputs[i].name     :=inputs[i]^.behavior^.getCaption;
      result.inputs[i].wireWidth:=inputs[i]^.behavior^.outputWidth(0);
      result.inputs[i].representation:=inputs[i]^.ioMode;
    end;
    setLength(result.outputs,length(outputs));
    for i:=0 to length(outputs)-1 do begin
      result.outputs[i].name     :=outputs[i]^.behavior^.getCaption;
      result.outputs[i].wireWidth:=outputs[i]^.behavior^.inputWidth(0);
      result.outputs[i].representation:=outputs[i]^.ioMode;
    end;
  end;

FUNCTION T_visualBoard.equals(CONST other: P_visualBoard): boolean;
  VAR k:longint;
  begin
    result:=(length(inputs )=length(other^.inputs )) and
            (length(outputs)=length(other^.outputs)) and
            (length(gates  )=length(other^.gates  )) and
            (length(wires  )=length(other^.wires  ));
    if not(result) then exit(false);
    for k:=0 to length(inputs )-1 do if not(inputs [k]^.behavior^.equals(other^.inputs [k]^.behavior)) then exit(false);
    for k:=0 to length(outputs)-1 do if not(outputs[k]^.behavior^.equals(other^.outputs[k]^.behavior)) then exit(false);
    for k:=0 to length(gates  )-1 do if not(gates  [k]^.behavior^.equals(other^.gates  [k]^.behavior)) then exit(false);
    for k:=0 to length(wires)-1 do if not(wires[k].equals(@self,other,other^.wires[k])) then exit(false);
  end;

FUNCTION T_visualBoard.hash: word;
  VAR g:P_visualGate;
  begin
    {$Q-}{$R-}
    result:=          length(inputs);
    result:=result*31+length(outputs);
    result:=result*31+length(gates);
    result:=result*31+length(wires);
    for g in gates do result:=result*31+byte(g^.behavior^.gateType);
    {$Q+}{$R+}
  end;

FUNCTION T_visualBoard.interfaceHash: word;
  VAR i:longint;
      Interfaces: T_gateInterfaces;
  begin
    Interfaces:=getInterfaces;
    {$Q-}{$R-}
    result:=          length(Interfaces.inputs);
    result:=result*31+length(Interfaces.outputs);
    for i:=0 to length(Interfaces.inputs) -1 do result:=result*31+Interfaces.inputs[i].wireWidth;
    for i:=0 to length(Interfaces.outputs)-1 do result:=result*31+Interfaces.outputs[i].wireWidth;
    {$Q+}{$R+}
  end;

FUNCTION T_visualBoard.cloneAsTrueCopy(CONST fixProperties: boolean): P_visualBoard;
  VAR cloned:P_visualBoard;
      i,j,k:longint;
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
    cloned^.wiresHaveChangedInBackground:=false;
    cloned^.captionString:=captionString;
    cloned^.descriptionString:=descriptionString;
    cloned^.myIndex:=-1;
    cloned^.underlyingPrototype:=nil;
    cloned^.gridX0:=gridX0;
    cloned^.gridX1:=gridX1;
    cloned^.gridY0:=gridY0;
    cloned^.gridY1:=gridY1;
    cloned^.modified:=false;
    setLength(cloned^.inputs ,length(inputs )); for i:=0 to length(inputs )-1 do begin cloned^.inputs [i]:=inputs [i]^.clone; if fixProperties then cloned^.inputs [i]^.fixedProperties:=true; end;
    setLength(cloned^.outputs,length(outputs)); for i:=0 to length(outputs)-1 do begin cloned^.outputs[i]:=outputs[i]^.clone; if fixProperties then cloned^.outputs[i]^.fixedProperties:=true; end;
    setLength(cloned^.gates  ,length(gates));   for i:=0 to length(gates  )-1 do begin cloned^.gates  [i]:=gates  [i]^.clone; if fixProperties then cloned^.gates  [i]^.fixedProperties:=true; end;
    setLength(cloned^.wires  ,length(wires));
    for i:=0 to length(wires)-1 do begin
      cloned^.wires[i].source:=gateInClone(wires[i].source);
      cloned^.wires[i].sourceOutputIndex:= wires[i].sourceOutputIndex;
      setLength(cloned^.wires[i].sink,length(wires[i].sink));
      for j:=0 to length(wires[i].sink)-1 do begin
        cloned^.wires[i].sink[j].gate:=gateInClone(wires[i].sink[j].gate);
        cloned^.wires[i].sink[j].gateInputIndex:=  wires[i].sink[j].gateInputIndex;
        setLength(cloned^.wires[i].sink[j].path,length(wires[i].sink[j].path));
        for k:=0 to length(wires[i].sink[j].path)-1 do cloned^.wires[i].sink[j].path[k]:=wires[i].sink[j].path[k];
      end;
    end;
    result:=cloned;
  end;

FUNCTION T_visualBoard.cloneWithBackReference:P_visualBoard;
  begin
    result:=cloneAsTrueCopy;
    result^.underlyingPrototype:=@self;
  end;

PROCEDURE T_visualBoard.enumerateIo;
  VAR i:longint;
      axis:longint;
      gateSet:T_visualGateArray;

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

  begin
    for i:=0 to length(inputs )-1 do P_inputGate (inputs[i]^.behavior)^.ioIndex:=i;
    for i:=0 to length(outputs)-1 do P_outputGate(outputs[i]^.behavior)^.ioIndex:=i;
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
    new(graph,create(gridX1+1,gridY1+1));
    graph^.initDirections;
    for gate in inputs  do punchOutGate(gate);
    for gate in outputs do punchOutGate(gate);
    for gate in gates   do punchOutGate(gate);
    if not(dropExistingWires) then exit(graph);
    enterCriticalSection(wiresCs);
    for i:=0 to length(wires)-1 do
    for j:=0 to length(wires[i].sink)-1 do
      graph^.dropWire(wires[i].sink[j].path);
    leaveCriticalSection(wiresCs);
    result:=graph;
  end;

PROCEDURE T_visualBoard.rewire(CONST forced: boolean);
  PROCEDURE dropWiresReferringToNonexitentPorts;
    VAR io, i, jo, j: integer;
    begin
      io:=0;
      for i:=0 to length(wires)-1 do begin
        with wires[i] do begin
          unhoverAll;
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
    end;

  PROCEDURE sortWires;
    VAR gateRank:array of longint;
    FUNCTION getGateRank(CONST gate:P_visualGate):longint;
      VAR k:longint;
      begin
        result:=maxLongint;
        for k:=0 to length(gates)-1 do if gates[k]=gate then exit(gateRank[k]);
      end;

    FUNCTION setGateRank(CONST gate:P_visualGate; CONST rank:longint):boolean;
      VAR k:longint;
      begin
        for k:=0 to length(gates)-1 do if gates[k]=gate then begin
          if rank<gateRank[k] then begin
            gateRank[k]:=rank;
            exit(true);
          end else exit(false);
        end;
        result:=false;
      end;

    VAR wireRank:array of longint;
        i,j,sourceRank:longint;
        anyChange:boolean;

        tempWire:T_visualWire;
    begin
      setLength(gateRank,length(gates));
      for i:=0 to length(gates)-1 do
        gateRank[i]:=maxLongint;

      setLength(wireRank,length(wires));
      for i:=0 to length(wires)-1 do
      if wires[i].source^.getBehavior^.gateType=gt_input
      then begin
        wireRank[i]:=0;
        for j:=0 to length(wires[i].sink)-1 do setGateRank(wires[i].sink[j].gate,1);
      end else wireRank[i]:=maxLongint;

      repeat
        anyChange:=false;
        for i:=0 to length(wires)-1 do if wireRank[i]=maxLongint then begin
          sourceRank:=getGateRank(wires[i].source);
          if sourceRank<maxLongint then begin
            anyChange:=true;
            wireRank[i]:=sourceRank+1;
            for j:=0 to length(wires[i].sink)-1 do
              if setGateRank(wires[i].sink[j].gate,sourceRank+2) then anyChange:=true;
          end;
        end;
      until not(anyChange);

      setLength(gateRank,0);

      for i:=1 to length(wires)-1 do for j:=0 to i-1 do
      if wireRank[i]<wireRank[j] then begin
        sourceRank:=wireRank[i]; wireRank[i]:=wireRank[j]; wireRank[j]:=sourceRank;
        tempWire  :=wires   [i]; wires   [i]:=wires   [j]; wires   [j]:=tempWire;
      end;
      setLength(wireRank,0);
    end;

  PROCEDURE updateWirePaths;
    VAR graph: P_wireGraph;
        i, j: integer;
        startPoint: T_point;
        endPoints: T_wirePath;
        paths: T_wirePathArray;
        wireNeedsUpdate: array of boolean;
    begin
      graph:=getWireGraph(false);
      setLength(wireNeedsUpdate,length(wires));

      for i:=0 to length(wires)-1 do begin
        startPoint:=wires[i].source^.getOutputPositionInGridSize(wires[i].sourceOutputIndex)+wires[i].source^.gridPos;
        setLength(endPoints,length(wires[i].sink));
        for j:=0 to length(endPoints)-1 do endPoints[j]:=wires[i].sink[j].gate^.getInputPositionInGridSize(wires[i].sink[j].gateInputIndex)+wires[i].sink[j].gate^.gridPos;
        wireNeedsUpdate[i]:=false;
        for j:=0 to length(wires[i].sink)-1 do
          wireNeedsUpdate[i]:=wireNeedsUpdate[i]
            or (length(wires[i].sink[j].path)<1)
            or (wires[i].sink[j].path[0]<>startPoint)
            or (wires[i].sink[j].path[length(wires[i].sink[j].path)-1]<>endPoints[j]);

        if not(wireNeedsUpdate[i]) then for j:=0 to length(wires[i].sink)-1 do graph^.dropWire(wires[i].sink[j].path,true);
      end;
      for i:=0 to length(wires)-1 do if wireNeedsUpdate[i] then begin
        startPoint:=wires[i].source^.getOutputPositionInGridSize(wires[i].sourceOutputIndex)+wires[i].source^.gridPos;
        setLength(endPoints,length(wires[i].sink));
        for j:=0 to length(endPoints)-1 do endPoints[j]:=wires[i].sink[j].gate^.getInputPositionInGridSize(wires[i].sink[j].gateInputIndex)+wires[i].sink[j].gate^.gridPos;

        paths:=graph^.findPaths(startPoint,endPoints,false);
        for j:=0 to length(paths)-1 do begin
          wires[i].sink[j].path:=paths[j];
          graph^.dropWire(paths[j]);
        end;
      end;

      dispose(graph,destroy);
    end;

  PROCEDURE startBackgroundTask;
    VAR i,j: longint;
    begin
      if (gridX1<4) or (gridY1<4) then exit; //Grid is too small anyway...
      if (ui=nil) then exit; //Nobody will care for the result in this case...

      new(wiringTask,create(getWireGraph(false),@updateWiresFromTask));
      for i:=0 to length(wires)-1 do begin
        wiringTask^.addStartPoint(i,wires[i].source^.getOutputPositionInGridSize(wires[i].sourceOutputIndex)+wires[i].source^.gridPos);
        for j:=0 to length(wires[i].sink)-1 do
          wiringTask^.addEndPoint(wires[i].sink[j].gate^.getInputPositionInGridSize(wires[i].sink[j].gateInputIndex)+wires[i].sink[j].gate^.gridPos);
      end;
      if ui^.wiringInBackground
      then wiringTask^.executeInBackground
      else begin
        wiringTask^.execute;
        dispose(wiringTask,destroy);
        wiringTask:=nil;
      end;
    end;

  {$ifdef debugMode} VAR startTicks: qword; {$endif}
  begin
    if ui=nil then exit;
    {$ifdef debugMode} startTicks:=GetTickCount64; {$endif}
    disposeTask(wiringTask);
    enterCriticalSection(wiresCs);
    dropWiresReferringToNonexitentPorts;
    sortWires;
    if forced
    then startBackgroundTask
    else updateWirePaths;
    leaveCriticalSection(wiresCs);
    wiresUpToDate:=false;
    {$ifdef debugMode} writeln('Rewiring (',forced,') took ',GetTickCount64-startTicks,' ticks'); {$endif}
  end;

PROCEDURE T_visualBoard.updateWiresFromTask;
  VAR k:longint;
      wantToWireInBackground:boolean;
  begin
    if (wiringTask=nil) or (ui=nil) then exit;
    enterCriticalSection(wiresCs);
    for k:=0 to length(wiringTask^.toFind)-1 do wires[wiringTask^.toFind[k].index].setPaths(wiringTask^.toFind[k].foundPath);
    leaveCriticalSection(wiresCs);
    wantToWireInBackground:=wiringTask^.execTime>100;
    if ui^.wiringInBackground then begin
      disposeTask(wiringTask);
      wiringTask:=nil;
      wiresHaveChangedInBackground:=true;
    end;
    ui^.wiringInBackground:=wantToWireInBackground;
  end;

PROCEDURE T_visualBoard.forceWireRepaint;
  begin
    wiresUpToDate:=false;
  end;

PROCEDURE T_visualBoard.paintWires(CONST init: boolean; CONST draw: boolean);
  VAR i:longint;
      Canvas: TBGRACanvas;
      canvasWidth,canvasHeight, zoom:longint;
      startTicks:qword;
  begin
    if not(wiresUpToDate and not(wiresHaveChangedInBackground)) and init then begin
      enterCriticalSection(wiresCs);
      startTicks:=GetTickCount64;
      wiresHaveChangedInBackground:=false;
      //setup
      zoom:=ui^.getZoom;
      canvasWidth :=zoom*(gridX1+4);
      canvasHeight:=zoom*(gridY1+4);
      ui^.uiElement.wireLayer.setSize(canvasWidth,canvasHeight);
      Canvas:=ui^.uiElement.wireLayer.CanvasBGRA;
      Canvas.AntialiasingMode:=amOff;

      if (ui^.uiElement.titleLayer.width<ui^.uiElement.wireLayer.width) or
         (ui^.uiElement.titleLayer.height<ui^.uiElement.wireLayer.height) then begin
        Canvas.Brush.color:=colorScheme.BOARD_COLOR;
        Canvas.FillRect(0,0,canvasWidth,canvasHeight);
      end;
      Canvas.draw(0,0,ui^.uiElement.titleLayer);

      //Draw boundaries:
      Canvas.Pen.color:=colorScheme.BOARD_BOUNDARY_COLOR;
      Canvas.Pen.width:=1;
      Canvas.MoveTo(0          ,gridY0*zoom);
      Canvas.LineTo(canvasWidth,gridY0*zoom);
      Canvas.MoveTo(0          ,gridY1*zoom);
      Canvas.LineTo(canvasWidth,gridY1*zoom);
      Canvas.MoveTo(gridX0*zoom,           0);
      Canvas.LineTo(gridX0*zoom,canvasHeight);
      Canvas.MoveTo(gridX1*zoom,           0);
      Canvas.LineTo(gridX1*zoom,canvasHeight);

      //Draw wires:
      for i:=0 to length(wires)-1 do wires[i].paint(zoom,Canvas);

      wiresUpToDate:=true;
      {$ifdef debugMode}
      writeln('Wire layer @',canvasWidth,'x',canvasHeight,' painted in ',GetTickCount64-startTicks,' ticks');
      {$endif}
      leaveCriticalSection(wiresCs);
    end;
    if draw then begin
      ui^.uiElement.boardImage.Canvas.Brush.color:=colorScheme.BOARD_COLOR;
      ui^.uiElement.boardImage.Canvas.Brush.style:=bsSolid;
      ui^.uiElement.boardImage.Canvas.Pen.style:=psClear;
      ui^.uiElement.boardImage.Canvas.FillRect(0,0,ui^.uiElement.boardImage.width,ui^.uiElement.boardImage.height);
      ui^.uiElement.wireLayer.draw(ui^.uiElement.boardImage.Canvas,
                                   ui^.uiElement.paletteWidth-ui^.uiElement.boardHorizontalScrollBar.position,
                                                             -ui^.uiElement.boardVerticalScrollBar  .position);
    end;
  end;

PROCEDURE T_visualBoard.reshapeGrid(CONST newGridX1, newGridY1: longint;
  CONST byMouseEvent: boolean);
  VAR movingBottom,
      movingLeft: boolean;

  begin
    movingLeft  :=(newGridX1<>gridX1) and byMouseEvent;
    movingBottom:=(newGridY1<>gridY1) and byMouseEvent;
    gridX1:=newGridX1;
    gridY1:=newGridY1;
    checkBoardExtend(movingLeft,movingBottom);
    rewire(false);
  end;

PROCEDURE T_visualBoard.dropWatchersAssociatedWith(CONST gate: P_visualGate);
  VAR i:longint;
      j:longint=0;
  begin
    for i:=0 to length(watchers)-1 do
    if watchers[i]^.gate=gate
    then dispose(watchers[i],destroy)
    else begin
      watchers[j]:=watchers[i];
      inc(j);
    end;
    setLength(watchers,j);
  end;

PROCEDURE T_visualBoard.deleteMarkedGates;
  VAR toRemove:T_visualGateArray;
      i:longint=0;
      g:P_visualGate;
  begin
    setLength(toRemove,length(inputs)+length(outputs)+length(gates));
    for g in inputs  do if g^.marked then begin toRemove[i]:=g; inc(i); end;
    for g in gates   do if g^.marked then begin toRemove[i]:=g; inc(i); end;
    for g in outputs do if g^.marked then begin toRemove[i]:=g; inc(i); end;
    setLength(toRemove,i);
    if i>0 then begin
      modified:=true;
      remove(toRemove,false);
      paintBoard();
      ui^.repaintImage;
    end;
  end;

PROCEDURE T_visualBoard.unhoverAllWires;
  VAR i:longint;
      changed:boolean=false;
  begin
    for i:=0 to length(wires)-1 do if wires[i].unhoverAll then changed:=true;
    if changed then begin
      wiresUpToDate:=false;
      ui^.paintAll;
    end;
  end;

PROCEDURE T_visualBoard.boardMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  VAR gridPos:T_point;
      i:longint;
      horizontal:boolean;
      hoverChanged: boolean=false;
      g:P_visualGate;
      hoverInfo: T_hoverInfo;
      posDelta:T_point;
      anyWireHover:boolean=false;

  begin
    gridPos:=ui^.canvasToGrid(x,y);
    if ui^.state<>uas_initial then unhoverAllWires;
    case ui^.state of
      uas_draggingGridOutputX0: begin
        ui^.uiElement.boardImage.Cursor:=crSizeWE;
        if gridPos[0]<>gridX1 then begin
          reshapeGrid(gridPos[0],gridY1,true);
          paintBoard;
          ui^.uiElement.boardImage.repaint;
        end;
      end;
      uas_draggingGridOutputY0: begin
        ui^.uiElement.boardImage.Cursor:=crSizeNS;
        if gridPos[1]<>gridY1 then begin
          reshapeGrid(gridX1,gridPos[1],true);
          paintBoard;
          ui^.uiElement.boardImage.repaint;
        end;
      end;
      uas_draggingSelectionFrame: begin
        ui^.uiElement.selectionShape.Left  :=min(x+ui^.uiElement.boardImage.Left,ui^.dragData.startX);
        ui^.uiElement.selectionShape.width :=abs(x+ui^.uiElement.boardImage.Left-ui^.dragData.startX);
        ui^.uiElement.selectionShape.top   :=min(y+ui^.uiElement.boardImage.top ,ui^.dragData.startY);
        ui^.uiElement.selectionShape.height:=abs(y+ui^.uiElement.boardImage.top -ui^.dragData.startY);
      end;
      uas_draggingWire: begin
        paintWirePreview(ui^.draggedGate^.getOutputPositionInGridSize(ui^.dragData.outputIndex)+ui^.draggedGate^.gridPos,x,y);
      end;
      uas_draggingFromBoard, uas_multiDragFromBoard: begin
        paintBoard(true);
        posDelta:=pointOf(x-ui^.dragData.startX,y-ui^.dragData.startY);
        for g in ui^.dragData.draggedGates do begin
          g^.canvasPos:=ui^.gridToCanvas(g^.gridPos)+posDelta;
          g^.paintAll(ui^.uiElement.boardImage.Canvas,ui^.zoom);
        end;
        ui^.uiElement.boardImage.repaint;
      end;
      uas_draggingFromPalette: begin
        paintBoard;
        posDelta:=pointOf(x-ui^.dragData.startX,y-ui^.dragData.startY);
        for g in ui^.dragData.draggedGates do begin
          g^.canvasPos:=pointOf(g^.gridPos[0]*ui^.zoom,
                                g^.gridPos[1]*ui^.zoom+ui^.paletteYOffset)+posDelta;
          g^.paintAll(ui^.uiElement.boardImage.Canvas,ui^.zoom);
        end;
        ui^.uiElement.boardImage.repaint;
      end

      else begin
        if x>ui^.paletteWidth then begin
          //Check hover over board gates
          for g in inputs do if g^.isAtGridPos(gridPos,hoverInfo) then
            begin
              ui^.uiElement.boardImage.ShowHint:=true;
              ui^.uiElement.boardImage.Hint:=g^.getBehavior^.getDescription;
              if (hoverInfo.subElement=block) and (hoverInfo.ioIndex>0)
              then ui^.uiElement.boardImage.Cursor:=crIBeam
              else if (hoverInfo.subElement=outputConnector)
              then ui^.uiElement.boardImage.Cursor:=crCross
              else ui^.uiElement.boardImage.Cursor:=crHandPoint;
              unhoverAllWires;
              exit;
            end;
          for g in gates   do if g^.isAtGridPos(gridPos,hoverInfo) then
            begin
              ui^.uiElement.boardImage.ShowHint:=true;
              ui^.uiElement.boardImage.Hint:=g^.getBehavior^.getDescription;
              if (hoverInfo.subElement=outputConnector)
              then ui^.uiElement.boardImage.Cursor:=crCross
              else ui^.uiElement.boardImage.Cursor:=crHandPoint;
              unhoverAllWires;
              exit;
            end;
          for g in outputs do if g^.isAtGridPos(gridPos,hoverInfo) then
            begin
              ui^.uiElement.boardImage.ShowHint:=true;
              ui^.uiElement.boardImage.Hint:=g^.getBehavior^.getDescription;
              ui^.uiElement.boardImage.Cursor:=crHandPoint;
              unhoverAllWires;
              exit;
            end;
          ui^.uiElement.boardImage.ShowHint:=false;
        end else begin
          //Check hover over palette gates
          g:=P_visualGate(associatedPalette^.getGateAtScreenPosition(x,y));
          if g<>nil then begin
            if g^.isMarkedAsDisabled
            then ui^.uiElement.boardImage.Hint:='[Wegen Bearbeitung gesperrt]'+LineEnding+g^.getBehavior^.getDescription
            else ui^.uiElement.boardImage.Hint:=                                          g^.getBehavior^.getDescription;
            ui^.uiElement.boardImage.Cursor:=crHandPoint;
          end else ui^.uiElement.boardImage.Cursor:=crCross;
          ui^.uiElement.boardImage.ShowHint:=(g<>nil);
          exit;
        end;

        //Check hover over right board boundary
        if gridPos[0]=gridX1 then begin
          ui^.uiElement.boardImage.Cursor:=crSizeWE;
          exit;
        end;
        //Check hover over lower board boundary
        if gridPos[1]=gridY1 then begin
          ui^.uiElement.boardImage.Cursor:=crSizeNS;
          exit;
        end;
        //Check hover over wires
        enterCriticalSection(wiresCs);
        for i:=0 to length(wires)-1 do if wires[i].isWirePosition(gridPos[0],gridPos[1],true,hoverChanged,horizontal) then begin
          if horizontal then ui^.uiElement.boardImage.Cursor:=crHSplit
                        else ui^.uiElement.boardImage.Cursor:=crVSplit;
          anyWireHover:=true;
        end;
        leaveCriticalSection(wiresCs);
        //Handle hover over wires
        if not(anyWireHover) then ui^.uiElement.boardImage.Cursor:=crCross;
        if hoverChanged then begin
          wiresUpToDate:=false;
          ui^.paintAll;
          ui^.repaintImage;
        end;
      end;
    end;

  end;

PROCEDURE T_visualBoard.boardMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR x0,x1,y0,y1,i:longint;
  PROCEDURE mark(CONST gate:P_visualGate);
    begin
      if (gate^.canvasPos[0]>=x0) and (gate^.canvasPos[0]+gate^.gridWidth *ui^.zoom<=x1) and
         (gate^.canvasPos[1]>=y0) and (gate^.canvasPos[1]+gate^.gridHeight*ui^.zoom<=y1) then gate^.marked:=true;
    end;

  VAR g:P_visualGate;
  begin
    case ui^.state of
      uas_draggingGridOutputY0,uas_draggingGridOutputX0: ui^.state:=uas_initial;
      uas_draggingSelectionFrame: begin
        x0:=ui^.dragData.startX-ui^.uiElement.boardImage.Left;
        x1:=x;
        y0:=ui^.dragData.startY-ui^.uiElement.boardImage.top;
        y1:=y;
        if x1<x0 then begin i:=x0; x0:=x1; x1:=i; end;
        if y1<y0 then begin i:=y0; y0:=y1; y1:=i; end;

        for g in inputs  do mark(g);
        for g in outputs do mark(g);
        for g in gates   do mark(g);
        paintBoard;
        ui^.uiElement.boardImage.repaint;
        ui^.endSelectionDrag;
      end;
      uas_draggingWire: with ui^.dragData do if dragTarget<>nil then begin
        addWire(draggedGates[0],outputIndex,dragTarget,inputIndex);
        paintBoard;
        ui^.uiElement.boardImage.repaint;
        ui^.state:=uas_initial;
      end;
      uas_draggingFromBoard,uas_multiDragFromBoard: begin
        ui^.state:=uas_initial;
        repositionElement(ui^.dragData.draggedGates,false,(x>=ui^.uiElement.paletteWidth) and (y>=0));
        ui^.paintAll;
      end;
      uas_draggingFromPalette: if x<=ui^.uiElement.paletteWidth then begin
        ui^.state:=uas_initial;
        associatedPalette^.countUpGate(ui^.draggedGate^.behavior);
        associatedPalette^.dropPaletteItem(ui^.draggedGate);
        dispose(ui^.dragData.draggedGates[0],destroy);
        setLength(ui^.dragData.draggedGates,0);
        ui^.paintAll;
      end else begin
        elementAdded(ui^.draggedGate,(x>=ui^.uiElement.paletteWidth) and (y>=0));
        ui^.state:=uas_initial;
        ui^.paintAll;
      end;
      else ui^.state:=uas_initial;
    end;
  end;

PROCEDURE T_visualBoard.ScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; VAR ScrollPos: integer);
  VAR g:P_visualGate;
  begin
    ui^.hideIoEdit;
    ui^.paintAll;
    if ui^.state=uas_propertyEditFromBoard then begin
      g:=ui^.draggedGate;
      ui^.callback.repositionPropertyEditorCallback(
        g^.canvasPos[0]+ui^.uiElement.boardImage.Left+g^.gridWidth*ui^.zoom,
        g^.canvasPos[1]+ui^.uiElement.boardImage.top,false);
    end;
    ui^.repaintImage;
  end;

PROCEDURE T_visualBoard.boardMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR gridPos: T_point;
  FUNCTION handleWatcherHit:boolean;
    VAR i,toDrop:longint;
        j:longint=0;
    begin
      toDrop:=0;
      while (toDrop<length(watchers)) and not(watchers[toDrop]^.isAtCanvasPos(ui,x,y)) do inc(toDrop);
      if toDrop>=length(watchers) then exit(false);

      result:=true;
      for i:=0 to length(watchers)-1 do if i=toDrop then dispose(watchers[i],destroy) else begin watchers[j]:=watchers[i]; inc(j); end;
      setLength(watchers,j);
      ui^.paintAll;
    end;

  PROCEDURE addWatcher(CONST gate:P_visualGate; CONST input:boolean; CONST ioIndex:longint);
    VAR w:P_watcher;
    begin
      for w in watchers do if (w^.gate=gate) and (w^.input=input) and (w^.ioIndex=ioIndex) then exit;
      new(w,create(gate,input,ioIndex));
      setLength(watchers,length(watchers)+1);
      watchers[length(watchers)-1]:=w;
      w^.paint(ui,ui^.uiElement.boardImage.Canvas);
      ui^.uiElement.boardImage.Invalidate;
      ui^.callback.boardModifiedCallback();
    end;

  FUNCTION handleGateHit(CONST g:P_visualGate):boolean;

    CONST next:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);

    VAR hoverInfo: T_hoverInfo;
    begin
      result:=g^.isAtGridPos(gridPos,hoverInfo);
      if not(result) then exit(false);
      case hoverInfo.subElement of
        block:begin
          if (button=mbLeft) then begin
            if (ssShift in Shift) then begin
              g^.marked:=not(g^.marked);
              if g^.marked then begin
                g^.paintAll(ui^.uiElement.boardImage.Canvas);
                ui^.uiElement.boardImage.Invalidate;
              end else paintBoard;
            end else if (g^.behavior^.gateType=gt_input) and (hoverInfo.ioIndex>0) then begin
              ui^.showIoEdit(g);
            end else begin
              if g^.behavior^.gateType=gt_input then g^.flipInputBits;
              if not(g^.fixedPosition) then begin
                ui^.startDrag(x,y,g,uas_draggingFromBoard);
                paintBoard(true);
              end;
            end;
            ui^.repaintImage;
          end else if (button=mbRight) then begin
            if (g^.behavior^.gateType in [gt_input,gt_output]) and (hoverInfo.ioIndex>0) then begin
              g^.ioMode:=next[g^.ioMode];
              g^.paintAll(ui^.uiElement.boardImage.Canvas);
              ui^.hideIoEdit;
              ui^.repaintImage;
            end else begin
              if not g^.fixedProperties then begin
                ui^.state:=uas_propertyEditFromBoard;
                ui^.callback.showPropertyEditorCallback(
                  g,
                  true,
                  g^.canvasPos[0]+ui^.uiElement.boardImage.Left+g^.gridWidth*ui^.zoom,
                  g^.canvasPos[1]+ui^.uiElement.boardImage.top);
              end else ui^.state:=uas_initial;
            end;
          end;
        end;
        outputConnector:begin
          if button=mbLeft
          then begin
            ui^.startDrag(x,y,g,uas_draggingWire,hoverInfo.ioIndex);
            paintWirePreview(ui^.draggedGate^.getOutputPositionInGridSize(ui^.dragData.outputIndex)+ui^.draggedGate^.gridPos,x,y);
          end else addWatcher(g,false,hoverInfo.ioIndex);
        end;
        inputConnector:
          if button<>mbLeft
          then addWatcher(g,true,hoverInfo.ioIndex);
    end;
  end;

  VAR g: P_visualGate;
      i: longint;
      j: longint=0;
      needToDropWire:boolean=false;
      horizontal    :boolean=false;
      dummy         :boolean=false;
  begin
    ui^.callback.repositionPropertyEditorCallback(x,y,true);
    if x<ui^.paletteWidth then begin
      gridPos:=pointOf(round(x/ui^.getZoom),
                       round((y-ui^.paletteYOffset)/ui^.getZoom));
      if ui^.callback.paletteItemAt(gridPos,g) and not(g^.isMarkedAsDisabled) then begin
        if button=mbLeft then begin
          g:=g^.clone;
          g^.setupVisuals;
          ui^.startDrag(x,y,g,uas_draggingFromPalette);
          associatedPalette^.countDownGate(g^.behavior);
        end else if (button=mbRight) and associatedPalette^.isWorkspacePalette then begin
          ui^.state:=uas_propertyEditFromPalette;
          ui^.callback.showPropertyEditorCallback(g,false,
            g^.canvasPos[0]+g^.gridWidth*ui^.zoom+ui^.uiElement.boardImage.Left,
            g^.canvasPos[1]                      +ui^.uiElement.boardImage.top);
        end;
      end;
      exit;
    end;

    gridPos:=ui^.canvasToGrid(x,y);

    if handleWatcherHit then exit;
    for g in inputs  do if handleGateHit(g) then exit;
    for g in gates   do if handleGateHit(g) then exit;
    for g in outputs do if handleGateHit(g) then exit;

    if gridPos[0]=gridX1 then begin ui^.state:=uas_draggingGridOutputX0; exit; end;
    if gridPos[1]=gridY1 then begin ui^.state:=uas_draggingGridOutputY0; exit; end;

    enterCriticalSection(wiresCs);
    for i:=0 to length(wires)-1 do needToDropWire:=needToDropWire or wires[i].isWirePosition(gridPos[0],gridPos[1],false,dummy,horizontal);
    if needToDropWire then begin
      modified:=true;
      ui^.saveStateToUndoList;
      for i:=0 to length(wires)-1 do begin
        wires[i].dropWiresTouchingPosition(gridPos[0],gridPos[1],horizontal);
        if length(wires[i].sink)>0 then begin
          if i<>j then wires[j]:=wires[i];
          inc(j);
        end;
      end;
      setLength(wires,j);
      rewire(false);
      paintBoard;
      ui^.repaintImage;
      ui^.callback.boardModifiedCallback();
      leaveCriticalSection(wiresCs);
      exit;
    end;
    leaveCriticalSection(wiresCs);

    if not(ssShift in Shift) then begin
      for g in inputs  do g^.marked:=false;
      for g in outputs do g^.marked:=false;
      for g in gates   do g^.marked:=false;
      paintBoard;
      ui^.repaintImage;
    end;
    ui^.startDragSelectionFrame(X+ui^.uiElement.boardImage.Left,Y+ui^.uiElement.boardImage.top);

  end;

PROCEDURE T_visualBoard.paintWirePreview(CONST wireStart: T_point;
  CONST screenX, screenY: longint);
  VAR tgt :T_point;
      dist,distToClosest:double;

      g:P_visualGate;
      i:longint;
      graph: P_wireGraph;
      wirePath, newWirePath: T_wirePath;
      wireWidth:byte;

      connectible:T_visualGateArray;
      Canvas: TCanvas;
      w: P_watcher;

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
    enterCriticalSection(wiresCs);
    wireWidth:=ui^.draggedGate^.behavior^.outputWidth(ui^.dragData.outputIndex);
    graph:=getWireGraph(false);

    distToClosest:=infinity;
    setLength(connectible,length(gates)+length(outputs));
    for i:=0 to length(gates)-1 do connectible[i]:=gates[i];
    for i:=0 to length(outputs)-1 do connectible[length(gates)+i]:=outputs[i];

    for g in connectible do for i:=0 to g^.behavior^.numberOfInputs-1 do
    if (g^.behavior^.inputWidth(i)=wireWidth) and not(isInputConnected(g,i)) then begin
      tgt:=g^.getInputPositionInGridSize(i)+g^.gridPos;
      dist:=euklideanDistance(ui^.gridToCanvas(tgt),pointOf(screenX,screenY));
      if dist<distToClosest then begin
        newWirePath:=graph^.findPath(wireStart,tgt{,wiresStartingAt(ui^.draggedGate,ui^.dragData.outputIndex)});
        if length(newWirePath)>0 then begin
          wirePath:=newWirePath;
          ui^.dragData.dragTarget:=g;
          ui^.dragData.inputIndex:=i;
          distToClosest:=dist;
        end;
      end;
    end;
    leaveCriticalSection(wiresCs);
    dispose(graph,destroy);

    if isInfinite(distToClosest) then exit;

    paintWires(true,false);
    Canvas:=ui^.uiElement.boardImage.Canvas;
    paintPreviewWire(ui^.zoom,
                     ui^.uiElement.wireLayer.CanvasBGRA,
                     wireWidth,
                     wirePath);
    paintWires(false,true);
    for g in inputs   do g^.paintAll(Canvas);
    for g in gates    do g^.paintAll(Canvas);
    for g in outputs  do g^.paintAll(Canvas);
    for w in watchers do w^.paint(ui,Canvas);
    wiresUpToDate:=false;

    ui^.callback.paintPalette(false);
    ui^.repaintImage;
  end;

FUNCTION T_visualBoard.getCaption: shortstring;
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
    if underlyingPrototype=nil then result:=myIndex else result:=underlyingPrototype^.myIndex;
  end;

PROCEDURE T_visualBoard.afterGatePropertiesEdited(CONST editedGate: P_visualGate; CONST editedIsBoardElement: boolean);
  VAR g:P_visualGate;
  begin
    for g in gates do if g^.behavior^.equals(editedGate^.behavior) then g^.propertyEditDone();
    if editedIsBoardElement then begin
      dropWatchersAssociatedWith(editedGate);
      repositionElement(editedGate,true,true);
      checkBoardExtend(false,false);
      modified:=true;
    end;
    rewire(true);
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
    modified:=true;
    if ui<>nil then ui^.saveStateToUndoList;
    while (i<length(wires)) and ((wires[i].source<>sourceGate) or (wires[i].sourceOutputIndex<>sourceOutputIndex)) do inc(i);
    if i>=length(wires) then setLength(wires,i+1);
    wires[i].source:=sourceGate;
    wires[i].sourceOutputIndex:=sourceOutputIndex;
    j:=length(wires[i].sink);
    setLength(wires[i].sink,j+1);
    wires[i].sink[j].gate:=sinkGate;
    wires[i].sink[j].gateInputIndex:=sinkInputIndex;
    rewire(false);
    if ui<>nil then ui^.callback.boardModifiedCallback();
  end;

PROCEDURE T_visualBoard.reset(CONST includeInputs: boolean);
  VAR g:P_visualGate;
  begin
    simStepCounter:=0;
    if includeInputs then
    for g in inputs  do g^.getBehavior^.reset;
    for g in outputs do g^.getBehavior^.reset;
    for g in gates   do g^.getBehavior^.reset;
  end;

FUNCTION T_visualBoard.coSimulateSteps(CONST count, timeOutInTicks: longint; CONST expectedBehavior: P_abstractGate): longint;
  VAR changed:boolean=true;
      g:P_visualGate;
      i:longint;
      timeout: qword;
  begin
    timeout:=GetTickCount64+timeOutInTicks;

    for i:=0 to length(inputs)-1 do expectedBehavior^.setInput(i,inputs[i]^.behavior^.getInput(0));

    result:=0;
    while changed and (result<count) and (GetTickCount64<timeout) do begin
      changed:=false;
      if expectedBehavior^.simulateStep then changed:=true;
      for i:=0 to length(wires)-1 do if wires[i].simulateStep then changed:=true;
      for g in gates              do if g^.simulateStep       then changed:=true;

      if changed then inc(result);
    end;
    inc(simStepCounter,result);
    for i:=0 to length(outputs)-1 do
      if   outputs[i]^.behavior^.getInput(0)=expectedBehavior^.getOutput(i)
      then outputs[i]^.outputMark:=iom_correctOutput
      else outputs[i]^.outputMark:=iom_incorrectOutput;

    if result>0 then ui^.paintAll;
  end;

FUNCTION T_visualBoard.outputsMatch(CONST expected: P_compoundGate): boolean;
  VAR i:longint;
  begin
    result:=true;
    for i:=0 to length(outputs)-1 do
      if   outputs[i]^.behavior^.getInput(0)=expected^.getOutput(i)
      then begin end
      else exit(false);
  end;

FUNCTION T_visualBoard.interfacesMatch(CONST expected: P_compoundGate): boolean;
  VAR i:longint;
  begin
    result:=(length(inputs) = expected^.numberOfInputs) and
            (length(outputs)=expected^.numberOfOutputs);
    if not(result) then exit(false);
    for i:=0 to length(inputs)-1 do if inputs[i]^.behavior^.inputWidth(0)<>expected^.inputWidth(i) then exit(false);
    for i:=0 to length(outputs)-1 do if outputs[i]^.behavior^.inputWidth(0)<>expected^.outputWidth(i) then exit(false);
  end;

PROCEDURE T_visualBoard.setInputs(CONST inputWires: T_wireValueArray);
  VAR i:longint;
  begin
    for i:=0 to min(length(inputs),length(inputWires)-1) do inputs[i]^.behavior^.setInput(0,inputWires[i]);
    ui^.paintAll;
  end;

FUNCTION T_visualBoard.simulateSteps(CONST count, timeOutInTicks: longint): longint;
  VAR changed:boolean=true;
      g:P_visualGate;
      i:longint;
      timeout: qword;
  begin
    timeout:=GetTickCount64+timeOutInTicks;

    result:=0;
    while changed and (result<count) and (GetTickCount64<timeout) do begin
      changed:=false;
      for i:=0 to length(wires)-1 do if wires[i].simulateStep then changed:=true;
      for g in gates              do if g^.simulateStep       then changed:=true;
      if changed then inc(result);
    end;
    inc(simStepCounter,result);
    if result>0 then ui^.paintAll;
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
      ui^.dragData.draggedGates:=marked;
      result:=true;
    end else begin
      setLength(marked,0);
      result:=false;
    end;
  end;

FUNCTION T_visualBoard.isVisualBoard: boolean;
  begin result:=true; end;

FUNCTION T_visualBoard.usesPrototype(CONST p: P_captionedAndIndexed; CONST transitiveScan:boolean): boolean;
  VAR g:P_visualGate;
  begin
    result:=false;
    if transitiveScan then begin
      for g in gates do
        if (g^.behavior^.gateType=gt_compound) and
           P_compoundGate(g^.behavior)^.usesPrototype(p)
        then exit(true);
    end else begin
      for g in gates do
        if (g^.behavior^.gateType=gt_compound) and
           (P_compoundGate(g^.behavior)^.prototype=p)
        then exit(true);
    end;
  end;

PROCEDURE T_visualBoard.prototypeUpdated(CONST oldPrototype, newPrototype: P_captionedAndIndexed);
  VAR g:P_visualGate;
      anyGateUpdated:boolean=false;
  begin
    if pointer(underlyingPrototype)=pointer(oldPrototype) then underlyingPrototype:=P_visualBoard(newPrototype);
    for g in gates do if (g^.behavior^.gateType=gt_compound) then begin
      if P_compoundGate(g^.behavior)^.prototype=P_captionedAndIndexed(oldPrototype) then begin
        dispose(g^.behavior,destroy);
        g^.behavior:=newPrototype^.newGateFromPrototype;
        if ui<>nil then begin
          g^.setupVisuals;
          repositionElement(g,true,true);
          dropWatchersAssociatedWith(g);
        end;
        anyGateUpdated:=true;
      end else P_compoundGate(g^.behavior)^.prototypeUpdated(oldPrototype,newPrototype);
    end;
    if anyGateUpdated and (ui<>nil) then rewire(true);
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

    Clipboard:=ui^.helperState.Clipboard;
    if Clipboard=nil
    then new(Clipboard,create(associatedPalette))
    else Clipboard^.clear;
    ui^.helperState.Clipboard:=Clipboard;

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
    ui^.saveStateToUndoList;
    Clipboard:=ui^.helperState.Clipboard;
    if Clipboard=nil then exit;
    setLength(newElements,0);
    oldInputsCount :=length(inputs );
    oldOutputsCount:=length(outputs);
    oldGatesCount  :=length(gates  );
    for g in Clipboard^.inputs  do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(inputs ,gCopy); end;
    for g in Clipboard^.outputs do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(outputs,gCopy); end;
    for g in Clipboard^.gates   do begin gCopy:=g^.clone; addGate(newElements,gCopy); addGate(gates  ,gCopy); end;
    if length(newElements)>0 then modified:=true;
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
      g^.gridPos+=posDelta;
      g^.canvasPos:=pointOf(g^.gridPos[0]*ui^.getZoom-ui^.uiElement.boardHorizontalScrollBar.position,
                            g^.gridPos[1]*ui^.getZoom-ui^.uiElement.boardVerticalScrollBar  .position);
      g^.paintAll(ui^.uiElement.boardImage.Canvas,ui^.getZoom);
      g^.marked:=true;
    end;
    repositionElement(newElements,true,true);
  end;

PROCEDURE T_visualBoard.paintBoard(CONST exludeDragged: boolean);
  VAR Canvas:TCanvas;
      g:P_visualGate;
      w:P_watcher;
      {$ifdef debugMode} start: qword; {$endif}
  FUNCTION isExcluded(CONST g:P_visualGate):boolean;
    VAR d:P_visualGate;
    begin
      if not(exludeDragged) then exit(false);
      for d in ui^.dragData.draggedGates do if d=g then exit(true);
      result:=false;
    end;

  begin
    {$ifdef debugMode} start:=GetTickCount64; {$endif}
    paintWires;
    Canvas:=ui^.uiElement.boardImage.Canvas;
    for g in inputs  do if not isExcluded(g) then g^.paintAll(Canvas);
    for g in gates   do if not isExcluded(g) then g^.paintAll(Canvas);
    for g in outputs do if not isExcluded(g) then g^.paintAll(Canvas);
    for w in watchers do w^.paint(ui,Canvas);
    ui^.callback.paintPalette(ui^.state in [uas_draggingFromBoard,uas_multiDragFromBoard]);
    {$ifdef debugMode} writeln('Full paint done in ',GetTickCount64-start,' ticks'); {$endif}
  end;

FUNCTION T_visualBoard.getGateCount: T_gateCount;
  VAR g:P_visualGate;
      gt:T_gateType;
  begin
    for gt in T_gateType do result[gt]:=0;
    for g in inputs  do g^.behavior^.countGates(result);
    for g in gates   do g^.behavior^.countGates(result);
    for g in outputs do g^.behavior^.countGates(result);
  end;

FUNCTION T_visualBoard.getInfoLabelText: string;
  VAR gateCount: T_gateCount;
  FUNCTION lineFor(CONST types:T_gateTypeSet):string;
    VAR gt:T_gateType;
        anyInLine:boolean=false;
    begin
      result:='';
      for gt in types do if gateCount[gt]>0 then begin
        if anyInLine then result+='; ';
        result+=C_gateTypeName[gt]+': '+intToStr(gateCount[gt]);
        anyInLine:=true;
      end;
      if anyInLine then result+=LineEnding;
    end;

  begin
    gateCount:=getGateCount;
    result:='Elemente insgesamt: '+intToStr(gatesTotal(gateCount))+LineEnding+
            lineFor([gt_input,gt_output,gt_notGate,gt_andGate,gt_orGate,gt_xorGate,gt_nandGate,gt_norGate,gt_xorGate])+
            lineFor([gt_adapter,gt_true,gt_false,gt_undeterminedToTrue,gt_undeterminedToFalse,gt_compound,gt_clock,gt_gatedClock,gt_ram,gt_rom,gt_7segmentDummy])+
            'Schritte simuliert: '+intToStr(simStepCounter);
  end;

PROCEDURE T_visualBoard.moveToPalette(CONST newPalette: P_abstractPrototypeSource);
  begin
    associatedPalette:=newPalette;
  end;

CONSTRUCTOR T_watcher.create(CONST gate_: P_visualGate; CONST input_: boolean; CONST ioIndex_: longint);
  begin
    gate:=gate_;
    input:=input_;
    ioIndex:=ioIndex_;
  end;

DESTRUCTOR T_watcher.destroy;
  begin
  end;

FUNCTION T_watcher.getCanvasPos(CONST uiAdapter: P_uiAdapter): T_point;
  begin
    if input
    then result:=gate^.getInputPositionInGridSize(ioIndex)
    else result:=gate^.getOutputPositionInGridSize(ioIndex);
    result:=uiAdapter^.gridToCanvas(result+gate^.gridPos);
  end;

PROCEDURE T_watcher.paint(CONST uiAdapter: P_uiAdapter; CONST Canvas: TCanvas);
  CONST gt:array[false..true] of T_gateType=(gt_output,gt_input);
  VAR wire:T_wireValue;
  begin
    if input
    then wire:=gate^.behavior^.getInput(ioIndex)
    else wire:=gate^.behavior^.getOutput(ioIndex);
    with gate^.ioLocations.p[gt[input],ioIndex] do
    getWatcherSprite(ioLabel,ioIndex,wire,input,leftOrRight)^.renderAt(Canvas,uiAdapter^.getZoom,getCanvasPos(uiAdapter));
  end;

FUNCTION T_watcher.isAtCanvasPos(CONST uiAdapter: P_uiAdapter; CONST x,y: longint): boolean;
  CONST gt:array[false..true] of T_gateType=(gt_output,gt_input);
  VAR wire:T_wireValue;
  begin
    if input
    then wire:=gate^.behavior^.getInput(ioIndex)
    else wire:=gate^.behavior^.getOutput(ioIndex);
    with gate^.ioLocations.p[gt[input],ioIndex] do
    result:=P_watcherSprite(getWatcherSprite(ioLabel,ioIndex,wire,input,leftOrRight))^.isAtPixel(pointOf(x,y)-getCanvasPos(uiAdapter))
  end;

{$endif}

