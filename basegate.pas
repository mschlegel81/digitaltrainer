UNIT baseGate;
{$mode objfpc}
INTERFACE
USES ExtCtrls, Classes, Controls, StdCtrls, UITypes, wiringUtil,
     serializationUtil, logicGates,Graphics,Menus,ComCtrls,Forms;

CONST defaultBoardCaption='unbenannt';
      TRI_STATE_NOT:array[T_triStateValue] of T_triStateValue=(tsv_true,tsv_false,tsv_false);
      MAX_UNDO=32;
      MultiBitColor  :TColor=$FF8800;
      TrueColor      :TColor=$00FF00;
      FalseColor     :TColor=$FFFFFF;
      BackgroundColor:TColor=$DDDDDD;
TYPE
{$define includeInterface}
  F_changeCallback=PROCEDURE(CONST inputModified:boolean) of object;

  P_circuitBoard=^T_circuitBoard;
  P_workspace=^T_workspace;
  {$i visualgates.inc}
  T_visualGateConnector=object
    gate:P_visualGate;
    index:longint;
    FUNCTION gateIndex(CONST board: P_circuitBoard):longint;
    FUNCTION loadFromStream(CONST board:P_circuitBoard; VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(CONST board:P_circuitBoard; VAR stream:T_bufferedOutputStreamWrapper);
  end;

  {$i workspaces.inc}
  {$i uiAdapter.inc}
  T_wireTrip=record
    sink:   T_visualGateConnector;
    visual: T_wirePath;
    marked: boolean;
  end;

  T_logicWire=record
    //One source can be associated with many sinks
    source:   T_visualGateConnector;
    width:byte; //Nonpersistent!
    wires:array of T_wireTrip;
  end;

  T_repositionOutput=(ro_positionUnchanged,ro_positionFound,ro_noPositionFound);
  {$i customGates.inc}
  T_circuitBoard=object
    public
      version    :longint;
      name       :string;
      description:string;
      paletteIndex,categoryIndex:longint;
    private
      gates     :array of P_visualGate;
      logicWires:array of T_logicWire;

      GUI: P_uiAdapter;

      FUNCTION repositionCustomRegion(VAR origin:T_point; CONST size:T_point; CONST considerWires:boolean):T_repositionOutput;
      FUNCTION canGateBeMovedBy(CONST gateToCheck:P_visualGate; CONST delta:T_point; CONST considerWires,considerMarkedGates:boolean):boolean;
      FUNCTION positionNewGate(CONST gateToAdd:P_visualGate):boolean;
      PROCEDURE addGateWithoutChecking(CONST gateToAdd:P_visualGate);
      FUNCTION isInputConnected(CONST gate:P_visualGate; CONST inputIndex:longint):boolean;

      FUNCTION clone(CONST includeWirePaths:boolean):P_circuitBoard;
      PROCEDURE enumerateIo;
      PROCEDURE repaint;
      PROCEDURE sortGates;
      FUNCTION wrapGate(CONST origin:T_point;CONST g:P_abstractGate; CONST template:P_visualGate=nil):P_visualGate;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;  virtual;
      PROCEDURE clear;
      PROCEDURE setSelectForAll(CONST doSelect:boolean);
      PROCEDURE attachGUI(CONST adapter:P_uiAdapter);
      FUNCTION detachGUI:P_uiAdapter;
      PROCEDURE deleteInvalidWires;
      PROCEDURE deleteMarkedElements;
      FUNCTION simulateSteps(CONST count:longint):longint;

      FUNCTION loadFromStream(CONST workspace:P_workspace; VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);

      PROCEDURE copySelectionToClipboard;
      PROCEDURE pasteFrom(CONST board:P_circuitBoard; CONST fullCopy:boolean=false);

      PROCEDURE reset;
      PROCEDURE getBoardExtend(OUT origin,size:T_point);
      PROCEDURE rewire(CONST forced:boolean=false);

      FUNCTION usesBoard(CONST other:P_circuitBoard; CONST recurse:boolean=false):boolean;

      FUNCTION behaviorEquals(CONST other:P_circuitBoard):boolean;
      PROCEDURE replaceCustomGates(CONST oldPrototype,newPrototype:P_circuitBoard);
  end;

{$undef includeInterface}
IMPLEMENTATION
USES sysutils,math,myGenerics,Dialogs,DateUtils;
OPERATOR =(CONST x,y:T_visualGateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

{$define includeImplementation}
{$i visualgates.inc}
{$i customGates.inc}
{$i workspaces.inc}
{$i uiAdapter.inc}
{$undef includeImplementation}

FUNCTION T_visualGateConnector.gateIndex(CONST board: P_circuitBoard):longint;
  begin
    result:=0;
    while (result<length(board^.gates)) and (gate<>board^.gates[result]) do inc(result);
  end;

FUNCTION T_visualGateConnector.loadFromStream(CONST board: P_circuitBoard; VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR gateIdx:longint;
  begin
    gateIdx:=stream.readNaturalNumber;
    if gateIdx>length(board^.gates) then exit(false);
    gate:=board^.gates[gateIdx];
    index:=stream.readNaturalNumber;
    result:=stream.allOkay and ((index<gate^.numberOfInputs) or (index<gate^.numberOfOutputs));
  end;

PROCEDURE T_visualGateConnector.saveToStream(CONST board: P_circuitBoard; VAR stream: T_bufferedOutputStreamWrapper);
  VAR gateIdx:longint=0;
  begin
    while (gateIdx<length(board^.gates)) and (gate<>board^.gates[gateIdx]) do inc(gateIdx);
    stream.writeNaturalNumber(gateIdx);
    stream.writeNaturalNumber(index);
  end;

CONSTRUCTOR T_circuitBoard.create;
  begin
    version:=random(maxLongint);
    GUI:=nil;
    paletteIndex:=-1;
    categoryIndex:=-1;
    name:=defaultBoardCaption;
    description:='';
    setLength(gates,0);
    setLength(logicWires,0);
  end;

DESTRUCTOR T_circuitBoard.destroy;
  begin
    detachGUI;
    clear;
  end;

PROCEDURE T_circuitBoard.attachGUI(CONST adapter:P_uiAdapter);
  VAR gate:P_visualGate;
  begin
    GUI:=adapter;
    GUI^.newBoardAttached(@self);
    for gate in gates do gate^.ensureGuiElements;
    rewire;
  end;

FUNCTION T_circuitBoard.detachGUI:P_uiAdapter;
  VAR gate:P_visualGate;
  begin
    for gate in gates do gate^.disposeGuiElements;
    if GUI=nil then exit(nil);
    result:=GUI;
    GUI^.currentBoard:=nil;
    GUI:=nil;
  end;

PROCEDURE T_circuitBoard.clear;
  VAR i,j:longint;
  begin
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    for i:=0 to length(logicWires)-1 do begin
      for j:=0 to length(logicWires[i].wires)-1 do
        setLength(logicWires[i].wires[j].visual,0);
      setLength(logicWires[i].wires,0);
    end;
    setLength(logicWires,0);
    paletteIndex:=-1;
    categoryIndex:=-1;
    description:='';
    version:=random(maxLongint);
    name:=defaultBoardCaption;
    if GUI<>nil then GUI^.repaint;
  end;

PROCEDURE T_circuitBoard.setSelectForAll(CONST doSelect: boolean);
  VAR gate:P_visualGate;
      i,j:longint;
  begin
    for gate in gates do gate^.setMarked(doSelect);
    for i:=0 to length(logicWires)-1 do
    with logicWires[i] do
    for j:=0 to length(wires)-1 do
    wires[j].marked:=doSelect;
    GUI^.drawAllWires;
  end;

FUNCTION T_circuitBoard.repositionCustomRegion(VAR origin:T_point; CONST size:T_point; CONST considerWires:boolean):T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_visualGate;
        i,j,k:longint;
    begin
      if (o[0]<5) or (o[0]+size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
         (o[1]<5) or (o[1]+size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
      then exit(false);
      result:=true;
      for gate in gates do
      if (o[0]+size[0]>gate^.origin[0]) and (o[0]<gate^.origin[0]+gate^.size[0]) and
         (o[1]+size[1]>gate^.origin[1]) and (o[1]<gate^.origin[1]+gate^.size[1])
      then exit(false);

      if considerWires then
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do begin
        for k:=0 to length(visual)-2 do
        if lineCrossesRectangle(visual[k],visual[k+1],o,size)
        then exit(false);
      end;
    end;

  VAR newOrigin:T_point;
  FUNCTION applyOrigin:T_repositionOutput;
    begin
      origin:=newOrigin;
      result:=ro_positionFound;
    end;

  VAR range:longint=1;
      i:longint;
  begin
    newOrigin:=origin;
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

FUNCTION T_circuitBoard.canGateBeMovedBy(CONST gateToCheck:P_visualGate; CONST delta:T_point; CONST considerWires,considerMarkedGates:boolean):boolean;
  VAR gate:P_visualGate;
      i,j,k:longint;
      o:T_point;
  begin
    o:=gateToCheck^.origin+delta;
    if (o[0]<5) or (o[0]+gateToCheck^.size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
       (o[1]<5) or (o[1]+gateToCheck^.size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
    then exit(false);
    result:=true;
    for gate in gates do
    if (gate<>gateToCheck) and (not(gate^.marked_) or considerMarkedGates)  and
       (o[0]+gateToCheck^.size[0]>gate^.origin[0]) and (o[0]<gate^.origin[0]+gate^.size[0]) and
       (o[1]+gateToCheck^.size[1]>gate^.origin[1]) and (o[1]<gate^.origin[1]+gate^.size[1])
    then exit(false);

    if considerWires then
    for i:=0 to length(logicWires)-1 do
    for j:=0 to length(logicWires[i].wires)-1 do
    with logicWires[i].wires[j] do begin
      for k:=0 to length(visual)-2 do
      if lineCrossesRectangle(visual[k],visual[k+1],o,gateToCheck^.size)
      then exit(false);
    end;
  end;

FUNCTION T_circuitBoard.positionNewGate(CONST gateToAdd: P_visualGate): boolean;
  VAR gateOrigin:T_point;
  begin
    gateOrigin:=gateToAdd^.origin;
    if repositionCustomRegion(gateOrigin,gateToAdd^.size,false)<>ro_noPositionFound then begin
      setLength(gates,length(gates)+1);
      gates[length(gates)-1]:=gateToAdd;
      gateToAdd^.origin:=gateOrigin;
      result:=true;
      gateToAdd^.ensureGuiElements;
      gateToAdd^.repaint;
    end else result:=false;
  end;

PROCEDURE T_circuitBoard.deleteInvalidWires;
  VAR k,j,i:longint;
      anyDeleted:boolean=false;
  begin
    for k:=0 to length(logicWires)-1 do with logicWires[k] do begin
      width:=source.gate^.behavior^.outputWidth(source.index);
      j:=0;
      for i:=0 to length(wires)-1 do
      if (wires[i].sink.gate^.behavior^.numberOfInputs>wires[i].sink.index) and
         (wires[i].sink.gate^.behavior^.inputWidth    (wires[i].sink.index)=width)
      then begin
        wires[j]:=wires[i];
        inc(j);
      end else anyDeleted:=true;
      setLength(wires,j);
    end;
    j:=0;
    for i:=0 to length(logicWires)-1 do
    if length(logicWires[i].wires)>0 then begin
      logicWires[j]:=logicWires[i];
      inc(j);
    end;
    if anyDeleted then begin
      rewire(true);
      version:=random(maxLongint);
    end;
    if GUI<>nil then GUI^.repaint;
  end;

PROCEDURE T_circuitBoard.enumerateIo;
  VAR gate:P_visualGate;
      inputIndex:longint=0;
      outputIndex:longint=0;
  begin
    for gate in gates do
    case gate^.behavior^.gateType of
      gt_input: begin
        P_inputGate(gate^.behavior)^.ioIndex:=inputIndex;
        inc(inputIndex);
      end;
      gt_output: begin
        P_outputGate(gate^.behavior)^.ioIndex:=outputIndex;
        inc(outputIndex);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.deleteMarkedElements;
  PROCEDURE removeAssociatedWires(CONST gateToDelete:P_visualGate);
    VAR i:longint;
        j:longint=0;
        i_,j_:longint;
    begin
      for i:=0 to length(logicWires)-1 do begin
        if logicWires[i].source.gate<>gateToDelete then begin
          logicWires[j]:=logicWires[i];
          with logicWires[j] do begin
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

  VAR k,i:longint;
      j:longint=0;
      ioDeleted:boolean=false;
      anyDeleted:boolean=false;
  begin
    GUI^.saveStateToUndoList;

    GUI^.lastClickedGate:=nil;
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.marked
      then begin
        anyDeleted:=true;
        GUI^.gateDeleted(gates[i]);
        ioDeleted:=ioDeleted or (gates[i]^.behavior^.gateType in [gt_input,gt_output]);
        removeAssociatedWires(gates[i]);
        dispose(gates[i],destroy)
      end
      else begin
        gates[j]:=gates[i];
        inc(j);
      end;
    end;
    setLength(gates,j);

    if ioDeleted then enumerateIo;

    for k:=0 to length(logicWires)-1 do with logicWires[k] do begin
      j:=0;
      for i:=0 to length(wires)-1 do
      if wires[i].marked
      then anyDeleted:=true
      else begin
        wires[j]:=wires[i];
        inc(j);
      end;
      setLength(wires,j);
    end;

    j:=0;
    for i:=0 to length(logicWires)-1 do
    if length(logicWires[i].wires)>0 then begin
      logicWires[j]:=logicWires[i];
      inc(j);
    end;
    setLength(logicWires,j);

    if anyDeleted then version:=random(maxLongint);

    rewire;
    if GUI<>nil then GUI^.repaint;
  end;

FUNCTION T_circuitBoard.simulateSteps(CONST count: longint): longint;
  VAR gate:P_visualGate;
      i,j,wireLoopCounter,step:longint;
      output:T_wireValue;
      anythingHappenedInThisStep:boolean=true;
      adapterValueChanged:boolean;
  begin
    result:=0;
    for step:=1 to count do if anythingHappenedInThisStep then begin
      anythingHappenedInThisStep:=false;
      for gate in gates do anythingHappenedInThisStep:=gate^.behavior^.simulateStep or anythingHappenedInThisStep;
      adapterValueChanged:=true;
      for wireLoopCounter:=0 to 99 do if adapterValueChanged then begin
        adapterValueChanged:=false;
        for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
          output:=source.gate^.behavior^.getOutput(source.index);
          if isFullyDefined(output) then
          for j:=0 to length(wires)-1 do
          if wires[j].sink.gate^.behavior^.setInput(wires[j].sink.index,output) then begin
            if wires[j].sink.gate^.behavior^.gateType=gt_adapter then adapterValueChanged:=true;
            anythingHappenedInThisStep:=true;
          end;
        end;
      end;
      if anythingHappenedInThisStep then inc(result);
    end;
    if result>0 then for gate in gates do gate^.updateIoVisuals;
  end;

FUNCTION T_circuitBoard.wrapGate(CONST origin: T_point; CONST g: P_abstractGate; CONST template:P_visualGate=nil): P_visualGate;
  begin
    case g^.gateType of
      gt_compound: new(P_visualGateForCustom(result),create(origin,g,@self));
      gt_output  : new(P_visualGateForOutput(result),create(origin,g,@self));
      gt_input   : new(P_visualGateForInput (result),create(origin,g,@self));
      else         new(                      result ,create(origin,g,@self));
    end;
    if (template<>nil) and (g^.gateType=gt_input) then begin
      P_visualGateForInput(result)^.inputMode:=P_visualGateForInput(template)^.inputMode;
    end;
  end;

FUNCTION T_circuitBoard.loadFromStream(CONST workspace: P_workspace; VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      gateType: T_gateType;
      k:longint;
      origin: T_point;
      behavior: P_abstractGate;
      gateCount: qword;
  begin
    name:=stream.readShortString;
    description:=stream.readShortString;
    paletteIndex:=stream.readLongint;
    categoryIndex:=stream.readLongint;

    if not(stream.allOkay) then exit(false);
    setLength(gates,0);
    gateCount:=stream.readNaturalNumber;
    if (gateCount>maxLongint) then exit(false);
    for i:=0 to longint(gateCount)-1 do begin
      gateType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));

      if not(stream.allOkay) then exit(false);

      if gateType=gt_compound then begin
        try
          k:=stream.readNaturalNumber;
        except
          k:=maxLongint;
        end;
        if k>=length(workspace^.paletteEntries) then exit(false);
        new(P_customGate(behavior),createFromBoard(workspace^.paletteEntries[k]));
      end else begin
        behavior:=newBaseGate(gateType);
        behavior^.readMetaDataFromStream(stream);
      end;
      origin:=readPoint(stream);
      behavior^.reset;
      setLength(gates,i+1);
      gates[i]:=wrapGate(origin,behavior);
    end;

    setLength(logicWires,stream.readNaturalNumber);
    for i:=0 to length(logicWires)-1 do begin
      logicWires[i].source.loadFromStream(@self,stream);
      logicWires[i].width:=logicWires[i].source.gate^.behavior^.outputWidth(logicWires[i].source.index);
      setLength(logicWires[i].wires,stream.readNaturalNumber);
      for k:=0 to length(logicWires[i].wires)-1 do begin
        logicWires[i].wires[k].sink.loadFromStream(@self,stream);
        setLength(logicWires[i].wires[k].visual,0);
      end;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_circuitBoard.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,k:longint;
  begin
    stream.writeShortString(name);
    stream.writeShortString(description);
    stream.writeLongint(paletteIndex);
    stream.writeLongint(categoryIndex);
    stream.writeNaturalNumber(length(gates));
    for i:=0 to length(gates)-1 do begin
      gates[i]^.behavior^.writeToStream(stream);
      writePointToStream(stream,gates[i]^.origin);
    end;
    stream.writeNaturalNumber(length(logicWires));
    for i:=0 to length(logicWires)-1 do begin
      logicWires[i].source.saveToStream(@self,stream);
      stream.writeNaturalNumber(length(logicWires[i].wires));
      for k:=0 to length(logicWires[i].wires)-1 do begin
        logicWires[i].wires[k].sink.saveToStream(@self,stream);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.copySelectionToClipboard;
  VAR gateToCopy:array of P_visualGate;
      gate:P_visualGate;
      offset:T_point;
      i,j,ic,jc:longint;
      clipboardSourceConnector,
      clipboardSinkConnector:T_visualGateConnector;
  FUNCTION canConvertConnector(CONST connector:T_visualGateConnector; OUT converted:T_visualGateConnector):boolean;
    VAR gateIndex:longint=0;
    begin
      while (gateIndex<length(gateToCopy)) and (gateToCopy[gateIndex]<>connector.gate) do inc(gateIndex);
      if gateIndex>=length(gateToCopy) then exit(false);

      converted.gate :=GUI^.Clipboard^.gates[gateIndex];
      converted.index:=connector.index;
      result:=true;
    end;
  VAR anyAdded:boolean;
  begin
    if GUI^.Clipboard<>nil then dispose(GUI^.Clipboard,destroy);
    new(GUI^.Clipboard,create);
    offset:=pointOf(BOARD_MAX_SIZE_IN_GRID_ENTRIES,BOARD_MAX_SIZE_IN_GRID_ENTRIES);
    initialize(gateToCopy);
    setLength(gateToCopy,0);
    for gate in gates do if gate^.marked then begin
      setLength(gateToCopy,length(gateToCopy)+1);
      gateToCopy[length(gateToCopy)-1]:=gate;
      offset:=pointOf(min(offset[0],gate^.origin[0]),
                      min(offset[1],gate^.origin[1]));
    end;
    offset:=pointOf(5-offset[0],5-offset[1]);

    for gate in gateToCopy do
      GUI^.Clipboard^.addGateWithoutChecking(
        GUI^.Clipboard^.wrapGate(gate^.origin+offset,
                                gate^.behavior^.clone(true)));

    ic:=0;
    for i:=0 to length(logicWires)-1 do if canConvertConnector(logicWires[i].source,clipboardSourceConnector) then begin
      anyAdded:=false;
      for j:=0 to length(logicWires[i].wires)-1 do if (logicWires[i].wires[j].marked) and canConvertConnector(logicWires[i].wires[j].sink,clipboardSinkConnector) then begin
        if not(anyAdded) then begin
          setLength(GUI^.Clipboard^.logicWires,ic+1);
          GUI^.Clipboard^.logicWires[ic].source:=clipboardSourceConnector;
          GUI^.Clipboard^.logicWires[ic].width :=logicWires[i].width;
          setLength(GUI^.Clipboard^.logicWires[ic].wires,0);
          anyAdded:=true;
        end;
        jc:=length(GUI^.Clipboard^.logicWires[ic].wires);
        setLength(GUI^.Clipboard^.logicWires[ic].wires,jc+1);
        GUI^.Clipboard^.logicWires[ic].wires[jc].marked:=false;
        GUI^.Clipboard^.logicWires[ic].wires[jc].sink:=clipboardSinkConnector;
        setLength(GUI^.Clipboard^.logicWires[ic].wires[jc].visual,0);
      end;
      if anyAdded then inc(ic);
    end;
    if length(GUI^.Clipboard^.gates)=0 then begin
      dispose(GUI^.Clipboard,destroy);
      GUI^.Clipboard:=nil;
    end;
  end;

PROCEDURE T_circuitBoard.addGateWithoutChecking(CONST gateToAdd: P_visualGate);
  begin
    setLength(gates,length(gates)+1);
    gates[length(gates)-1]:=gateToAdd;
    gateToAdd^.ensureGuiElements;
    if gateToAdd^.behavior^.gateType in [gt_input,gt_output] then enumerateIo;
  end;

PROCEDURE T_circuitBoard.pasteFrom(CONST board: P_circuitBoard; CONST fullCopy:boolean);
  VAR indexOfFirstGateAdded:longint;
  FUNCTION addLogicWire(CONST clipboardSource:T_visualGateConnector; CONST width:byte):longint;
    begin
      result:=length(logicWires);
      setLength(logicWires,result+1);
      logicWires[result].source.gate:=gates[indexOfFirstGateAdded+clipboardSource.gateIndex(board)];
      logicWires[result].source.index:=clipboardSource.index;
      logicWires[result].width:=width;
      setLength(logicWires[result].wires,0);
    end;

  PROCEDURE addWire(CONST logicWireIndex:longint; CONST clipboardSink:T_visualGateConnector; CONST path:T_wirePath);
    VAR k,i:longint;
    begin
      k:=length(logicWires[logicWireIndex].wires);
      setLength(logicWires[logicWireIndex].wires,k+1);
      with logicWires[logicWireIndex].wires[k] do begin
        sink.gate :=gates[indexOfFirstGateAdded+clipboardSink.gateIndex(board)];
        sink.index:=clipboardSink.index;
        marked:=false;
        if fullCopy then begin
          setLength(visual,length(path));
          for i:=0 to length(path)-1 do visual[i]:=path[i];
        end else setLength(visual,0);
      end;
    end;

  VAR clipOrigin,
      clipSize,
      clipNewOrigin,
      clipOffset:T_point;

      gate:P_visualGate;
      i,j,
      lwi:longint;

      anyWireAdded:boolean=false;
  begin
    if board=nil then exit;
    if fullCopy then begin
      clear;
      name         :=board^.name;
      description  :=board^.description;
      paletteIndex :=board^.paletteIndex;
      categoryIndex:=board^.categoryIndex;
      version      :=board^.version;
      clipOffset   :=ZERO_POINT;
    end else begin
      board^.getBoardExtend(clipOrigin,clipSize);
      clipNewOrigin:=clipOrigin;
      if GUI<>nil then clipNewOrigin+=GUI^.positionForNextGate(clipSize);
      if repositionCustomRegion(clipNewOrigin,clipSize,true)=ro_noPositionFound then exit;
      clipOffset:=clipNewOrigin-clipOrigin;
    end;

    indexOfFirstGateAdded:=length(gates);
    for gate in board^.gates do addGateWithoutChecking(wrapGate(gate^.origin+clipOffset,gate^.behavior^.clone(fullCopy),gate));

    for i:=0 to length(board^.logicWires)-1 do begin
      lwi:=addLogicWire(board^.logicWires[i].source,board^.logicWires[i].width);
      for j:=0 to length(board^.logicWires[i].wires)-1 do begin
        addWire(lwi,board^.logicWires[i].wires[j].sink,board^.logicWires[i].wires[j].visual);
        anyWireAdded:=true;
      end;
    end;

    if anyWireAdded and not(fullCopy) then rewire;
    if GUI<>nil then GUI^.repaint;
  end;

PROCEDURE T_circuitBoard.reset;
  VAR gate:P_visualGate;
  begin
    for gate in gates do begin
      gate^.behavior^.reset;
      gate^.updateIoVisuals;
    end;
  end;

PROCEDURE T_circuitBoard.getBoardExtend(OUT origin, size: T_point);
  VAR gate:P_visualGate;
      maximum,tmp:T_point;
  begin
    if length(gates)=0 then begin
      origin:=ZERO_POINT;
      size  :=ZERO_POINT;
      exit;
    end;
    origin:=pointOf(BOARD_MAX_SIZE_IN_GRID_ENTRIES+1,BOARD_MAX_SIZE_IN_GRID_ENTRIES+1);
    maximum:=pointOf(-1,-1);
    for gate in gates do begin
      tmp:=gate^.origin;
      if tmp[0]<origin[0] then origin[0]:=tmp[0];
      if tmp[1]<origin[1] then origin[1]:=tmp[1];
      tmp+=gate^.size;
      if tmp[0]>maximum[0] then maximum[0]:=tmp[0];
      if tmp[1]>maximum[1] then maximum[1]:=tmp[1];
    end;
    size:=maximum-origin;
  end;

PROCEDURE T_circuitBoard.repaint;
  VAR gate:P_visualGate;
  begin
    if (GUI<>nil) then begin
      for gate in gates do gate^.repaint;
      GUI^.drawAllWires;
    end;
  end;

PROCEDURE T_circuitBoard.sortGates;
  TYPE T_annotatedGate=record gate:P_visualGate; distanceFromInput,inputIndex,fromIndex:longint; end;

  FUNCTION lesserThan(CONST x,y:T_annotatedGate):boolean;
    begin
      result:=(x.distanceFromInput<y.distanceFromInput) or
              (x.distanceFromInput=y.distanceFromInput) and
                 ((x.inputIndex<y.inputIndex) or
                  (x.inputIndex=y.inputIndex) and
                    (x.fromIndex<y.fromIndex));
    end;

  PROCEDURE copyAnnotations(CONST src:T_annotatedGate; VAR dest:T_annotatedGate);
    begin
      dest.distanceFromInput:=src.distanceFromInput;
      dest.inputIndex       :=src.inputIndex;
      dest.fromIndex        :=src.fromIndex;
    end;

  VAR tmp:T_annotatedGate;
      annotatedGates:array of T_annotatedGate;
      i,j:longint;
      logicWire : T_logicWire;
      wireTrip  : T_wireTrip;
      sourceGate: T_annotatedGate;
      updated: boolean;
  begin
    setLength(annotatedGates,length(gates));
    for i:=0 to length(gates)-1 do begin
      annotatedGates[i].distanceFromInput:=IfThen(gates[i]^.behavior^.gateType=gt_input,0,maxLongint);
      annotatedGates[i].inputIndex:=0;
      annotatedGates[i].fromIndex :=0;
      annotatedGates[i].gate:=gates[i];
    end;

    repeat
      updated:=false;
      for logicWire in logicWires do with logicWire do begin
        sourceGate:=annotatedGates[source.gateIndex(@self)];
        tmp.distanceFromInput:=sourceGate.distanceFromInput+1;
        tmp.fromIndex        :=source.index;
        if sourceGate.distanceFromInput<maxLongint then for wireTrip in wires do begin
          i:=wireTrip.sink.gateIndex(@self);
          tmp.inputIndex:=wireTrip.sink.index;
          if lesserThan(tmp,annotatedGates[i])
          then begin
            copyAnnotations(tmp,annotatedGates[i]);
            updated:=true;
          end;
        end;
      end;
    until not updated;
    //When sorting we must ensure that the order of I/O gates remains unchanged!

    for j:=1 to length(annotatedGates)-1 do
    for i:=0 to j-1 do
    if lesserThan(annotatedGates[j],annotatedGates[i]) and
       not((annotatedGates[j].gate^.getBehavior^.gateType in [gt_input,gt_output]) and
           (annotatedGates[j].gate^.getBehavior^.gateType=annotatedGates[i].gate^.getBehavior^.gateType)) then begin
      tmp              :=annotatedGates[i];
      annotatedGates[i]:=annotatedGates[j];
      annotatedGates[j]:=tmp;
    end;

    for i:=0 to length(gates)-1 do gates[i]:=annotatedGates[i].gate;
    setLength(annotatedGates,0);
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

//TODO: Move to T_uiAdapter ?
PROCEDURE T_circuitBoard.rewire(CONST forced:boolean);
  FUNCTION hasHigherPrio(CONST a,b:T_logicWire):boolean;
    VAR aDist:int64=0;
        bDist:int64=0;
        trip:T_wireTrip;
        start:T_point;
    begin
      if length(a.wires)>length(b.wires) then exit(true) else
      if length(a.wires)<length(b.wires) then exit(false);

      start:=a.source.gate^.getOutputPositionInGridSize(a.source.index);
      for trip in a.wires do aDist+=maxNormDistance(start,trip.sink.gate^.getInputPositionInGridSize(trip.sink.index));
      start:=b.source.gate^.getOutputPositionInGridSize(b.source.index);
      for trip in b.wires do bDist+=maxNormDistance(start,trip.sink.gate^.getInputPositionInGridSize(trip.sink.index));

      result:=aDist<bDist;
    end;

  PROCEDURE ensureSorting;
    VAR alreadySorted:boolean=true;
        i,j:longint;
        tmp:T_logicWire;
    begin
      for i:=0 to length(logicWires)-2 do alreadySorted:=alreadySorted and not(hasHigherPrio(logicWires[i+1],logicWires[i]));
      if alreadySorted then exit;
      for j:=1 to length(logicWires)-1 do
      for i:=0 to j-1 do if hasHigherPrio(logicWires[j],logicWires[i]) then begin
        tmp          :=logicWires[i];
        logicWires[i]:=logicWires[j];
        logicWires[j]:=tmp;
      end;
    end;

  FUNCTION wireCrossesGate(CONST path:T_wirePath):boolean;
    VAR gate:P_visualGate;
        gateOrigin,gateSize:T_point;
        k:longint;
    begin
      result:=false;
      for gate in gates do begin
        gateOrigin:=gate^.origin;
        gateSize  :=gate^.size;
        for k:=0 to length(path)-2 do
        if lineCrossesRectangle(path[k],path[k+1],gateOrigin,gateSize)
        then exit(true);
      end;
    end;

  VAR connector:T_visualGateConnector;
      i,j:longint;

      needAnyRewire:boolean=false;
      preview:array of record
        startPoint:T_point;
        targetPoints:T_wirePath;
        needRewire:boolean;
      end;
      paths:T_wirePathArray;

  begin
    if forced then ensureSorting;

    connector.gate:=nil;
    connector.index:=0;
    GUI^.initWireGraph(connector,false);
    initialize(preview);
    setLength(preview,length(logicWires));
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      preview[i].needRewire:=forced;
      preview[i].startPoint:=source.gate^.getOutputPositionInGridSize(source.index);
      setLength(preview[i].targetPoints,length(wires));
      for j:=0 to length(preview[i].targetPoints)-1 do begin
        preview[i].targetPoints[j]:=wires[j].sink.gate^.getInputPositionInGridSize(wires[j].sink.index);
        preview[i].needRewire:=preview[i].needRewire or
           (length(wires[j].visual)<=0) or //no wire there at all
           (wires[j].visual[0]<>preview[i].startPoint) or //start point off
           (wires[j].visual[length(wires[j].visual)-1]<>preview[i].targetPoints[j]) or
           wireCrossesGate(wires[j].visual) or
           not(GUI^.wireGraph.isWireAllowed(wires[j].visual));
      end;
      needAnyRewire:=needAnyRewire or preview[i].needRewire;
    end;
    if not(needAnyRewire) or (GUI=nil) then exit;

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if not(preview[i].needRewire)
      then for j:=0 to length(wires)-1 do GUI^.wireGraph.dropWire(wires[j].visual);

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if preview[i].needRewire then begin
        paths:=GUI^.wireGraph.findPaths(preview[i].startPoint,preview[i].targetPoints);
        for j:=0 to length(wires)-1 do begin
          wires[j].visual:=   paths[j];
          GUI^.wireGraph.dropWire(paths[j]);
        end;
        setLength(paths,0);
      end;
  end;

FUNCTION T_circuitBoard.clone(CONST includeWirePaths:boolean): P_circuitBoard;
  FUNCTION convert(CONST connector:T_visualGateConnector; CONST tgt:P_circuitBoard):T_visualGateConnector;
    VAR gateIndex:longint=0;
    begin
      while (gateIndex<length(gates)) and (connector.gate<>gates[gateIndex]) do inc(gateIndex);
      result.gate:=tgt^.gates[gateIndex];
      result.index:=connector.index;
    end;

  PROCEDURE copyPath(CONST source:T_wirePath; VAR target:T_wirePath);
    VAR i:longint;
    begin
      if includeWirePaths then begin
        setLength(target,length(source));
        for i:=0 to length(target)-1 do target[i]:=source[i];
      end else setLength(target,0);
    end;

  VAR i,j:longint;
  begin
    new(result,create);
    result^.name         :=name;
    result^.description  :=description;
    result^.paletteIndex :=paletteIndex;
    result^.categoryIndex:=categoryIndex;
    setLength(result^.gates,length(gates));
    for i:=0 to length(gates)-1 do begin
      result^.gates[i]:=result^.wrapGate(gates[i]^.origin,gates[i]^.behavior^.clone(includeWirePaths),gates[i]);
    end;

    setLength(result^.logicWires,length(logicWires));
    for i:=0 to length(logicWires)-1 do begin
      result^.logicWires[i].source:=
      convert(logicWires[i].source,result);
      result^.logicWires[i].width:=logicWires[i].width;
      setLength(result^.logicWires[i].wires,length(logicWires[i].wires));
      for j:=0 to length(logicWires[i].wires)-1 do begin
        setLength(result^.logicWires[i].wires[j].visual,0);
        result^.logicWires[i].wires[j].sink:=
        convert(logicWires[i].wires[j].sink,result);
        copyPath(logicWires[i].wires[j].visual,
         result^.logicWires[i].wires[j].visual);
      end;
    end;
  end;

FUNCTION T_circuitBoard.usesBoard(CONST other:P_circuitBoard; CONST recurse:boolean=false):boolean;
  VAR gate:P_visualGate;
      board:P_circuitBoard;
  begin
    result:=(other=@self) or (other^.paletteIndex=paletteIndex);
    for gate in gates do
    if (gate^.behavior^.gateType=gt_compound) and
       ((P_customGate(gate^.behavior)^.prototype=other) or
        (recurse and P_customGate(gate^.behavior)^.prototype^.usesBoard(other,recurse))) then exit(true);
    if GUI=nil then exit;
    for board in GUI^.undoList do if board^.usesBoard(other) then exit(true);
    for board in GUI^.redoList do if board^.usesBoard(other) then exit(true);
  end;

FUNCTION T_circuitBoard.behaviorEquals(CONST other:P_circuitBoard):boolean;
  VAR g1,g2:P_customGate;
      i:longint=0;
      j:longint=0;

      inputWidths,outputWidths:array of longint;
      gate:P_visualGate;
  begin
    for gate in gates do case gate^.getBehavior^.gateType of
      gt_input: begin
        setLength(inputWidths,i+1);
        inputWidths[i]:=gate^.getBehavior^.outputWidth(0);
        inc(i);
      end;
      gt_output: begin
        setLength(outputWidths,j+1);
        outputWidths[j]:=gate^.getBehavior^.inputWidth(0);
        inc(j);
      end;
    end;
    i:=0;
    j:=0;
    for gate in other^.gates do case gate^.getBehavior^.gateType of
      gt_input: begin
        if (i>=length(inputWidths)) or (inputWidths[i]<>gate^.getBehavior^.outputWidth(0)) then exit(false);
        inc(i);
      end;
      gt_output: begin
        if (j>=length(outputWidths)) or (outputWidths[j]<>gate^.getBehavior^.inputWidth(0)) then exit(false);
        inc(j);
      end;
    end;
    if (i<>length(inputWidths)) or (j<>length(outputWidths)) then exit(false);
    new(g1,createFromBoard(@self));
    new(g2,createFromBoard(other));
    result:=g1^.behaviorEquals(g2);
    dispose(g1,destroy);
    dispose(g2,destroy);

  end;

PROCEDURE T_circuitBoard.replaceCustomGates(CONST oldPrototype,newPrototype:P_circuitBoard);
  VAR gate:P_visualGate;
  begin
    for gate in gates do
    if (gate^.behavior^.gateType=gt_compound) and
        (P_customGate(gate^.behavior)^.prototype =oldPrototype)
    then P_customGate(gate^.behavior)^.prototype:=newPrototype;
  end;

end.

