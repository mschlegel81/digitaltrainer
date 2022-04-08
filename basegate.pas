UNIT baseGate;
{$mode objfpc}{$H+}
INTERFACE
USES ExtCtrls, Classes, Controls, StdCtrls, UITypes, wiringUtil,
     serializationUtil, logicGates,Menus;

CONST defaultBoardCaption='unbenannt';
      TRI_STATE_NOT:array[T_triStateValue] of T_triStateValue=(tsv_true,tsv_false,tsv_false);

TYPE
{$define includeInterface}
  F_simpleCallback=PROCEDURE of object;

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

  { T_workspace }

  T_workspace=object(T_serializable)
    paletteEntries:array of P_circuitBoard;
    currentBoard:P_circuitBoard;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType);
    PROCEDURE addCustomGate(CONST index:longint);

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE addCurrentBoardToPalette;

    PROCEDURE removePaletteEntry(CONST index:longint);
    PROCEDURE editPaletteEntry(CONST index:longint);
  end;

  T_repositionOutput=(ro_positionUnchanged,ro_positionFound,ro_noPositionFound);
  {$i customGates.inc}
  T_circuitBoard=object
    public
      name       :string;
      description:string;
      paletteIndex:longint;
    private
      GUI:record
        zoom:longint;
        container:TWinControl;
        wireImage:TImage;
        gateContextMenu:TPopupMenu;
        anyChangeCallback:F_simpleCallback;
        lastClickedGate:P_visualGate;
        selectionFrame:TShape;
        selectionStart:T_point;
        Clipboard:P_circuitBoard;
      end;

      gates      :array of P_visualGate;

      //One source can be associated with many sinks
      logicWires:array of record
        source:   T_visualGateConnector;
        width:byte; //Nonpersistent!
        wires:array of record
          sink:   T_visualGateConnector;
          visual: T_wirePath; //Nonpersistent!
          marked: boolean;    //Nonpersistent!
        end;
      end;

      incompleteWire:record
        dragging:boolean;
        width:byte;
        source:T_visualGateConnector;
        sourcePoint:T_point;
      end;
      wireGraph:P_wireGraph;
      FUNCTION repositionGate(VAR origin:T_point; CONST size:T_point; CONST considerWires:boolean):T_repositionOutput;
      FUNCTION repositionGate(CONST gateToCheck:P_visualGate; CONST considerWires:boolean):T_repositionOutput;
      FUNCTION positionNewGate(CONST gateToAdd:P_visualGate):boolean;
      FUNCTION isInputConnected(CONST gate:P_visualGate; CONST inputIndex:longint):boolean;
      PROCEDURE initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
      PROCEDURE drawAllWires;
      FUNCTION findWirePath(CONST start:T_visualGateConnector; CONST endPoint:T_point):T_wirePath;
      PROCEDURE finishWireDrag(CONST targetPoint:T_point; CONST previewDuringDrag:boolean=false);
      PROCEDURE rewire;
      PROCEDURE WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE WireImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE WireImageMouseUp  (Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      FUNCTION wrapGate(CONST origin:T_point;CONST g:P_abstractGate):P_visualGate;
      FUNCTION clone:P_circuitBoard;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;  virtual;
      PROCEDURE clear;
      PROCEDURE setSelectForAll(CONST doSelect:boolean);
      PROCEDURE attachGUI(CONST zoom:longint; CONST container:TWinControl; CONST wireImage:TImage; CONST gatePopup:TPopupMenu; CONST anyChangeCallback:F_simpleCallback);
      PROCEDURE detachGUI;
      PROCEDURE gateMoved(CONST gate:P_visualGate; CONST doneDragging:boolean);
      PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE fixWireImageSize;
      PROCEDURE setZoom(CONST zoom:longint);
      PROCEDURE deleteInvalidWires;
      PROCEDURE deleteMarkedElements;
      PROCEDURE Repaint;
      FUNCTION simulateSteps(CONST count:longint):boolean;

      FUNCTION loadFromStream(CONST workspace:P_workspace; VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);

      PROCEDURE copySelectionToClipboard;
      PROCEDURE pasteFromClipboard;

      PROPERTY lastClickedGate:P_visualGate read GUI.lastClickedGate;
      PROCEDURE reset;
      PROCEDURE getBoardExtend(OUT origin,size:T_point);
  end;

{$undef includeInterface}
IMPLEMENTATION
USES sysutils,math,Graphics,myGenerics,Dialogs;
{$define includeImplementation}
{$i visualGates.inc}
{$i customGates.inc}
{$undef includeImplementation}

OPERATOR =(CONST x,y:T_visualGateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

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

PROCEDURE T_workspace.addBaseGate(CONST gateType:T_gateType);
  FUNCTION numberOf(CONST gateType:T_gateType):longint;
    VAR gate:P_visualGate;
    begin
      result:=0;
      for gate in currentBoard^.gates do
      if gate^.behavior^.gateType=gateType
      then inc(result);
    end;

  VAR gateToAdd:P_abstractGate=nil;
      visual:P_visualGate;

      p:T_point;
  begin
    gateToAdd:=newBaseGate(gateType);
    if gateToAdd<>nil then begin
      case gateToAdd^.gateType of
        gt_input:  P_inputGate (gateToAdd)^.ioIndex:=numberOf(gt_input);
        gt_output: P_outputGate(gateToAdd)^.ioIndex:=numberOf(gt_output);
      end;

      if length(currentBoard^.gates)=0
      then p:=pointOf(5,5)
      else p:=currentBoard^.gates[length(currentBoard^.gates)-1]^.origin;

      visual:=currentBoard^.wrapGate(p,gateToAdd);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

PROCEDURE T_workspace.addCustomGate(CONST index: longint);
  VAR visual:P_visualGate;
      gateToAdd:P_customGate;
      p: T_point;
  begin
    if (index>=0) and (index<length(paletteEntries)) then begin
      new(gateToAdd,create(paletteEntries[index]));

      if length(currentBoard^.gates)=0
      then p:=pointOf(5,5)
      else p:=currentBoard^.gates[length(currentBoard^.gates)-1]^.origin;

      visual:=currentBoard^.wrapGate(p,gateToAdd);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=5;
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      count: qword;
      board:P_circuitBoard;
  begin
    if not(inherited) then exit(false);
    count:=stream.readNaturalNumber;
    result:=true;
    if (count<maxLongint) then begin
      for i:=0 to longint(count)-1 do begin
        new(board,create);
        if board^.loadFromStream(@self,stream) then begin
          setLength(paletteEntries,i+1);
          paletteEntries[i]:=board;
          board^.paletteIndex:=i;
        end else begin
          dispose(board,destroy);
          exit(false);
        end;
      end;
    end else exit(false);
    result:=currentBoard^.loadFromStream(@self,stream);
    result:=result and stream.allOkay;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    inherited;
    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do paletteEntries[i]^.saveToStream(stream);
    currentBoard^.saveToStream(stream);
  end;

PROCEDURE T_workspace.addCurrentBoardToPalette;
  VAR i:longint;
      gate:P_visualGate;
      anyOut:boolean=false;
      newPaletteEntry:P_circuitBoard;
      doReplace: TModalResult;
  begin
    if currentBoard^.name=defaultBoardCaption then begin
      ShowMessage('Die aktuelle Schaltung muss erst noch benannt werden.');
      exit;
    end;
    for gate in currentBoard^.gates do begin
      anyOut:=anyOut or (gate^.behavior^.gateType=gt_output);
      for i:=0 to gate^.numberOfInputs-1 do
      if not(currentBoard^.isInputConnected(gate,i))
      then begin
        ShowMessage('Es gibt unbelegte Eingänge in der Schaltung.');
        exit;
      end;
    end;
    if not(anyOut) then begin
      ShowMessage('Die Schaltung hat keine Ausgänge.');
      exit;
    end;

    if currentBoard^.paletteIndex>=0 then begin
      doReplace:=QuestionDlg('Ersetzen?','Soll die Schaltung in der Palette aktualisiert werden?',TMsgDlgType.mtConfirmation,[mrYes, 'Ja', mrNo, 'Nein', 'IsDefault'],'');
    end else doReplace:=mrNo;

    if (doReplace=mrNo) then
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i]^.name=currentBoard^.name
    then begin
      ShowMessage('Es gibt schon eine Schaltung mit diesem Namen in der Palette.');
      exit;
    end;

    newPaletteEntry:=currentBoard;
    currentBoard   :=nil;
    new(currentBoard,create);
    currentBoard^.attachGUI(
      newPaletteEntry^.GUI.zoom,
      newPaletteEntry^.GUI.container,
      newPaletteEntry^.GUI.wireImage,
      newPaletteEntry^.GUI.gateContextMenu,
      newPaletteEntry^.GUI.anyChangeCallback);
    newPaletteEntry^.detachGUI;

    if doReplace=mrYes then begin
      i:=newPaletteEntry^.paletteIndex;
      dispose(paletteEntries[i],destroy);
      paletteEntries[i]:=newPaletteEntry;
      //TODO: The palette may be used in other boards, which would lead to errors...
    end else begin
      i:=length(paletteEntries);
      setLength(paletteEntries,i+1);
      paletteEntries[i]:=newPaletteEntry;
      newPaletteEntry^.paletteIndex:=i;
    end;
  end;

{ T_circuitBoard }

CONSTRUCTOR T_circuitBoard.create;
  begin
    with GUI do begin
      zoom:=1;
      container:=nil;
      wireImage:=nil;
      Clipboard:=nil;
    end;
    paletteIndex:=-1;
    name:=defaultBoardCaption;
    description:='';
    setLength(gates,0);
    setLength(logicWires,0);
    wireGraph:=nil;
    incompleteWire.dragging:=false;
  end;

DESTRUCTOR T_circuitBoard.destroy;
  begin
    detachGUI;
    clear;
  end;

PROCEDURE T_circuitBoard.attachGUI(CONST zoom: longint;
  CONST container: TWinControl; CONST wireImage: TImage;
  CONST gatePopup: TPopupMenu; CONST anyChangeCallback: F_simpleCallback);
  VAR gate:P_visualGate;
  begin
    GUI.zoom:=zoom;
    GUI.container:=container;
    GUI.wireImage:=wireImage;
    GUI.gateContextMenu:=gatePopup;
    GUI.anyChangeCallback:=anyChangeCallback;
    GUI.selectionFrame:=TShape.create(container);
    GUI.selectionFrame.parent:=container;
    GUI.selectionFrame.visible:=false;
    GUI.selectionFrame.Brush.style:=bsClear;
    GUI.selectionFrame.Pen.color:=clRed;
    GUI.selectionFrame.Pen.style:=psDashDot;
    wireImage.OnMouseDown:=@WireImageMouseDown;
    wireImage.OnMouseUp  :=@WireImageMouseUp;
    wireImage.OnMouseMove:=@WireImageMouseMove;
    for gate in gates do gate^.ensureGuiElements;
    rewire;
  end;

PROCEDURE T_circuitBoard.detachGUI;
  VAR gate:P_visualGate;
  begin
    for gate in gates do gate^.disposeGuiElements;
    GUI.zoom:=1;
    GUI.container:=nil;
    GUI.wireImage:=nil;
    GUI.gateContextMenu:=nil;
    GUI.anyChangeCallback:=nil;
    FreeAndNil(GUI.selectionFrame);
    if GUI.Clipboard<>nil then begin
      dispose(GUI.Clipboard,destroy);
      GUI.Clipboard:=nil;
    end;
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
    if wireGraph<>nil then dispose(wireGraph,destroy);
    wireGraph:=nil;
    paletteIndex:=-1;
    description:='';
    name:=defaultBoardCaption;
    Repaint;
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
    drawAllWires;
  end;

FUNCTION T_circuitBoard.repositionGate(VAR origin:T_point; CONST size:T_point; CONST considerWires:boolean):T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_visualGate;
        i,j,k,run:longint;
        p:T_point;
        direction:T_wireDirection;
    begin
      if (o[0]<5) or (o[0]+size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
         (o[1]<5) or (o[1]+size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
      then exit(false);
      result:=true;
      for gate in gates do
      if (o[0]+size[0]>origin[0]) and (o[0]<origin[0]+size[0]) and
         (o[1]+size[1]>origin[1]) and (o[1]<origin[1]+size[1])
      then exit(false);

      if considerWires then
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do begin
        for k:=0 to length(visual)-2 do begin
          p:=visual[k];
          direction:=directionBetween  (p,visual[k+1]);
          for run:=0 to maxNormDistance(p,visual[k+1]) do begin
            if (p[0]>=o[0]-1) and (p[0]<=o[0]+size[0]+1) and
               (p[1]>=o[1]-1) and (p[1]<=o[1]+size[1]+1) then exit(false);
            p+=direction;
          end;
        end;
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

FUNCTION T_circuitBoard.repositionGate(CONST gateToCheck: P_visualGate; CONST considerWires: boolean): T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_visualGate;
        i,j,k,run:longint;
        p:T_point;
        direction:T_wireDirection;
    begin
      if (o[0]<5) or (o[0]+gateToCheck^.size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
         (o[1]<5) or (o[1]+gateToCheck^.size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
      then exit(false);
      result:=true;
      for gate in gates do
      if (gate<>gateToCheck) and
         (o[0]+gateToCheck^.size[0]>gate^.origin[0]) and (o[0]<gate^.origin[0]+gate^.size[0]) and
         (o[1]+gateToCheck^.size[1]>gate^.origin[1]) and (o[1]<gate^.origin[1]+gate^.size[1])
      then exit(false);

      if considerWires then
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do begin
        for k:=0 to length(visual)-2 do begin
          p:=visual[k];
          direction:=directionBetween  (p,visual[k+1]);
          for run:=0 to maxNormDistance(p,visual[k+1]) do begin
            if (p[0]>=o[0]-1) and (p[0]<=o[0]+gateToCheck^.size[0]+1) and
               (p[1]>=o[1]-1) and (p[1]<=o[1]+gateToCheck^.size[1]+1) then exit(false);
            p+=direction;
          end;
        end;
      end;
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
    if repositionGate(gateToAdd,true)<>ro_noPositionFound then begin
      setLength(gates,length(gates)+1);
      gates[length(gates)-1]:=gateToAdd;
      result:=true;
      gateToAdd^.ensureGuiElements;
      gateToAdd^.Repaint;
    end else result:=false;
  end;

PROCEDURE T_circuitBoard.deleteInvalidWires;
  VAR k,j,i:longint;
  begin
    for k:=0 to length(logicWires)-1 do with logicWires[k] do begin
      width:=source.gate^.behavior^.outputWidth(source.index);
      j:=0;
      for i:=0 to length(wires)-1 do
      if wires[i].sink.gate^.behavior^.inputWidth(wires[i].sink.index)=width
      then begin
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
    Repaint;
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

  PROCEDURE enumerateIo;
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

  VAR k,i:longint;
      j:longint=0;
      ioDeleted:boolean=false;
  begin
    GUI.lastClickedGate:=nil;
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.marked
      then begin
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
      if not(wires[i]).marked
      then begin
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

    rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.fixWireImageSize;
  VAR width :longint=0;
      height:longint=0;
      gate:P_visualGate;
      p:T_point;
      i,j:longint;
  begin
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
      width +=1; width *=GUI.zoom; width +=max(1,round(GUI.zoom*0.15));
      height+=1; height*=GUI.zoom; height+=max(1,round(GUI.zoom*0.15));
      GUI.wireImage.SetBounds(0,0,width,height);
      GUI.wireImage.picture.Bitmap.Canvas.Brush.color:=clBtnFace;
      GUI.wireImage.picture.Bitmap.setSize(width,height);
      GUI.wireImage.picture.Bitmap.Canvas.clear;
    end;
  end;

PROCEDURE T_circuitBoard.setZoom(CONST zoom: longint);
  begin
    GUI.zoom:=zoom;
    Repaint;
  end;

FUNCTION T_circuitBoard.simulateSteps(CONST count: longint): boolean;
  VAR gate:P_visualGate;
      i,j,step:longint;
      output:T_wireValue;
  begin
    result:=false;
    for step:=1 to count do begin
      for gate in gates do result:=gate^.behavior^.simulateStep or result;
      for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
        output:=source.gate^.behavior^.getOutput(source.index);
        if isFullyDefined(output) then for j:=0 to length(wires)-1 do
          result:=wires[j].sink.gate^.behavior^.setInput(wires[j].sink.index,output) or result;
      end;
    end;
    for gate in gates do gate^.updateIoVisuals;
  end;

FUNCTION T_circuitBoard.wrapGate(CONST origin: T_point; CONST g: P_abstractGate
  ): P_visualGate;
  begin
    case g^.gateType of
      gt_compound: new(P_visualGateForCustom(result),create(origin,g,@self));
      gt_output  : new(P_visualGateForOutput(result),create(origin,g,@self));
      gt_input   : new(P_visualGateForInput (result),create(origin,g,@self));
      else         new(                      result ,create(origin,g,@self));
    end;
  end;

FUNCTION T_circuitBoard.loadFromStream(CONST workspace: P_workspace;
  VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      gateType: T_gateType;
      k:longint;
      origin: T_point;
      behavior: P_abstractGate;
  begin
    name:=stream.readShortString;
    description:=stream.readShortString;
    paletteIndex:=stream.readLongint;
    if not(stream.allOkay) then exit(false);
    setLength(gates,stream.readNaturalNumber);
    for i:=0 to length(gates)-1 do begin
      gateType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));

      if not(stream.allOkay) then exit(false);

      if gateType=gt_compound then begin
        k:=stream.readNaturalNumber;
        //TODO: This provokes later NPEs!
        if k>=length(workspace^.paletteEntries) then exit(false);
        new(P_customGate(behavior),create(workspace^.paletteEntries[k]));
      end else begin
        behavior:=newBaseGate(gateType);
        case gateType of
          gt_input : begin
            P_inputGate (behavior)^.ioIndex:=stream.readNaturalNumber;
            P_inputGate (behavior)^.ioLabel:=stream.readShortString;
            P_inputGate (behavior)^.width  :=stream.readByte;
          end;
          gt_output: begin
            P_outputGate(behavior)^.ioIndex:=stream.readNaturalNumber;
            P_outputGate(behavior)^.ioLabel:=stream.readShortString;
            P_inputGate (behavior)^.width  :=stream.readByte;
          end;
          gt_clock   : P_clock(behavior)^.interval:=stream.readNaturalNumber;
        end;
      end;
      origin:=readPoint(stream);
      behavior^.reset;
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
    stream.writeNaturalNumber(length(gates));
    for i:=0 to length(gates)-1 do begin
      stream.writeByte(byte(gates[i]^.behavior^.gateType));
      case gates[i]^.behavior^.gateType of
        gt_input   : begin
          stream.writeNaturalNumber(P_inputGate (gates[i]^.behavior)^.ioIndex);
          stream.writeShortString  (P_inputGate (gates[i]^.behavior)^.ioLabel);
          stream.writeByte         (P_inputGate (gates[i]^.behavior)^.width);
        end;
        gt_output  : begin
          stream.writeNaturalNumber(P_outputGate(gates[i]^.behavior)^.ioIndex);
          stream.writeShortString  (P_outputGate(gates[i]^.behavior)^.ioLabel);
          stream.writeByte         (P_outputGate(gates[i]^.behavior)^.width);
        end;
        gt_clock   : stream.writeNaturalNumber(P_clock     (gates[i]^.behavior)^.interval);
        gt_compound: stream.writeNaturalNumber(P_customGate(gates[i]^.behavior)^.prototype^.paletteIndex);
      end;

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

      converted.gate :=GUI.Clipboard^.gates[gateIndex];
      converted.index:=connector.index;
      result:=true;
    end;
  VAR anyAdded:boolean;
  begin
    if GUI.Clipboard<>nil then dispose(GUI.Clipboard,destroy);
    new(GUI.Clipboard,create);
    offset:=pointOf(0,0);
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
      GUI.Clipboard^.positionNewGate(
        GUI.Clipboard^.wrapGate(gate^.origin+offset,
                                gate^.behavior^.clone));

    ic:=0;
    for i:=0 to length(logicWires)-1 do if canConvertConnector(logicWires[i].source,clipboardSourceConnector) then begin
      anyAdded:=false;
      for j:=0 to length(logicWires[i].wires)-1 do if (logicWires[i].wires[j].marked) and canConvertConnector(logicWires[i].wires[j].sink,clipboardSinkConnector) then begin
        if not(anyAdded) then begin
          setLength(GUI.Clipboard^.logicWires,ic+1);
          GUI.Clipboard^.logicWires[ic].source:=clipboardSourceConnector;
          GUI.Clipboard^.logicWires[ic].width :=logicWires[i].width;
          setLength(GUI.Clipboard^.logicWires[ic].wires,0);
          anyAdded:=true;
        end;
        jc:=length(GUI.Clipboard^.logicWires[ic].wires);
        setLength(GUI.Clipboard^.logicWires[ic].wires,jc+1);
        GUI.Clipboard^.logicWires[ic].wires[jc].marked:=false;
        GUI.Clipboard^.logicWires[ic].wires[jc].sink:=clipboardSinkConnector;
        setLength(GUI.Clipboard^.logicWires[ic].wires[jc].visual,0);
      end;
      if anyAdded then inc(ic);
    end;
    if length(GUI.Clipboard^.gates)=0 then begin
      dispose(GUI.Clipboard,destroy);
      GUI.Clipboard:=nil;
    end;
  end;

PROCEDURE T_circuitBoard.pasteFromClipboard;
  VAR indexOfFirstGateAdded:longint;
  FUNCTION addLogicWire(CONST clipboardSource:T_visualGateConnector; CONST width:byte):longint;
    begin
      result:=length(logicWires);
      setLength(logicWires,result+1);
      logicWires[result].source.gate:=gates[indexOfFirstGateAdded+clipboardSource.gateIndex(GUI.Clipboard)];
      logicWires[result].source.index:=clipboardSource.index;
      logicWires[result].width:=width;
      setLength(logicWires[result].wires,0);
    end;

  PROCEDURE addWire(CONST logicWireIndex:longint; CONST clipboardSink:T_visualGateConnector);
    VAR k:longint;
    begin
      k:=length(logicWires[logicWireIndex].wires);
      setLength(logicWires[logicWireIndex].wires,k+1);
      with logicWires[logicWireIndex].wires[k] do begin
        sink.gate :=gates[indexOfFirstGateAdded+clipboardSink.gateIndex(GUI.Clipboard)];
        sink.index:=clipboardSink.index;
        marked:=false;
        setLength(visual,0);
      end;
    end;

  VAR clipOrigin,
      clipSize,
      clipNewOrigin,
      clipOffset:T_point;

      newVisualGate:P_visualGate;
      gate:P_visualGate;
      i,j,
      lwi:longint;

      anyWireAdded:boolean=false;
  begin
    if GUI.Clipboard=nil then exit;
    GUI.Clipboard^.getBoardExtend(clipOrigin,clipSize);
    clipNewOrigin:=clipOrigin;
    if repositionGate(clipNewOrigin,clipSize,true)=ro_noPositionFound then exit;
    clipOffset:=clipNewOrigin-clipOrigin;

    indexOfFirstGateAdded:=length(gates);
    for gate in GUI.Clipboard^.gates do begin
      newVisualGate:=wrapGate(gate^.origin+clipOffset,gate^.behavior^.clone);
      if not positionNewGate(newVisualGate) then begin
        dispose(newVisualGate,destroy);
        exit;
      end;
    end;

    for i:=0 to length(GUI.Clipboard^.logicWires)-1 do begin
      lwi:=addLogicWire(GUI.Clipboard^.logicWires[i].source,GUI.Clipboard^.logicWires[i].width);
      for j:=0 to length(GUI.Clipboard^.logicWires[i].wires)-1 do begin
        addWire(lwi,GUI.Clipboard^.logicWires[i].wires[j].sink);
        anyWireAdded:=true;
      end;
    end;

    if anyWireAdded then rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.reset;
  VAR gate:P_visualGate;
  begin
    for gate in gates do begin
      gate^.behavior^.reset;
      gate^.updateIoVisuals;
    end;
  end;

PROCEDURE T_circuitBoard.getBoardExtend(OUT origin,size:T_point);
  VAR gate:P_visualGate;
      maximum,tmp:T_point;
  begin
    origin:=pointOf(BOARD_MAX_SIZE_IN_GRID_ENTRIES+1,BOARD_MAX_SIZE_IN_GRID_ENTRIES+1);
    maximum:=pointOf(-1,-1);
    for gate in gates do begin
      tmp:=gate^.origin+gate^.size;
      origin [0]:=min(origin[0],gate^.origin[0]);
      origin [1]:=min(origin[1],gate^.origin[1]);
      maximum[0]:=max(origin[0],tmp[0]);
      maximum[1]:=max(origin[1],tmp[1]);
    end;
    size:=maximum-origin;
  end;

PROCEDURE T_circuitBoard.Repaint;
  VAR gate:P_visualGate;
  begin
    if (GUI.container<>nil) then begin
      for gate in gates do gate^.Repaint;
      fixWireImageSize;
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

PROCEDURE T_circuitBoard.anyMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    dispose(wireGraph,destroy);
    incompleteWire.dragging:=false;
    drawAllWires;
  end;

PROCEDURE T_circuitBoard.drawAllWires;
  PROCEDURE drawWires(CONST index:longint; CONST foreground:boolean);
    VAR j,k:longint;
    begin for j:=0 to length(logicWires[index].wires)-1 do with logicWires[index].wires[j] do if length(visual)>1 then begin
      if foreground then begin
        if logicWires[index].wires[j].marked
        then GUI.wireImage.Canvas.Pen.color:=clYellow
        else GUI.wireImage.Canvas.Pen.color:=clBlack;
      end;
      GUI.wireImage.Canvas.MoveTo(visual[0,0]*GUI.zoom,visual[0,1]*GUI.zoom);
      for k:=1 to length(visual)-1 do
      GUI.wireImage.Canvas.LineTo(visual[k,0]*GUI.zoom,visual[k,1]*GUI.zoom);
    end; end;

  VAR i:longint;
      wireWidth,gapWidth:longint;
  begin
    if GUI.wireImage=nil then exit;
    with GUI.wireImage.Canvas do begin
      Brush.color:=clBtnFace;
      clear;
      for i:=0 to length(logicWires)-1 do begin
        case logicWires[i].width of
          0..1: begin
            wireWidth:=max(1,round(GUI.zoom*0.08));
            gapWidth :=max(1,round(GUI.zoom*0.38));
          end;
          4..7: begin
            wireWidth:=max(1,round(GUI.zoom*0.15));
            gapWidth :=max(1,round(GUI.zoom*0.45));
          end;
          else begin
            wireWidth:=max(1,round(GUI.zoom*0.25));
            gapWidth :=max(1,round(GUI.zoom*0.55));
          end;
        end;
        Pen.color:=clBtnFace; Pen.width:=gapWidth;
        drawWires(i,false);

        Pen.width:=wireWidth;
        drawWires(i,true);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.initWireGraph(CONST start: T_visualGateConnector; CONST includeWires: boolean);
  VAR gate:P_visualGate;
      x,y,i,j:longint;
  begin
    new(wireGraph,create);
    for gate in gates do begin
      //Points within the gate cannot be reached
      for x:=gate^.origin[0] to gate^.origin[0]+gate^.size[0] do
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do
      wireGraph^.dropNode(pointOf(x,y));
      //Input connections:
      for i:=0 to gate^.numberOfInputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getInputPositionInGridSize(i)+wd_left,wd_right);
      //Output connections:
      for i:=0 to gate^.numberOfOutputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getOutputPositionInGridSize(i),wd_right);
      //No diagonals right left and right of the gate (to prevent blocking of I/O)
      x:=gate^.origin[0];
      for y:=gate^.origin[1]+1 to gate^.origin[1]+gate^.size[1]-1 do wireGraph^.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
      x:=gate^.origin[1]+1;
      for y:=gate^.origin[1]+1 to gate^.origin[1]+gate^.size[1]-1 do wireGraph^.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
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

PROCEDURE T_circuitBoard.gateMoved(CONST gate: P_visualGate; CONST doneDragging: boolean);
  begin
    repositionGate(gate,false);
    if doneDragging then rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.finishWireDrag(CONST targetPoint: T_point; CONST previewDuringDrag: boolean);

  PROCEDURE drawTempWire(CONST targetPoint: T_point);
    VAR wire:T_wirePath;
        i:longint;
    begin
      wire:=findWirePath(incompleteWire.source,targetPoint);
      if length(wire)<=0 then begin
        setLength(wire,2);
        wire[0]:=incompleteWire.sourcePoint;
        wire[1]:=targetPoint;
      end;
      drawAllWires;
      with GUI.wireImage.Canvas do begin
        Pen.color:=clRed;
        Pen.width:=max(1,round(GUI.zoom*0.15));
        MoveTo(wire[0,0]*GUI.zoom,wire[0,1]*GUI.zoom);
        for i:=1 to length(wire)-1 do LineTo(wire[i,0]*GUI.zoom,wire[i,1]*GUI.zoom);
      end;
    end;

  PROCEDURE cleanup;
    begin
      fixWireImageSize;
      drawAllWires;
      dispose(wireGraph,destroy);
      wireGraph:=nil;
      incompleteWire.dragging:=false;
    end;

  VAR i:longint=0;
      j:longint;
      gate:P_visualGate;
      connector:T_visualGateConnector;

      distanceToConnector:longint=maxLongint;
      newDistance:longint;
  begin
    if not(incompleteWire.dragging) then exit;
    connector.gate:=nil;
    for gate in gates do
    for j:=0 to gate^.numberOfInputs-1 do begin
      newDistance:=maxNormDistance(gate^.getInputPositionInGridSize(j)*GUI.zoom,targetPoint);
      if (newDistance<distanceToConnector) and
         (gate^.behavior^.inputWidth(j)=incompleteWire.width) and
         not(isInputConnected(gate,j)) then begin
        connector.gate:=gate;
        connector.index:=j;
        distanceToConnector:=newDistance;
      end;
    end;

    if (connector.gate=nil) then begin
      if not previewDuringDrag
      then cleanup;
      exit;
    end;
    if previewDuringDrag then begin
      drawTempWire(connector.gate^.getInputPositionInGridSize(connector.index));
      exit;
    end;

    i:=0;
    while (i<length(logicWires)) and (logicWires[i].source<>incompleteWire.source) do inc(i);
    if i>=length(logicWires) then setLength(logicWires,i+1);
    with logicWires[i] do begin
      source:=incompleteWire.source;
      width :=incompleteWire.width;
      j:=length(wires);
      setLength(wires,j+1);
      wires[j].sink:=connector;
    end;

    rewire;
    if (GUI.anyChangeCallback<>nil) then GUI.anyChangeCallback();
    cleanup;
    Repaint;
  end;

PROCEDURE T_circuitBoard.rewire;
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
    if wireGraph<>nil then dispose(wireGraph,destroy);
    connector.gate:=nil;
    connector.index:=0;
    initWireGraph(connector,false);
    initialize(preview);
    setLength(preview,length(logicWires));
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      preview[i].needRewire:=false;
      preview[i].startPoint:=source.gate^.getOutputPositionInGridSize(source.index);
      setLength(preview[i].targetPoints,length(wires));
      for j:=0 to length(preview[i].targetPoints)-1 do begin
        preview[i].targetPoints[j]:=wires[j].sink.gate^.getInputPositionInGridSize(wires[j].sink.index);
        preview[i].needRewire:=preview[i].needRewire or
           (length(wires[j].visual)<=0) or //no wire there at all
           (wires[j].visual[0]<>preview[i].startPoint) or //start point off
           (wires[j].visual[length(wires[j].visual)-1]<>preview[i].targetPoints[j]) or
           not(wireGraph^.isWireAllowed(wires[j].visual));
      end;
      needAnyRewire:=needAnyRewire or preview[i].needRewire;
    end;

    if not(needAnyRewire) then exit;

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if not(preview[i].needRewire)
      then for j:=0 to length(wires)-1 do wireGraph^.dropWire(wires[j].visual);

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if preview[i].needRewire then begin
        paths:=wireGraph^.findPaths(preview[i].startPoint,preview[i].targetPoints);
        for j:=0 to length(wires)-1 do begin
          wires[j].visual:=   paths[j];
          wireGraph^.dropWire(paths[j]);
        end;
        setLength(paths,0);
      end;

    fixWireImageSize;
  end;

PROCEDURE T_circuitBoard.WireImageMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  FUNCTION wireHitsPoint(CONST p:T_point; CONST wire:T_wirePath):boolean;
    VAR i,j,len:longint;
        a,b,pointBetween:T_point;
        dir:T_wireDirection;
    begin
      for i:=0 to length(wire)-2 do begin
        a:=wire[i];
        b:=wire[i+1];
        if (a=p) or (b=p) then exit(true);
        len:=maxNormDistance(a,b);
        if len>1 then begin
          dir:=directionBetween(a,b);
          pointBetween:=a;
          for j:=1 to len-1 do begin
            pointBetween+=dir;
            if p=pointBetween then exit(true);
          end;
        end;
      end;
      result:=false;
    end;

  VAR p:T_point;
      i,j:longint;
      anyChange:boolean=false;
  begin
    if (button=mbLeft) and ((ssShift in Shift) or (ssCtrl in Shift)) then begin
      p:=pointOf(round(x / GUI.zoom),round(y / GUI.zoom));
      for i:=0 to length(logicWires         )-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do
      if wireHitsPoint(p,visual)
      then begin
        marked:=not(marked);
        anyChange:=true;
      end;
      if anyChange then drawAllWires;
    end else if (button=mbLeft) then begin
      GUI.selectionFrame.visible:=true;
      GUI.selectionFrame.top :=y; GUI.selectionFrame.height:=1;
      GUI.selectionFrame.Left:=X; GUI.selectionFrame.width :=1;
      GUI.selectionStart:=pointOf(x,y);
    end;
  end;

PROCEDURE T_circuitBoard.WireImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
  VAR low,size:T_point;
  begin
    if GUI.selectionFrame.visible then with GUI do begin
      low :=pointOf(min(x,selectionStart[0]),
                    min(y,selectionStart[1]));
      size:=pointOf(max(x,selectionStart[0]),
                    max(y,selectionStart[1]))-low;

      GUI.selectionFrame.Left  :=low[0];
      GUI.selectionFrame.top   :=low[1];
      GUI.selectionFrame.width :=size[0];
      GUI.selectionFrame.height:=size[1];
    end;
  end;

PROCEDURE T_circuitBoard.WireImageMouseUp(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selStart,selEnd:T_point;
      gate:P_visualGate;
      i,j:longint;
      p:T_point;
  begin
    if GUI.selectionFrame.visible then with GUI do begin
      selStart:=pointOf(floor(min(x,GUI.selectionStart[0])/zoom),
                        floor(min(y,GUI.selectionStart[1])/zoom));
      selEnd  :=pointOf(ceil (max(x,GUI.selectionStart[0])/zoom),
                        ceil (max(y,GUI.selectionStart[1])/zoom));
      selectionFrame.visible:=false;

      for gate in gates do gate^.setMarked(gate^.isCompletelyInsideRect(selStart,selEnd));
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do begin
        marked:=length(visual)>0;
        for p in visual do marked:=marked and (p[0]>=selStart[0]) and (p[0]<=selEnd[0]) and (p[1]>=selStart[1]) and (p[1]<=selEnd[1]);
      end;
      Repaint;
    end;
  end;

FUNCTION T_circuitBoard.clone: P_circuitBoard;
  FUNCTION convert(CONST connector:T_visualGateConnector; CONST tgt:P_circuitBoard):T_visualGateConnector;
    VAR gateIndex:longint=0;
    begin
      while (gateIndex<length(gates)) and (connector.gate<>gates[gateIndex]) do inc(gateIndex);
      result.gate:=tgt^.gates[gateIndex];
      result.index:=connector.index;
    end;

  VAR i,j:longint;
  begin
    new(result,create);
    result^.name:=name;
    result^.description:=description;
    result^.paletteIndex:=paletteIndex;

    setLength(result^.gates,length(gates));
    for i:=0 to length(gates)-1 do begin
      result^.gates[i]:=result^.wrapGate(gates[i]^.origin,gates[i]^.behavior^.clone);
    end;

    setLength(result^.logicWires,length(logicWires));
    for i:=0 to length(logicWires)-1 do begin
      result^.logicWires[i].source:=
      convert(logicWires[i].source,result);
      setLength(result^.logicWires[i].wires,length(logicWires[i].wires));
      for j:=0 to length(logicWires[i].wires)-1 do begin
        setLength(result^.logicWires[i].wires[j].visual,0);
        result^.logicWires[i].wires[j].sink:=
        convert(logicWires[i].wires[j].sink,result);
      end;
    end;
  end;

PROCEDURE T_workspace.removePaletteEntry(CONST index:longint);
  VAR j:longint;
  begin
    //TODO: Entry may be used!
    if (index<0) or (index>length(paletteEntries)) then exit;
    dispose(paletteEntries[index],destroy);
    for j:=index to length(paletteEntries)-2 do begin
      paletteEntries[j]:=paletteEntries[j+1];
      paletteEntries[j]^.paletteIndex:=j;
    end;
    setLength(paletteEntries,length(paletteEntries)-1);
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST index:longint);
  VAR previous:P_circuitBoard;
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=paletteEntries[index]^.clone;
    currentBoard^.attachGUI(
      previous^.GUI.zoom,
      previous^.GUI.container,
      previous^.GUI.wireImage,
      previous^.GUI.gateContextMenu,
      previous^.GUI.anyChangeCallback);
    currentBoard^.rewire;
    dispose(previous,destroy);
    currentBoard^.Repaint;
  end;

end.

