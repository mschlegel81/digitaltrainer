{$ifdef includeInterface}
P_uiAdapter=^T_uiAdapter;
T_uiAdapter=object
  private
    zoom:longint;
    container:TScrollBox;
    wireImage:TImage;
    gateContextMenu:TPopupMenu;
    anyChangeCallback:F_changeCallback;
    lastClickedGate:P_visualGate;
    selectionFrame:TShape;
    selectionStart:T_point;
    Clipboard:P_circuitBoard;
    undoList,redoList:array of P_circuitBoard;

    incompleteWire:record
      dragging:boolean;
      width:byte;
      source:T_visualGateConnector;
      sourcePoint:T_point;
    end;
    wireGraph:T_wireGraph;

    wiringInputCs,wiringOutputCs:TRTLCriticalSection;

    currentBoard:P_circuitBoard;

    peekPanels:array of record
      panel:TPanel;
      peekLabel:TLabel;

      connector:T_visualGateConnector;
    end;
    PROCEDURE peekLabelClick(Sender: TObject);
    PROCEDURE fixWireImageSize;
    PROCEDURE initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
    FUNCTION findWirePath(CONST start:T_visualGateConnector; CONST endPoint:T_point):T_wirePath;
  public
    CONSTRUCTOR create(CONST zoom_:longint; CONST container_:TScrollBox; CONST wireImage_:TImage; CONST gatePopup:TPopupMenu; CONST anyChangeCallback_:F_changeCallback);
    DESTRUCTOR destroy;
    PROCEDURE clearUndoLists;
    PROCEDURE newBoardAttached(CONST board:P_circuitBoard);
    PROPERTY getLastClickedGate:P_visualGate read lastClickedGate;

    PROCEDURE addPeekPanel(CONST panel:TPanel; CONST label_:TLabel);
    PROCEDURE gateDeleted(CONST gate:P_visualGate);
    PROCEDURE simStepDone;
    PROCEDURE startPeeking(CONST connector:T_visualGateConnector);
    PROCEDURE hideAllPeekPanels;
    PROCEDURE repaint;
    PROCEDURE setZoom(CONST newZoom:longint);
    FUNCTION positionForNextGate(CONST gateExtend:T_point):T_point;
    PROCEDURE recenter;
    PROCEDURE saveStateToUndoList;
    PROCEDURE performUndo;
    PROCEDURE performRedo;
    PROCEDURE fixAfterGatePropertiesEdited(CONST gate:P_abstractGate);

    PROCEDURE drawAllWires;
    PROCEDURE finishWireDrag(CONST targetPoint:T_point; CONST previewDuringDrag:boolean=false);
    PROCEDURE WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE WireImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
    PROCEDURE WireImageMouseUp  (Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE pasteFromClipboard;
end;
{$endif}
{$ifdef includeImplementation}
PROCEDURE T_uiAdapter.peekLabelClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=TLabel(Sender).Tag;
    peekPanels[k].panel.visible:=false;
  end;

CONSTRUCTOR T_uiAdapter.create(CONST zoom_: longint; CONST container_: TScrollBox; CONST wireImage_: TImage; CONST gatePopup: TPopupMenu; CONST anyChangeCallback_: F_changeCallback);
  begin
    initCriticalSection(wiringInputCs);
    initCriticalSection(wiringOutputCs);

    zoom:=zoom_;
    container:=container_;
    wireImage:=wireImage_;
    gateContextMenu:=gatePopup;
    anyChangeCallback:=anyChangeCallback_;
    selectionFrame:=TShape.create(container);
    selectionFrame.parent:=container;
    selectionFrame.visible:=false;
    selectionFrame.Brush.style:=bsClear;
    selectionFrame.Pen.color:=clRed;
    selectionFrame.Pen.style:=psDashDot;
    currentBoard:=nil;
    wireGraph.create;
    setLength(peekPanels,0);

    wireImage.OnMouseDown:=@WireImageMouseDown;
    wireImage.OnMouseUp  :=@WireImageMouseUp;
    wireImage.OnMouseMove:=@WireImageMouseMove;
    container.OnMouseDown:=@WireImageMouseDown;
    container.OnMouseUp  :=@WireImageMouseUp;
    container.OnMouseMove:=@WireImageMouseMove;
  end;

DESTRUCTOR T_uiAdapter.destroy;
  begin
    if Clipboard<>nil then dispose(Clipboard,destroy);
    clearUndoLists;
    wireGraph.destroy;
  end;

PROCEDURE T_uiAdapter.clearUndoLists;
  VAR i:longint;
  begin
    for i:=0 to length(undoList)-1 do dispose(undoList[i],destroy);
    setLength(undoList,0);
    for i:=0 to length(redoList)-1 do dispose(redoList[i],destroy);
    setLength(redoList,0);
  end;

PROCEDURE T_uiAdapter.newBoardAttached(CONST board: P_circuitBoard);
  begin
    hideAllPeekPanels;
    assert(currentBoard=nil);
    currentBoard:=board;
    clearUndoLists;

    if (Clipboard<>nil) and (Clipboard^.usesBoard(board,true)) then begin
      dispose(Clipboard,destroy);
      Clipboard:=nil;
    end;
    recenter;
  end;

PROCEDURE T_uiAdapter.addPeekPanel(CONST panel: TPanel; CONST label_: TLabel);
  VAR k:longint;
  begin
    k:=length(peekPanels);
    setLength(peekPanels,k+1);
    peekPanels[k].panel:=panel;
    peekPanels[k].peekLabel:=label_;
    label_.OnClick:=@peekLabelClick;
    label_.Tag:=k;
    panel.visible:=false;
  end;

PROCEDURE T_uiAdapter.gateDeleted(CONST gate: P_visualGate);
  VAR k:longint;
  begin
    for k:=0 to length(peekPanels)-1 do if peekPanels[k].panel.visible and (peekPanels[k].connector.gate=gate) then peekPanels[k].panel.visible:=false;
  end;

PROCEDURE T_uiAdapter.simStepDone;
  VAR k:longint;
      upperleftCorner:T_point;
  begin
    for k:=0 to length(peekPanels)-1 do with peekPanels[k] do if panel.visible then begin
      upperleftCorner:=connector.gate^.getOutputPositionInGridSize(connector.index)*zoom;
      panel.Left:=upperleftCorner[0];
      panel.top :=upperleftCorner[1];
      peekLabel.caption:=connector.gate^.getOuputDescription(connector.index);
    end;
  end;

PROCEDURE T_uiAdapter.startPeeking(CONST connector: T_visualGateConnector);
  VAR k: integer;
  begin
    for k:=0 to length(peekPanels)-1 do
    if (peekPanels[k].connector.gate=connector.gate) and
       (peekPanels[k].connector.index=connector.index) and
       (peekPanels[k].panel.visible) then exit;
    k:=0;
    while (k<length(peekPanels)) and (peekPanels[k].panel.visible) do inc(k);
    if k>=length(peekPanels) then exit;
    peekPanels[k].panel.visible:=true;
    peekPanels[k].connector:=connector;
    simStepDone;
  end;

PROCEDURE T_uiAdapter.hideAllPeekPanels;
  VAR i:longint;
  begin
    for i:=0 to length(peekPanels)-1 do peekPanels[i].panel.visible:=false;
  end;

PROCEDURE T_uiAdapter.repaint;
  begin
    if currentBoard<>nil then begin
      fixWireImageSize;
      currentBoard^.repaint;
      simStepDone;
    end;
  end;

PROCEDURE T_uiAdapter.setZoom(CONST newZoom: longint);
  begin
    if zoom=newZoom then exit;
    zoom:=newZoom;
    repaint;
  end;

FUNCTION T_uiAdapter.positionForNextGate(CONST gateExtend:T_point):T_point;
  VAR srMin,
      srMax:T_point;
  begin
    srMin:=pointOf(container.HorzScrollBar.position,
                   container.VertScrollBar.position);
    srMax:=pointOf((srMin[0]+container.width ) div zoom,
                   (srMin[1]+container.height) div zoom);
    srMin:=pointOf(srMin[0] div zoom,srMin[1] div zoom);
    if length(currentBoard^.gates)=0
    then result:=pointOf((srMin[0]+srMax[0]-gateExtend[0]) div 2,
                         (srMin[1]+srMax[1]-gateExtend[1]) div 2)
    else begin
      if length(currentBoard^.gates)=1
      then begin
        result:=currentBoard^.gates[length(currentBoard^.gates)-1]^.origin;
        result[1]+=currentBoard^.gates[length(currentBoard^.gates)-1]^.size[1];
      end else
        result:=currentBoard^.gates[length(currentBoard^.gates)-1]^.origin*2-
                currentBoard^.gates[length(currentBoard^.gates)-2]^.origin;
      if (result[0]<srMin[0]) or (result[0]+gateExtend[0]>srMax[0]) or
         (result[1]<srMin[1]) or (result[1]+gateExtend[1]>srMax[1])
      then result:=pointOf((srMin[0]+srMax[0]-gateExtend[0]) div 2,
                           (srMin[1]+srMax[1]-gateExtend[1]) div 2);

    end;
  end;

PROCEDURE T_uiAdapter.recenter;
  VAR size, origin,center: T_point;
  begin
    if not(container.HandleAllocated) then exit;
    if (currentBoard=nil) or (length(currentBoard^.gates)=0) then begin
      size:=ZERO_POINT;
      origin:=pointOf(5,5);
    end else currentBoard^.getBoardExtend(origin,size);

    center:=pointOf(origin[0]+size[0] shr 1,
                    origin[1]+size[1] shr 1)*zoom;
    origin:=center-pointOf(container.width shr 1,container.height shr 1);
    if origin[0]<0 then origin[0]:=0; if origin[0]>=container.HorzScrollBar.range then origin[0]:=container.HorzScrollBar.range-1;
    if origin[1]<0 then origin[1]:=0; if origin[1]>=container.VertScrollBar.range then origin[1]:=container.VertScrollBar.range-1;

    container.HorzScrollBar.position:=origin[0];
    container.VertScrollBar.position:=origin[1];
  end;

PROCEDURE T_uiAdapter.fixWireImageSize;
  VAR width :longint=0;
      height:longint=0;
      p:T_point;
      i,j:longint;
      gate:P_visualGate;
  begin
    for gate in currentBoard^.gates do begin
      p:=gate^.origin+gate^.size;
      if p[0]>width  then width:=p[0];
      if p[1]>height then height:=p[1];
    end;
    for i:=0 to length(currentBoard^.logicWires)-1 do
    for j:=0 to length(currentBoard^.logicWires[i].wires)-1 do
    for p in currentBoard^.logicWires[i].wires[j].visual do begin
      if p[0]>width  then width :=p[0];
      if p[1]>height then height:=p[1];
    end;
    width +=1; width *=zoom; width +=max(1,round(zoom*0.15));
    height+=1; height*=zoom; height+=max(1,round(zoom*0.15));
    if (width<>wireImage.width) or (height<>wireImage.height) then begin
      wireImage.SetInitialBounds(0,0,width,height);
      wireImage.SetBounds(0,0,width,height);
      wireImage.picture.Bitmap.Canvas.Brush.color:=BackgroundColor;
      wireImage.picture.Bitmap.setSize(width,height);
      wireImage.picture.Bitmap.Canvas.clear;
    end;
  end;

PROCEDURE T_uiAdapter.saveStateToUndoList;
  VAR k:longint;
  begin
    if currentBoard=nil then exit;
    k:=length(undoList);
    setLength(undoList,k+1);
    undoList[k]:=currentBoard^.clone(true);

    for k:=0 to length(redoList)-1 do dispose(redoList[k],destroy);
    setLength(redoList,0);

    if length(undoList)>MAX_UNDO then begin
      dispose(undoList[0],destroy);
      for k:=0 to length(undoList)-2 do undoList[k]:=undoList[k+1];
      setLength(undoList,length(undoList)-1);
    end;
  end;

PROCEDURE T_uiAdapter.performUndo;
  VAR k:longint;
  begin
    if currentBoard=nil then exit;
    //TODO: Reassign peek panels?
    hideAllPeekPanels;
    if length(undoList)=0 then exit;

    k:=length(redoList);
    setLength(redoList,k+1);
    redoList[k]:=currentBoard^.clone(true);

    k:=length(undoList)-1;
    currentBoard^.pasteFrom(undoList[k],true);
    dispose(undoList[k],destroy);
    setLength(undoList,k);

    repaint;
    anyChangeCallback(false);
  end;

PROCEDURE T_uiAdapter.performRedo;
  VAR k:longint;
  begin
    if currentBoard=nil then exit;
    //TODO: Reassign peek panels?
    hideAllPeekPanels;
    if length(redoList)=0 then exit;

    k:=length(undoList);
    setLength(undoList,k+1);
    undoList[k]:=currentBoard^.clone(true);

    k:=length(redoList)-1;
    currentBoard^.pasteFrom(redoList[k],true);
    dispose(redoList[k],destroy);
    setLength(redoList,k);

    repaint;
    anyChangeCallback(false);
  end;

PROCEDURE T_uiAdapter.fixAfterGatePropertiesEdited(CONST gate:P_abstractGate);
  VAR relatedVisual:P_visualGate=nil;
      vGate:P_visualGate;
      i:longint;
  PROCEDURE fixIoOrder;
    VAR idx:array of longint;
        iUpd   :longint=-1;
        iSwitch:longint=-1;
        kSwitch:longint=-1;
        tmp:P_visualGate;
        i:longint;
    begin
      // 0 1 3 3 4 => 0 1 2 3 4
      //     ^            ' ^
      // 0 1 7 3 4 => 0 1 2 3 4
      //     ^            '   ^
      // 0 1 0 3 4 => 0 1 2 3 4
      //     ^        ^   '
      setLength(idx,0);
      kSwitch:=P_inputGate(gate)^.ioIndex;

      for i:=0 to length(currentBoard^.gates)-1 do if (currentBoard^.gates[i]^.getBehavior^.gateType=gate^.gateType) then begin
        setLength(idx,length(idx)+1);
        idx[length(idx)-1]:=i;
        if currentBoard^.gates[i]=relatedVisual then begin
          iUpd:=i;
        end;
        if length(idx)-1=kSwitch then iSwitch:=i;
      end;

      if iSwitch<0 then begin
        iSwitch:=idx[length(idx)-1];
        kSwitch:=    length(idx)-1;
      end;

      if (iSwitch<>iUpd) then begin
        assert(currentBoard^.gates[iSwitch]^.behavior^.gateType=gate^.gateType);
        assert(currentBoard^.gates[iUpd]^.behavior^.gateType=gate^.gateType);
        tmp                         :=currentBoard^.gates[iSwitch];
        currentBoard^.gates[iSwitch]:=currentBoard^.gates[iUpd];
        currentBoard^.gates[iUpd   ]:=tmp;
      end;
      currentBoard^.enumerateIo;
    end;

  begin
    if currentBoard=nil then exit;
    for vGate in currentBoard^.gates do if (relatedVisual=nil) and (vGate^.behavior=gate) then relatedVisual:=vGate;
    if relatedVisual=nil then exit;

    case gate^.gateType of
      gt_input,gt_output: begin
        fixIoOrder;
        for vGate in currentBoard^.gates do if vGate^.behavior^.gateType=gate^.gateType then vGate^.forcedFullRepaint;
      end
      else relatedVisual^.forcedFullRepaint;
    end;

    for i:=0 to length(peekPanels)-1 do
    if  peekPanels[i].panel.visible and
       (peekPanels[i].connector.gate=relatedVisual) and
       (peekPanels[i].connector.index>gate^.numberOfOutputs) then peekPanels[i].panel.visible:=false;
    currentBoard^.deleteInvalidWires;
  end;

PROCEDURE T_uiAdapter.initWireGraph(CONST start: T_visualGateConnector; CONST includeWires: boolean);
  VAR gate:P_visualGate;
      x,y,i,j:longint;
  begin
    if currentBoard=nil then exit;
    wireGraph.initDirections;
    for gate in currentBoard^.gates do begin
      //Points within the gate cannot be reached
      for x:=gate^.origin[0] to gate^.origin[0]+gate^.size[0] do
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do
      wireGraph.dropNode(pointOf(x,y));
      //Input connections:
      for i:=0 to gate^.numberOfInputs-1
      do wireGraph.addUnidirectionalEdge(gate^.getInputPositionInGridSize(i)+wd_left,wd_right);
      //Output connections:
      for i:=0 to gate^.numberOfOutputs-1
      do wireGraph.addUnidirectionalEdge(gate^.getOutputPositionInGridSize(i),wd_right);
      //No diagonals right left and right of the gate (to prevent blocking of I/O)
      x:=gate^.origin[0];
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do wireGraph.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
      x:=gate^.origin[1]+1;
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do wireGraph.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
    end;
    if includeWires then
    for i:=0 to length(currentBoard^.logicWires)-1 do
    if (currentBoard^.logicWires[i].source<>start) then with currentBoard^.logicWires[i] do
      for j:=0 to length(wires)-1 do wireGraph.dropWire(wires[j].visual);
  end;

FUNCTION T_uiAdapter.findWirePath(CONST start: T_visualGateConnector; CONST endPoint: T_point): T_wirePath;
  begin
    initWireGraph(start,true);
    if wireGraph.anyEdgeLeadsTo(endPoint)
    then result:=wireGraph.findPath(start.gate^.getOutputPositionInGridSize(start.index),endPoint)
    else setLength(result,0);
  end;

PROCEDURE T_uiAdapter.anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    incompleteWire.dragging:=false;
    drawAllWires;
  end;

PROCEDURE T_uiAdapter.drawAllWires;
  PROCEDURE drawWires(CONST index:longint; CONST foreground:boolean);
    VAR j,k:longint;
    begin for j:=0 to length(currentBoard^.logicWires[index].wires)-1 do with currentBoard^.logicWires[index].wires[j] do if length(visual)>1 then begin
      if foreground then begin
        if currentBoard^.logicWires[index].wires[j].marked
        then wireImage.Canvas.Pen.color:=clYellow
        else wireImage.Canvas.Pen.color:=clBlack;
      end;
      wireImage.Canvas.MoveTo(visual[0,0]*zoom,visual[0,1]*zoom);
      for k:=1 to length(visual)-1 do
      wireImage.Canvas.LineTo(visual[k,0]*zoom,visual[k,1]*zoom);
    end; end;

  VAR i:longint;
      wireWidth,gapWidth:longint;
  begin
    if wireImage=nil then exit;
    with wireImage.Canvas do begin
      Brush.color:=BackgroundColor;
      clear;
      for i:=0 to length(currentBoard^.logicWires)-1 do begin
        case currentBoard^.logicWires[i].width of
          0..1: begin
            wireWidth:=max(1,round(zoom*0.08));
            gapWidth :=max(1,round(zoom*0.38));
          end;
          4..7: begin
            wireWidth:=max(1,round(zoom*0.15));
            gapWidth :=max(1,round(zoom*0.45));
          end;
          8..15: begin
            wireWidth:=max(1,round(zoom*0.25));
            gapWidth :=max(1,round(zoom*0.55));
          end;
          else begin
            wireWidth:=max(1,round(zoom*0.35));
            gapWidth :=max(1,round(zoom*0.65));
          end;
        end;
        Pen.color:=BackgroundColor; Pen.width:=gapWidth;
        drawWires(i,false);

        Pen.width:=wireWidth;
        drawWires(i,true);
      end;
    end;
  end;

PROCEDURE T_uiAdapter.finishWireDrag(CONST targetPoint: T_point; CONST previewDuringDrag: boolean);

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
      with wireImage.Canvas do begin
        Pen.color:=clRed;
        Pen.width:=max(1,round(zoom*0.15));
        MoveTo(wire[0,0]*zoom,wire[0,1]*zoom);
        for i:=1 to length(wire)-1 do LineTo(wire[i,0]*zoom,wire[i,1]*zoom);
      end;
    end;

  PROCEDURE cleanup;
    begin
      drawAllWires;
      incompleteWire.dragging:=false;
    end;

  VAR j:longint;
      gate:P_visualGate;
      connector:T_visualGateConnector;

      distanceToConnector:double=infinity;
      newDistance:double;
  begin
    if not(incompleteWire.dragging) then exit;
    connector.gate:=nil;
    for gate in currentBoard^.gates do
    for j:=0 to gate^.numberOfInputs-1 do begin
      newDistance:=euklideanDistance(gate^.getInputPositionInGridSize(j)*zoom,targetPoint);
      if (newDistance<distanceToConnector) and
         (gate^.behavior^.inputWidth(j)=incompleteWire.width) and
         not(currentBoard^.isInputConnected(gate,j)) then begin
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
    saveStateToUndoList;
    currentBoard^.VERSION:=random(maxLongint);

    currentBoard^.addWire(incompleteWire.source,connector);
    currentBoard^.rewire(false);
    anyChangeCallback(false);
    cleanup;
    repaint;
  end;

PROCEDURE T_uiAdapter.WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  FUNCTION wireHitsPoint(CONST p:T_point; CONST wire:T_wirePath):boolean;
    VAR i:longint;
        a,b:T_point;
        t:double;
    begin
      for i:=0 to length(wire)-2 do begin
        a:=wire[i];
        b:=wire[i+1];
        if a<>b then begin

          //distance(t)=(a-p)+(b-a)*t
          //minimize distance(t) ->
          // 0 = d ((a-p)+(b-a)*t)² / dt
          //   = d ((a-p)²+2*(a-p)*(b-a)*t+  (b-a)²*t²) / dt
          //   =           2*(a-p)*(b-a)  +2*(b-a)²*t
          // -(a-p)*(b-a)/(b-a)² =  t
          // accept only if 0<=t<=1 !!
          t:=-((a-p)*(b-a))/((b-a)*(b-a));
          if (0<t) and (t<1) and (sqr((a[0]-p[0])+(b[0]-a[0])*t)
                                 +sqr((a[1]-p[1])+(b[1]-a[1])*t)<1) then exit(true);

        end;
      end;
      result:=false;
    end;

  VAR p:T_point;
      i,j:longint;
      anyChange:boolean=false;
  begin
    if (button=mbLeft) and (ssCtrl in Shift) then begin
      p:=pointOf(round(x / zoom),round(y / zoom));
      for i:=0 to length(currentBoard^.logicWires         )-1 do
      for j:=0 to length(currentBoard^.logicWires[i].wires)-1 do
      with currentBoard^.logicWires[i].wires[j] do
      if wireHitsPoint(p,visual)
      then begin
        marked:=not(marked);
        anyChange:=true;
      end;
      if anyChange then drawAllWires;
    end else if (button=mbLeft) then begin
      selectionFrame.visible:=true;
      selectionFrame.top :=y; selectionFrame.height:=1;
      selectionFrame.Left:=X; selectionFrame.width :=1;
      selectionStart:=pointOf(x,y);
    end;
  end;

PROCEDURE T_uiAdapter.WireImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR low,size:T_point;
  begin
    if selectionFrame.visible then begin
      low :=pointOf(min(x,selectionStart[0]),
                    min(y,selectionStart[1]));
      size:=pointOf(max(x,selectionStart[0]),
                    max(y,selectionStart[1]))-low;

      selectionFrame.Left  :=low[0];
      selectionFrame.top   :=low[1];
      selectionFrame.width :=size[0];
      selectionFrame.height:=size[1];
    end;
  end;

PROCEDURE T_uiAdapter.WireImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR selStart,selEnd:T_point;
      gate:P_visualGate;
      i,j:longint;
      p:T_point;
      extendSelection:boolean;
  begin
    if selectionFrame.visible then begin
      extendSelection:=ssShift in Shift;

      selStart:=pointOf(floor(min(x,selectionStart[0])/zoom),
                        floor(min(y,selectionStart[1])/zoom));
      selEnd  :=pointOf(ceil (max(x,selectionStart[0])/zoom),
                        ceil (max(y,selectionStart[1])/zoom));
      selectionFrame.visible:=false;

      for gate in currentBoard^.gates do gate^.setMarked((extendSelection and gate^.marked_) or
                                         gate^.isCompletelyInsideRect(selStart,selEnd));
      for i:=0 to length(currentBoard^.logicWires)-1 do
      for j:=0 to length(currentBoard^.logicWires[i].wires)-1 do
      with currentBoard^.logicWires[i].wires[j] do if not(extendSelection and currentBoard^.logicWires[i].wires[j].marked) then begin
        marked:=length(visual)>0;
        for p in visual do marked:=marked and (p[0]>=selStart[0]) and (p[0]<=selEnd[0]) and (p[1]>=selStart[1]) and (p[1]<=selEnd[1]);
      end;
      repaint;
    end;
  end;

PROCEDURE T_uiAdapter.pasteFromClipboard;
  begin
    saveStateToUndoList;
    currentBoard^.pasteFrom(Clipboard);
  end;

{$endif}
