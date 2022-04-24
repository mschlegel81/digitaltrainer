{$ifdef includeInterface}
P_paletteTreeData=^T_paletteTreeData;
T_paletteTreeData=object
  private
    isLeaf:boolean;
    name,description:string;
    index:longint;
    children: array of P_paletteTreeData;
    PROCEDURE addBoard(CONST board:P_circuitBoard);
  public
    CONSTRUCTOR create(CONST name_:string; CONST index_:longint);
    CONSTRUCTOR createLeaf(CONST board:P_circuitBoard);
    DESTRUCTOR destroy;
    PROCEDURE updateName(CONST newName:string; CONST workspace:P_workspace);
    PROPERTY isBoard:boolean read isLeaf;
end;

T_workspace=object(T_serializable)
  private
    paletteEntries:array of P_circuitBoard;
    paletteCategories:array of string;

    //For GUI-Interaction:
    currentPalette:array of P_paletteTreeData;
    categoryRootItem:TMenuItem;
    paletteView     :TTreeView;

    PROCEDURE rebuildTree;
    PROCEDURE rebuildMenu;
  public
    currentBoard  :P_circuitBoard;
    draftEntries  :array of P_circuitBoard;
    CONSTRUCTOR create(CONST categoryRootItem_:TMenuItem; CONST paletteView_:TTreeView);
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType);
    PROCEDURE addCustomGate(CONST index:longint);

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    PROCEDURE addCurrentBoardToPalette;
    PROCEDURE removePaletteEntry(CONST index:longint);
    PROCEDURE editPaletteEntry(CONST index:longint; CONST resetPaletteIndex:boolean);

    PROCEDURE addCurrentBoardToDrafts(CONST indexToOverwrite:longint=maxLongint);
    PROCEDURE removeDraftEntry(CONST index:longint);
    PROCEDURE editDraftEntry  (CONST index:longint);

    PROCEDURE setBoardCategory(CONST paletteIndex,categoryIndex:longint);
    PROCEDURE deleteCategory(CONST categoryIndex:longint);
    PROCEDURE addCategory;
end;

{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_paletteTreeData.create(CONST name_:string; CONST index_:longint);
  begin
    name:=name_;
    index:=index_;
    description:='';
    isLeaf:=false;
    setLength(children,0);
  end;

CONSTRUCTOR T_paletteTreeData.createLeaf(CONST board:P_circuitBoard);
  begin
    name:=board^.name;
    index:=board^.paletteIndex;
    description:=board^.description;
    isLeaf:=true;
    setLength(children,0);
  end;

PROCEDURE T_paletteTreeData.addBoard(CONST board:P_circuitBoard);
  VAR k:longint;
  begin
    k:=length(children);
    new(children[k],createLeaf(board));
  end;

DESTRUCTOR T_paletteTreeData.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(children)-1 do dispose(children[i],destroy);
    setLength(children,0);
  end;

PROCEDURE T_paletteTreeData.updateName(CONST newName:string; CONST workspace:P_workspace);
  VAR i:longint;
  begin
    if isLeaf then begin
      for i:=0 to length(workspace^.paletteEntries)-1 do if (i<>index) and (workspace^.paletteEntries[i]^.name=newName) then exit;
      workspace^.paletteEntries[index]^.name:=newName;
      name:=newName;
    end else begin
      for i:=0 to length(workspace^.paletteCategories)-1 do if (i<>index) and (workspace^.paletteCategories[i]=newName) then exit;
      workspace^.paletteCategories[index]:=newName;
      name:=newName;
    end;
  end;

CONSTRUCTOR T_workspace.create(CONST categoryRootItem_:TMenuItem; CONST paletteView_:TTreeView);
  begin
    setLength(paletteEntries,0);
    setLength(draftEntries,0);
    setLength(currentPalette,0);
    categoryRootItem:=categoryRootItem_;
    paletteView:=paletteView_;
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
      currentBoard^.saveStateToUndoList;
      case gateToAdd^.gateType of
        gt_input:  P_inputGate (gateToAdd)^.ioIndex:=numberOf(gt_input);
        gt_output: P_outputGate(gateToAdd)^.ioIndex:=numberOf(gt_output);
      end;

      if length(currentBoard^.gates)=0
      then p:=pointOf(5,5)
      else p:=currentBoard^.gates[length(currentBoard^.gates)-1]^.nextGateAfter;

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
      currentBoard^.saveStateToUndoList;

      if length(currentBoard^.gates)=0
      then p:=pointOf(5,5)
      else p:=currentBoard^.gates[length(currentBoard^.gates)-1]^.nextGateAfter;

      visual:=currentBoard^.wrapGate(p,gateToAdd);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=7;
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
      setLength(paletteCategories,count);
      for i:=0 to length(paletteCategories)-1 do paletteCategories[i]:=stream.readShortString;
    end else exit(false);
    count:=stream.readNaturalNumber;
    result:=true;
    if (count<maxLongint) then begin
      for i:=0 to longint(count)-1 do begin
        new(board,create);
        if (board^.loadFromStream(@self,stream)) and (board^.categoryIndex>=-1) and (board^.categoryIndex<length(paletteCategories)) then begin
          setLength(paletteEntries,i+1);
          paletteEntries[i]:=board;
          board^.paletteIndex:=i;
        end else begin
          dispose(board,destroy);
          exit(false);
        end;
      end;
    end else exit(false);
    count:=stream.readNaturalNumber;
    result:=true;
    if (count<maxLongint) then begin
      for i:=0 to longint(count)-1 do begin
        new(board,create);
        if board^.loadFromStream(@self,stream) then begin
          setLength(draftEntries,i+1);
          draftEntries[i]:=board;
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
    stream.writeNaturalNumber(length(paletteCategories));
    for i:=0 to length(paletteCategories)-1 do stream.writeShortString(paletteCategories[i]);
    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do paletteEntries[i]^.saveToStream(stream);
    stream.writeNaturalNumber(length(draftEntries));
    for i:=0 to length(draftEntries)-1 do draftEntries[i]^.saveToStream(stream);
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

    if doReplace=mrYes then begin
      i:=currentBoard^.paletteIndex;
      paletteEntries[i]^.clear;
      paletteEntries[i]^.pasteFrom(currentBoard);
      paletteEntries[i]^.paletteIndex:=i;
      paletteEntries[i]^.name        :=currentBoard^.name;
      paletteEntries[i]^.description :=currentBoard^.description;
      currentBoard^.clear;
    end else begin
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

      i:=length(paletteEntries);
      setLength(paletteEntries,i+1);
      paletteEntries[i]:=newPaletteEntry;
      newPaletteEntry^.paletteIndex:=i;
    end;
  end;

PROCEDURE T_workspace.addCurrentBoardToDrafts(CONST indexToOverwrite:longint=maxLongint);
  VAR i:longint;
      newDraft: P_circuitBoard;
  begin
    if (indexToOverwrite<0) or (indexToOverwrite>length(draftEntries))
    then begin
      i:=length(draftEntries);
      setLength(draftEntries,i+1);
    end
    else begin
      i:=indexToOverwrite;
      dispose(draftEntries[i],destroy);
    end;

    if currentBoard^.name=defaultBoardCaption then currentBoard^.name:='Unbenannter Entwurf vom '+FormatDateTime('dd.mm.yyyy - hh:nn',now);

    newDraft:=currentBoard;
    currentBoard   :=nil;
    new(currentBoard,create);
    currentBoard^.attachGUI(
      newDraft^.GUI.zoom,
      newDraft^.GUI.container,
      newDraft^.GUI.wireImage,
      newDraft^.GUI.gateContextMenu,
      newDraft^.GUI.anyChangeCallback);
    newDraft^.detachGUI;
    draftEntries[i]:=newDraft;
  end;

PROCEDURE T_workspace.removePaletteEntry(CONST index:longint);
  VAR j:longint;
      usedBy:ansistring='';
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    if currentBoard^.usesBoard(paletteEntries[index]) then usedBy:='Aktuelle Schaltung';

    for j:=index+1 to length(paletteEntries)-1 do
    if paletteEntries[j]^.usesBoard(paletteEntries[index]) then begin
      if usedBy='' then usedBy:=     paletteEntries[j]^.name
                   else usedBy+=', '+paletteEntries[j]^.name;
    end;
    if usedBy<>'' then begin
      ShowMessage('Der Eintrag kann nicht gelöscht werden weil er verwendet wird von: '+usedBy);
      exit;
    end;

    dispose(paletteEntries[index],destroy);
    for j:=index to length(paletteEntries)-2 do begin
      paletteEntries[j]:=paletteEntries[j+1];
      paletteEntries[j]^.paletteIndex:=j;
    end;
    setLength(paletteEntries,length(paletteEntries)-1);
  end;

PROCEDURE T_workspace.removeDraftEntry(CONST index:longint);
  VAR j:longint;
  begin
    if (index<0) or (index>length(draftEntries)) then exit;
    dispose(draftEntries[index],destroy);
    for j:=index to length(draftEntries)-2 do draftEntries[j]:=draftEntries[j+1];
    setLength(draftEntries,length(draftEntries)-1);
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST index:longint; CONST resetPaletteIndex:boolean);
  VAR previous:P_circuitBoard;
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=paletteEntries[index]^.clone(false);
    if resetPaletteIndex then currentBoard^.paletteIndex:=-1;
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

PROCEDURE T_workspace.editDraftEntry  (CONST index:longint);
  VAR previous:P_circuitBoard;
      j:longint;
  begin
    if (index<0) or (index>length(draftEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=draftEntries[index];
    currentBoard^.attachGUI(
      previous^.GUI.zoom,
      previous^.GUI.container,
      previous^.GUI.wireImage,
      previous^.GUI.gateContextMenu,
      previous^.GUI.anyChangeCallback);
    currentBoard^.rewire;
    dispose(previous,destroy);
    for j:=index to length(draftEntries)-2 do draftEntries[j]:=draftEntries[j+1];
    setLength(draftEntries,length(draftEntries)-1);
    currentBoard^.Repaint;
  end;

PROCEDURE T_workspace.rebuildTree;
  begin
    //paletteView.drop;
    //TODO: Stub
  end;

PROCEDURE T_workspace.rebuildMenu;
  begin
    //TODO: Stub
  end;

PROCEDURE T_workspace.setBoardCategory(CONST paletteIndex,categoryIndex:longint);
  begin
    if (paletteIndex >= 0) and (paletteIndex <length(paletteEntries)) and
       (categoryIndex>=-1) and (categoryIndex<length(paletteCategories)) then begin
      if paletteEntries[paletteIndex]^.categoryIndex=categoryIndex then exit;
      paletteEntries[paletteIndex]^.categoryIndex:=categoryIndex;
      rebuildTree;
    end;
  end;

PROCEDURE T_workspace.deleteCategory(CONST categoryIndex:longint);
  begin
    //TODO: Stub
  end;

PROCEDURE T_workspace.addCategory;
  begin
    //TODO: Stub
  end;
{$endif}
