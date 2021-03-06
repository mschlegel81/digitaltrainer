{$ifdef includeInterface}
P_paletteTreeData=^T_paletteTreeData;
T_paletteTreeData=object
  private
    board:P_circuitBoard;
    catName:string;
    catIndex:longint;
    children: array of P_paletteTreeData;
    FUNCTION addBoard(CONST board_:P_circuitBoard):P_paletteTreeData;
  public
    CONSTRUCTOR create(CONST name_:string; CONST index_:longint);
    CONSTRUCTOR createLeaf(CONST board_:P_circuitBoard);
    DESTRUCTOR destroy;
    PROCEDURE updateName(CONST newName:string; CONST workspace:P_workspace);
    FUNCTION getName:string;
    PROPERTY getBoard:P_circuitBoard read board;
end;

P_importItem=^T_importItem;
T_importItem=object
  private
    board:P_circuitBoard;
    categoryName:string;
    draft:boolean;
    equivalentBoard:P_circuitBoard;
    dependsOn:array of P_importItem;
    isUsedBy:array of P_importItem;
    Selected:boolean;
    PROCEDURE setSelected(CONST value:boolean);
  public
    CONSTRUCTOR create(CONST board_:P_circuitBoard; CONST categoryName_:string; CONST draft_:boolean=false);
    DESTRUCTOR destroy;
    PROPERTY getBoard:P_circuitBoard read board;
    PROCEDURE addDependency(CONST d:P_importItem);
    PROPERTY selectedForImport:boolean read Selected write setSelected;
    FUNCTION alreadyExists:boolean;
    FUNCTION itemCaption:string;
end;

T_importItems=object
  private
    workspace,importInto:P_workspace;
  public
    items:array of P_importItem;
    CONSTRUCTOR create(CONST workspace_,importInto_:P_workspace);
    DESTRUCTOR destroy;
    PROPERTY getWorkspace:P_workspace read workspace;
    PROCEDURE executeImport;
end;

T_workspace=object(T_serializable)
  private
    paletteEntries:array of P_circuitBoard;
    paletteCategories:array of string;
    currentBoard  :P_circuitBoard;
    draftEntries  :array of P_circuitBoard;

    //For GUI-Interaction:
    currentPalette:array of P_paletteTreeData;
    categoryRootItem:TMenuItem;
    paletteView     :TTreeView;

    PROCEDURE rebuildTree;
    PROCEDURE rebuildMenu;
    PROCEDURE addBoardToTree(CONST board:P_circuitBoard);
    PROCEDURE setBoardCategoryEvent(Sender: TObject);
    PROCEDURE paletteViewEdited(Sender: TObject; node: TTreeNode; VAR S: ansistring);
    PROCEDURE removePaletteEntry(CONST index:longint);
    PROCEDURE setBoardCategory(CONST paletteIndex,categoryIndex:longint);
    PROCEDURE deleteCategory(CONST categoryIndex:longint);
    PROCEDURE addDraft       (CONST board:P_circuitBoard);
    FUNCTION addPaletteEntry(CONST board:P_circuitBoard):P_circuitBoard;
  public
    CONSTRUCTOR create(CONST categoryRootItem_:TMenuItem; CONST paletteView_:TTreeView);
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType);
    PROCEDURE addSelectedCustomGate;
    PROCEDURE clearCurrentBoard;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    PROCEDURE addCurrentBoardToPalette;
    PROCEDURE deleteSelectedTreeItem;
    PROCEDURE editSelectedTreeItem(CONST resetPaletteIndex:boolean);
    PROCEDURE editPaletteEntry(CONST index:longint; CONST resetPaletteIndex:boolean);

    PROCEDURE addCurrentBoardToDrafts(CONST indexToOverwrite:longint=maxLongint);
    PROCEDURE removeDraftEntry(CONST index:longint);
    PROCEDURE editDraftEntry  (CONST index:longint);
    FUNCTION  numberOfDraftEntries:longint;
    FUNCTION  draftEntryName(CONST index:longint):string;

    FUNCTION addCategory(CONST caption:string=''):longint;

    PROPERTY getCurrentBoard:P_circuitBoard read currentBoard;
end;

{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_paletteTreeData.create(CONST name_:string; CONST index_:longint);
  begin
    board:=nil;
    catName:=name_;
    catIndex:=index_;
    setLength(children,0);
  end;

CONSTRUCTOR T_paletteTreeData.createLeaf(CONST board_:P_circuitBoard);
  begin
    board:=board_;
    catName:='';
    catIndex:=-1;
    setLength(children,0);
  end;

FUNCTION T_paletteTreeData.addBoard(CONST board_:P_circuitBoard):P_paletteTreeData;
  VAR k:longint;
  begin
    if board<>nil then exit;
    new(result,createLeaf(board_));
    k:=length(children);
    setLength(children,k+1);
    children[k]:=result;
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
    if board<>nil then begin
      for i:=0 to length(workspace^.paletteEntries)-1 do if (workspace^.paletteEntries[i]<>board) and (workspace^.paletteEntries[i]^.name=newName) then exit;
      board^.name:=newName;
    end else begin
      for i:=0 to length(workspace^.paletteCategories)-1 do if (i<>catIndex) and (workspace^.paletteCategories[i]=newName) then exit;
      workspace^.paletteCategories[catIndex]:=newName;
      catName:=newName;
      workspace^.rebuildMenu;
    end;
    with workspace^.paletteView do
    for i:=0 to items.count-1 do if items[i].data=@self then items[i].text:=getName;
  end;

FUNCTION T_paletteTreeData.getName:string;
  begin
    if board=nil then result:=catName else result:=board^.name;
  end;

CONSTRUCTOR T_workspace.create(CONST categoryRootItem_:TMenuItem; CONST paletteView_:TTreeView);
  begin
    setLength(paletteEntries,0);
    setLength(draftEntries,0);
    setLength(currentPalette,0);
    categoryRootItem:=categoryRootItem_;
    paletteView:=paletteView_;
    new(currentBoard,create);
    rebuildMenu;
    rebuildTree;
  end;

DESTRUCTOR T_workspace.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do dispose(paletteEntries[i],destroy);
    for i:=0 to length(draftEntries)  -1 do dispose(draftEntries  [i],destroy);
    for i:=0 to length(currentPalette)-1 do dispose(currentPalette[i],destroy);
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
  begin
    gateToAdd:=newBaseGate(gateType);
    if gateToAdd<>nil then begin
      currentBoard^.GUI^.saveStateToUndoList;
      currentBoard^.VERSION:=random(maxLongint);
      case gateToAdd^.gateType of
        gt_input:  P_inputGate (gateToAdd)^.ioIndex:=numberOf(gt_input);
        gt_output: P_outputGate(gateToAdd)^.ioIndex:=numberOf(gt_output);
      end;
      visual:=currentBoard^.wrapGate(ZERO_POINT,gateToAdd);
      visual^.origin:=currentBoard^.GUI^.positionForNextGate(visual^.size);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

PROCEDURE T_workspace.addSelectedCustomGate;
  VAR node:TTreeNode;
      data:P_paletteTreeData;
      visual:P_visualGate;
      gateToAdd:P_customGate;
  begin
    node:=paletteView.Selected;
    if node=nil then exit;
    data:=P_paletteTreeData(node.data);
    if data^.board=nil then exit;
    currentBoard^.GUI^.saveStateToUndoList;
    currentBoard^.VERSION:=random(maxLongint);

    new(gateToAdd,createFromBoard(data^.board));
    visual:=currentBoard^.wrapGate(ZERO_POINT,gateToAdd);
    visual^.origin:=currentBoard^.GUI^.positionForNextGate(visual^.size);
    if not currentBoard^.positionNewGate(visual)
    then dispose(visual,destroy);
  end;

PROCEDURE T_workspace.clearCurrentBoard;
  begin
    if currentBoard^.GUI<>nil then currentBoard^.GUI^.hideAllPeekPanels;
    currentBoard^.clear;
    rebuildTree;
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

    rebuildMenu;
    rebuildTree;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
      swapTmp:P_circuitBoard;
      paletteEntryMoved:boolean=true;
  begin
    //Sorting helps preventing errors on restore.
    //The reason is that custom gates can only be constructed successfully when their prototype-board is already available.

    while paletteEntryMoved do begin
      paletteEntryMoved:=false;
      for i:=0 to length(paletteEntries)-1 do
      for j:=i+1 to length(paletteEntries)-1 do if paletteEntries[i]^.usesBoard(paletteEntries[j],true) then begin
        swapTmp          :=paletteEntries[i];
        paletteEntries[i]:=paletteEntries[j];
        paletteEntries[j]:=swapTmp;

        paletteEntries[i]^.paletteIndex:=i;
        paletteEntries[j]^.paletteIndex:=j;
        paletteEntryMoved:=true;
      end;
    end;

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
      //usedBy:string='';
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

    if (doReplace=mrNo) then begin
      for i:=0 to length(paletteEntries)-1 do if paletteEntries[i]^.name=currentBoard^.name
      then begin
        ShowMessage('Es gibt schon eine Schaltung mit diesem Namen in der Palette.');
        exit;
      end;
    end;

    if doReplace=mrYes then begin
      i:=currentBoard^.paletteIndex;
      paletteEntries[i]^.clear;
      paletteEntries[i]^.pasteFrom(currentBoard,true);
      clearCurrentBoard;
    end else begin
      newPaletteEntry:=currentBoard;
      currentBoard   :=nil;
      new(currentBoard,create);

      currentBoard^.attachGUI(newPaletteEntry^.detachGUI);

      i:=length(paletteEntries);
      setLength(paletteEntries,i+1);
      paletteEntries[i]:=newPaletteEntry;
      newPaletteEntry^.paletteIndex:=i;

      addBoardToTree(newPaletteEntry);
      rebuildTree;
    end;
    for i:=0 to length(paletteEntries)-1 do paletteEntries[i]^.deleteInvalidWires;
    for i:=0 to length(draftEntries  )-1 do draftEntries  [i]^.deleteInvalidWires;
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
    currentBoard^.paletteIndex:=-1;

    newDraft:=currentBoard;
    currentBoard   :=nil;
    new(currentBoard,create);
    currentBoard^.attachGUI(newDraft^.detachGUI);
    draftEntries[i]:=newDraft;
    rebuildTree;
  end;

PROCEDURE T_workspace.removePaletteEntry(CONST index:longint);
  VAR j:longint;
      usedBy:ansistring='';
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    if currentBoard^.usesBoard(paletteEntries[index]) then usedBy:='Aktuelle Schaltung';

    for j:=0 to length(paletteEntries)-1 do
    if (j<>index) and paletteEntries[j]^.usesBoard(paletteEntries[index]) then begin
      if usedBy='' then usedBy:=     paletteEntries[j]^.name
                   else usedBy+=', '+paletteEntries[j]^.name;
    end;
    for j:=0 to length(draftEntries)-1 do
    if draftEntries[j]^.usesBoard(paletteEntries[index]) then begin
      if usedBy='' then usedBy:=     draftEntries[j]^.name+'(Entwurf)'
                   else usedBy+=', '+draftEntries[j]^.name+'(Entwurf)';
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

    rebuildTree;
  end;

PROCEDURE T_workspace.deleteSelectedTreeItem;
  VAR node:TTreeNode;
      data:P_paletteTreeData;
  begin
    node:=paletteView.Selected;
    if node=nil then exit;
    data:=P_paletteTreeData(node.data);

    if data^.board=nil
    then deleteCategory(data^.catIndex)
    else removePaletteEntry(data^.board^.paletteIndex);
  end;

PROCEDURE T_workspace.removeDraftEntry(CONST index:longint);
  VAR j:longint;
  begin
    if (index<0) or (index>length(draftEntries)) then exit;
    dispose(draftEntries[index],destroy);
    for j:=index to length(draftEntries)-2 do draftEntries[j]:=draftEntries[j+1];
    setLength(draftEntries,length(draftEntries)-1);
  end;

PROCEDURE T_workspace.editSelectedTreeItem(CONST resetPaletteIndex:boolean);
  VAR node:TTreeNode;
      data:P_paletteTreeData;
  begin
    node:=paletteView.Selected;
    if node=nil then exit;
    data:=P_paletteTreeData(node.data);

    if data^.board=nil then exit;
    editPaletteEntry(data^.board^.paletteIndex,resetPaletteIndex);
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST index:longint; CONST resetPaletteIndex:boolean);
  VAR previous:P_circuitBoard;
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=paletteEntries[index]^.clone(false);
    currentBoard^.deleteInvalidWires;
    if resetPaletteIndex then begin
      currentBoard^.paletteIndex :=-1;
      currentBoard^.categoryIndex:=-1;
    end;
    currentBoard^.attachGUI(previous^.detachGUI);
    currentBoard^.rewire;
    dispose(previous,destroy);
    currentBoard^.repaint;
    rebuildTree;
  end;

PROCEDURE T_workspace.editDraftEntry  (CONST index:longint);
  VAR previous:P_circuitBoard;
  begin
    if (index<0) or (index>length(draftEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=draftEntries[index]^.clone(false);
    currentBoard^.deleteInvalidWires;
    currentBoard^.attachGUI(previous^.detachGUI);
    currentBoard^.rewire;
    dispose(previous,destroy);
    currentBoard^.repaint;
    rebuildTree;
  end;

FUNCTION T_workspace.numberOfDraftEntries:longint;
  begin
    result:=length(draftEntries);
  end;

FUNCTION T_workspace.draftEntryName(CONST index:longint):string;
  begin
    result:=draftEntries[index]^.name;
  end;

PROCEDURE T_workspace.addBoardToTree(CONST board:P_circuitBoard);
  VAR parentData,
      nodeData:P_paletteTreeData;
      parent,
      node:TTreeNode;
      i:longint;
  begin
    parent:=nil;
    parentData:=nil;
    if board^.categoryIndex<0 then begin
      new(nodeData,createLeaf(board));
      node:=paletteView.items.add(nil,nodeData^.getName);
      node.data:=nodeData;

      i:=length(currentPalette);
      setLength(currentPalette,i+1);
      currentPalette[i]:=nodeData;
    end else begin
      for i:=0 to paletteView.items.count-1 do
      if (paletteView.items[i].data<>nil) and
         (P_paletteTreeData(paletteView.items[i].data)^.board=nil) and
         (P_paletteTreeData(paletteView.items[i].data)^.catIndex=board^.categoryIndex) then begin
        parent:=paletteView.items[i];
        parentData:=P_paletteTreeData(paletteView.items[i].data);
      end;
      nodeData:=parentData^.addBoard(board);
      node:=paletteView.items.addChild(parent,nodeData^.getName);
      node.data:=nodeData;
    end;
  end;

PROCEDURE T_workspace.setBoardCategoryEvent(Sender: TObject);
  VAR board:P_circuitBoard;
  begin
    board:=P_paletteTreeData(paletteView.Selected.data)^.board;
    if board=nil then exit;
    setBoardCategory(board^.paletteIndex,TMenuItem(Sender).Tag);
  end;

PROCEDURE T_workspace.rebuildTree;
  VAR i:longint;
      nodeData:P_paletteTreeData;
      node: TTreeNode;
  begin
    if paletteView=nil then exit;
    for i:=0 to length(currentPalette)-1 do dispose(currentPalette[i],destroy);
    setLength(currentPalette,0);

    paletteView.items.clear;

    for i:=0 to length(paletteCategories)-1 do begin
      new(nodeData,create(paletteCategories[i],i));
      node:=paletteView.items.add(nil,nodeData^.getName);
      node.data:=nodeData;
      setLength(currentPalette,length(currentPalette)+1);
      currentPalette[length(currentPalette)-1]:=nodeData;
    end;

    for i:=0 to length(paletteEntries)-1 do
    if (paletteEntries[i]^.categoryIndex>=0) and
       not(paletteEntries[i]^.usesBoard(currentBoard,true))
    then addBoardToTree(paletteEntries[i]);

    paletteView.AlphaSort;
    for node in paletteView.items do if node.HasChildren then node.AlphaSort;

    for i:=0 to length(paletteEntries)-1 do
    if (paletteEntries[i]^.categoryIndex<0) and
       not(paletteEntries[i]^.usesBoard(currentBoard,true))
    then addBoardToTree(paletteEntries[i]);

    paletteView.OnEdited:=@paletteViewEdited;
  end;

PROCEDURE T_workspace.paletteViewEdited(Sender: TObject; node: TTreeNode; VAR S: ansistring);
  begin
    P_paletteTreeData(node.data)^.updateName(s,@self);
  end;

PROCEDURE T_workspace.rebuildMenu;
  TYPE T_sortableCat=record caption:string; index:longint; end;
  VAR i,j:longint;
      item:TMenuItem;
      sortable:array of T_sortableCat;
      tmp:T_sortableCat;
  begin
    if categoryRootItem=nil then exit;
    categoryRootItem.clear;
    item:=TMenuItem.create(categoryRootItem);
    item.caption:='<keine>';
    item.Tag:=-1;
    item.OnClick:=@setBoardCategoryEvent;
    categoryRootItem.add(item);

    setLength(sortable,length(paletteCategories));
    for i:=0 to length(paletteCategories)-1 do begin
      sortable[i].index  :=i;
      sortable[i].caption:=paletteCategories[i];
      for j:=0 to i-1 do if sortable[j].caption>sortable[i].caption then begin
        tmp        :=sortable[i];
        sortable[i]:=sortable[j];
        sortable[j]:=tmp;
      end;
    end;

    for tmp in sortable do with tmp do begin
      item:=TMenuItem.create(categoryRootItem);
      item.caption:=caption;
      item.Tag:=index;
      categoryRootItem.add(item);
      item.OnClick:=@setBoardCategoryEvent;
    end;

    //for i:=0 to length(paletteCategories)-1 do begin
    //  item:=TMenuItem.create(categoryRootItem);
    //  item.caption:=paletteCategories[i];
    //  item.Tag:=i;
    //  categoryRootItem.add(item);
    //  item.OnClick:=@setBoardCategoryEvent;
    //end;
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
  VAR i:longint;
  begin
    if (categoryIndex<0) or (categoryIndex>=length(paletteCategories)) then exit;

    //Remove category from all boards (set to -1 = "unassigned")
    for i:=0 to length(paletteEntries)-1 do begin
      if      paletteEntries[i]^.categoryIndex=categoryIndex then paletteEntries[i]^.categoryIndex:=-1
      else if paletteEntries[i]^.categoryIndex>categoryIndex then paletteEntries[i]^.categoryIndex-= 1;
    end;
    for i:=categoryIndex to length(paletteCategories)-2 do paletteCategories[i]:=paletteCategories[i+1];
    setLength(paletteCategories,length(paletteCategories)-1);

    rebuildTree;
    rebuildMenu;
  end;

FUNCTION T_workspace.addCategory(CONST caption:string=''):longint;
  VAR k:longint;
      nodeData:P_paletteTreeData;
      node: TTreeNode;
  begin
    k:=length(paletteCategories);
    setLength(paletteCategories,k+1);
    if caption=''
    then paletteCategories[k]:='<Unbenannte Kategorie '+intToStr(k+1)+'>'
    else paletteCategories[k]:=caption;
    result:=k;

    rebuildMenu;

    new(nodeData,create(paletteCategories[k],k));

    k:=length(currentPalette);
    setLength(currentPalette,k+1);
    currentPalette[k]:=nodeData;

    node:=paletteView.items.add(nil,nodeData^.getName);
    node.data:=nodeData;
  end;

CONSTRUCTOR T_importItems.create(CONST workspace_,importInto_:P_workspace);
  VAR i:longint;
      k:longint=0;
      board:P_circuitBoard;
      cat:string;
  begin
    workspace :=workspace_;
    importInto:=importInto_;
    setLength(items,length(workspace^.paletteEntries)+length(workspace^.draftEntries));
    for i:=0 to length(workspace^.paletteEntries)-1 do begin
      board:=workspace^.paletteEntries[i];
      if board^.categoryIndex<0
      then cat:=''
      else cat:=workspace^.paletteCategories[board^.categoryIndex];
      new(items[k],create(board,cat,false));
      inc(k);
    end;
    for i:=0 to length(workspace^.draftEntries)-1 do begin
      board:=workspace^.draftEntries[i];
      if board^.categoryIndex<0
      then cat:=''
      else cat:=workspace^.paletteCategories[board^.categoryIndex];
      new(items[k],create(board,cat,true));
      inc(k);
    end;

    for k:=0 to length(items)-1 do begin
      for i:=0 to length(workspace^.paletteEntries)-1 do
      if (i<>k) and items[k]^.board^.usesBoard(workspace^.paletteEntries[i])
      then items[k]^.addDependency(items[i]);

      for board in importInto^.paletteEntries do
      if (items[k]^.equivalentBoard=nil) and (items[k]^.board^.behaviorEquals(board))
      then items[k]^.equivalentBoard:=board;

      if items[k]^.draft then
      for board in importInto^.draftEntries do
      if (items[k]^.equivalentBoard=nil) and (items[k]^.board^.behaviorEquals(board))
      then items[k]^.equivalentBoard:=board;

      items[k]^.Selected:=items[k]^.equivalentBoard<>nil;
    end;
  end;

DESTRUCTOR T_importItems.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(items)-1 do dispose(items[i],destroy);
    setLength(items,0);
  end;

PROCEDURE T_importItems.executeImport;
  VAR newCategories:array of longint;

  FUNCTION remapCategory(CONST oldIndex:longint):longint;
    VAR oldName:string;
        i:longint;
    begin
      if oldIndex<0 then exit(-1);

      result:=newCategories[oldIndex];
      if result>=0 then exit(result);

      oldName:=uppercase(workspace^.paletteCategories[oldIndex]);
      for i:=0 to length(importInto^.paletteCategories)-1 do if uppercase(importInto^.paletteCategories[i])=oldName then begin
        newCategories[oldIndex]:=i;
        exit(i);
      end;

      result:=importInto^.addCategory(workspace^.paletteCategories[oldIndex]+' (importiert)');
      newCategories[oldIndex]:=result;
    end;

  VAR k,c:longint;
      anyImported:boolean=false;
      importedBoard: P_circuitBoard;
  begin
    setLength(newCategories,length(workspace^.paletteCategories));
    for k:=0 to length(newCategories)-1 do newCategories[k]:=-1;

    for k:=0 to length(items)-1 do with items[k]^  do begin
      for c:=0 to length(items)-1 do if (items[c]^.equivalentBoard<>nil) then
        board^.replaceCustomGates(items[c]^.board,items[c]^.equivalentBoard);
      if Selected and not(alreadyExists) then begin

        board^.categoryIndex:=remapCategory(board^.categoryIndex);
        if board^.categoryIndex<0 then board^.name:=board^.name+LineEnding+'(importiert)';

        if draft
        then importInto^.addDraft(board)
        else begin
          importedBoard:=importInto^.addPaletteEntry(board);
          for c:=0 to length(items)-1 do if c<>k then items[c]^.board^.replaceCustomGates(board,importedBoard);
        end;
      end;
      anyImported:=true;
    end;
    if anyImported then begin
      importInto^.rebuildMenu;
      importInto^.rebuildTree;
    end;
  end;

CONSTRUCTOR T_importItem.create(CONST board_:P_circuitBoard; CONST categoryName_:string; CONST draft_:boolean=false);
  begin
    board:=board_;
    categoryName:=categoryName_;
    draft:=draft_;
    equivalentBoard:=nil;
    setLength(dependsOn,0);
    setLength(isUsedBy,0);
    Selected:=false;
  end;

DESTRUCTOR T_importItem.destroy;
  begin
    setLength(dependsOn,0);
    setLength(isUsedBy,0);
  end;

PROCEDURE T_importItem.addDependency(CONST d:P_importItem);
  begin
    setLength(dependsOn,length(dependsOn)+1);
    dependsOn[length(dependsOn)-1]:=d;

    setLength(d^.isUsedBy,length(d^.isUsedBy)+1);
    d^.isUsedBy[length(d^.isUsedBy)-1]:=@self;
  end;

PROCEDURE T_importItem.setSelected(CONST value:boolean);
  VAR i:P_importItem;
  begin
    if (equivalentBoard=nil) and (value<>Selected) then begin
      Selected:=value;
      if value
      then for i in dependsOn do i^.setSelected(true)
      else for i in isUsedBy  do i^.setSelected(false);
    end;
  end;

FUNCTION T_importItem.alreadyExists:boolean;
  begin result:=equivalentBoard<>nil; end;

FUNCTION T_importItem.itemCaption:string;
  begin
    if categoryName<>''
    then result:=categoryName+' - '+board^.name
    else result:=                   board^.name;
    if draft then result+=' (Entwurf)';
  end;

PROCEDURE T_workspace.addDraft       (CONST board:P_circuitBoard);
  begin
    setLength(draftEntries,length(draftEntries)+1);
    draftEntries[length(draftEntries)-1]:=board^.clone(false);
  end;

FUNCTION T_workspace.addPaletteEntry(CONST board:P_circuitBoard):P_circuitBoard;
  VAR i:longint;
  begin
    i:=length(paletteEntries);
    setLength(paletteEntries,i+1);
    paletteEntries[i]:=board^.clone(false);
    paletteEntries[i]^.paletteIndex:=i;
    result:=paletteEntries[i];
  end;

{$endif}
