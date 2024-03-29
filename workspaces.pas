UNIT workspaces;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil,visualGates,paletteHandling,challenges;

TYPE
  T_workspaceStateEnum=(editingNewBoard,
                        editingPaletteEntry,
                        solvingChallenge,
                        editingChallengeTemplate,
                        editingChallengeSolution);

  T_workspaceState=record
    state:T_workspaceStateEnum;
    prototypeInWorkspacePalette:P_visualBoard;
    newBoard:P_visualBoard;
    challenge:P_challenge;
    originalChallengeIndex:longint;
    paletteIndex,paletteAssociation:longint;
  end;

  { T_workspace }
  P_workspace=^T_workspace;
  T_workspace=object(T_serializable)
  private
    workspacePalette:P_workspacePalette;
    workspaceBoard  :P_visualBoard;
    challenges      :P_challengeSet;
    activeChallengeIndex :longint;

    activeChallenge :P_challenge;

    //Not persisted
    currentState:T_workspaceState;
    previousState:array of T_workspaceState;

  public
    PROCEDURE clearPreviousStates;
  private
    PROCEDURE initCurrentState;
    PROCEDURE stateTransition(CONST newState:T_workspaceStateEnum);
    CONSTRUCTOR create;
  public
    simplisticUi:boolean;
    CONSTRUCTOR createAndRestore;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    PROCEDURE restartChallenge;
    FUNCTION nextUncompletedChallenge:longint;
    FUNCTION startChallenge(CONST challengeIndex:longint):boolean;
    PROCEDURE setFreeEditMode;
    PROCEDURE challengeDeleted(CONST deletedChallengeIndex:longint);

    FUNCTION  activePalette:P_palette;
    FUNCTION  activeBoard  :P_visualBoard;
    PROCEDURE setActiveBoard(CONST board:P_visualBoard);
    PROPERTY  getActiveChallenge:P_challenge read activeChallenge;

    PROPERTY  state:T_workspaceStateEnum read currentState.state;
    PROPERTY  currentPaletteAssociation:longint read currentState.paletteAssociation;
    FUNCTION  isEditingNewChallenge:boolean;
    PROCEDURE editPaletteEntry(CONST prototype:P_visualBoard; CONST uiAdapter:P_uiAdapter);
    PROCEDURE prototypeUpdated(CONST oldPrototype,newPrototype:P_visualBoard);
    PROCEDURE clearBoard(CONST uiAdapter: P_uiAdapter);
    PROPERTY  getChallenges:P_challengeSet read challenges;

    FUNCTION getInfoLabelText(CONST uiIdle:boolean):string;

    PROPERTY getWorkspacePalette:P_workspacePalette read workspacePalette;
    PROCEDURE startEditingChallenge(CONST challenge:P_challenge; CONST challengeIndex:longint; CONST editExpected:boolean; CONST uiAdapter:P_uiAdapter);

    FUNCTION firstStart:boolean;
    FUNCTION canGoBack:boolean;
    PROCEDURE goBack(CONST uiAdapter: P_uiAdapter; OUT challenge:P_challenge; OUT originalChallengeIndex:longint);

    PROCEDURE updatePaletteEntry(CONST board:P_visualBoard);
  end;

  T_workspaceHistorizationTriggerEnum=(
    wht_onStartup,
    wht_beforeDeletingEntry,
    wht_beforeDuplicateRemoval,
    wht_beforeTaskImport,
    wht_beforePaletteImport,
    wht_beforeDeletingTask,
    wht_beforeRestore,
    wht_beforeEditingEntry);

  P_workspaceHistoryEntryMetaData=^T_workspaceHistoryEntryMetaData;
  T_workspaceHistoryEntryMetaData=record
    triggeredBy:T_workspaceHistorizationTriggerEnum;
    datetime:double;
    numberOfPaletteEntries:longint;
    numberOfTasks:longint;

    dataStartAt:longint;
    dataSize:longint;
  end;

  T_workspaceHistoryEntryIndex=record
    size:byte;
    entries:array[0..254] of T_workspaceHistoryEntryMetaData;
  end;

CONST C_workspaceHistorizationTrigger:array[T_workspaceHistorizationTriggerEnum] of string=(
    {wht_onStartup}             'beim Start',
    {wht_beforeDeletingEntry}   'vor Löschen eines Paletteneintrags',
    {wht_beforeDuplicateRemoval}'vor Entfernen von Duplikaten',
    {wht_beforeTaskImport}      'vor Importieren von Aufgaben',
    {wht_beforePaletteImport}   'vor Importieren einer Palette',
    {wht_beforeDeletingTask}    'vor Entfernen einer Aufgabe',
    {wht_beforeRestore}         'vor Wiederherstellung',
    {wht_beforeEditingEntry}    'vor Bearbeiten eines Paletteneintrags');
FUNCTION workspaceFilename:string;
FUNCTION addBackup(CONST workspace:P_workspace; CONST reason:T_workspaceHistorizationTriggerEnum):T_workspaceHistoryEntryMetaData;
FUNCTION getBackupsIndex:T_workspaceHistoryEntryIndex;
FUNCTION tryRestoreBackup(CONST workspace:P_workspace; CONST entry:T_workspaceHistoryEntryMetaData):boolean;
FUNCTION dropBackup(CONST toDrop:T_workspaceHistoryEntryMetaData):T_workspaceHistoryEntryIndex;
PROCEDURE cleanUpBackups(CONST checkForBrokenBackups:boolean);
PROCEDURE initializeWorkspaces;
VAR workspace:T_workspace;
    errorOnLoadWorkspace:boolean=false;
IMPLEMENTATION
USES sysutils,FileUtil,Classes,zstream,Dialogs,visuals;
FUNCTION backupsFileName:string;
  begin
    result:=GetAppConfigDir(false)+'backups';
  end;

PROCEDURE listBackups(CONST historyIndex:T_workspaceHistoryEntryIndex);
  VAR i:longint;
  begin
    writeln('Total backups: ',historyIndex.size);
    for i:=0 to historyIndex.size-1 do with historyIndex.entries[i] do begin
      writeln(FormatDateTime('dd.mm.yyyy hh:nn:ss ',datetime),' ',dataStartAt,'--',dataStartAt+dataSize);

    end;
  end;

FUNCTION addBackup(CONST workspace:P_workspace; CONST reason:T_workspaceHistorizationTriggerEnum):T_workspaceHistoryEntryMetaData;
  VAR historyIndex:T_workspaceHistoryEntryIndex;
  PROCEDURE invalidateOldestBackup;
    VAR i:longint;
    begin
      for i:=0 to length(historyIndex.entries)-2 do
        historyIndex.entries[i]:=historyIndex.entries[i+1];
      dec(historyIndex.size);
    end;

  VAR fileStream: TFileStream;

  FUNCTION createEntry(CONST bytesToWrite:longint):T_workspaceHistoryEntryMetaData;
    VAR gapStart,gapEnd,i:longint;
        iCollision:longint;
    begin
      result.triggeredBy:=reason;
      result.datetime:=now;
      result.numberOfPaletteEntries:=length(workspace^.workspacePalette^.paletteEntries);
      result.numberOfTasks         :=length(workspace^.challenges^.challenge);
      result.dataSize:=bytesToWrite;

      gapStart:=sizeOf(T_workspaceHistoryEntryIndex);
      gapEnd:=gapStart+bytesToWrite;
      if historyIndex.size>0 then begin
        repeat
          iCollision:=-1;
          for i:=0 to historyIndex.size-1 do with historyIndex.entries[i] do if (dataStartAt<gapEnd) and (dataStartAt+dataSize>gapStart) then iCollision:=i;
          if iCollision>=0 then with historyIndex.entries[iCollision] do begin
            {$ifdef debugMode}
            writeln('Writing at ',gapStart,'--',gapEnd,' would collide with entry #',iCollision,' ',dataStartAt,'--',dataStartAt+dataSize);
            {$endif}
            gapStart:=dataStartAt+dataSize;
            gapEnd:=gapStart+bytesToWrite;
          end;
        until iCollision<0;
      end;
      result.dataStartAt:=gapStart;
      {$ifdef debugMode}
      writeln('Next entry will be written at: ',result.dataStartAt);
      {$endif}
    end;

  PROCEDURE writeCompressedBackup;
    VAR streamWrapper: T_bufferedOutputStreamWrapper;
        compressionstream:TCompressionStream;
        memoryStream:TMemoryStream;

        newEntry:T_workspaceHistoryEntryMetaData;
    begin
      memoryStream     :=TMemoryStream.create;
      memoryStream.setSize(1 shl 20);
      memoryStream.Seek(0,soBeginning);
      compressionstream:=TCompressionStream.create(clMax,memoryStream);
      streamWrapper.create(compressionstream);
      workspace^.saveToStream(streamWrapper);
      streamWrapper.destroy;

      newEntry:=createEntry(memoryStream.position);
      memoryStream.setSize(memoryStream.position);
      result:=newEntry;
      with historyIndex do begin
        entries[size]:=newEntry;
        inc(size);
      end;
      fileStream.Seek(0,soBeginning);
      fileStream.write(historyIndex,sizeOf(historyIndex));

      fileStream.Seek(newEntry.dataStartAt,soBeginning);
      memoryStream.Seek(0,soBeginning);
      memoryStream.saveToStream(fileStream);
      FreeAndNil(memoryStream);
    end;

  VAR
    acutallyRead: longint;
  begin
    if fileExists(backupsFileName)
    then fileStream:=TFileStream.create(backupsFileName,fmOpenReadWrite or fmShareDenyWrite)
    else fileStream:=TFileStream.create(backupsFileName,fmCreate        or fmShareDenyWrite);
    fileStream.Seek(0,soBeginning);
    acutallyRead:=fileStream.read(historyIndex,sizeOf(historyIndex));
    if acutallyRead<sizeOf(historyIndex) then historyIndex.size:=0;

    if historyIndex.size>=254 then invalidateOldestBackup;
    {$ifdef debugMode}
    listBackups(historyIndex);
    {$endif}
    writeCompressedBackup;
    fileStream.destroy;
  end;

FUNCTION getBackupsIndex:T_workspaceHistoryEntryIndex;
  VAR fileStream: TFileStream;
    acutallyRead: longint;
  begin
    if not(fileExists(backupsFileName)) then begin
      result.size:=0;
      exit;
    end;
    initialize(result);
    fileStream:=TFileStream.create(backupsFileName,fmOpenRead);
    fileStream.Seek(0,soBeginning);
    acutallyRead:=fileStream.read(result,sizeOf(result));
    if acutallyRead<sizeOf(result) then result.size:=0;
    fileStream.destroy;
  end;

FUNCTION tryRestoreBackup(CONST workspace:P_workspace; CONST entry:T_workspaceHistoryEntryMetaData):boolean;
  VAR fileStream: TFileStream;

      streamWrapper: T_bufferedInputStreamWrapper;
      decompressionstream:TDecompressionStream;
      memoryStream:TMemoryStream;
  begin
    if not(fileExists(backupsFileName)) then exit(false);
    fileStream:=TFileStream.create(backupsFileName,fmOpenRead);
    fileStream.Seek(entry.dataStartAt,soBeginning);
    memoryStream:=TMemoryStream.create;
    memoryStream.setSize(entry.dataSize);
    fileStream.read(memoryStream.memory^,entry.dataSize);
    fileStream.destroy;

    memoryStream.Seek(0,soBeginning);
    decompressionstream:=TDecompressionStream.create(memoryStream);
    streamWrapper.create(decompressionstream);

    workspace^.clear;
    result:=workspace^.loadFromStream(streamWrapper);
    if result then workspace^.initCurrentState;
    streamWrapper.destroy;
    memoryStream.destroy;
  end;

FUNCTION dropBackup(CONST toDrop: T_workspaceHistoryEntryMetaData): T_workspaceHistoryEntryIndex;
  VAR fileStream: TFileStream;
      i, acutallyRead:longint;
      dropIdx:longint=-1;
  begin
    if fileExists(backupsFileName)
    then fileStream:=TFileStream.create(backupsFileName,fmOpenReadWrite or fmShareDenyWrite)
    else fileStream:=TFileStream.create(backupsFileName,fmCreate        or fmShareDenyWrite);
    fileStream.Seek(0,soBeginning);
    acutallyRead:=fileStream.read(result,sizeOf(T_workspaceHistoryEntryIndex));
    if acutallyRead<sizeOf(T_workspaceHistoryEntryIndex) then result.size:=0
    else begin
      for i:=0 to result.size-1 do
        if (result.entries[i].datetime   =toDrop.datetime) and
           (result.entries[i].dataStartAt=toDrop.dataStartAt) and
           (result.entries[i].dataSize   =toDrop.dataSize)
        then dropIdx:=i;
      if dropIdx>=0 then begin
        for i:=dropIdx to result.size-2 do result.entries[i]:=result.entries[i+1];
        result.size-=1;
        fileStream.Seek(0,soBeginning);
        fileStream.write(result,sizeOf(T_workspaceHistoryEntryIndex));
      end;
    end;
    fileStream.destroy;
  end;

PROCEDURE cleanUpBackups(CONST checkForBrokenBackups:boolean);
  VAR index:T_workspaceHistoryEntryIndex;
      i,j, acutallyRead:longint;
      fileStream: TFileStream;
  FUNCTION areBackupsIdentical(CONST x,y:T_workspaceHistoryEntryMetaData):boolean;
    VAR dataX,dataY:PByte;
        k:longint;
    begin
      result:=(x.dataSize=y.dataSize) and (x.numberOfPaletteEntries=y.numberOfPaletteEntries) and (x.numberOfTasks=y.numberOfTasks);

      if result then begin
        //load data regions and do actual comparison
        getMem(dataX,x.dataSize); fileStream.Seek(x.dataStartAt,soBeginning); fileStream.read(dataX^,x.dataSize);
        getMem(dataY,y.dataSize); fileStream.Seek(y.dataStartAt,soBeginning); fileStream.read(dataY^,y.dataSize);

        for k:=0 to x.dataSize-1 do result:=result and (dataX[k]=dataY[k]);

        freeMem(dataX,x.dataSize);
        freeMem(dataY,y.dataSize);
      end;
    end;

  FUNCTION canRestoreBackup(CONST entry:T_workspaceHistoryEntryMetaData):boolean;
    VAR ws:T_workspace;
      memoryStream: TMemoryStream;
      decompressionstream: TDecompressionStream;
      streamWrapper: T_bufferedInputStreamWrapper;
    begin
      result:=false;
      try
        memoryStream:=TMemoryStream.create;
        fileStream.Seek(entry.dataStartAt,soBeginning);
        memoryStream.setSize(entry.dataSize);
        fileStream.read(memoryStream.memory^,entry.dataSize);
        memoryStream.Seek(0,soBeginning);
        decompressionstream:=TDecompressionStream.create(memoryStream);
        streamWrapper.create(decompressionstream);
        ws.create;
        result:=ws.loadFromStream(streamWrapper);
      finally
        streamWrapper.destroy;
        memoryStream.destroy;
        ws.destroy;
      end;
    end;

  PROCEDURE rewriteHistory;
    VAR rawData:array[0..254] of PByte;
        i:longint;
    begin
      for i:=0 to index.size-1 do begin
        getMem(rawData[i],index.entries[i].dataSize);
        fileStream.Seek(index.entries[i].dataStartAt,soBeginning);
        fileStream.read(rawData[i]^,index.entries[i].dataSize);
      end;

      //move positions in index
      index.entries[0].dataStartAt:=sizeOf(T_workspaceHistoryEntryIndex);
      for i:=1 to index.size-1 do index.entries[i].dataStartAt:=index.entries[i-1].dataStartAt+index.entries[i-1].dataSize;

      fileStream.destroy;
      fileStream:=TFileStream.create(backupsFileName,fmCreate or fmShareDenyWrite);
      fileStream.Seek(0,soBeginning);

      fileStream.Seek(0,soBeginning);
      fileStream.write(index,sizeOf(T_workspaceHistoryEntryIndex));
      for i:=0 to index.size-1 do begin
        fileStream.Seek (index.entries[i].dataStartAt,soBeginning);
        fileStream.write(rawData[i]^,index.entries[i].dataSize);
        freeMem (rawData[i],index.entries[i].dataSize);
      end;
    end;

  begin
    if not fileExists(backupsFileName) then exit;
    fileStream:=TFileStream.create(backupsFileName,fmOpenReadWrite or fmShareDenyWrite);

    fileStream.Seek(0,soBeginning);
    acutallyRead:=fileStream.read(index,sizeOf(T_workspaceHistoryEntryIndex));
    if acutallyRead<sizeOf(T_workspaceHistoryEntryIndex) then index.size:=0;

    i:=0;
    while i<index.size-1 do begin
      if areBackupsIdentical(index.entries[i],index.entries[i+1]) then begin
        {$ifdef debugMode}
        writeln('Backups ',i,' and ',i+1,' are identical');
        {$endif}

        //backups are in chronological order; dropping the lower index means dropping the older one...
        for j:=i to index.size-2 do index.entries[j]:=index.entries[j+1];
        dec(index.size);
        //By dropping we moved closer to the end, so don't inc(i)
      end else inc(i);
    end;

    if checkForBrokenBackups then begin
      //Remaining backups may be broken...
      i:=0;
      while i<index.size do begin
        if not(canRestoreBackup(index.entries[i])) then begin
          {$ifdef debugMode}
          writeln('Backup ',i,' cannot be restored.');
          {$endif}
          for j:=i to index.size-2 do index.entries[j]:=index.entries[j+1];
          dec(index.size);
        end else inc(i);
      end;
    end;

    j:=0;
    for i:=0 to index.size-1 do j+=index.entries[i].dataSize;

    {$ifdef debugMode}
    writeln('Data size: ',j,'; file size: ',fileStream.size,'; overhead: ',(fileStream.size/j-1)*100:0:3,'%');
    {$endif}
    if fileStream.size>2*j
    then rewriteHistory
    else begin
      //Update index:
      fileStream.Seek(0,soBeginning);
      fileStream.write(index,sizeOf(T_workspaceHistoryEntryIndex));
    end;
    fileStream.destroy;
  end;

FUNCTION workspaceFilename:string;
  begin
    result:=GetAppConfigDir(false)+'workspace';
  end;

{ T_workspace }

PROCEDURE T_workspace.clearPreviousStates;
  VAR i:longint;
  begin
    for i:=0 to length(previousState)-1 do with previousState[i] do begin
      case state of
        editingNewBoard: dispose(newBoard,destroy);
        editingPaletteEntry: prototypeInWorkspacePalette:=nil;
        editingChallengeTemplate,
        editingChallengeSolution: begin
          if originalChallengeIndex<0
          then dispose(challenge,destroy);
        end;
      end;
    end;
    setLength(previousState,0);

  end;

PROCEDURE T_workspace.initCurrentState;
  begin
    currentState.prototypeInWorkspacePalette:=nil;
    currentState.newBoard                   :=nil;
    currentState.challenge                  :=nil;
    currentState.originalChallengeIndex     :=-1;
    currentState.paletteIndex               :=0;

    if activeChallengeIndex>=0 then begin
      currentState.state:=solvingChallenge;
      currentState.originalChallengeIndex:=activeChallengeIndex;
      exit;
    end else if workspaceBoard^.getIndexInPalette>=0 then begin
      currentState.state:=editingPaletteEntry;
      currentState.prototypeInWorkspacePalette:=workspacePalette^.paletteEntries[workspaceBoard^.getIndexInPalette].prototype;
      currentState.paletteAssociation         :=workspacePalette^.paletteEntries[workspaceBoard^.getIndexInPalette].subPaletteIndex;
      if currentState.prototypeInWorkspacePalette=nil then begin
        currentState.state:=editingNewBoard;
        currentState.newBoard:=workspaceBoard;
        workspaceBoard^.setIndexInPalette(-1,true);
      end else workspacePalette^.setFilter(workspaceBoard^.getIndexInPalette);
    end else begin
      currentState.state:=editingNewBoard;
      currentState.newBoard:=workspaceBoard;
    end;
  end;

PROCEDURE T_workspace.stateTransition(CONST newState: T_workspaceStateEnum);
  PROCEDURE pushState;
    begin
      //Save some things before doing the rest...
      with currentState do begin
        if state in [editingNewBoard,editingPaletteEntry]
        then paletteIndex:=workspacePalette^.lastSubPaletteIndex;
        if state=editingNewBoard
        then currentState.newBoard:=workspaceBoard^.cloneAsTrueCopy;
        if state=solvingChallenge
        then originalChallengeIndex:=activeChallengeIndex;
      end;
      setLength(previousState,length(previousState)+1);
      previousState[length(previousState)-1]:=currentState;
    end;

  begin
     case currentState.state of
       editingNewBoard         : pushState;
       editingPaletteEntry     : pushState;
       solvingChallenge        :
       case newState of
         editingNewBoard         ,
         editingPaletteEntry     : pushState;
       //solvingChallenge        : begin end;
         editingChallengeTemplate,
         editingChallengeSolution: assert(false,'This should never happen');
       end;
       editingChallengeTemplate,
       editingChallengeSolution:
       case newState of
         editingNewBoard,
         editingPaletteEntry,
         solvingChallenge:
         begin
           dispose(currentState.challenge,destroy);
           currentState.challenge:=nil;
         end;
         editingChallengeTemplate,
         editingChallengeSolution: assert(false,'This should never happen');
       end;
     end;
     currentState.state:=newState;
     currentState.prototypeInWorkspacePalette:=nil;
     currentState.newBoard                   :=nil;
     currentState.challenge                  :=nil;
     currentState.originalChallengeIndex     :=-1;
     currentState.paletteIndex               :=0;
  end;

PROCEDURE T_workspace.goBack(CONST uiAdapter: P_uiAdapter; OUT
  challenge: P_challenge; OUT originalChallengeIndex: longint);
  begin
    challenge             :=currentState.challenge;
    originalChallengeIndex:=currentState.originalChallengeIndex;
    case currentState.state of
      editingChallengeSolution: begin
        if challenge^.expectedBehavior<>nil then dispose(challenge^.expectedBehavior,destroy);
        challenge^.expectedBehavior:=workspaceBoard;
        challenge^.dropBehavior; //behavior may have changed, so we drop the previously extracted behavior
        workspaceBoard^.detachUI;
        workspaceBoard:=nil;
      end;
      editingChallengeTemplate: begin
        if challenge^.resultTemplate<>nil then dispose(challenge^.resultTemplate,destroy);
        challenge^.resultTemplate:=workspaceBoard;
        workspaceBoard^.detachUI;
        workspaceBoard:=nil;
      end;
    end;

    currentState:=previousState[length(previousState)-1];
    setLength    (previousState,length(previousState)-1);
    case currentState.state of
      editingNewBoard: begin
        if workspaceBoard<>nil then dispose(workspaceBoard,destroy);
        workspaceBoard:=currentState.newBoard; currentState.newBoard:=nil;

        activeChallenge:=nil;
        activeChallengeIndex:=-1;
        workspaceBoard  ^.attachUI(uiAdapter);
        workspacePalette^.attachUI(uiAdapter);
        workspacePalette^.setFilter(maxLongint);
        workspacePalette^.selectSubPalette(currentState.paletteIndex);
        uiAdapter^.paintAll;
      end;
      editingPaletteEntry: begin
        if workspaceBoard<>nil then dispose(workspaceBoard,destroy);
        workspaceBoard:=currentState.prototypeInWorkspacePalette^.cloneWithBackReference;

        activeChallenge:=nil;
        activeChallengeIndex:=-1;
        workspaceBoard  ^.attachUI(uiAdapter);
        workspacePalette^.attachUI(uiAdapter);
        workspacePalette^.setFilter(workspaceBoard^.getIndexInPalette);
        workspacePalette^.selectSubPalette(currentState.paletteIndex);
        uiAdapter^.paintAll;
      end;
      solvingChallenge: begin
        startChallenge(currentState.originalChallengeIndex);

        activePalette^.attachUI(uiAdapter);
        activeBoard  ^.attachUI(uiAdapter);
        activeBoard  ^.reset(true);
        uiAdapter^.paintAll;
      end;
    else assert(false,'Not implemented.');
    end;
  end;

PROCEDURE T_workspace.updatePaletteEntry(CONST board: P_visualBoard);
  VAR i: SizeInt;
  begin
    if board^.underlyingPrototype<>nil then
    for i:=length(previousState)-2 downto 0 do
    if (previousState[i].state in [editingNewBoard,editingPaletteEntry,editingChallengeSolution]) and (previousState[i].newBoard<>nil) then
        previousState[i].newBoard^.prototypeUpdated(board^.underlyingPrototype,board);
    workspacePalette^.updateEntry(board);
  end;

CONSTRUCTOR T_workspace.create;
  begin
    new(workspacePalette,create);
    new(workspaceBoard,create(workspacePalette));
    new(challenges,create);
    activeChallengeIndex:=-1;
    activeChallenge:=nil;
    setLength(previousState,0);
  end;

CONSTRUCTOR T_workspace.createAndRestore;
  begin
    create;
    if loadFromFile(workspaceFilename)
    then begin
      addBackup(@self,wht_onStartup);
    end else begin
      dispose(challenges,destroy);
      dispose(workspaceBoard,destroy);
      dispose(workspacePalette,destroy);

      new(workspacePalette,create);
      workspacePalette^.initDefaults;
      new(workspaceBoard,create(workspacePalette));
      new(challenges,create);
      activeChallengeIndex:=-1;
      activeChallenge:=nil;
      challenges^.markAllAsPending;
      simplisticUi:=true;
      errorOnLoadWorkspace:=true;
    end;
    initCurrentState;
  end;

DESTRUCTOR T_workspace.destroy;
  begin
    clearPreviousStates;
    dispose(challenges,destroy);
    dispose(workspaceBoard,destroy);
    dispose(workspacePalette,destroy);
  end;

PROCEDURE T_workspace.clear;
  begin
    clearPreviousStates;
    dispose(challenges,destroy);
    dispose(workspaceBoard,destroy);
    dispose(workspacePalette,destroy);
    new(workspacePalette,create);
    new(workspaceBoard,create(workspacePalette));
    new(challenges,create);
    activeChallengeIndex:=-1;
    activeChallenge:=nil;
    setLength(previousState,0);
    initCurrentState;
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_workspace',2);
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
  begin
    result:=inherited and challenges^.loadFromStream(stream);
    activeChallengeIndex:=stream.readLongint;

    if result and stream.allOkay then begin
      if (activeChallengeIndex>=0) and (activeChallengeIndex<length(challenges^.challenge))
      then begin
        activeChallenge:=challenges^.challenge[activeChallengeIndex];
        workspaceBoard^.moveToPalette(activeChallenge^.palette);
      end else activeChallenge:=nil;

      result:=result and
      workspacePalette^.loadFromStream(stream) and
      workspaceBoard  ^.loadFromStream(stream,false);

      if tutorial.equals(activeChallenge) then restartChallenge;

    end else result:=false;
    simplisticUi:=not(stream.allOkay) or stream.readBoolean;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    challenges^.saveToStream(stream);
    stream.writeLongint(activeChallengeIndex);
    workspacePalette^.saveToStream(stream);
    workspaceBoard^.saveToStream(stream,false);
    stream.writeBoolean(simplisticUi);
  end;

PROCEDURE T_workspace.restartChallenge;
  begin
    startChallenge(activeChallengeIndex);
  end;

FUNCTION T_workspace.nextUncompletedChallenge: longint;
  VAR i:longint;
  begin
    result:=-1;
    for i:=activeChallengeIndex+1 to length(challenges^.challenge)-1 do if not(challenges^.challenge[i]^.callengeCompleted) then exit(i);
  end;

FUNCTION T_workspace.startChallenge(CONST challengeIndex: longint): boolean;
  begin
    if (challengeIndex<0) or (challengeIndex>=length(challenges^.challenge)) then exit(false);
    activeChallengeIndex:=challengeIndex;
    activeChallenge:=challenges^.challenge[challengeIndex];

    stateTransition(solvingChallenge);
    currentState.originalChallengeIndex:=challengeIndex;

    dispose(workspaceBoard,destroy);
    workspaceBoard:=activeChallenge^.resetChallenge;
    result:=true;
  end;

PROCEDURE T_workspace.setFreeEditMode;
  begin
    stateTransition(editingNewBoard);
    activeChallenge:=nil;
    activeChallengeIndex:=-1;
    dispose(workspaceBoard,destroy);
    new(workspaceBoard,create(workspacePalette));
  end;

PROCEDURE T_workspace.challengeDeleted(CONST deletedChallengeIndex: longint);
  begin
    if activeChallenge=nil then exit;
    if activeChallengeIndex=deletedChallengeIndex then begin
      setFreeEditMode;
      exit;
    end;
    if deletedChallengeIndex<activeChallengeIndex then dec(activeChallengeIndex);
  end;

FUNCTION T_workspace.activePalette: P_palette;
  begin
    case currentState.state of
      editingNewBoard         ,
      editingPaletteEntry     : result:=workspacePalette;
      solvingChallenge        : result:=activeChallenge^.palette;
      editingChallengeTemplate,
      editingChallengeSolution: result:=currentState.challenge^.palette;
    end;
  end;

FUNCTION T_workspace.activeBoard: P_visualBoard;
  begin
    result:=workspaceBoard;
  end;

PROCEDURE T_workspace.setActiveBoard(CONST board: P_visualBoard);
  begin
    workspaceBoard:=board;
  end;

FUNCTION T_workspace.isEditingNewChallenge: boolean;
  begin
    result:=(currentState.state in [editingChallengeSolution,editingChallengeTemplate]) and (currentState.originalChallengeIndex<0);
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST prototype: P_visualBoard;
  CONST uiAdapter: P_uiAdapter);
  begin
    if activeChallenge<>nil then exit;
    addBackup(@self, wht_beforeEditingEntry);
    stateTransition(editingPaletteEntry);
    currentState.prototypeInWorkspacePalette:=prototype;
    currentState.paletteAssociation:=workspacePalette^.lastSubPaletteIndex;
    dispose(workspaceBoard,destroy);
    workspacePalette^.setFilter(prototype^.getIndexInPalette);
    workspaceBoard:=prototype^.cloneWithBackReference;
    workspaceBoard^.attachUI(uiAdapter);
    workspaceBoard^.reset(true);
    uiAdapter^.updateBoardScrollbars;
    uiAdapter^.paintAll;
  end;

PROCEDURE T_workspace.prototypeUpdated(CONST oldPrototype,
  newPrototype: P_visualBoard);
  PROCEDURE update(VAR entry:T_workspaceState);
    begin
      if   entry.prototypeInWorkspacePalette =oldPrototype
      then entry.prototypeInWorkspacePalette:=newPrototype;
    end;

  VAR i:longint;
  begin
    update(currentState);
    for i:=0 to length(previousState)-1 do update(previousState[i]);
  end;

PROCEDURE T_workspace.clearBoard(CONST uiAdapter: P_uiAdapter);
  begin
    clearPreviousStates;

    if activeChallenge<>nil then begin
      dispose(workspaceBoard,destroy);
      workspaceBoard:=activeChallenge^.resetChallenge;
    end else begin
      workspaceBoard^.clear;
      workspacePalette^.setFilter(maxLongint);
    end;
    workspaceBoard^.attachUI(uiAdapter);
    uiAdapter^.updateBoardScrollbars;
  end;

FUNCTION T_workspace.getInfoLabelText(CONST uiIdle: boolean): string;
  begin
    case currentState.state of
      editingChallengeTemplate: exit('Vorgabe von Aufgabe: '+currentState.challenge^.challengeTitle);
      editingChallengeSolution: exit('Lösung von Aufgabe: '+currentState.challenge^.challengeTitle);
    end;
    if activeChallenge<>nil
    then result:=activeChallenge^.getInfoLabelText(uiIdle)
    else result:=workspaceBoard ^.getInfoLabelText;
  end;

PROCEDURE T_workspace.startEditingChallenge(CONST challenge: P_challenge;
  CONST challengeIndex: longint; CONST editExpected: boolean;
  CONST uiAdapter: P_uiAdapter);
  CONST CHALLENGE_EDIT_STATE:array[false..true] of T_workspaceStateEnum=(editingChallengeTemplate,editingChallengeSolution);
  begin
    stateTransition(CHALLENGE_EDIT_STATE[editExpected]);
    currentState.challenge:=challenge;
    currentState.originalChallengeIndex:=challengeIndex;
    if editExpected
    then workspaceBoard:=challenge^.expectedBehavior^.cloneAsTrueCopy
    else workspaceBoard:=challenge^.resultTemplate  ^.cloneAsTrueCopy;
    challenge^.palette^.finalizePalette(workspaceBoard,challenge^.expectedBehavior);

    workspaceBoard^.attachUI(uiAdapter);
    challenge^.palette^.attachUI(uiAdapter);
    challenge^.palette^.ensureVisualPaletteItems;
    uiAdapter^.paintAll;
  end;

FUNCTION T_workspace.firstStart: boolean;
  begin
    result:=not(tutorial.callengeCompleted) and (length(workspacePalette^.paletteEntries)<=19);
  end;

FUNCTION T_workspace.canGoBack: boolean;
  begin
    result:=length(previousState)>0;
  end;

PROCEDURE initializeWorkspaces;
  begin
    try
      workspace.createAndRestore;
    except
      DeleteFile(workspaceFilename);
      halt;
    end;
  end;

FINALIZATION
  workspace.saveToFile(workspaceFilename);
  workspace.destroy;
  if cleanupHistoryOnShutdown>0 then cleanUpBackups(cleanupHistoryOnShutdown>1);
end.

