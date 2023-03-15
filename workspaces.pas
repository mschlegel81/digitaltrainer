UNIT workspaces;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil,visualGates,paletteHandling,challenges;

TYPE

  { T_workspace }
  P_workspace=^T_workspace;
  T_workspace=object(T_serializable)
  private
    workspacePalette:P_workspacePalette;
    workspaceBoard  :P_visualBoard;
    challenges      :P_challengeSet;
    activeChallengeIndex :longint;

    activeChallenge :P_challenge;
  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
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

    FUNCTION  EditorMode   :boolean;
    PROCEDURE editPaletteEntry(CONST prototype:P_visualBoard; CONST uiAdapter:P_uiAdapter);
    PROCEDURE clearBoard(CONST uiAdapter: P_uiAdapter);
    PROPERTY  getChallenges:P_challengeSet read challenges;

    FUNCTION getInfoLabelText:string;
    PROCEDURE replaceChallengeSet(CONST challengeSet:P_challengeSet);

    PROPERTY getWorkspacePalette:P_workspacePalette read workspacePalette;
  end;

IMPLEMENTATION
USES sysutils,FileUtil;
FUNCTION workspaceFilename:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.workspace');
  end;

FUNCTION workspaceBackupFilename:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.workspace.backup');
  end;

{ T_workspace }

CONSTRUCTOR T_workspace.create;
  begin
    new(workspacePalette,create);
    new(workspaceBoard,create(workspacePalette));
    new(challenges,create);
    activeChallengeIndex:=-1;
    activeChallenge:=nil;

    if loadFromFile(workspaceFilename)
    then CopyFile(workspaceFilename,workspaceBackupFilename)
    else begin
      dispose(challenges,destroy);
      dispose(workspaceBoard,destroy);
      dispose(workspacePalette,destroy);

      new(workspacePalette,create);
      workspacePalette^.initDefaults;
      new(workspaceBoard,create(workspacePalette));
      new(challenges,create);
      activeChallengeIndex:=-1;
      activeChallenge:=nil;
    end;
  end;

DESTRUCTOR T_workspace.destroy;
  begin
    saveToFile(workspaceFilename);
    dispose(challenges,destroy);
    dispose(workspaceBoard,destroy);
    dispose(workspacePalette,destroy);
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=serialVersionOf('T_workspace',2);
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
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
    end else result:=false;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    challenges^.saveToStream(stream);
    stream.writeLongint(activeChallengeIndex);
    workspacePalette^.saveToStream(stream);
    workspaceBoard^.saveToStream(stream,false);
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
    dispose(workspaceBoard,destroy);
    workspaceBoard:=activeChallenge^.resetChallenge;
    result:=true;
  end;

PROCEDURE T_workspace.setFreeEditMode;
  begin
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
    if activeChallenge=nil
    then result:=workspacePalette
    else result:=activeChallenge^.palette;
  end;

FUNCTION T_workspace.activeBoard: P_visualBoard;
  begin
    result:=workspaceBoard;
  end;

PROCEDURE T_workspace.setActiveBoard(CONST board: P_visualBoard);
  begin
    workspaceBoard:=board;
  end;

FUNCTION T_workspace.EditorMode: boolean;
  begin
    result:=activeChallenge=nil;
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST prototype: P_visualBoard;
  CONST uiAdapter: P_uiAdapter);
  begin
    if activeChallenge<>nil then exit;
    dispose(workspaceBoard,destroy);
    workspacePalette^.setFilter(prototype^.getIndexInPalette);
    workspaceBoard:=prototype^.clone;
    workspaceBoard^.attachUI(uiAdapter);
    workspaceBoard^.reset(true);
    uiAdapter^.paintAll;
  end;

PROCEDURE T_workspace.clearBoard(CONST uiAdapter: P_uiAdapter);
  begin
    if activeChallenge<>nil then begin
      dispose(workspaceBoard,destroy);
      workspaceBoard:=activeChallenge^.resetChallenge;
    end else begin
      workspaceBoard^.clear;
      workspacePalette^.setFilter(maxLongint);
    end;
    workspaceBoard^.attachUI(uiAdapter);
  end;

FUNCTION T_workspace.getInfoLabelText: string;
  begin
    if activeChallenge<>nil
    then result:=activeChallenge^.getInfoLabelText
    else result:=workspaceBoard ^.getInfoLabelText;
  end;

PROCEDURE T_workspace.replaceChallengeSet(CONST challengeSet: P_challengeSet);
  begin
    setFreeEditMode;
    dispose(challenges,destroy);
    challenges:=challengeSet;
    activeChallenge:=nil;
    activeChallengeIndex:=-1;
  end;

end.

