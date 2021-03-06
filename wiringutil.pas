UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil;
CONST BOARD_MAX_SIZE_IN_GRID_ENTRIES=200;
TYPE T_wireDirection=(wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up,wd_leftUp);
     T_wireDirectionSet=bitpacked set of T_wireDirection;
     T_wireDirections=array of T_wireDirection;
     T_point=array[0..1] of longint;
     T_wirePath=array of T_point;
     T_wirePathArray=array of T_wirePath;

     T_sortedDirections=array[0..7] of T_wireDirection;

CONST ZERO_POINT:T_point=(0,0);
      WIRE_DELTA:array[T_wireDirection] of T_point=(
      {wd_left     } (-1, 0),
      {wd_leftDown } (-1, 1),
      {wd_down     } ( 0, 1),
      {wd_rightDown} ( 1, 1),
      {wd_right    } ( 1, 0),
      {wd_rightUp  } ( 1,-1),
      {wd_up       } ( 0,-1),
      {wd_leftUp   } (-1,-1));
      OppositeDirection:array[T_wireDirection] of T_wireDirection=(wd_right,wd_rightUp,wd_up,wd_leftUp,wd_left,wd_leftDown,wd_down,wd_rightDown);
      AllDirections=[wd_left..wd_leftUp];
      StraightDirections=[wd_left,wd_up,wd_right,wd_down];

      allowedSuccessiveDirection:array[T_wireDirection] of T_wireDirectionSet=
          {wd_left     }([wd_left,wd_leftDown,wd_down             ,                    wd_up,wd_leftUp],
          {wd_leftDown } [wd_left,wd_leftDown,wd_down                                                 ],
          {wd_down     } [wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right                           ],
          {wd_rightDown} [                    wd_down,wd_rightDown,wd_right                           ],
          {wd_right    } [                    wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up          ],
          {wd_rightUp  } [                                         wd_right,wd_rightUp,wd_up          ],
          {wd_up       } [wd_left                                 ,wd_right,wd_rightUp,wd_up,wd_leftUp],
          {wd_leftUp   } [wd_left                                 ,                    wd_up,wd_leftUp]);

TYPE
  { T_wireGraph }
  P_wireGraph=^T_wireGraph;
  T_wireGraph=object
    private
      allowedDirectionsPerPoint:array[0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1] of T_wireDirectionSet;
      FUNCTION findPath(CONST startPoint,endPoint:T_point; CONST pathsToPrimeWith:T_wirePathArray; CONST exhaustiveScan:boolean; CONST directionMask:T_wireDirectionSet=AllDirections):T_wirePath;
      PROCEDURE dropWireSection(CONST a,b:T_point; CONST diagonalsOnly:boolean=false);
      FUNCTION isPathFree(CONST startPoint,endPoint:T_point; VAR path:T_wirePath):boolean;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE initDirections;
      PROCEDURE dropEdges(CONST i:T_point; CONST dirs:T_wireDirectionSet);
      PROCEDURE dropNode(CONST i:T_point);
      PROCEDURE addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
      PROCEDURE dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath; CONST exhaustiveScan:boolean):T_wirePathArray;
      FUNCTION anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
      FUNCTION isWireAllowed(CONST path:T_wirePath):boolean;
  end;

FUNCTION pointOf(CONST x,y:longint):T_point;
OPERATOR +(CONST x,y:T_point):T_point;
OPERATOR -(CONST x,y:T_point):T_point;
OPERATOR +(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR -(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR =(CONST x,y:T_point):boolean;
OPERATOR *(CONST x:T_point; CONST y:longint):T_point;
OPERATOR *(CONST x,y:T_point):longint;
FUNCTION allPointsBetween(CONST startP,endP:T_point; OUT dir:T_wireDirection):T_wirePath;
PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p:T_point);
FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper):T_point;
FUNCTION maxNormDistance(CONST x,y:T_point):longint;
FUNCTION euklideanDistance(CONST x, y: T_point): double;
FUNCTION pathScore(CONST path:T_wirePath):double;

FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
FUNCTION lineCrossesRectangle(CONST a0,a1,rectangleOrigin,rectangleExtend:T_point):boolean;

VAR enableShortcuts:boolean=true;
    allowDiagonals :boolean=false;
IMPLEMENTATION
USES math,sysutils;
FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
  FUNCTION inUnitRange(CONST x:double):boolean; inline;
    begin result:=(x>=0) and (x<=1); end;
  VAR u,v,w:T_point;
      f:double;
  begin
    u:=a1-a0;
    v:=b0-b1;
    w:=b1-a0;
    f:= -u[0]*v[1]+u[1]*v[0];
    result:=(abs(f)>1E-3) and
            inUnitRange((-w[0]*v[1]+w[1]*v[0])/f) and
            inUnitRange(( u[0]*w[1]-u[1]*w[0])/f);
  end;

FUNCTION lineCrossesRectangle(CONST a0,a1,rectangleOrigin,rectangleExtend:T_point):boolean;
  begin
    if (a0[0]<=rectangleOrigin[0]                   ) and (a1[0]<=rectangleOrigin[0]                   ) then exit(false);
    if (a0[0]>=rectangleOrigin[0]+rectangleExtend[0]) and (a1[0]>=rectangleOrigin[0]+rectangleExtend[0]) then exit(false);
    if (a0[1]<=rectangleOrigin[1]                   ) and (a1[1]<=rectangleOrigin[1]                   ) then exit(false);
    if (a0[1]>=rectangleOrigin[1]+rectangleExtend[1]) and (a1[1]>=rectangleOrigin[1]+rectangleExtend[1]) then exit(false);

    result:=linesIntersect(a0,a1,rectangleOrigin                ,pointOf(rectangleOrigin[0]+rectangleExtend[0],rectangleOrigin[1]))
         or linesIntersect(a0,a1,rectangleOrigin+rectangleExtend,pointOf(rectangleOrigin[0]+rectangleExtend[0],rectangleOrigin[1]))
         or linesIntersect(a0,a1,rectangleOrigin+rectangleExtend,pointOf(rectangleOrigin[0]                   ,rectangleOrigin[1]+rectangleExtend[1]))
         or linesIntersect(a0,a1,rectangleOrigin                ,pointOf(rectangleOrigin[0]                   ,rectangleOrigin[1]+rectangleExtend[1]));
  end;

FUNCTION pointOf(CONST x, y: longint): T_point;
  begin
    result[0]:=x;
    result[1]:=y;
  end;

OPERATOR+(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR-(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

OPERATOR+(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]+WIRE_DELTA[y,0];
    result[1]:=x[1]+WIRE_DELTA[y,1];
  end;

OPERATOR-(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]-WIRE_DELTA[y,0];
    result[1]:=x[1]-WIRE_DELTA[y,1];
  end;

OPERATOR*(CONST x: T_point; CONST y: longint): T_point;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR *(CONST x,y:T_point):longint;
  begin
    result:=x[0]*y[0]+x[1]*y[1];
  end;

OPERATOR=(CONST x, y: T_point): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]);
  end;

PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper;
  CONST p: T_point);
  begin
    stream.writeLongint(p[0]);
    stream.writeLongint(p[1]);
  end;

FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper): T_point;
  begin
    result[0]:=stream.readLongint;
    result[1]:=stream.readLongint;
  end;

FUNCTION maxNormDistance(CONST x, y: T_point): longint;
  begin
    result:=max(abs(x[0]-y[0]),abs(x[1]-y[1]));
  end;

FUNCTION euklideanDistance(CONST x, y: T_point): double;
  begin
    result:=sqrt(sqr(x[0]-y[0])+sqr(x[1]-y[1]));
  end;

FUNCTION directionBetween(CONST x, y: T_point; OUT valid:boolean): T_wireDirection;
  begin
    valid:=true;
    if y[0]=x[0] then begin
      if y[1]>x[1]
      then result:=wd_down
      else result:=wd_up;
    end else if y[1]=x[1] then begin
      if y[0]>x[0]
      then result:=wd_right
      else result:=wd_left;
    end else if y[1]-x[1]=y[0]-x[0] then begin
      if y[0]>x[0]
      then result:=wd_rightDown
      else result:=wd_leftUp;
    end else if y[1]-x[1]=x[0]-y[0] then begin
      if y[0]>x[0]
      then result:=wd_rightUp
      else result:=wd_leftDown;
    end else begin
      valid:=false;
      result:=wd_right;
    end;
  end;

FUNCTION wireStep(CONST start:T_point; CONST direction:T_wireDirection; CONST steps:longint):T_point;
  begin
    result[0]:=start[0]+WIRE_DELTA[direction,0]*steps;
    result[1]:=start[1]+WIRE_DELTA[direction,1]*steps;
  end;

CONSTRUCTOR T_wireGraph.create;
  begin
    initDirections;
  end;

DESTRUCTOR T_wireGraph.destroy;
  begin
  end;

PROCEDURE T_wireGraph.initDirections;
  VAR x,y:longint;
      dirs:T_wireDirectionSet;
  begin
    if allowDiagonals
    then dirs:=AllDirections
    else dirs:=StraightDirections;

    for x:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do allowedDirectionsPerPoint[x,y]:=dirs;
    y:=BOARD_MAX_SIZE_IN_GRID_ENTRIES-1;
    for x:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do begin
      allowedDirectionsPerPoint[x,0]-=[wd_up  ,wd_leftUp  ,wd_rightUp  ];
      allowedDirectionsPerPoint[x,y]-=[wd_down,wd_leftDown,wd_rightDown];
    end;
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
      allowedDirectionsPerPoint[0,y]-=[wd_left ,wd_leftUp ,wd_leftDown ];
    x:=BOARD_MAX_SIZE_IN_GRID_ENTRIES-1;
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
      allowedDirectionsPerPoint[x,y]-=[wd_right,wd_rightUp,wd_rightDown];
  end;

PROCEDURE T_wireGraph.dropEdges(CONST i: T_point; CONST dirs: T_wireDirectionSet);
  VAR j:T_point;
      dir:T_wireDirection;
  begin
    allowedDirectionsPerPoint[i[0],i[1]]-=dirs;
    for dir in dirs do begin
      j:=i+dir;
      if (j[0]>=0) and (j[0]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (j[1]>=0) and (j[1]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) then
      Exclude(allowedDirectionsPerPoint[j[0],j[1]],OppositeDirection[dir]);
    end;
  end;

PROCEDURE T_wireGraph.addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
  begin
    if (from[0]<0) or (from[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) or
       (from[1]<0) or (from[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) then exit;
    include(allowedDirectionsPerPoint[from[0],from[1]],dir);
  end;

PROCEDURE T_wireGraph.dropNode(CONST i: T_point);
  begin
    dropEdges(i,AllDirections);
  end;

FUNCTION allPointsBetween(CONST startP, endP: T_point; OUT dir: T_wireDirection): T_wirePath;
  VAR p:T_point;
      len:longint;
      i:longint;
      validDirectionFound:boolean;
  begin
    len:=maxNormDistance(startP,endP);
    setLength(result,len+1);
    dir:=directionBetween(startP,endP,validDirectionFound);
    if not(validDirectionFound) then begin
      setLength(result,0);
      dir:=wd_left;
    end;
    p:=startP;
    for i:=0 to len do begin
      result[i]:=p;
      p+=dir;
    end;
  end;

PROCEDURE T_wireGraph.dropWireSection(CONST a, b: T_point; CONST diagonalsOnly:boolean=false);
  CONST DIRECTIONS_TO_DROP:array[T_wireDirection] of T_wireDirectionSet=(
  {wd_left     }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  {wd_leftDown }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  {wd_down     }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  {wd_rightDown}[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp],
  {wd_right    }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  {wd_rightUp  }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  {wd_up       }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  {wd_leftUp   }[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp]);
  VAR dir:T_wireDirection;
      len,i:longint;
      validDirectionFound:boolean;
  begin
    len:=maxNormDistance(a,b);
    if len>1 then begin
      dir:=directionBetween(a,b,validDirectionFound);
      if validDirectionFound then
      for i:=1 to len-1 do dropEdges(wireStep(a,dir,i),DIRECTIONS_TO_DROP[dir]);
    end;
    if diagonalsOnly then exit;
    dropNode(a);
    dropNode(b);
  end;

PROCEDURE T_wireGraph.dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
  VAR i:longint;
  begin
    for i:=0 to length(path)-2 do dropWireSection(path[i],path[i+1],diagonalsOnly);
  end;

FUNCTION T_wireGraph.isPathFree(CONST startPoint,endPoint:T_point; VAR path:T_wirePath):boolean;
  VAR len:longint;
      i:longint;
      w:double;
      p:T_point;
  begin
    if not(enableShortcuts) or (endPoint[0]<startPoint[0]+2) then exit(false);
    if endPoint=startPoint then begin
      setLength(path,1);
      path[0]:=startPoint;
      exit(true);
    end;
    len:=ceil(sqrt(2)*max(endPoint[0]-startPoint[0]-2,abs(endPoint[1]-startPoint[1])));
    if len>0 then for i:=0 to len do begin
      w:=i/len;
      p:=pointOf(round((startPoint[0]+1)*(1-w)+(endPoint[0]-1)*w),
                 round( startPoint[1]   *(1-w)+ endPoint[1]   *w));
      if allowedDirectionsPerPoint[p[0],p[1]]=[] then exit(false);
    end;
    if startPoint[1]=endPoint[1] then begin
      setLength(path,2);
      path[0]:=startPoint;
      path[1]:=endPoint;
    end else begin
      setLength(path,4);
      path[0]:=startPoint;
      path[1]:=pointOf(startPoint[0]+1,startPoint[1]);
      path[2]:=pointOf(endPoint  [0]-1,endPoint  [1]);
      path[3]:=endPoint;
    end;

    result:=true;
  end;

CONST DirectionCost:array[T_wireDirection] of double=(1,1.5,
                                                      1,1.5,
                                                      1,1.5,
                                                      1,1.5);
      ChangeDirectionPenalty=1;

FUNCTION pathScore(CONST path:T_wirePath):double;
  VAR i:longint;
      dir,dirBefore:T_wireDirection;
      valid:boolean;
  begin
    if length(path)=0 then exit(infinity);
    result:=0;
    for i:=0 to length(path)-2 do begin
      dir:=directionBetween(path[i],path[i+1],valid);
      if (i>0) and (dir<>dirBefore) then result+=ChangeDirectionPenalty;
      dirBefore:=dir;
      if valid then result+=maxNormDistance(path[i],path[i+1])*DirectionCost[dir]
               else result+=euklideanDistance(path[i],path[i+1]);
    end;
  end;

FUNCTION simplifyPath(CONST path:T_wirePath):T_wirePath;
  VAR i:longint;
      j:longint=1;
      lastDir,dir:T_wireDirection;
      directionIsValid: boolean;
  begin
    if length(path)<2 then exit(path);
    setLength(result,length(path));
    result[0]:=path[0];
    lastDir:=directionBetween(path[0],path[1],directionIsValid);
    for i:=1 to length(path)-1 do begin
      dir:=directionBetween(path[i-1],path[i],directionIsValid);
      if dir<>lastDir then begin
        inc(j);
        lastDir:=dir;
      end;
      result[j]:=path[i];
    end;
    setLength(result,j+1);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point; CONST pathsToPrimeWith:T_wirePathArray; CONST exhaustiveScan:boolean; CONST directionMask:T_wireDirectionSet=AllDirections): T_wirePath;
  CONST UNVISITED=maxLongint;
  VAR map:array[0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1] of record comeFrom:T_point; score:longint; end;
      p:array of T_point;
      n0:longint=0;
      n1:longint=0;
      j:longint=1;

  PROCEDURE prime;
    VAR path:T_wirePath;
        k:longint;
        point:T_point;
        aggregatedCost:longint;
    begin
      p[0]:=startPoint;
      map[startPoint[0],startPoint[1]].score:=0;
      inc(n0);
      for path in pathsToPrimeWith do begin
        aggregatedCost:=0;
        for k:=1 to length(path)-1 do begin
          if (k=1) or (path[k-1]-path[k-2]=path[k]-path[k-1])
          then aggregatedCost+=1
          else aggregatedCost+=2;
          point:=path[k];
          with map[point[0],point[1]] do if score>aggregatedCost then begin
            if score=UNVISITED then begin
              p[n0]:=point;
              inc(n0);
            end;
            score:=aggregatedCost;
            comeFrom:=path[k-1];
          end;
        end;
      end;
      j :=n0;
    end;

  PROCEDURE rescore(CONST cf:T_point);
    VAR x,y:longint;
        newScore, cf_score:longint;
        cf_dir: T_point;
    begin
      cf_score:=map[cf[0],cf[1]].score;
      cf_dir  :=cf-map[cf[0],cf[1]].comeFrom;

      for x:=max(0,cf[0]-1) to min(BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,cf[0]+1) do
      for y:=max(0,cf[1]-1) to min(BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,cf[1]+1) do
      if map[x,y].comeFrom=cf then begin
        newScore:=cf_score+2;
        if pointOf(x,y)-cf<>cf_dir then newScore+=2;
        if newScore<map[x,y].score then begin
          map[x,y].score:=newScore;
          rescore(pointOf(x,y));
        end;
      end;
    end;

  VAR i0:longint=0;
      i1:longint=BOARD_MAX_SIZE_IN_GRID_ENTRIES*BOARD_MAX_SIZE_IN_GRID_ENTRIES;
      i:longint=0;
      found:boolean=false;
      searching:boolean=true;
      ii:longint;
      prevStep,
      dir:T_wireDirection;
      xy,xyNeighbor:T_point;
      newCost:longint;
      directionIsValid: boolean;
      startTicks: qword;
      needRescore:boolean;
  begin
    startTicks:=GetTickCount64;
    for i:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
    for j:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do with map[i,j] do begin
      score:=UNVISITED;
      comeFrom:=startPoint;
    end;
    setLength(p,i1*2);

    prime;

    while searching and (exhaustiveScan or not found) do begin
      searching:=false;
      for ii:=i0 to i0+n0-1 do begin
        xy:=p[ii];

        prevStep:=directionBetween(map[xy[0],xy[1]].comeFrom,xy,directionIsValid);
        for dir in directionMask*allowedDirectionsPerPoint[xy[0],xy[1]] do begin
          newCost:=map[xy[0],xy[1]].score+2;
          if prevStep<>dir then newCost+=2;
          xyNeighbor:=xy+dir;
          if (newCost<map[xyNeighbor[0],xyNeighbor[1]].score) then begin
            found:=found or (xyNeighbor=endPoint);
            with map[xyNeighbor[0],xyNeighbor[1]] do begin
              if score=UNVISITED then begin
                p[i1+n1]:=xyNeighbor;
                n1+=1;
                needRescore:=false;
              end else needRescore:=true;
              score:=newCost;
              comeFrom:=xy;
            end;
            if needRescore then rescore(xyNeighbor);
            searching:=true;
          end;
        end;
      end;
      j+=1;
      ii:=i0+n0; i0:=i1; i1:=ii; n0:=n1; n1:=0;
    end;
    if found then begin
      xy:=endPoint;
      xyNeighbor:=xy;
      setLength(result,map[endPoint[0],endPoint[1]].score);
      j:=0;
      i:=0;
      while (xyNeighbor<>startPoint) and (i<length(result)) do begin
        result[i]:=xy; inc(i);
        xyNeighbor:=xy;
        xy:=map[xy[0],xy[1]].comeFrom;
      end;
      setLength(result,i);
      for i:=0 to (length(result)-1) div 2 do begin
        j:=length(result)-1-i;
        xy:=result[j];
        result[j]:=result[i];
        result[i]:=xy;
      end;
    end else begin
      setLength(result,0);
    end;
    setLength(p,0);
    writeln('Path scan from ',startPoint[0],',',startPoint[1],' to ',endPoint[0],',',endPoint[1],' finished in ',GetTickCount64-startTicks,'; successful: ',found);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint,endPoint:T_point):T_wirePath;
  VAR toPrimeWith:T_wirePathArray;
  begin
    if isPathFree(startPoint,endPoint,result) then exit(result);
    setLength(toPrimeWith,0);
    result:=simplifyPath(findPath(startPoint,endPoint,toPrimeWith,false));
  end;

FUNCTION T_wireGraph.findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath; CONST exhaustiveScan:boolean):T_wirePathArray;
  TYPE T_indexAndDist=record
         idx:longint;
         dist:double;
       end;

  VAR nextPath:T_wirePath;
      initialRun:array of T_indexAndDist;
      swapTemp:T_indexAndDist;
      i,j:longint;
      anyImproved: boolean;
      mask:T_wireDirectionSet;
  FUNCTION listExceptEntry(CONST list:T_wirePathArray; CONST indexToDrop:longint):T_wirePathArray;
    VAR i:longint;
        k:longint=0;
    begin
      setLength(result,length(list)-1);
      for i:=0 to length(list)-1 do if i<>indexToDrop then begin
        result[k]:=list[i];
        k+=1;
      end;
    end;

  begin
    if (length(endPoints)=1) and isPathFree(startPoint,endPoints[0],nextPath) then begin
      setLength(result,1);
      result[0]:=nextPath;
      exit(result);
    end;

    setLength(initialRun,length(endPoints));
    for i:=0 to length(endPoints)-1 do begin
      initialRun[i].idx:=i;
      initialRun[i].dist:=sqr(startPoint[0]-endPoints[i,0])+sqr(startPoint[1]-endPoints[i,1]);
      for j:=0 to i-1 do if initialRun[i].dist<initialRun[j].dist then begin
        swapTemp     :=initialRun[i];
        initialRun[i]:=initialRun[j];
        initialRun[j]:=swapTemp;
      end;
    end;

    if (length(endPoints)>1)
    then mask:=StraightDirections
    else mask:=AllDirections;

    setLength(result,length(endPoints));
    for swapTemp in initialRun do with swapTemp do
      result[idx]:=findPath(startPoint,endPoints[idx],listExceptEntry(result,idx),exhaustiveScan,mask);

    if (length(endPoints)<4) then mask:=AllDirections;

    if length(endPoints)>1 then repeat

      anyImproved:=false;
      for swapTemp in initialRun do with swapTemp do begin
        if length(result[idx])>0 then begin
          nextPath:=findPath(startPoint,endPoints[idx],listExceptEntry(result,idx),exhaustiveScan,mask);
          if pathScore(nextPath)<pathScore(result[idx])
          then begin
            //writeln('Path score improved from ',pathScore(result[idx]),' to ',pathScore(nextPath));
            setLength(result[idx],0);
            result[idx]:=nextPath;
            anyImproved:=true;
          end;
        end;
      end;
    until not(anyImproved);
    for i:=0 to length(result)-1 do result[i]:=simplifyPath(result[i]);

    setLength(initialRun,0);
  end;

FUNCTION T_wireGraph.anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
  VAR d:T_wireDirection;
      p:T_point;
  begin
    for d in AllDirections do begin
      p:=endPoint+d;
      if (p[0]>=0) and (p[0]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (p[1]>=0) and (p[1]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (OppositeDirection[d] in allowedDirectionsPerPoint[p[0],p[1]]) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.isWireAllowed(CONST path:T_wirePath):boolean;
  VAR p:T_point;
      i,k,len:longint;
      dir:T_wireDirection;
      valid: boolean;
  begin
    for p in path do if (p[0]<0)
    or (p[1]<0)
    or (p[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES)
    or (p[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) then exit(false);

    for i:=0 to length(path)-2 do begin
      p:=path[i];
      dir:=directionBetween(p,path[i+1],valid);
      if valid then begin
        len:=maxNormDistance(path[i],path[i+1]);
        for k:=0 to len-1 do begin
          if not(dir in allowedDirectionsPerPoint[p[0],p[1]]) then exit(false);
          p+=dir;
        end;
      end;
    end;
    result:=true;
  end;

end.

