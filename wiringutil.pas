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
      allowedDirectionsPerPoint:bitpacked array[0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1] of T_wireDirectionSet;
      FUNCTION findPath(CONST startPoint,endPoint:T_point; CONST pathsToPrimeWith:T_wirePathArray; CONST directionMask:T_wireDirectionSet=AllDirections):T_wirePath;
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
      FUNCTION findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath):T_wirePathArray;
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
FUNCTION pathScore(CONST path:T_wirePath):double;

FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
FUNCTION lineCrossesRectangle(CONST a0,a1,rectangleOrigin,rectangleExtend:T_point):boolean;

VAR enableShortcuts:boolean=true;
    allowDiagonals :boolean=true;
IMPLEMENTATION
USES math,sysutils;
TYPE
  P_aStarNodeInfo=^T_aStarNodeInfo;
  T_aStarNodeInfo=record
    p:T_point;
    cameFrom:T_wireDirection;
    costToGetThere:double;
    estimatedCostToGoal:double;
  end;

  { T_nodeMap }

  T_nodeMap=object
    map:array of array of P_aStarNodeInfo;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION containsKey(CONST p:T_point; OUT value:T_aStarNodeInfo):boolean;
    PROCEDURE put(CONST value:T_aStarNodeInfo);
  end;

  T_priorityQueueElement=record
    path:T_wirePath;
    estimatedCostToGoal:double;
  end;

  { T_priorityQueue }

  T_priorityQueue=object
    unsortedEntries,
    sortedEntries:array of T_priorityQueueElement;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE add(CONST path:T_wirePath; CONST fScore:double);
    FUNCTION isEmpty:boolean;
    FUNCTION ExtractMin(OUT fScore:double):T_wirePath;
    PROCEDURE cleanup;
  end;

FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
  VAR Q,P,R:T_point;
  begin
    Q:=a1-a0; Q:=pointOf(-Q[1],Q[0]);
    P:=b0-a0;
    R:=b1-a0;
    if Q*P+Q*R<0 then begin
      Q:=b1-b0; Q:=pointOf(-Q[1],Q[0]);
      P:=a0-b0;
      R:=a1-b0;
      result:=Q*P+Q*R<0;
    end else result:=false;

//    writeln('[',a0[0],',',a0[1],'],',
//            '[',a1[0],',',a1[1],'],',
//            '[',b0[0],',',b0[1],'],',
//            '[',b1[0],',',b1[1],'],',result);

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
      result:=wd_left;
    end;
  end;

FUNCTION wireStep(CONST start:T_point; CONST direction:T_wireDirection; CONST steps:longint):T_point;
  begin
    result[0]:=start[0]+WIRE_DELTA[direction,0]*steps;
    result[1]:=start[1]+WIRE_DELTA[direction,1]*steps;
  end;

CONSTRUCTOR T_priorityQueue.create;
  begin
    setLength(sortedEntries,0);
    setLength(unsortedEntries,0);
  end;

DESTRUCTOR T_priorityQueue.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(sortedEntries)-1 do setLength(sortedEntries[i].path,0);
    setLength(sortedEntries,0);
    for i:=0 to length(unsortedEntries)-1 do setLength(unsortedEntries[i].path,0);
    setLength(unsortedEntries,0);
  end;

PROCEDURE T_priorityQueue.add(CONST path: T_wirePath; CONST fScore: double);
  VAR k:longint;
  begin
    if (length(sortedEntries)=0) or (fScore<sortedEntries[length(sortedEntries)-1].estimatedCostToGoal)
    then begin
      k:=length(sortedEntries);
      setLength(sortedEntries,k+1);
      sortedEntries[k].path  :=path;
      sortedEntries[k].estimatedCostToGoal:=fScore;
    end else begin
      k:=length(unsortedEntries);
      setLength(unsortedEntries,k+1);
      unsortedEntries[k].path  :=path;
      unsortedEntries[k].estimatedCostToGoal:=fScore;
      if length(unsortedEntries)>16 then cleanup;
    end;
  end;

FUNCTION T_priorityQueue.isEmpty: boolean;
  begin
    result:=(length(sortedEntries)=0) and
          (length(unsortedEntries)=0)
  end;

FUNCTION T_priorityQueue.ExtractMin(OUT fScore: double): T_wirePath;
  VAR k:longint;
  begin
    cleanup;
    k:=length(sortedEntries)-1;
    result:=sortedEntries[k].path;
    fScore:=sortedEntries[k].estimatedCostToGoal;
    setLength(sortedEntries,k);
  end;

PROCEDURE T_priorityQueue.cleanup;
  VAR tmp:T_priorityQueueElement;
      i,j,k:longint;
      copyOfSorted:array of T_priorityQueueElement;
  begin
    //Bubblesort unsorted...
    for j:=1 to length(unsortedEntries)-1 do for i:=0 to j-1 do
    if unsortedEntries[i].estimatedCostToGoal<unsortedEntries[j].estimatedCostToGoal
    then begin
      tmp               :=unsortedEntries[i];
      unsortedEntries[i]:=unsortedEntries[j];
      unsortedEntries[j]:=tmp;
    end;
    //Single Merge (as implemented for mergesort)
    setLength(copyOfSorted,length(sortedEntries));
    for i:=0 to length(sortedEntries)-1 do copyOfSorted[i]:=sortedEntries[i];
    i:=0;
    j:=0;
    k:=0;
    setLength(sortedEntries,length(copyOfSorted)+length(unsortedEntries));
    while (i<length(copyOfSorted)) and (j<length(unsortedEntries)) do
      if copyOfSorted[i].estimatedCostToGoal>=unsortedEntries[j].estimatedCostToGoal
      then begin sortedEntries[k]:=copyOfSorted   [i]; inc(k); inc(i); end
      else begin sortedEntries[k]:=unsortedEntries[j]; inc(k); inc(j); end;
    while (i<length(copyOfSorted   )) do begin sortedEntries[k]:=copyOfSorted   [i]; inc(k); inc(i); end;
    while (j<length(unsortedEntries)) do begin sortedEntries[k]:=unsortedEntries[j]; inc(k); inc(j); end;
    setLength(unsortedEntries,0);
    setLength(copyOfSorted,0);
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
      //2+changeDirectionPenalty < diagonalCost+2*changeDirectionPenalty
      //2 < diagonalCost+changeDirectionPenalty

FUNCTION pathScore(CONST path:T_wirePath):double;
  VAR i:longint;
      dir:T_wireDirection;
      valid:boolean;
  begin
    if length(path)=0 then exit(infinity);
    result:=(length(path)-1)*ChangeDirectionPenalty;
    for i:=0 to length(path)-2 do begin
      dir:=directionBetween(path[i],path[i+1],valid);
      if valid then result+=maxNormDistance(path[i],path[i+1])*DirectionCost[dir]
               else result+=euklideanDistance(path[i],path[i+1]);
    end;
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point; CONST pathsToPrimeWith:T_wirePathArray; CONST directionMask:T_wireDirectionSet=AllDirections): T_wirePath;
  FUNCTION distance(CONST p:T_point):double;
    begin
      result:=sqrt(sqr(p[0]-endPoint[0])+sqr(p[1]-endPoint[1]));
    end;
  VAR nodeMap:T_nodeMap;

  FUNCTION continuePath(CONST previous:T_wirePath; CONST newPoint:T_point; CONST directionChanged:boolean):T_wirePath;
    VAR i:longint;
    begin
      if directionChanged or (length(previous)<=1) then begin
        setLength(result,length(previous)+1);
        for i:=0 to length(previous)-1 do result[i]:=previous[i];
        result[length(result)-1]:=newPoint;
      end else begin
        setLength(result,length(previous));
        for i:=0 to length(previous)-2 do result[i]:=previous[i];
        result[length(result)-1]:=newPoint;
      end;
    end;

  VAR openSet:T_priorityQueue;
      entry  :T_aStarNodeInfo;
      successfullyPrimed:boolean=false;
  PROCEDURE prime;
    VAR i,j,k:longint;
        dir:T_wireDirection;
        pathUpToHere:T_wirePath;
        pointsBetween:T_wirePath;

    begin
      initialize(pathUpToHere);
      for i:=0 to length(pathsToPrimeWith)-1 do if length(pathsToPrimeWith[i])>0 then begin
        setLength(pathUpToHere,1);
        pathUpToHere[0]:=pathsToPrimeWith[i,0];
        openSet.add(pathUpToHere,distance(pathUpToHere[0]));
        for j:=0 to length(pathsToPrimeWith[i])-2 do begin
          pointsBetween :=allPointsBetween(pathsToPrimeWith[i,j],pathsToPrimeWith[i,j+1],dir);
          for k:=1 to length(pointsBetween)-1 do begin
            try
              pathUpToHere:=continuePath(pathUpToHere,pointsBetween[k],k<=1);
              if not(nodeMap.containsKey(pointsBetween[k],entry)) then begin
                entry.p:=pointsBetween[k];
                entry.cameFrom:=dir;
                entry.costToGetThere:=0;
                entry.estimatedCostToGoal:=distance(pointsBetween[k]);
                nodeMap.put(entry);
                openSet.add(pathUpToHere,entry.estimatedCostToGoal);
              end;
            except
            end;
          end;
          successfullyPrimed:=true;
        end;
      end;
    end;

  FUNCTION sortedDirection(CONST preferred:T_wireDirection; CONST allowed:T_wireDirectionSet):T_wireDirections;
    VAR d:T_wireDirection;
        i:longint=1;
    begin
      setLength(result,8);
      if preferred in allowed then result[0]:=preferred else i:=0;
      for d in allowed do if (d<>preferred) then begin
        result[i]:=d;
        inc(i);
      end;
      setLength(result,i);
    end;

  VAR n,neighbor:T_point;
      previousDirection,dir:T_wireDirection;
      score:double;
      scoreBasis:double;
      scanRadius:double;
      directionChanged,knownPrevious,firstStep:boolean;
      nextDirections:T_wireDirections;
  begin
    nodeMap.create;
    openSet.create;
    scanRadius:=2*distance(startPoint);

    if length(pathsToPrimeWith)>0 then prime;
    if not(successfullyPrimed) then begin
      initialize(result);
      setLength(result,1);
      result[0]:=startPoint;
      openSet.add(result,distance(startPoint));
    end;

    while not openSet.isEmpty do begin
      result:=openSet.ExtractMin(scoreBasis);
      n:=result[length(result)-1];
      if (n=endPoint) then begin
        openSet.destroy;
        nodeMap.destroy;
        exit(result);
      end;
      scoreBasis-=distance(n);

      if nodeMap.containsKey(n,entry) then begin
        firstStep:=entry.costToGetThere=0;
        knownPrevious:=true;
        previousDirection:=entry.cameFrom;
        nextDirections:=sortedDirection(previousDirection,
          allowedSuccessiveDirection[previousDirection]*
          allowedDirectionsPerPoint[n[0],n[1]]*
          allowedSuccessiveDirection[previousDirection]);
      end else begin
        firstStep:=false;
        knownPrevious:=false;
        nextDirections:=sortedDirection(wd_right,allowedDirectionsPerPoint[n[0],n[1]]);
      end;

      for dir in nextDirections do begin
        score:=scoreBasis+DirectionCost[dir];
        if knownPrevious and (dir<>previousDirection) then begin
          if not(firstStep) then score+=ChangeDirectionPenalty;
          directionChanged:=true;
        end else directionChanged:=false;
        neighbor:=n+dir;
        if not(nodeMap.containsKey(neighbor,entry)) or (score<entry.costToGetThere) then begin
          entry.p:=neighbor;
          entry.cameFrom:=dir;
          entry.costToGetThere:=score;
          entry.estimatedCostToGoal:=score+distance(neighbor);
          nodeMap.put(entry);
          if distance(neighbor)<scanRadius then openSet.add(continuePath(result,neighbor,directionChanged),entry.estimatedCostToGoal);
        end;
      end;
    end;
    openSet.destroy;
    nodeMap.destroy;
    setLength(result,0);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint,endPoint:T_point):T_wirePath;
  VAR toPrimeWith:T_wirePathArray;
  begin
    if isPathFree(startPoint,endPoint,result) then exit(result);
    setLength(toPrimeWith,0);
    result:=findPath(startPoint,endPoint,toPrimeWith);
  end;

FUNCTION T_wireGraph.findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath):T_wirePathArray;
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

    if length(endPoints)>1
    then mask:=StraightDirections
    else mask:=AllDirections;

    setLength(result,length(endPoints));
    for swapTemp in initialRun do with swapTemp do
      result[idx]:=findPath(startPoint,endPoints[idx],listExceptEntry(result,idx),mask);
    setLength(initialRun,0);

    if length(endPoints)<4 then mask:=AllDirections;

    if length(endPoints)>1 then repeat
      anyImproved:=false;
      for i:=0 to length(endPoints)-1 do begin
        if length(result[i])>0 then begin
          nextPath:=findPath(startPoint,endPoints[i],listExceptEntry(result,i),mask);
          if pathScore(nextPath)<pathScore(result[i])
          then begin
            result[i]:=nextPath;
            anyImproved:=true;
          end;
        end;
      end;
    until not(anyImproved);

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

{ T_nodeMap }

CONSTRUCTOR T_nodeMap.create;
  begin
    setLength(map,0);
  end;

DESTRUCTOR T_nodeMap.destroy;
  VAR i,j:longint;
  begin
    for i:=0 to length(map)-1 do begin
      for j:=0 to length(map[i])-1 do if map[i,j]<>nil then freeMem(map[i,j],sizeOf(T_aStarNodeInfo));
      setLength(map[i],0);
    end;
    setLength(map,0);
  end;

FUNCTION T_nodeMap.containsKey(CONST p: T_point; OUT value: T_aStarNodeInfo): boolean;
  VAR info:P_aStarNodeInfo;
  begin
    if length(map      )<=p[0] then exit(false);
    if length(map[p[0]])<=p[1] then exit(false);
    info:=map[p[0],p[1]];
    if info=nil
    then exit(false)
    else begin
      value:=info^;
      result:=true;
    end;
  end;

PROCEDURE T_nodeMap.put(CONST value: T_aStarNodeInfo);
  VAR i0,i,j0,j:longint;
  begin
    if length(map)<=value.p[0] then begin
      i0:=length(map);
      setLength(map,value.p[0]+1);
      for i:=i0 to length(map)-1 do setLength(map[i],0);
    end;
    i:=value.p[0];
    if length(map[i])<=value.p[1] then begin
      j0:=length(map[i]);
      setLength(map[i],value.p[1]+1);
      for j:=j0 to length(map[i])-1 do map[i,j]:=nil;
    end;
    j:=value.p[1];
    if map[i,j]=nil then getMem(map[i,j],sizeOf(T_aStarNodeInfo));
    map[i,j]^:=value;
  end;

end.

