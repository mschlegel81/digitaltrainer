UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil;
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
      width,height:longint;
      allowedDirections:array of T_wireDirectionSet;

      FUNCTION findPath(CONST startPoint,endPoint:T_point; CONST pathsToPrimeWith:T_wirePathArray; CONST exhaustiveScan:boolean; CONST directionMask:T_wireDirectionSet=AllDirections):T_wirePath;
      PROCEDURE dropWireSection(CONST a,b:T_point; CONST diagonalsOnly:boolean=false);
      FUNCTION isPathFree(CONST startPoint,endPoint:T_point; VAR path:T_wirePath):boolean;
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      DESTRUCTOR destroy;
      PROCEDURE initDirections;
      PROCEDURE dropEdges(CONST i:T_point; CONST dirs:T_wireDirectionSet);
      PROCEDURE dropNode(CONST i:T_point);
      PROCEDURE addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
      PROCEDURE dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION findPath(CONST startPoint,endPoint:T_point; CONST toPrimeWith:T_wirePathArray):T_wirePath;
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
FUNCTION pathContains(CONST path:T_wirePath; CONST x,y:longint; OUT orientation:T_wireDirection):boolean;
PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p:T_point);
FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper):T_point;
FUNCTION maxNormDistance(CONST x,y:T_point):longint;
FUNCTION euklideanDistance(CONST x, y: T_point): double;
FUNCTION pathScore(CONST path:T_wirePath):double;

FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
FUNCTION lineCrossesRectangle(CONST a0,a1,rectangleOrigin,rectangleExtend:T_point):boolean;

VAR enableShortcuts:boolean=false;
    allowDiagonals :boolean=true;
IMPLEMENTATION
USES math,sysutils;
FUNCTION linesIntersect(CONST a0, a1, b0, b1: T_point): boolean;
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

FUNCTION lineCrossesRectangle(CONST a0, a1, rectangleOrigin,
  rectangleExtend: T_point): boolean;
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

OPERATOR*(CONST x, y: T_point): longint;
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

CONSTRUCTOR T_wireGraph.create(CONST width_, height_: longint);
  begin
    width:=width_;
    height:=height_;
    setLength(allowedDirections,width*height);
    initDirections;
  end;

DESTRUCTOR T_wireGraph.destroy;
  begin
  end;

PROCEDURE T_wireGraph.initDirections;
  VAR x,y,i:longint;
      dirs:T_wireDirectionSet;
  begin
    if allowDiagonals
    then dirs:=AllDirections
    else dirs:=StraightDirections;

    i:=0;
    for y:=0 to height-1 do for x:=0 to width-1 do begin
      allowedDirections[i]:=dirs;
      if y=height-1 then allowedDirections[i]-=[wd_down ,wd_leftDown ,wd_rightDown];
      if y=0        then allowedDirections[i]-=[wd_up   ,wd_leftUp   ,wd_rightUp  ];
      if x=width-1  then allowedDirections[i]-=[wd_right,wd_rightDown,wd_rightUp ];
      if x=0        then allowedDirections[i]-=[wd_left ,wd_leftDown ,wd_leftUp];
      inc(i);
    end;
  end;

PROCEDURE T_wireGraph.dropEdges(CONST i: T_point; CONST dirs: T_wireDirectionSet);
  VAR j:T_point;
      dir:T_wireDirection;
  begin
    if (i[0]<0) or (i[0]>=width) or (i[1]<0) or (i[1]>=height) then exit;
    allowedDirections[i[0]+i[1]*width]-=dirs;
    for dir in dirs do begin
      j:=i+dir;
      if (j[0]>=0) and (j[0]<width) and
         (j[1]>=0) and (j[1]<height) then
      Exclude(allowedDirections[j[0]+j[1]*width],OppositeDirection[dir]);
    end;
  end;

PROCEDURE T_wireGraph.addUnidirectionalEdge(CONST from: T_point; CONST dir: T_wireDirection);
  begin
    if (from[0]<0) or (from[0]>=width) or
       (from[1]<0) or (from[1]>=height) then exit;
    include(allowedDirections[from[0]+from[1]*width],dir);
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

PROCEDURE T_wireGraph.dropWireSection(CONST a, b: T_point;
  CONST diagonalsOnly: boolean);
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

PROCEDURE T_wireGraph.dropWire(CONST path: T_wirePath;
  CONST diagonalsOnly: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(path)-2 do dropWireSection(path[i],path[i+1],diagonalsOnly);
  end;

FUNCTION T_wireGraph.isPathFree(CONST startPoint, endPoint: T_point; VAR path: T_wirePath): boolean;
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
      if allowedDirections[p[0]+p[1]*width]=[] then exit(false);
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

FUNCTION pathScore(CONST path: T_wirePath): double;
  CONST DirectionCost:array[T_wireDirection] of double=(1,1.5,
                                                        1,1.5,
                                                        1,1.5,
                                                        1,1.5);
      ChangeDirectionPenalty=1;
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

FUNCTION expandPath(CONST path:T_wirePath):T_wirePath;
  VAR i,j,j0:longint;
      dir:T_wireDirection;
      intermediate:T_wirePath;
  begin
    if length(path)<=1 then exit(path);
    setLength(result,1);
    result[0]:=path[0];
    for i:=1 to length(path)-1 do begin
      intermediate:=allPointsBetween(path[i-1],path[i],dir);
      j0:=length(result)-1;
      setLength(result,j0+length(intermediate));
      for j:=1 to length(intermediate)-1 do result[j+j0]:=intermediate[j];
    end;
  end;

FUNCTION pathContains(CONST path: T_wirePath; CONST x, y: longint; OUT orientation: T_wireDirection): boolean;
  VAR i:longint;
      intermediate: T_wirePath;
      p:T_point;
  begin
    if length(path)<=0 then exit(false);
    for i:=1 to length(path)-1 do begin
      intermediate:=allPointsBetween(path[i-1],path[i],orientation);
      for p in intermediate do if (p[0]=x) and (p[1]=y) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point; CONST pathsToPrimeWith: T_wirePathArray; CONST exhaustiveScan: boolean; CONST directionMask: T_wireDirectionSet): T_wirePath;
  CONST UNVISITED=maxLongint;
  TYPE T_scan=record
         p:T_point;
         dirCount:longint;
         dir    :array[0..7] of T_wireDirection;
       end;
  CONST DirectionCost:array[T_wireDirection] of longint=(2,3,
                                                         2,3,
                                                         2,3,
                                                         2,3);
        ChangeDirectionPenalty=4;

  VAR map:array of record comeFrom:T_point; score:longint; end;
      pointToExamine:array of T_point;
      pointToExamine1:longint=0;
      pointToExamine0:longint=0;
  PROCEDURE addPointToExamine(CONST p:T_point);
    VAR i:longint;
    begin
      if pointToExamine1>=length(pointToExamine) then begin
        if pointToExamine0>0 then begin
          for i:=0 to length(pointToExamine)-pointToExamine0-1 do pointToExamine[i]:=pointToExamine[i+pointToExamine0];
          dec(pointToExamine1,pointToExamine0);
          pointToExamine0:=0;
        end;
        if pointToExamine1*2>length(pointToExamine) then        setLength(pointToExamine,pointToExamine1*2);
      end;
      pointToExamine[pointToExamine1]:=p;
      inc(pointToExamine1);
    end;

  FUNCTION nextPointToExamine:T_scan;
    VAR tmp:array[-1..7] of record dtt:double; dir:T_wireDirection; end;

        i:longint=0;
        j:longint;
        dir:T_wireDirection;
    begin
      result.p:=pointToExamine[pointToExamine0];
      inc(pointToExamine0);
      result.dirCount:=0;
      for dir in directionMask*allowedDirections[result.p[0]+result.p[1]*width] do begin
        tmp[result.dirCount].dir:=dir;
        tmp[result.dirCount].dtt:=euklideanDistance(endPoint,result.p+dir);
        for j:=0 to result.dirCount-1 do if tmp[result.dirCount].dtt>tmp[j].dtt then begin
          tmp[-1]:=tmp[result.dirCount]; tmp[result.dirCount]:=tmp[j]; tmp[j]:=tmp[-1];
        end;
        inc(result.dirCount);
      end;
      for j:=0 to result.dirCount-1 do result.dir[j]:=tmp[j].dir;
    end;

  PROCEDURE prime;
    VAR path:T_wirePath;
        k,j:longint;
        point:T_point;
        aggregatedCost:longint;
        dir:T_wireDirection;
        directionIsValid: boolean;
    begin
      addPointToExamine(startPoint);
      map[startPoint[0]+startPoint[1]*width].score:=0;

      for path in pathsToPrimeWith do begin
        aggregatedCost:=0;
        if length(path)>1 then dir:=directionBetween(path[0],path[1],directionIsValid);
        for k:=1 to length(path)-1 do begin
          if (k=1) or (path[k-1]-path[k-2]=path[k]-path[k-1])
          then aggregatedCost+=DirectionCost[dir]
          else begin
            directionBetween(path[k],path[k-1],directionIsValid);
            aggregatedCost+=DirectionCost[dir]+ChangeDirectionPenalty;
          end;
          point:=path[k];
          with map[point[0]+point[1]*width] do if score>(aggregatedCost shr 1) then begin
            if score=UNVISITED then begin
              addPointToExamine(point);
            end;
            score:=aggregatedCost shr 1;
            comeFrom:=path[k-1];
          end;
        end;
      end;
    end;

  PROCEDURE rescore(CONST cf:T_point);
    VAR x,y:longint;
        newScore, cf_score:longint;
        cf_dir: T_point;
        directionIsValid: boolean;
    begin
      cf_score:=map[cf[0]+cf[1]*width].score;
      cf_dir  :=cf-map[cf[0]+cf[1]*width].comeFrom;

      for y:=max(0,cf[1]-1) to min(height-1,cf[1]+1) do
      for x:=max(0,cf[0]-1) to min(width -1,cf[0]+1) do
      if map[x+y*width].comeFrom=cf then begin
        newScore:=cf_score+DirectionCost[directionBetween(cf,pointOf(x,y),directionIsValid)];
        if pointOf(x,y)-cf<>cf_dir then newScore+=ChangeDirectionPenalty;
        if newScore<map[x+y*width].score then begin
          map[x+y*width].score:=newScore;
          rescore(pointOf(x,y));
        end;
      end;
    end;

  VAR scan: T_scan;
      i,j, newCost:longint;
      found:boolean=false;
      prevStep:T_wireDirection;
      directionIsValid, needRescore: boolean;
      next: T_point;
  begin
    if (startPoint[0]<0) or (endPoint[0]<0) or (startPoint[1]<0) or (endPoint[1]<0) or
       (startPoint[0]>=width) or (endPoint[0]>=width) or (startPoint[1]>=height) or (endPoint[1]>=height) then begin
      setLength(result,0);
      exit(result);
    end;

    setLength(pointToExamine,16);
    setLength(map,width*height);
    for i:=0 to length(map)-1 do with map[i] do begin score:=UNVISITED; comeFrom:=startPoint; end;

    prime;

    while (pointToExamine1>pointToExamine0) and not found do begin// and (exhaustiveScan or not found) do begin
      scan:=nextPointToExamine;
//      writeln('Scan: ',scan.p[0],',',scan.p[1],' towards ',endPoint[0],',',endPoint[1]);
      prevStep:=directionBetween(map[scan.p[0]+scan.p[1]*width].comeFrom,scan.p,directionIsValid);

      for i:=0 to scan.dirCount-1 do begin
        next:=scan.p+scan.dir[i];
        newCost:=map[scan.p[0]+scan.p[1]*width].score+DirectionCost[scan.dir[i]];
        if directionIsValid and (prevStep<>scan.dir[i]) then newCost+=ChangeDirectionPenalty;
        if newCost<map[next[0]+next[1]*width].score then begin
          found:=found or (next=endPoint);
          with map[next[0]+next[1]*width] do begin
            if score=UNVISITED then begin
              addPointToExamine(next);
              needRescore:=false;
            end else needRescore:=true;
            score:=newCost;
            comeFrom:=scan.p;
          end;
          if needRescore then rescore(next);
        end;
      end;

    end;
    if found then begin
      scan.p:=endPoint;
      next:=scan.p;
      setLength(result,map[endPoint[0]+endPoint[1]*width].score);
      j:=0;
      i:=0;
      while (next<>startPoint) and (i<length(result)) do begin
        result[i]:=scan.p; inc(i);
        next:=scan.p;
        scan.p:=map[scan.p[0]+scan.p[1]*width].comeFrom;
      end;
      setLength(result,i);
      for i:=0 to (length(result)-1) div 2 do begin
        j:=length(result)-1-i;
        scan.p:=result[j];
        result[j]:=result[i];
        result[i]:=scan.p;
      end;
    end else begin
      setLength(result,0);
    end;
    setLength(map,0);

  end;

//FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point;
//  CONST pathsToPrimeWith: T_wirePathArray; CONST exhaustiveScan: boolean;
//  CONST directionMask: T_wireDirectionSet): T_wirePath;
//  CONST UNVISITED=maxLongint;
//  CONST DirectionCost:array[T_wireDirection] of longint=(2,3,
//                                                         2,3,
//                                                         2,3,
//                                                         2,3);
//        ChangeDirectionPenalty=4;
//
//  VAR map:array of record comeFrom:T_point; score:longint; end;
//      pointToExamine:array of T_point;
//      n0:longint=0;
//      n1:longint=0;
//      j:longint=1;
//
//  PROCEDURE prime;
//    VAR path:T_wirePath;
//        k,j:longint;
//        point:T_point;
//        aggregatedCost:longint;
//        dir:T_wireDirection;
//        directionIsValid: boolean;
//    begin
//      pointToExamine[0]:=startPoint;
//      map[startPoint[0]+startPoint[1]*width].score:=0;
//      inc(n0);
//      for path in pathsToPrimeWith do begin
//        aggregatedCost:=0;
//        if length(path)>1 then dir:=directionBetween(path[0],path[1],directionIsValid);
//        for k:=1 to length(path)-1 do begin
//          if (k=1) or (path[k-1]-path[k-2]=path[k]-path[k-1])
//          then aggregatedCost+=DirectionCost[dir]
//          else begin
//            directionBetween(path[k],path[k-1],directionIsValid);
//            aggregatedCost+=DirectionCost[dir]+ChangeDirectionPenalty;
//          end;
//          point:=path[k];
//          with map[point[0]+point[1]*width] do if score>(aggregatedCost shr 1) then begin
//            if score=UNVISITED then begin
//              pointToExamine[n0]:=point;
//              j:=n0;
//              while (j>0) and (euklideanDistance(pointToExamine[j-1],endPoint)<euklideanDistance(pointToExamine[j],endPoint)) do begin
//                point:=pointToExamine[j]; pointToExamine[j]:=pointToExamine[j-1]; pointToExamine[j-1]:=point; dec(j);
//              end;
//              inc(n0);
//            end;
//            score:=aggregatedCost shr 1;
//            comeFrom:=path[k-1];
//          end;
//        end;
//      end;
//      j :=n0;
//    end;
//
//  PROCEDURE rescore(CONST cf:T_point);
//    VAR x,y:longint;
//        newScore, cf_score:longint;
//        cf_dir: T_point;
//        directionIsValid: boolean;
//    begin
//      cf_score:=map[cf[0]+cf[1]*width].score;
//      cf_dir  :=cf-map[cf[0]+cf[1]*width].comeFrom;
//
//      for y:=max(0,cf[1]-1) to min(height-1,cf[1]+1) do
//      for x:=max(0,cf[0]-1) to min(width -1,cf[0]+1) do
//      if map[x+y*width].comeFrom=cf then begin
//        newScore:=cf_score+DirectionCost[directionBetween(cf,pointOf(x,y),directionIsValid)];
//        if pointOf(x,y)-cf<>cf_dir then newScore+=ChangeDirectionPenalty;
//        if newScore<map[x+y*width].score then begin
//          map[x+y*width].score:=newScore;
//          rescore(pointOf(x,y));
//        end;
//      end;
//    end;
//
//  VAR i0:longint=0;
//      i1:longint;
//      i:longint=0;
//      found:boolean=false;
//      searching:boolean=true;
//      II:longint;
//      prevStep,
//      dir:T_wireDirection;
//      xy,xyNeighbor:T_point;
//      newCost:longint;
//      directionIsValid: boolean;
//      //startTicks: qword;
//      needRescore:boolean;
//  begin
//    if (startPoint[0]<0) or (endPoint[0]<0) or (startPoint[1]<0) or (endPoint[1]<0) or
//       (startPoint[0]>=width) or (endPoint[0]>=width) or (startPoint[1]>=height) or (endPoint[1]>=height) then begin
//     setLength(result,0);
//     exit(result);
//    end;
//
//    setLength(map,width*height);
//    i1:=width*height;
//    for j:=0 to height-1 do for i:=0 to width-1 do with map[i+j*width] do begin
//      score:=UNVISITED;
//      comeFrom:=startPoint;
//    end;
//    setLength(pointToExamine,i1*2);
//
//    prime;
//
//    while searching and (exhaustiveScan or not found) do begin
//      searching:=false;
//      for II:=i0 to i0+n0-1 do begin
//        xy:=pointToExamine[II];
//
//        prevStep:=directionBetween(map[xy[0]+xy[1]*width].comeFrom,xy,directionIsValid);
//        for dir in directionMask*allowedDirections[xy[0]+xy[1]*width] do begin
//          newCost:=map[xy[0]+xy[1]*width].score+DirectionCost[dir];
//          if directionIsValid and (prevStep<>dir) then newCost+=ChangeDirectionPenalty;
//          xyNeighbor:=xy+dir;
//          if (newCost<map[xyNeighbor[0]+xyNeighbor[1]*width].score) then begin
//            found:=found or (xyNeighbor=endPoint);
//            with map[xyNeighbor[0]+xyNeighbor[1]*width] do begin
//              if score=UNVISITED then begin
//                pointToExamine[i1+n1]:=xyNeighbor;
//                n1+=1;
//                needRescore:=false;
//              end else needRescore:=true;
//              score:=newCost;
//              comeFrom:=xy;
//            end;
//            if needRescore then rescore(xyNeighbor);
//            searching:=true;
//          end;
//        end;
//      end;
//      j+=1;
//      II:=i0+n0; i0:=i1; i1:=II; n0:=n1; n1:=0;
//    end;
//    if found then begin
//      xy:=endPoint;
//      xyNeighbor:=xy;
//      setLength(result,map[endPoint[0]+endPoint[1]*width].score);
//      j:=0;
//      i:=0;
//      while (xyNeighbor<>startPoint) and (i<length(result)) do begin
//        result[i]:=xy; inc(i);
//        xyNeighbor:=xy;
//        xy:=map[xy[0]+xy[1]*width].comeFrom;
//      end;
//      setLength(result,i);
//      for i:=0 to (length(result)-1) div 2 do begin
//        j:=length(result)-1-i;
//        xy:=result[j];
//        result[j]:=result[i];
//        result[i]:=xy;
//      end;
//    end else begin
//      setLength(result,0);
//    end;
//    setLength(map,0);
//  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point): T_wirePath;
  VAR toPrimeWith:T_wirePathArray;
  begin
    if isPathFree(startPoint,endPoint,result) then exit(result);
    setLength(toPrimeWith,0);
    result:=simplifyPath(findPath(startPoint,endPoint,toPrimeWith,false));
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint,endPoint:T_point; CONST toPrimeWith:T_wirePathArray):T_wirePath;
  VAR expanded:T_wirePathArray;
      k:longint;
  begin
    if isPathFree(startPoint,endPoint,result) then exit(result);
    setLength(expanded,length(toPrimeWith));
    for k:=0 to length(expanded)-1 do expanded[k]:=expandPath(toPrimeWith[k]);
    result:=simplifyPath(findPath(startPoint,endPoint,expanded,false));
  end;

FUNCTION T_wireGraph.findPaths(CONST startPoint: T_point; CONST endPoints: T_wirePath; CONST exhaustiveScan: boolean): T_wirePathArray;
  TYPE T_indexAndDist=record
         idx:longint;
         dist:double;
       end;

  VAR nextPath:T_wirePath;
      initialRun:array of T_indexAndDist;
      swapTemp:T_indexAndDist;
      i,j,k:longint;
      anyImproved: boolean;
      mask:T_wireDirectionSet;
      pt:T_point;
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

    if (length(endPoints)>4)
    then mask:=StraightDirections
    else mask:=AllDirections;

    setLength(initialRun,length(endPoints));
    setLength(result,length(endPoints));
    for i:=0 to length(endPoints)-1 do begin
      initialRun[i].idx:=i;
      initialRun[i].dist:=euklideanDistance(startPoint,endPoints[i]);
      for j:=0 to i-1 do if initialRun[i].dist>initialRun[j].dist then begin
        swapTemp     :=initialRun[i];
        initialRun[i]:=initialRun[j];
        initialRun[j]:=swapTemp;
      end;
    end;

    for i:=0 to length(initialRun)-1 do with initialRun[i] do begin
      result[idx]:=findPath(startPoint,endPoints[idx],listExceptEntry(result,idx),exhaustiveScan,mask);
      for j:=i+1 to length(initialRun)-1 do begin
        for pt in result[idx] do initialRun[j].dist:=min(initialRun[j].dist,euklideanDistance(endPoints[initialRun[j].idx],pt));
        for k:=i+1 to j-1 do if initialRun[j].dist<initialRun[k].dist then begin
          swapTemp     :=initialRun[k];
          initialRun[i]:=initialRun[j];
          initialRun[k]:=swapTemp;
        end;
      end;
    end;

    if length(endPoints)>1 then repeat
      anyImproved:=false;
      for swapTemp in initialRun do with swapTemp do begin
        if length(result[idx])>0 then begin
          nextPath:=findPath(startPoint,endPoints[idx],listExceptEntry(result,idx),exhaustiveScan,mask);
          if pathScore(nextPath)<pathScore(result[idx])
          then begin
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

FUNCTION T_wireGraph.anyEdgeLeadsTo(CONST endPoint: T_point): boolean;
  VAR d:T_wireDirection;
      p:T_point;
  begin
    for d in AllDirections do begin
      p:=endPoint+d;
      if (p[0]>=0) and (p[0]<width) and
         (p[1]>=0) and (p[1]<height) and
         (OppositeDirection[d] in allowedDirections[p[0]+p[1]*width]) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.isWireAllowed(CONST path: T_wirePath): boolean;
  VAR p:T_point;
      i,k,len:longint;
      dir:T_wireDirection;
      valid: boolean;
  begin
    for p in path do if (p[0]<0)
    or (p[1]<0)
    or (p[0]>=width)
    or (p[1]>=height) then exit(false);

    for i:=0 to length(path)-2 do begin
      p:=path[i];
      dir:=directionBetween(p,path[i+1],valid);
      if valid then begin
        len:=maxNormDistance(path[i],path[i+1]);
        for k:=0 to len-1 do begin
          if not(dir in allowedDirections[p[0]+p[1]*width]) then exit(false);
          p+=dir;
        end;
      end;
    end;
    result:=true;
  end;

end.

