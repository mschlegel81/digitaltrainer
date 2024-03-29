UNIT sprites;

{$mode objfpc}{$H+}

INTERFACE
USES Graphics, BGRABitmap, wiringUtil, visuals, BGRABitmapTypes, BGRAGraphics,
  logicalGates, myGenerics, BGRACanvas;

CONST
  C_inputKey:array[0..35] of char=('1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

TYPE
  P_sprite=^T_sprite;

  T_ioMark=(iom_none,iom_correctOutput,iom_incorrectOutput,iom_disabled);

  { T_sprite }

  T_sprite=object
    private
      lastUsed:double;
      Bitmap:TBGRABitmap;
      preparedForZoom:longint;
      screenOffset:T_point;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION screenWidth:longint;
      FUNCTION screenHeight:longint;
      PROCEDURE setZoom(CONST zoom:longint); virtual; abstract;
      PROCEDURE renderAt(CONST Canvas:TCanvas; CONST zoom:longint; CONST screenPosition:T_point);
      FUNCTION isAtPixel(CONST p:T_point):boolean;
  end;

  { T_gradientSprite }

  T_gradientSprite=object(T_sprite)
    CONSTRUCTOR create;
    PROCEDURE setZoom(CONST zoom:longint); virtual;
    PROCEDURE renderRect(CONST Canvas: TCanvas; CONST zoom: longint; CONST x,y0,y1:longint);
  end;

  { T_blockSprite }
  P_blockSprite=^T_blockSprite;
  T_blockSprite=object(T_sprite)
    private
      caption:shortstring;
      width,height:longint;
      marked:boolean;
      ioMark:T_ioMark;
    protected
      PROCEDURE initBaseShape(CONST zoom:longint);
    public
      CONSTRUCTOR create(CONST caption_:string; CONST gridWidth,gridHeight:longint; CONST marked_:boolean; CONST ioMark_:T_ioMark);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  { T_adapterSprite }
  P_adapterSprite=^T_adapterSprite;
  T_adapterSprite=object(T_blockSprite)
    private
      inCount,outCount,inWidth,outWidth:byte;
    public
    CONSTRUCTOR create(CONST inWireWidth,outWireWidth:byte; CONST marked_:boolean);
    PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  { T_clockSprite }
  P_clockSprite=^T_clockSprite;
  T_clockSprite=object(T_blockSprite)
    private
      prog:longint;
      trueOut:boolean;
    public
    CONSTRUCTOR create(CONST doubleWidth,tick:boolean; CONST progress:longint; CONST marked_:boolean);
    PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  { T_blockSprite }
  P_ioBlockSprite=^T_ioBlockSprite;
  T_ioBlockSprite=object(T_blockSprite)
    private
      inIdx:longint;
    public
      CONSTRUCTOR create(CONST caption_:string; CONST inputIndex:longint; CONST marked_:boolean; CONST ioMark_:T_ioMark);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  P_ioTextSprite=^T_ioTextSprite;
  { T_ioTextSprite }

  T_ioTextSprite=object(T_blockSprite)
    private
      wireModeText:shortstring;
    public
      CONSTRUCTOR create(CONST wireMode:T_multibitWireRepresentation; CONST value:shortstring);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  T_ioState=(io_undetermined,io_high,io_low,io_multibit);
  T_ioDirection=(io_left,io_right,io_top,io_bottom);

  { T_ioSprite }
  P_ioSprite=^T_ioSprite;
  T_ioSprite=object(T_sprite)
    private
      position:T_ioDirection; //no diagonals, only main axes
      state:T_ioState;
      cap:shortstring;
    public
      CONSTRUCTOR create(CONST pos:T_ioDirection; CONST s:T_ioState; CONST caption:shortstring);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  P_7SegmentSprite=^T_7SegmentSprite;
  T_7SegmentSprite=object(T_blockSprite)
    private
      active:byte;
    public
      CONSTRUCTOR create(CONST a:byte; CONST marked_:boolean);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  P_watcherSprite=^T_watcherSprite;
  T_watcherSprite=object(T_sprite)
    anchor:T_ioDirection;
    ioLabel:shortstring;
    wire:T_wireValue;
    CONSTRUCTOR create(CONST ioLabel_:shortstring; CONST wireValue:T_wireValue; CONST ioDir:T_ioDirection);
    PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  P_binSprite=^T_binSprite;

  { T_binSprite }

  T_binSprite=object(T_sprite)
    CONSTRUCTOR create();
    PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  T_spriteMap=specialize G_stringKeyMap<P_sprite>;

PROCEDURE textOut(CONST CanvasBGRA:TBGRACanvas; CONST s:string; CONST x0,y0,x1,y1:longint; CONST textColor:longint; CONST autoRotate:boolean=true);

FUNCTION getIoSprite(CONST pos:T_ioDirection; CONST wireValue:T_wireValue; CONST caption:string):P_sprite;
FUNCTION getBlockSprite(CONST caption:shortstring; CONST gridWidth,gridHeight:longint; CONST marked:boolean; CONST ioMark:T_ioMark=iom_none):P_sprite;
FUNCTION getIoBlockSprite(CONST caption:shortstring; CONST inputIndex:longint; CONST marked:boolean; CONST ioMark:T_ioMark=iom_none):P_sprite;
FUNCTION getIoTextSprite(CONST wireValue:T_wireValue; mode:T_multibitWireRepresentation):P_sprite;
FUNCTION get7SegmentSprite(CONST wireValue: T_wireValue; CONST marked:boolean):P_sprite;
FUNCTION getWatcherSprite(CONST ioLabel_:shortstring; CONST ioIndex:longint; CONST wireValue:T_wireValue; CONST isInput,leftOrRight:boolean):P_sprite;
FUNCTION getClockSprite(CONST gated,tick:boolean; CONST interval,counter:longint; CONST marked:boolean):P_sprite;
FUNCTION getAdapterSprite(CONST inWireWidth,outWireWidth:byte; CONST marked_:boolean):P_sprite;
PROCEDURE clearSpriteCaches;
VAR gradientSprite:T_gradientSprite;
    binSprite:T_binSprite;
IMPLEMENTATION
USES sysutils,types,Classes,math,strutils;
VAR ioSpriteMap,
    blockSpriteMap,
    ioBlockSpriteMap,
    ioTextSpriteMap,
    sevSegSpriteMap,
    watcherSpriteMap,
    adapterSpriteMap,
    clockSpriteMap:T_spriteMap;
    allocationCounter:longint=0;
    lastCleanupTime:double=0;
CONST oneMinute=1/(24*60);

PROCEDURE dropOldSprites(CONST timeoutInDays:double);
  VAR threshold:double;
  PROCEDURE dropFromMap(VAR map:T_spriteMap);
    VAR entries: T_spriteMap.KEY_VALUE_LIST;
        i:longint;
    begin
      entries:=map.entrySet;
      for i:=0 to length(entries)-1 do if P_sprite(entries[i].value)^.lastUsed<threshold then map.dropKey(entries[i].key);
      setLength(entries,0);
    end;

  begin
    threshold:=now-timeoutInDays;
    dropFromMap(ioSpriteMap     );
    dropFromMap(blockSpriteMap  );
    dropFromMap(ioBlockSpriteMap);
    dropFromMap(ioTextSpriteMap );
    dropFromMap(sevSegSpriteMap );
    dropFromMap(watcherSpriteMap);
  end;

PROCEDURE disposeSprite(VAR s:P_sprite);
  begin
    dispose(s,destroy);
    s:=nil;
  end;

PROCEDURE init;
  begin
    lastCleanupTime:=now;
    ioSpriteMap     .create(@disposeSprite);
    blockSpriteMap  .create(@disposeSprite);
    ioBlockSpriteMap.create(@disposeSprite);
    ioTextSpriteMap .create(@disposeSprite);
    sevSegSpriteMap .create(@disposeSprite);
    watcherSpriteMap.create(@disposeSprite);
    adapterSpriteMap.create(@disposeSprite);
    clockSpriteMap  .create(@disposeSprite);
  end;

PROCEDURE finalize;
  begin
    ioSpriteMap     .destroy;
    blockSpriteMap  .destroy;
    ioBlockSpriteMap.destroy;
    ioTextSpriteMap .destroy;
    sevSegSpriteMap .destroy;
    watcherSpriteMap.destroy;
    adapterSpriteMap.destroy;
    clockSpriteMap  .destroy;
  end;

PROCEDURE clearSpriteCaches;
  begin
    lastCleanupTime:=now;
    ioSpriteMap     .clear;
    blockSpriteMap  .clear;
    ioBlockSpriteMap.clear;
    ioTextSpriteMap .clear;
    sevSegSpriteMap .clear;
    watcherSpriteMap.clear;
    adapterSpriteMap.clear;
    clockSpriteMap  .clear;
    gradientSprite.preparedForZoom:=-1;
    binSprite     .preparedForZoom:=-1;
  end;

PROCEDURE spriteAllocated;
  begin
    inc(allocationCounter);
    if (allocationCounter mod 1000=0) or (now>lastCleanupTime+oneMinute) then begin
      {$ifdef debugMode}
      writeln('Sprites allocated total: ',allocationCounter);
      writeln('         before cleanup: ',ioSpriteMap.size+blockSpriteMap.size+ioBlockSpriteMap.size+ioTextSpriteMap.size+sevSegSpriteMap.size+watcherSpriteMap.size);
      {$endif}
      dropOldSprites(oneMinute);
      {$ifdef debugMode}
      writeln('          after cleanup: ',ioSpriteMap.size+blockSpriteMap.size+ioBlockSpriteMap.size+ioTextSpriteMap.size+sevSegSpriteMap.size+watcherSpriteMap.size);
      {$endif}
      lastCleanupTime:=now;
    end;
  end;

FUNCTION getIoSprite(CONST pos: T_ioDirection; CONST wireValue:T_wireValue; CONST caption:string): P_sprite;
  CONST posKey  :array[T_ioDirection] of char=('l','r','t','b');
        stateKey:array[T_ioState] of char=('?','1','0','M');
  VAR state:T_ioState;
      key:string;
  begin
    if isFullyDefined(wireValue) then begin
      if wireValue.width>1
      then state:=io_multibit
      else if wireValue.bit[0]=tsv_true
           then state:=io_high
           else state:=io_low;
    end else state:=io_undetermined;

    key:=posKey[pos]+stateKey[state]+caption;
    if not ioSpriteMap.containsKey(key,result) then begin
      new(P_ioSprite(result),create(pos,state,caption));
      ioSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getBlockSprite(CONST caption: shortstring; CONST gridWidth, gridHeight: longint; CONST marked: boolean; CONST ioMark:T_ioMark=iom_none): P_sprite;
  VAR key:string;
  begin
    key:=caption+' '+intToStr(gridWidth)+' '+intToStr(gridHeight)+BoolToStr(marked,'M','');
    case ioMark of
      iom_incorrectOutput: key+='_mC';
      iom_correctOutput  : key+='_mI';
      iom_disabled       : key+='_dis';
    end;
    if not blockSpriteMap.containsKey(key,result) then begin
      new(P_blockSprite(result),create(caption,gridWidth,gridHeight,marked,ioMark));
      blockSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getIoBlockSprite(CONST caption: shortstring; CONST inputIndex:longint; CONST marked: boolean; CONST ioMark:T_ioMark): P_sprite;
  VAR key:string;

  begin
    key:=caption+' '+BoolToStr(marked,'M','')+intToStr(inputIndex);
    case ioMark of
      iom_incorrectOutput: key+='_mC';
      iom_correctOutput  : key+='_mI';
      iom_disabled       : key+='_dis';
    end;
    if not ioBlockSpriteMap.containsKey(key,result) then begin
      new(P_ioBlockSprite(result),create(caption,inputIndex,marked,ioMark));
      ioBlockSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getIoTextSprite(CONST wireValue: T_wireValue; mode: T_multibitWireRepresentation): P_sprite;
  VAR key:string;
      cap:shortstring;
  begin
    if wireValue.width<=1 then mode:=wr_binary;
    cap:=getWireString(wireValue,mode);
    key:=C_multibitWireRepresentationName[mode]+cap;

    if not ioTextSpriteMap.containsKey(key,result) then begin
      new(P_ioTextSprite(result),create(mode,cap));
      ioTextSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION get7SegmentSprite(CONST wireValue: T_wireValue; CONST marked: boolean): P_sprite;
  VAR key:string;
      valid: boolean;
      active: byte;
  begin
    active:=getDecimalValue(wireValue,valid) and 127;
    key:=BoolToStr(marked,'M','')+intToStr(active);

    if not sevSegSpriteMap.containsKey(key,result) then begin
      new(P_7SegmentSprite(result),create(active,marked));
      sevSegSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getWatcherSprite(CONST ioLabel_: shortstring; CONST ioIndex:longint; CONST wireValue: T_wireValue; CONST isInput, leftOrRight: boolean): P_sprite;
  VAR key:string;
      ioLabel:shortstring;
      dir:T_ioDirection;
  begin
    if isInput then begin
      if leftOrRight
      then begin dir:=io_right;  key:='R'; end
      else begin dir:=io_bottom; key:='B'; end;
    end else begin
      if leftOrRight
      then begin dir:=io_left;   key:='L'; end
      else begin dir:=io_top;    key:='T'; end;
    end;

    if ioLabel_=''
    then ioLabel:=BoolToStr(isInput,'in #','out #')+intToStr(ioIndex)
    else ioLabel:=BoolToStr(isInput,'in: ','out: ')+ioLabel_;

    key+=getBinaryString(wireValue)+' '+ioLabel;
    if not watcherSpriteMap.containsKey(key,result) then begin
      new(P_watcherSprite(result),create(ioLabel,wireValue,dir));
      watcherSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getClockSprite(CONST gated,tick:boolean; CONST interval,counter:longint; CONST marked:boolean):P_sprite;
  VAR key:string;
      progress:longint;
  begin
    progress:=(counter shl 8 div interval);
    if progress>255 then progress:=255 else if progress<0 then progress:=0;

    key:=intToStr(progress);
    if tick then key+='_1' else key+='_0';
    if marked then key+='M';
    if gated then key+='G';

    if not(clockSpriteMap.containsKey(key,result)) then begin
      new(P_clockSprite(result),create(gated,tick,progress,marked));
      clockSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getAdapterSprite(CONST inWireWidth,outWireWidth:byte; CONST marked_:boolean):P_sprite;
  VAR key:string;
  begin
    key:=intToStr(inWireWidth)+'_'+intToStr(outWireWidth);
    if marked_ then key+='M';

    if not(adapterSpriteMap.containsKey(key,result)) then begin
      new(P_adapterSprite(result),create(inWireWidth,outWireWidth,marked_));
      adapterSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

{ T_clockSprite }

CONSTRUCTOR T_clockSprite.create(CONST doubleWidth,tick:boolean; CONST progress:longint; CONST marked_:boolean);
  VAR w:longint=2;
  begin
    if doubleWidth then w:=4;
    inherited create('',w,2,marked_,iom_none);
    prog:=progress*128;
    if tick then prog+=32768;
    trueOut:=tick;
  end;

PROCEDURE T_clockSprite.setZoom(CONST zoom: longint);
  VAR centerX,centerY,extend,innerEx:longint;
  begin
    initBaseShape(zoom);
    centerX:=(width *zoom) div 2+screenOffset[0];
    centerY:=(height*zoom) div 2+screenOffset[1];
    extend :=round(0.75*zoom);
    innerEx:=round(0.5*zoom);
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_BORDER_COLOR;
    Bitmap.CanvasBGRA.Brush.style:=bsSolid;
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.TRUE_COLOR;
    Bitmap.CanvasBGRA.Arc65536(centerX-extend,centerY-extend,
                               centerX+extend,centerY+extend,16384,49152,[aoFillPath]);
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.FALSE_COLOR;
    Bitmap.CanvasBGRA.Arc65536(centerX-extend,centerY-extend,
                               centerX+extend,centerY+extend,49152,16384,[aoFillPath]);

    Bitmap.CanvasBGRA.Brush.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Arc65536(centerX-innerEx,centerY-innerEx,
                               centerX+innerEx,centerY+innerEx,0,65535,[aoFillPath]);

    if trueOut
    then Bitmap.CanvasBGRA.Brush.color:=colorScheme.TRUE_COLOR
    else Bitmap.CanvasBGRA.Brush.color:=colorScheme.FALSE_COLOR;
    Bitmap.CanvasBGRA.Pen.color:=Bitmap.CanvasBGRA.Brush.color;

    Bitmap.CanvasBGRA.Arc65536(centerX-extend+2,centerY-extend+2,
                               centerX+extend-2,centerY+extend-2,
                               16384-prog-512,16384-prog+512,[aoPie,aoFillPath]);
    preparedForZoom:=zoom;
  end;

{ T_adapterSprite }

CONSTRUCTOR T_adapterSprite.create(CONST inWireWidth, outWireWidth: byte; CONST marked_:boolean);
  begin
    inWidth:=inWireWidth;
    outWidth:=outWireWidth;
    if inWidth>outWidth then begin
      inCount:=1;
      outCount:=inWidth div outWidth;
    end else begin
      outCount:=1;
      inCount:=outWidth div inWidth;
    end;
    inherited create('Adapter '+intToStr(inWireWidth)+'→'+intToStr(outWireWidth),4,2*max(inCount,outCount),marked_,iom_none);

  end;

PROCEDURE T_adapterSprite.setZoom(CONST zoom: longint);
  CONST xTab:array[0..16] of double=(0,0.09980701170952077,0.19762808521908887,0.28003732550840094,0.3412714874946232,0.38729633120523604,0.42743449462932426,0.4644090098356542,0.5000004768371582,0.53559190862806827,0.5725663872555563,0.61270466521797728,0.65872964026516456,0.7199638756285448,0.8023728933200015,0.90019348247662878,1);
        yTab:array[0..16] of double=(0,0.0014656458228474722,0.021330923565097359,0.078961843334802717,0.15872999090330236,0.24095249223852566,0.32620355132116474,0.4128136062044595,0.5000011765485287,0.5871886113589848,0.6737984361708965,0.7590494801920233,0.8412717951214279,0.9210393884355061,0.9786694823143469,0.998534382952748,1);
  VAR k,j:longint;
      startY,endY:longint;
      path:array [0..16] of TPoint;
  begin
    initBaseShape(zoom);

    Bitmap.CanvasBGRA.DrawFontBackground:=true;
    textOut(Bitmap.CanvasBGRA,caption,
            screenOffset[0]            +zoom shr 1,
            screenOffset[1]            +zoom shr 1,
            screenOffset[0]+zoom*width -zoom shr 1,
            screenOffset[1]+zoom*height-zoom shr 1,
            colorScheme.SHADOW_COLOR);

    Bitmap.CanvasBGRA.Pen.color:=colorScheme.WIRE_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    if inCount=1 then begin
      case outWidth of
         1..3: Bitmap.CanvasBGRA.Pen.width:=(1*zoom) shr 4;
         4..7: Bitmap.CanvasBGRA.Pen.width:=(2*zoom) shr 4;
        8..15: Bitmap.CanvasBGRA.Pen.width:=(3*zoom) shr 4;
         else  Bitmap.CanvasBGRA.Pen.width:=(4*zoom) shr 4;
      end;
      startY:=max(inCount,outCount)*zoom+screenOffset[1];
      for k:=0 to outCount-1 do begin
        endY  :=(k*2-(outCount-1)+outCount)*zoom+screenOffset[1];
        for j:=0 to 16 do path[j]:=point(round(zoom*4*xTab[j])+screenOffset[0],round(startY+(endY-startY)*yTab[j]));
        Bitmap.CanvasBGRA.PolyBezier(path);
      end;
    end else begin
      case inWidth of
         1..3: Bitmap.CanvasBGRA.Pen.width:=(1*zoom) shr 4;
         4..7: Bitmap.CanvasBGRA.Pen.width:=(2*zoom) shr 4;
        8..15: Bitmap.CanvasBGRA.Pen.width:=(3*zoom) shr 4;
         else  Bitmap.CanvasBGRA.Pen.width:=(4*zoom) shr 4;
      end;
      endY:=max(inCount,outCount)*zoom+screenOffset[1];
      for k:=0 to inCount-1 do begin
        startY:=(k*2-(inCount-1)+inCount)*zoom+screenOffset[1];
        for j:=0 to 16 do path[j]:=point(round(zoom*4*xTab[j])+screenOffset[0],round(startY+(endY-startY)*yTab[j]));
        Bitmap.CanvasBGRA.PolyBezier(path);
      end;
    end;
    preparedForZoom:=zoom;
  end;

{ T_binSprite }

CONSTRUCTOR T_binSprite.create;
  begin
    inherited;
  end;

PROCEDURE T_binSprite.setZoom(CONST zoom: longint);
  VAR sz:longint;
      poly: array of TPoint;
  begin
    sz:=zoom*6;
    screenOffset:=pointOf(zoom*3,zoom*3);
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(sz,sz,colorScheme.BOARD_COLOR)
    else Bitmap.setSize(sz,sz);
    preparedForZoom:=zoom;

    Bitmap.CanvasBGRA.Brush.color:=colorScheme.BOARD_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psClear;
    Bitmap.CanvasBGRA.Rectangle(0,0,sz+1,sz+1);

    Bitmap.CanvasBGRA.Brush.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_BORDER_COLOR;

    setLength(poly,9);
    poly[0]:=point((sz-1)*0,(sz-1)*0);
    poly[1]:=point((sz-1)*1,(sz-1)*0);
    poly[2]:=point((sz-1)*1,round((sz-1)*0.099999999999999978));
    poly[3]:=point(round((sz-1)*0.95),round((sz-1)*0.099999999999999978));
    poly[4]:=point(round((sz-1)*0.8),(sz-1)*1);
    poly[5]:=point(round((sz-1)*0.2),(sz-1)*1);
    poly[6]:=point(round((sz-1)*0.05),round((sz-1)*0.099999999999999978));
    poly[7]:=point((sz-1)*0,round((sz-1)*0.099999999999999978));
    poly[8]:=point((sz-1)*0,(sz-1)*0);

    Bitmap.CanvasBGRA.Polygon(poly);
    textOut(Bitmap.CanvasBGRA,'♺',round(sz*0.2),round(sz*0.2),round(sz*0.8),round(sz*0.8),colorScheme.SHADOW_COLOR);

    preparedForZoom:=zoom;
  end;

{ T_gradientSprite }

CONSTRUCTOR T_gradientSprite.create();
  begin
    inherited;
  end;

PROCEDURE T_gradientSprite.setZoom(CONST zoom: longint);
  VAR b0,g0,r0,
      b1,g1,r1,
      i,step:longint;
      Canvas: TBGRACanvas;
  begin
    screenOffset:=pointOf(0,0);
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(zoom,1,colorScheme.BOARD_COLOR)
    else Bitmap.setSize(zoom,1);

    b0:= colorScheme.BOARD_COLOR         and 255;
    g0:=(colorScheme.BOARD_COLOR shr  8) and 255;
    r0:=(colorScheme.BOARD_COLOR shr 16) and 255;

    b1:= colorScheme.SHADOW_COLOR         and 255; b1-=b0; b0:=b0 shl 8;
    g1:=(colorScheme.SHADOW_COLOR shr  8) and 255; g1-=g0; g0:=g0 shl 8;
    r1:=(colorScheme.SHADOW_COLOR shr 16) and 255; r1-=r0; r0:=r0 shl 8;

    Canvas:=Bitmap.CanvasBGRA;
    for i:=0 to zoom-1 do begin
      step:=(i shl 8) div (zoom-1);
      Canvas.Pixels[i,0]:=(((b0+step*b1) shr 8)       ) or
                          (((g0+step*g1) shr 8) shl  8) or
                          (((r0+step*r1) shr 8) shl 16);
    end;
    preparedForZoom:=zoom;
  end;

PROCEDURE T_gradientSprite.renderRect(CONST Canvas: TCanvas; CONST zoom: longint; CONST x, y0, y1: longint);
  VAR rect:TRect;
  begin
    if zoom<>preparedForZoom then setZoom(zoom);
    rect.Left:=x;
    rect.width:=Bitmap.width;
    rect.top:=y0;
    rect.height:=y1-y0;
    Bitmap.draw(Canvas,rect);
  end;

{ T_watcherSprite }

CONSTRUCTOR T_watcherSprite.create(CONST ioLabel_:shortstring; CONST wireValue:T_wireValue; CONST ioDir:T_ioDirection);
  begin
    inherited create;
    ioLabel:=ioLabel_;
    wire   :=wireValue;
    anchor :=ioDir;
    setZoom(1);
  end;

PROCEDURE T_watcherSprite.setZoom(CONST zoom: longint);
  CONST rectWidth =132;
        rectHeight=60;
        tipSize   =12;
  VAR i:longint;
      width,height:longint;
      x0:longint=0;
      y0:longint=0;
      poly: array of TPoint;
  begin
    screenOffset:=pointOf(0,0);

    setLength(poly,8);

    width:=rectWidth;
    height:=rectHeight;
    case anchor of
      io_left: begin
        screenOffset:=pointOf(-(zoom shr 1),30);
        width+=tipSize;
        x0   +=tipSize;
        poly[0]:=point(tipSize,0);
        poly[1]:=point(tipSize,30-tipSize);
        poly[2]:=point(0      ,30);
        poly[3]:=point(tipSize,30+tipSize);
        poly[4]:=point(tipSize,rectHeight-1);
        poly[5]:=point(width-1,rectHeight-1);
        poly[6]:=point(width-1,0         );
        poly[7]:=poly[0];
      end;
      io_right: begin
        width+=tipSize;
        screenOffset:=pointOf(width+zoom shr 1,30);
        poly[0]:=point(0        ,0);
        poly[1]:=point(0          ,rectHeight-1);
        poly[2]:=point(rectWidth-1,rectHeight-1);
        poly[3]:=point(rectWidth-1,30+tipSize);
        poly[4]:=point(rectWidth+tipSize,30);
        poly[5]:=point(rectWidth-1,30-tipSize);
        poly[6]:=point(rectWidth-1,0);
        poly[7]:=poly[0];
      end;
      io_top: begin
        height+=tipSize;
        y0    +=tipSize;
        screenOffset:=pointOf(66,-(zoom shr 1));
        poly[0]:=point(0        ,tipSize);
        poly[1]:=point(0        ,height-1);
        poly[2]:=point(rectWidth-1,height-1);
        poly[3]:=point(rectWidth-1,tipSize);
        poly[4]:=point(66+tipSize,tipSize);
        poly[5]:=point(66,0);
        poly[6]:=point(66-tipSize,tipSize);
        poly[7]:=poly[0];
      end;
      io_bottom: begin
        height+=tipSize;
        screenOffset:=pointOf(66,height+zoom shr 1);
        poly[0]:=point(0         ,0);
        poly[1]:=point(0         ,rectHeight);
        poly[2]:=point(66-tipSize,rectHeight);
        poly[3]:=point(66,rectHeight+tipSize);
        poly[4]:=point(66+tipSize,rectHeight);
        poly[5]:=point(rectWidth-1,rectHeight);
        poly[6]:=point(rectWidth-1,0);
        poly[7]:=poly[0];
      end;
    end;

    if preparedForZoom>=0 then begin
      preparedForZoom:=zoom;
      exit;
    end;

    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(width,height,colorScheme.BOARD_COLOR)
    else Bitmap.setSize(width,height);
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_BORDER_COLOR;

    Bitmap.CanvasBGRA.Polygon(poly);

    Bitmap.CanvasBGRA.Font.orientation:=0;
    Bitmap.CanvasBGRA.Font.quality:=fqFineAntialiasing;
    Bitmap.CanvasBGRA.Font.Antialiasing:=true;
    Bitmap.CanvasBGRA.Font.height:=16;
    Bitmap.CanvasBGRA.Font.color:=colorScheme.GATE_LABEL_COLOR;
    Bitmap.CanvasBGRA.textOut(x0+2,y0+2 ,ioLabel);
    Bitmap.CanvasBGRA.textOut(x0+2,y0+18,'dec: '+getDecimalString(wire));
    Bitmap.CanvasBGRA.textOut(x0+2,y0+34,'2cmp: '+get2ComplementString(wire));

    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    Bitmap.CanvasBGRA.Pen.color:=0;
    for i:=0 to wire.width-1 do begin
      case wire.bit[i] of
        tsv_true        : Bitmap.CanvasBGRA.Brush.color:=colorScheme.TRUE_COLOR;
        tsv_false       : Bitmap.CanvasBGRA.Brush.color:=colorScheme.FALSE_COLOR;
        tsv_undetermined: Bitmap.CanvasBGRA.Brush.color:=colorScheme.UNDETERMINED_COLOR;
      end;
      Bitmap.CanvasBGRA.Rectangle(x0+2+8* i   ,y0+50,
                                  x0+2+8*(i+1),y0+50+8);
    end;
    preparedForZoom:=zoom;
  end;

{ T_7SegmentSprite }

CONSTRUCTOR T_7SegmentSprite.create(CONST a: byte; CONST marked_: boolean);
  begin
    inherited create('',4,6,marked_,iom_none);
    active:=a;
  end;

PROCEDURE T_7SegmentSprite.setZoom(CONST zoom: longint);
  VAR poly: array of TPoint;
  begin
    initBaseShape(zoom);
    Bitmap.CanvasBGRA.Pen.style:=psClear;
    setLength(poly,7);
    poly[0]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*0.90));
    poly[1]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*0.90));
    poly[2]:=point(round(screenOffset[0]+zoom*3.10),round(screenOffset[1]+zoom*0.70));
    poly[3]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*0.50));
    poly[4]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*0.50));
    poly[5]:=point(round(screenOffset[0]+zoom*0.90),round(screenOffset[1]+zoom*0.70));
    poly[6]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*0.90));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 1)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*1.00));
    poly[1]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*2.80));
    poly[2]:=point(round(screenOffset[0]+zoom*0.80),round(screenOffset[1]+zoom*3.00));
    poly[3]:=point(round(screenOffset[0]+zoom*0.60),round(screenOffset[1]+zoom*2.80));
    poly[4]:=point(round(screenOffset[0]+zoom*0.60),round(screenOffset[1]+zoom*1.00));
    poly[5]:=point(round(screenOffset[0]+zoom*0.80),round(screenOffset[1]+zoom*0.80));
    poly[6]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*1.00));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 2)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*1.00));
    poly[1]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*2.80));
    poly[2]:=point(round(screenOffset[0]+zoom*3.20),round(screenOffset[1]+zoom*3.00));
    poly[3]:=point(round(screenOffset[0]+zoom*3.00),round(screenOffset[1]+zoom*2.80));
    poly[4]:=point(round(screenOffset[0]+zoom*3.00),round(screenOffset[1]+zoom*1.00));
    poly[5]:=point(round(screenOffset[0]+zoom*3.20),round(screenOffset[1]+zoom*0.80));
    poly[6]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*1.00));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 4)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*3.30));
    poly[1]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*3.30));
    poly[2]:=point(round(screenOffset[0]+zoom*3.10),round(screenOffset[1]+zoom*3.00));
    poly[3]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*2.90));
    poly[4]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*2.90));
    poly[5]:=point(round(screenOffset[0]+zoom*0.90),round(screenOffset[1]+zoom*3.10));
    poly[6]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*3.30));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 8)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*3.40));
    poly[1]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*5.20));
    poly[2]:=point(round(screenOffset[0]+zoom*0.80),round(screenOffset[1]+zoom*5.40));
    poly[3]:=point(round(screenOffset[0]+zoom*0.60),round(screenOffset[1]+zoom*5.20));
    poly[4]:=point(round(screenOffset[0]+zoom*0.60),round(screenOffset[1]+zoom*3.40));
    poly[5]:=point(round(screenOffset[0]+zoom*0.80),round(screenOffset[1]+zoom*3.20));
    poly[6]:=point(round(screenOffset[0]+zoom*1.00),round(screenOffset[1]+zoom*3.40));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 16)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*3.40));
    poly[1]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*5.20));
    poly[2]:=point(round(screenOffset[0]+zoom*3.20),round(screenOffset[1]+zoom*5.40));
    poly[3]:=point(round(screenOffset[0]+zoom*3.00),round(screenOffset[1]+zoom*5.20));
    poly[4]:=point(round(screenOffset[0]+zoom*3.00),round(screenOffset[1]+zoom*3.40));
    poly[5]:=point(round(screenOffset[0]+zoom*3.20),round(screenOffset[1]+zoom*3.20));
    poly[6]:=point(round(screenOffset[0]+zoom*3.40),round(screenOffset[1]+zoom*3.40));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 32)>0];
    Bitmap.CanvasBGRA.Polygon(poly);

    poly[0]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*5.70));
    poly[1]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*5.70));
    poly[2]:=point(round(screenOffset[0]+zoom*3.10),round(screenOffset[1]+zoom*5.50));
    poly[3]:=point(round(screenOffset[0]+zoom*2.90),round(screenOffset[1]+zoom*5.30));
    poly[4]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*5.30));
    poly[5]:=point(round(screenOffset[0]+zoom*0.90),round(screenOffset[1]+zoom*5.50));
    poly[6]:=point(round(screenOffset[0]+zoom*1.10),round(screenOffset[1]+zoom*5.70));
    Bitmap.CanvasBGRA.Brush.color:=colorScheme.SEVEN_SEGMENT_COLOR[(active and 64)>0];
    Bitmap.CanvasBGRA.Polygon(poly);
    preparedForZoom:=zoom;
  end;

{ T_ioTextSprite }

CONSTRUCTOR T_ioTextSprite.create(CONST wireMode: T_multibitWireRepresentation; CONST value: shortstring);
  begin
    inherited create(value,4,4,false,iom_none);
    wireModeText:=C_multibitWireRepresentationName[wireMode];
  end;

PROCEDURE T_ioTextSprite.setZoom(CONST zoom: longint);
  VAR
    newWidth, newHeight: longint;
  begin
    screenOffset:=pointOf(-3,-2*zoom);
    newWidth :=4*zoom-6;
    newHeight:=2*zoom-3;
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(newWidth,newHeight,colorScheme.GATE_COLOR)
    else Bitmap.setSize(newWidth,newHeight);

    Bitmap.CanvasBGRA.Brush.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Brush.style:=bsSolid;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_BORDER_COLOR;
    Bitmap.CanvasBGRA.Rectangle(0,0,newWidth,newHeight);
    Bitmap.CanvasBGRA.DrawFontBackground:=true;
    textOut(Bitmap.CanvasBGRA,wireModeText,1,0,newWidth shr 1,newHeight,colorScheme.SHADOW_COLOR,false);

    Bitmap.CanvasBGRA.Brush.style:=bsClear;
    Bitmap.CanvasBGRA.DrawFontBackground:=false;
    textOut(Bitmap.CanvasBGRA,caption,3,3,newWidth-3,newHeight-3,colorScheme.GATE_LABEL_COLOR);
    preparedForZoom:=zoom;
  end;

{ T_ioBlockSprite }

CONSTRUCTOR T_ioBlockSprite.create(CONST caption_: string; CONST inputIndex:longint; CONST marked_: boolean; CONST ioMark_:T_ioMark);
  begin
    inherited create(caption_,4,4,marked_,ioMark_);
    inIdx:=inputIndex;
  end;

{ T_ioSprite }

CONSTRUCTOR T_ioSprite.create(CONST pos: T_ioDirection; CONST s: T_ioState; CONST caption:shortstring);
  begin
    inherited create;
    position:=pos;
    state:=s;
    cap:=caption;
  end;

PROCEDURE T_ioSprite.setZoom(CONST zoom: longint);
  VAR radius:longint;
      c:longint;
      r,g,b,alpha:word;
      baseR,baseG,baseB:word;
      transparentCol:TBGRAPixel;

  PROCEDURE addCol(CONST color:longint);
    begin
      r+= color         and 255;
      g+=(color shr  8) and 255;
      b+=(color shr 16) and 255;
    end;

  PROCEDURE getColorAt(CONST fx,fy:double);
    VAR dtc,fz:double;
        level:longint;
    begin
      dtc:=sqrt(fx*fx+fy*fy);
      if dtc>radius then exit;
      if dtc>radius-1.5 then begin
        case position of
          io_left  : dtc:= fx/radius;
          io_right : dtc:=-fx/radius;
          io_top   : dtc:= fy/radius;
          io_bottom: dtc:=-fy/radius;
        end;
        if dtc<0 then level:=0 else level:=round(dtc*256);
        if level<0 then level:=0 else if level>256 then level:=256;
        alpha+=255;
        r+=((256-level)*( colorScheme.GATE_BORDER_COLOR         and 255)+level*( colorScheme.WIRE_COLOR         and 255)) shr 8;
        g+=((256-level)*((colorScheme.GATE_BORDER_COLOR shr  8) and 255)+level*((colorScheme.WIRE_COLOR shr  8) and 255)) shr 8;
        b+=((256-level)*((colorScheme.GATE_BORDER_COLOR shr 16) and 255)+level*((colorScheme.WIRE_COLOR shr 16) and 255)) shr 8;
      end else begin
        fz:=sqrt(1-sqr(dtc/radius))*radius;
        dtc:=1/sqrt(sqr(fx)+sqr(fy)+sqr(fz));

        dtc:=fx*dtc*-0.40824829046386307
            +fy*dtc*-0.40824829046386307
            +fz*dtc* 0.8164965809277261 ;
        dtc:=sqr(sqr(sqr(sqr(dtc))));

        alpha+=255;
        r+=round(baseR*(1-dtc)+dtc*255);
        g+=round(baseG*(1-dtc)+dtc*255);
        b+=round(baseB*(1-dtc)+dtc*255);
      end;
    end;

  CONST sub:array[0..7] of double=(-0.4375,-0.3125,-0.1875,-0.0625,0.0625,0.1875,0.3125,0.4375);

      //(0.0625,0.1875,0.3125,0.4375,0.5625,0.6875,0.8125,0.9375);

  VAR x,y:longint;
      subX,subY:double;
      px: PCardinal;
      captioned:boolean=false;
      ll:longint;
  begin
    case state of
      io_undetermined: begin baseR:=colorScheme.UNDETERMINED_COLOR and 255; baseG:=(colorScheme.UNDETERMINED_COLOR shr 8) and 255; baseB:=(colorScheme.UNDETERMINED_COLOR shr 16) and 255; end;
      io_high        : begin baseR:=colorScheme.TRUE_COLOR         and 255; baseG:=(colorScheme.TRUE_COLOR         shr 8) and 255; baseB:=(colorScheme.TRUE_COLOR         shr 16) and 255; end;
      io_low         : begin baseR:=colorScheme.FALSE_COLOR        and 255; baseG:=(colorScheme.FALSE_COLOR        shr 8) and 255; baseB:=(colorScheme.FALSE_COLOR        shr 16) and 255; end;
      io_multibit    : begin baseR:=colorScheme.MULTIBIT_COLOR     and 255; baseG:=(colorScheme.MULTIBIT_COLOR     shr 8) and 255; baseB:=(colorScheme.MULTIBIT_COLOR     shr 16) and 255; end;
    end;
    radius:=zoom shr 1;
    screenOffset:=pointOf(radius,radius);
    c:=radius;
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(radius*2+1,radius*2+1,0)
    else Bitmap            .setSize(radius*2+1,radius*2+1);

    Bitmap.CanvasBGRA.Brush.color:=0;
    Bitmap.CanvasBGRA.Rectangle(0,0,radius*2+1,radius*2+1,true);
    if cap<>'' then begin
      captioned:=true;
      x:=round(radius*0.2);
      Bitmap.CanvasBGRA.AntialiasingMode:=amOn;

      transparentCol.alpha:=255;
      transparentCol.RED:=0;
      transparentCol.GREEN:=0;
      transparentCol.BLUE:=0;
      Bitmap.CanvasBGRA.Brush.BGRAColor:=transparentCol;
      Bitmap.CanvasBGRA.Brush.style:=bsSolid;

      Bitmap.CanvasBGRA.DrawFontBackground:=true;
      textOut(Bitmap.CanvasBGRA,cap,x,x,radius*2-x,radius*2-x,$00FFFFFF);    //This will be replaced by the actual color down below
    end;

    for y:=0 to radius*2 do begin
      px:=PCardinal(Bitmap.ScanLine[y]);
      for x:=0 to radius*2 do begin
        r:=0; g:=0; b:=0; alpha:=0;
        for subX in sub do for subY in sub do getColorAt(x+subX-c,y+subY-c);
        alpha:=alpha shr 6;
        b:=b shr 6;
        g:=g shr 6;
        r:=r shr 6;
        if captioned then begin
          ll:=px^ and 255;
          if ll>0 then begin
            r:=((ll*( colorScheme.GATE_LABEL_COLOR         and 255))+(256-ll)*r) shr 8;
            g:=((ll*((colorScheme.GATE_LABEL_COLOR shr  8) and 255))+(256-ll)*g) shr 8;
            b:=((ll*((colorScheme.GATE_LABEL_COLOR shr 16) and 255))+(256-ll)*b) shr 8;
          end;
        end;
        px^:=(b       ) or
             (g shl  8) or
             (r shl 16) or
             (alpha shl 24);
        inc(px);
      end;
    end;

    preparedForZoom:=zoom;
  end;

{ T_blockSprite }

PROCEDURE textOut(CONST CanvasBGRA:TBGRACanvas; CONST s: string; CONST x0, y0, x1, y1: longint; CONST textColor: longint; CONST autoRotate:boolean=true);
  VAR
    lines       :T_arrayOfString;
    line:string;
    maxTextWidth:longint=0;
    textHeight  :longint=0;
    yTally:longint=0;
    extend: TSize;
    rotate:boolean=false;
    fontSizeFactor:double;

  PROCEDURE updateTextExtend;
    VAR s:string;
    begin
      maxTextWidth:=0;
      textHeight  :=0;
      for s in lines do begin
        extend:=CanvasBGRA.TextExtent(s);
        textHeight+=extend.cy;
        if extend.cx>maxTextWidth then maxTextWidth:=extend.cx;
      end;
    end;

  FUNCTION split(CONST s:ansistring; CONST splitters:T_arrayOfString; CONST retainSplitters:boolean=false):T_arrayOfString;
    PROCEDURE nextSplitterPos(CONST startSearchAt:longint; OUT splitterStart,splitterEnd:longint); inline;
      VAR splitter:string;
          i:longint;
      begin
        splitterStart:=length(s)+1;
        splitterEnd:=splitterStart;
        for splitter in splitters do if length(splitter)>0 then begin
          i:=PosEx(splitter,s,startSearchAt);
          if (i>0) and (i<splitterStart) then begin
            splitterStart:=i;
            splitterEnd:=i+length(splitter);
          end;
        end;
      end;

    VAR resultLen:longint=0;
    PROCEDURE appendToResult(CONST part:string); inline;
      begin
        if length(result)<resultLen+1 then setLength(result,round(length(result)*1.2)+2);
        result[resultLen]:=part;
        inc(resultLen);
      end;

    VAR partStart:longint=1;
        splitterStart,splitterEnd:longint;
        endsOnSplitter:boolean=false;
    begin
      setLength(result,0);
      nextSplitterPos(partStart,splitterStart,splitterEnd);
      endsOnSplitter:=splitterEnd>splitterStart;
      while(partStart<=length(s)) do begin
        appendToResult(copy(s,partStart,splitterStart-partStart));
        partStart:=splitterEnd;
        endsOnSplitter:=splitterEnd>splitterStart;
        if endsOnSplitter and retainSplitters then appendToResult(copy(s,splitterStart,splitterEnd-splitterStart));
        nextSplitterPos(partStart,splitterStart,splitterEnd);
      end;
      if endsOnSplitter and not retainSplitters then appendToResult('');
      setLength(result,resultLen);
    end;

  begin
    if (x1<x0+5) or (y1<y0+5) then exit;
    lines:=split(s,LineEnding);

    CanvasBGRA.Font.orientation:=0;
    CanvasBGRA.Font.quality:=fqFineAntialiasing;
    CanvasBGRA.Font.Antialiasing:=true;
    updateTextExtend;
    if (maxTextWidth=0) or (textHeight=0) then exit;

    //fit for aspect ratio:
    if ((y1-y0)>(x1-x0)) and (maxTextWidth>textHeight) and autoRotate then begin
      fontSizeFactor:=min((x1-x0)/textHeight,(y1-y0)/maxTextWidth);
      rotate:=true;
    end else begin
      fontSizeFactor:=min((y1-y0)/textHeight,(x1-x0)/maxTextWidth);
    end;

    CanvasBGRA.Font.height:=round(CanvasBGRA.Font.height*fontSizeFactor);
    CanvasBGRA.Font.color:=textColor;

    updateTextExtend;

    if rotate then begin
      CanvasBGRA.Font.orientation:=2700;
      yTally:=(x0+x1) shr 1 + textHeight shr 1;
      for line in lines do begin
        CanvasBGRA.textOut(yTally,(y0+y1) shr 1 - CanvasBGRA.textWidth (line) shr 1,line);
        yTally-=                                  CanvasBGRA.textHeight(line);
      end;
    end else begin
      CanvasBGRA.Font.orientation:=0;
      yTally:=(y0+y1) shr 1 - textHeight shr 1;
      for line in lines do begin
        CanvasBGRA.textOut((x0+x1) shr 1 - CanvasBGRA.textWidth (line) shr 1,yTally,line);
        yTally+=                           CanvasBGRA.textHeight(line);
      end;
    end;

  end;

PROCEDURE T_blockSprite.initBaseShape(CONST zoom: longint);
  VAR newWidth,newHeight:longint;
  PROCEDURE drawGlowingFrame(CONST FrameColor:longint);
    VAR i:longint;
        bw,mw:longint;
    begin
      for i:=1 to 3 do begin
        bw:=(i-1)*256 div 3;
        mw:=256-bw;

        Bitmap.CanvasBGRA.Pen.color:=
        (((( colorScheme.BOARD_COLOR         and 255)*bw+( FrameColor         and 255)*mw) shr 8) and 255) or
        (((((colorScheme.BOARD_COLOR shr  8) and 255)*bw+((FrameColor shr  8) and 255)*mw) shr 8) and 255) shl 8 or
        (((((colorScheme.BOARD_COLOR shr 16) and 255)*bw+((FrameColor shr 16) and 255)*mw) shr 8) and 255) shl 16;

        Bitmap.CanvasBGRA.Rectangle(3          -i+screenOffset[0],
                                    3          -i+screenOffset[1],
                                    newWidth -3+i+screenOffset[0],
                                    newHeight-3+i+screenOffset[1],false);
      end;
    end;

  PROCEDURE drawShadow;
    VAR i:longint;
    begin
      for i:=1 to 4 do begin

        Bitmap.CanvasBGRA.Pen.color:=
         round( colorScheme.BOARD_COLOR         and 255*i/5+ colorScheme.SHADOW_COLOR         and 255*(1-i/5)) or
         round((colorScheme.BOARD_COLOR shr  8) and 255*i/5+(colorScheme.SHADOW_COLOR shr  8) and 255*(1-i/5)) shl 8 or
         round((colorScheme.BOARD_COLOR shr 16) and 255*i/5+(colorScheme.SHADOW_COLOR shr 16) and 255*(1-i/5)) shl 16;

        Bitmap.CanvasBGRA.MoveTo(3+i         +screenOffset[0],newHeight-4+i                );
        Bitmap.CanvasBGRA.LineTo(newWidth-4+i                ,newHeight-4+i                );
        Bitmap.CanvasBGRA.LineTo(newWidth-4+i                ,          2+i+screenOffset[1]);
      end;
    end;

  begin
    if not marked and (ioMark in [iom_none,iom_disabled])
    then screenOffset:=pointOf(-3,-3)
    else screenOffset:=pointOf(0,0);

    newWidth :=width *zoom+screenOffset[0];
    newHeight:=height*zoom+screenOffset[1];
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(newWidth,newHeight,colorScheme.BOARD_COLOR)
    else Bitmap.setSize(newWidth,newHeight);

    Bitmap.CanvasBGRA.Brush.color:=colorScheme.BOARD_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psClear;
    Bitmap.CanvasBGRA.Rectangle(0,0,newWidth+1,newHeight+1);

    if ioMark=iom_disabled
    then Bitmap.CanvasBGRA.Brush.color:=colorScheme.BOARD_COLOR
    else Bitmap.CanvasBGRA.Brush.color:=colorScheme.GATE_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;
    Bitmap.CanvasBGRA.Pen.color:=colorScheme.GATE_BORDER_COLOR;

    Bitmap.CanvasBGRA.Rectangle(3+screenOffset[0],
                                3+screenOffset[1],
                      newWidth -3                ,
                      newHeight-3                );
    if marked
    then drawGlowingFrame(colorScheme.MARK_COLOR)
    else case ioMark of
      iom_none: drawShadow;
      iom_correctOutput:   drawGlowingFrame(colorScheme.CORRECT_COLOR);
      iom_incorrectOutput: drawGlowingFrame(colorScheme.INCORRECT_COLOR);
    end;
  end;

CONSTRUCTOR T_blockSprite.create(CONST caption_: string; CONST gridWidth, gridHeight: longint; CONST marked_: boolean; CONST ioMark_:T_ioMark);
  begin
    inherited create;
    caption:=caption_;
    width:=gridWidth;
    height:=gridHeight;
    marked:=marked_;
    ioMark:=ioMark_;
  end;

PROCEDURE T_blockSprite.setZoom(CONST zoom: longint);
  VAR textColor:longint;
  begin
    initBaseShape(zoom);
    Bitmap.CanvasBGRA.DrawFontBackground:=true;

    if ioMark=iom_disabled
    then textColor:=colorScheme.SHADOW_COLOR
    else textColor:=colorScheme.GATE_LABEL_COLOR;

    textOut(Bitmap.CanvasBGRA,caption,
            screenOffset[0]            +zoom shr 1,
            screenOffset[1]            +zoom shr 1,
            screenOffset[0]+zoom*width -zoom shr 1,
            screenOffset[1]+zoom*height-zoom shr 1,
            textColor);
    preparedForZoom:=zoom;
  end;

PROCEDURE T_ioBlockSprite.setZoom(CONST zoom: longint);
  begin
    initBaseShape(zoom);
    Bitmap.CanvasBGRA.Pen.color:=0;
    Bitmap.CanvasBGRA.MoveTo(screenOffset[0]           +3,screenOffset[1]+(zoom*height   shr 1));
    Bitmap.CanvasBGRA.LineTo(screenOffset[0]+zoom*width-3,screenOffset[1]+(zoom*height   shr 1));
    if (inIdx>=0) and (inIdx<length(C_inputKey)) then begin
      Bitmap.CanvasBGRA.DrawFontBackground:=true;
      textOut(Bitmap.CanvasBGRA,
              C_inputKey[inIdx],
              screenOffset[0]+(width-1)*zoom-3,
              screenOffset[1]               +4,
              screenOffset[0]+(width  )*zoom-6,
              screenOffset[1]+zoom,0);
      Bitmap.CanvasBGRA.MoveTo(screenOffset[0]+(width-1)*zoom-3,screenOffset[1]+3);
      Bitmap.CanvasBGRA.LineTo(screenOffset[0]+(width-1)*zoom-3,screenOffset[1]+zoom);
      Bitmap.CanvasBGRA.LineTo(screenOffset[0]+(width  )*zoom-6,screenOffset[1]+zoom);
    end;
    Bitmap.CanvasBGRA.DrawFontBackground:=false;
    textOut(Bitmap.CanvasBGRA,
           caption,
            screenOffset[0]            +zoom shr 1,
            screenOffset[1]            +zoom shr 1,
            screenOffset[0]+ zoom*width-zoom shr 1,
            screenOffset[1]+(zoom*height div 2),
            colorScheme.GATE_LABEL_COLOR);
    preparedForZoom:=zoom;
  end;

{ T_sprite }

CONSTRUCTOR T_sprite.create;
  begin
    lastUsed:=now;
    Bitmap:=nil;
    preparedForZoom:=-1;
  end;

DESTRUCTOR T_sprite.destroy;
  begin
    if Bitmap<>nil then Bitmap.free;
  end;

FUNCTION T_sprite.screenWidth: longint;
  begin
    result:=Bitmap.width;
  end;

FUNCTION T_sprite.screenHeight: longint;
  begin
    result:=Bitmap.height;
  end;

PROCEDURE T_sprite.renderAt(CONST Canvas: TCanvas; CONST zoom: longint; CONST screenPosition: T_point);
  begin
    if zoom<>preparedForZoom then setZoom(zoom);
    if (screenPosition[0]-screenOffset[0]>Canvas.width) or
       (screenPosition[0]-screenOffset[0]+Bitmap.width<0) or
       (screenPosition[1]-screenOffset[1]>Canvas.height) or
       (screenPosition[1]-screenOffset[1]+Bitmap.height<0) then exit;
    Bitmap.draw(Canvas,
                screenPosition[0]-screenOffset[0],
                screenPosition[1]-screenOffset[1],false);
  end;

FUNCTION T_sprite.isAtPixel(CONST p: T_point): boolean;
  begin
    result:=(p[0]>=-screenOffset[0]) and (p[0]<=Bitmap.width -screenOffset[0]) and
            (p[1]>=-screenOffset[1]) and (p[1]<=Bitmap.height-screenOffset[1]);
  end;

INITIALIZATION
  gradientSprite.create;
  binSprite.create;
  init;

FINALIZATION
  gradientSprite.destroy;
  binSprite.destroy;
  finalize;

end.

