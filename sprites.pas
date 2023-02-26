UNIT sprites;

{$mode objfpc}{$H+}

INTERFACE
USES Graphics, BGRABitmap, wiringUtil, visuals, BGRABitmapTypes, BGRAGraphics,
  logicalGates, myGenerics;

CONST
  C_inputKey:array[0..35] of char=('1','2','3','4','5','6','7','8','9','0','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

TYPE
  P_sprite=^T_sprite;

  { T_sprite }

  T_sprite=object
    private
      lastUsed:double;
      Bitmap:TBGRABitmap;
      preparedForZoom:longint;
      screenOffset:T_point;
    protected
      PROCEDURE textOut(CONST s:string; CONST x0,y0,x1,y1:longint; CONST textColor:longint=$00FFFFFF);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION screenWidth:longint;
      FUNCTION screenHeight:longint;
      PROCEDURE setZoom(CONST zoom:longint); virtual; abstract;
      PROCEDURE renderAt(CONST Canvas:TCanvas; CONST zoom:longint; CONST screenPosition:T_point);
  end;

  { T_blockSprite }
  P_blockSprite=^T_blockSprite;
  T_blockSprite=object(T_sprite)
    private
      caption:string;
      width,height:longint;
      marked:boolean;
    protected
      PROCEDURE initBaseShape(CONST zoom:longint);
    public
      CONSTRUCTOR create(CONST caption_:string; CONST gridWidth,gridHeight:longint; CONST marked_:boolean);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  { T_blockSprite }
  P_ioBlockSprite=^T_ioBlockSprite;
  T_ioBlockSprite=object(T_blockSprite)
    private
      inIdx:longint;
    public
      CONSTRUCTOR create(CONST caption_:string; CONST inputIndex:longint; CONST marked_:boolean);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  P_ioTextSprite=^T_ioTextSprite;
  { T_ioTextSprite }

  T_ioTextSprite=object(T_blockSprite)
    private
      wireModeText:string;
    public
      CONSTRUCTOR create(CONST wireMode:T_multibitWireRepresentation; CONST value:string);
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
      cap:string;
    public
      CONSTRUCTOR create(CONST pos:T_ioDirection; CONST s:T_ioState; CONST caption:string);
      PROCEDURE setZoom(CONST zoom:longint); virtual;
  end;

  T_spriteMap=specialize G_stringKeyMap<P_sprite>;

FUNCTION getIoSprite(CONST pos:T_ioDirection; CONST wireValue:T_wireValue; CONST caption:string):P_sprite;
FUNCTION getBlockSprite(CONST caption:string; CONST gridWidth,gridHeight:longint; CONST marked:boolean):P_sprite;
FUNCTION getIoBlockSprite(CONST caption:string; CONST inputIndex:longint; CONST marked:boolean):P_sprite;
FUNCTION getIoTextSprite(CONST wireValue:T_wireValue; mode:T_multibitWireRepresentation):P_sprite;
IMPLEMENTATION
USES sysutils,myStringUtil,types,Classes,math;
VAR ioSpriteMap,
    blockSpriteMap,
    ioBlockSpriteMap,
    ioTextSpriteMap:T_spriteMap;
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
  end;

PROCEDURE finalize;
  begin
    ioSpriteMap     .destroy;
    blockSpriteMap  .destroy;
    ioBlockSpriteMap.destroy;
    ioTextSpriteMap .destroy;
  end;

PROCEDURE spriteAllocated;
  begin
    inc(allocationCounter);
    if (allocationCounter mod 1000=0) or (now>lastCleanupTime+oneMinute) then begin
      writeln('Sprites allocated total: ',allocationCounter);
      writeln('         before cleanup: ',ioSpriteMap.size+blockSpriteMap.size+ioBlockSpriteMap.size+ioTextSpriteMap.size);
      dropOldSprites(oneMinute);
      writeln('          after cleanup: ',ioSpriteMap.size+blockSpriteMap.size+ioBlockSpriteMap.size+ioTextSpriteMap.size);
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

FUNCTION getBlockSprite(CONST caption: string; CONST gridWidth, gridHeight: longint; CONST marked: boolean): P_sprite;
  VAR key:string;
  begin
    key:=caption+' '+intToStr(gridWidth)+' '+intToStr(gridHeight)+BoolToStr(marked,'M','');
    if not blockSpriteMap.containsKey(key,result) then begin
      new(P_blockSprite(result),create(caption,gridWidth,gridHeight,marked));
      blockSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getIoBlockSprite(CONST caption: string; CONST inputIndex:longint; CONST marked: boolean): P_sprite;
  VAR key:string;
  begin
    key:=caption+' '+BoolToStr(marked,'M','')+intToStr(inputIndex);
    if not ioBlockSpriteMap.containsKey(key,result) then begin
      new(P_ioBlockSprite(result),create(caption,inputIndex,marked));
      ioBlockSpriteMap.put(key,result);
      spriteAllocated;
    end;
    result^.lastUsed:=now;
  end;

FUNCTION getIoTextSprite(CONST wireValue: T_wireValue; mode: T_multibitWireRepresentation): P_sprite;
  VAR key:string;
      cap:string;
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

{ T_ioTextSprite }

CONSTRUCTOR T_ioTextSprite.create(CONST wireMode: T_multibitWireRepresentation; CONST value: string);
  begin
    inherited create(value,4,4,false);
    wireModeText:=C_multibitWireRepresentationName[wireMode];
  end;

PROCEDURE T_ioTextSprite.setZoom(CONST zoom: longint);
  VAR
    newWidth, newHeight: longint;
  begin
    screenOffset:=pointOf(-4,-2-2*zoom);
    newWidth :=4*zoom-8;
    newHeight:=2*zoom-6;
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(newWidth,newHeight,GATE_COLOR)
    else Bitmap.setSize(newWidth,newHeight);

    Bitmap.CanvasBGRA.Brush.color:=GATE_COLOR;
    Bitmap.CanvasBGRA.Brush.style:=bsSolid;
    Bitmap.CanvasBGRA.Pen.style:=psClear;
    Bitmap.CanvasBGRA.Rectangle(0,0,newWidth,newHeight);
    Bitmap.CanvasBGRA.DrawFontBackground:=true;
    textOut(wireModeText,0,0,newWidth shr 1,newHeight,SHADOW_COLOR);

    Bitmap.CanvasBGRA.Brush.style:=bsClear;
    Bitmap.CanvasBGRA.DrawFontBackground:=false;
    textOut(caption     ,newWidth shr 3,0,newWidth,newHeight);
    preparedForZoom:=zoom;
  end;

{ T_ioBlockSprite }

CONSTRUCTOR T_ioBlockSprite.create(CONST caption_: string; CONST inputIndex:longint; CONST marked_: boolean);
  begin
    inherited create(caption_,4,4,marked_);
    inIdx:=inputIndex;
  end;

{ T_ioSprite }

CONSTRUCTOR T_ioSprite.create(CONST pos: T_ioDirection; CONST s: T_ioState; CONST caption:string);
  begin
    inherited create;
    position:=pos;
    state:=s;
    cap:=caption;
  end;

PROCEDURE T_ioSprite.setZoom(CONST zoom: longint);
  VAR radius:longint;
      c:longint;
      r,g,b:word;
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
        level:word;
    begin
      dtc:=sqrt(fx*fx+fy*fy);
      if dtc>radius then begin
        case position of
          io_left  : if fx<0 then addCol(GATE_COLOR) else addCol(BOARD_COLOR);
          io_right : if fx<0 then addCol(BOARD_COLOR) else addCol(GATE_COLOR);
          io_top   : if fy<0 then addCol(GATE_COLOR) else addCol(BOARD_COLOR);
          io_bottom: if fy<0 then addCol(BOARD_COLOR) else addCol(GATE_COLOR);
        end;
      end else if dtc>radius-1 then begin
        case position of
          io_left  : dtc:= fx/radius;
          io_right : dtc:=-fx/radius;
          io_top   : dtc:= fy/radius;
          io_bottom: dtc:=-fy/radius;
        end;
        if dtc<0 then level:=0 else level:=round(dtc*255);
        r+=level;
        g+=level;
        b+=level;
      end else begin
        fz:=sqrt(1-sqr(dtc/radius))*radius;
        dtc:=1/sqrt(sqr(fx)+sqr(fy)+sqr(fz));

        dtc:=fx*dtc*-0.40824829046386307
            +fy*dtc*-0.40824829046386307
            +fz*dtc* 0.8164965809277261 ;
        dtc:=sqr(sqr(sqr(sqr(dtc))));

        r+=round(baseR*(1-dtc)+dtc*255);
        g+=round(baseG*(1-dtc)+dtc*255);
        b+=round(baseB*(1-dtc)+dtc*255);
      end;
    end;

  CONST sub:array[0..7] of double=(0.0625,0.1875,0.3125,0.4375,0.5625,0.6875,0.8125,0.9375);

  VAR x,y:longint;
      subX,subY:double;
      px: PCardinal;
      captioned:boolean=false;
      ll:longint;
  begin
    case state of
      io_undetermined: begin baseR:=BOARD_COLOR and 255; baseG:=(BOARD_COLOR shr 8) and 255; baseB:=(BOARD_COLOR shr 16) and 255; end;
      io_high        : begin baseR:=255; baseG:=128; baseB:=0; end;
      io_low         : begin baseR:=0;   baseG:=0;   baseB:=0; end;
      io_multibit    : begin baseR:=128; baseG:=128; baseB:=128; end;
    end;
    radius:=zoom shr 1;
    screenOffset:=pointOf(radius,radius);
    c:=radius;
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(radius*2+1,radius*2+1,BOARD_COLOR)
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
      textOut(cap,x,x,radius*2-x,radius*2-x);
    end;

    for y:=0 to radius*2 do begin
      px:=PCardinal(Bitmap.ScanLine[y]);
      for x:=0 to radius*2 do begin
        r:=0; g:=0; b:=0;
        for subX in sub do for subY in sub do getColorAt(x+subX-c,y+subY-c);
        b:=b shr 6;
        g:=g shr 6;
        r:=r shr 6;
        if captioned then begin
          ll:=px^ and 255;
          if ll>0 then begin
            r:=ll+((256-ll)*r) shr 8;
            g:=ll+((256-ll)*g) shr 8;
            b:=ll+((256-ll)*b) shr 8;
          end;
        end;
        px^:=(b       ) or
             (g shl  8) or
             (r shl 16);
        inc(px);
      end;
    end;

    preparedForZoom:=zoom;
  end;

{ T_blockSprite }

PROCEDURE T_sprite.textOut(CONST s: string; CONST x0, y0, x1, y1: longint; CONST textColor:longint=$00FFFFFF);
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
        extend:=Bitmap.CanvasBGRA.TextExtent(s);
        textHeight+=extend.cy;
        if extend.cx>maxTextWidth then maxTextWidth:=extend.cx;
      end;
    end;

  begin
    lines:=split(s,LineEnding);

    Bitmap.CanvasBGRA.Font.orientation:=0;
    Bitmap.CanvasBGRA.Font.quality:=fqFineAntialiasing;
    Bitmap.CanvasBGRA.Font.Antialiasing:=true;
    updateTextExtend;

    //fit for aspect ratio:
    if ((y1-y0)>(x1-x0)) and (maxTextWidth>textHeight) then begin
      fontSizeFactor:=min((x1-x0)/textHeight,(y1-y0)/maxTextWidth);
      rotate:=true;
    end else begin
      fontSizeFactor:=min((y1-y0)/textHeight,(x1-x0)/maxTextWidth);
    end;

    Bitmap.CanvasBGRA.Font.height:=round(Bitmap.CanvasBGRA.Font.height*fontSizeFactor);
    Bitmap.CanvasBGRA.Font.color:=textColor;

    updateTextExtend;

    if rotate then begin
      Bitmap.CanvasBGRA.Font.orientation:=2700;
      yTally:=(x0+x1) shr 1 + textHeight shr 1;
      for line in lines do begin
        Bitmap.CanvasBGRA.textOut(yTally,(y0+y1) shr 1 - Bitmap.CanvasBGRA.textWidth (line) shr 1,line);
        yTally-=                                         Bitmap.CanvasBGRA.textHeight(line);
      end;
    end else begin
      Bitmap.CanvasBGRA.Font.orientation:=0;
      yTally:=(y0+y1) shr 1 - textHeight shr 1;
      for line in lines do begin
        Bitmap.CanvasBGRA.textOut((x0+x1) shr 1 - Bitmap.CanvasBGRA.textWidth (line) shr 1,yTally,line);
        yTally+=                                  Bitmap.CanvasBGRA.textHeight(line);
      end;
    end;

  end;

PROCEDURE T_blockSprite.initBaseShape(CONST zoom: longint);
  VAR newWidth,newHeight:longint;
  PROCEDURE drawGlowingFrame;
    VAR i:longint;
        bw,mw:longint;
    begin
      for i:=1 to 3 do begin
        bw:=(i-1)*256 div 3;
        mw:=256-bw;

        Bitmap.CanvasBGRA.Pen.color:=
        (((( BOARD_COLOR         and 255)*bw+( MARK_COLOR         and 255)*mw) shr 8) and 255) or
        (((((BOARD_COLOR shr  8) and 255)*bw+((MARK_COLOR shr  8) and 255)*mw) shr 8) and 255) shl 8 or
        (((((BOARD_COLOR shr 16) and 255)*bw+((MARK_COLOR shr 16) and 255)*mw) shr 8) and 255) shl 16;

        Bitmap.CanvasBGRA.Rectangle(3          -i,
                                    3          -i,
                                    newWidth -3+i,
                                    newHeight-3+i,false);
      end;
    end;

  PROCEDURE drawShadow;
    VAR i:longint;
        bw:longint;
    begin
      for i:=1 to 4 do begin
        bw:=i*256 div 4;

        Bitmap.CanvasBGRA.Pen.color:=
        ((( BOARD_COLOR         and 255)*bw shr 8) and 255) or
        ((((BOARD_COLOR shr  8) and 255)*bw shr 8) and 255) shl 8 or
        ((((BOARD_COLOR shr 16) and 255)*bw shr 8) and 255) shl 16;

        Bitmap.CanvasBGRA.MoveTo(3+i         ,newHeight-4+i);
        Bitmap.CanvasBGRA.LineTo(newWidth-4+i,newHeight-4+i);
        Bitmap.CanvasBGRA.LineTo(newWidth-4+i,3+i);
      end;
    end;

  begin
    screenOffset:=pointOf(0,0);
    newWidth :=width *zoom;
    newHeight:=height*zoom;
    if Bitmap=nil
    then Bitmap:=TBGRABitmap.create(newWidth,newHeight,BOARD_COLOR)
    else Bitmap.setSize(newWidth,newHeight);

    Bitmap.CanvasBGRA.Brush.color:=BOARD_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psClear;
    Bitmap.CanvasBGRA.Rectangle(0,0,newWidth+1,newHeight+1);

    Bitmap.CanvasBGRA.Brush.color:=GATE_COLOR;
    Bitmap.CanvasBGRA.Pen.style:=psSolid;

    Bitmap.CanvasBGRA.Pen.color:=0;

    Bitmap.CanvasBGRA.Rectangle(3,
                                3,
                                newWidth -3,
                                newHeight-3);
    if marked
    then drawGlowingFrame
    else drawShadow;
  end;

CONSTRUCTOR T_blockSprite.create(CONST caption_: string; CONST gridWidth, gridHeight: longint; CONST marked_: boolean);
  begin
    inherited create;
    caption:=caption_;
    width:=gridWidth;
    height:=gridHeight;
    marked:=marked_;
  end;

PROCEDURE T_blockSprite.setZoom(CONST zoom: longint);
  begin
    initBaseShape(zoom);
    Bitmap.CanvasBGRA.DrawFontBackground:=true;
    textOut(caption,
            screenOffset[0]            +zoom shr 1,
            screenOffset[1]            +zoom shr 1,
            screenOffset[0]+zoom*width -zoom shr 1,
            screenOffset[1]+zoom*height-zoom shr 1);
    preparedForZoom:=zoom;
  end;

PROCEDURE T_ioBlockSprite.setZoom(CONST zoom: longint);
  begin
    initBaseShape(zoom);
    Bitmap.CanvasBGRA.Pen.color:=0;
    Bitmap.CanvasBGRA.MoveTo(screenOffset[0]           ,screenOffset[1]+(zoom*height   shr 1));
    Bitmap.CanvasBGRA.LineTo(screenOffset[0]+zoom*width,screenOffset[1]+(zoom*height   shr 1));
    if (inIdx>=0) and (inIdx<length(C_inputKey)) then begin
      Bitmap.CanvasBGRA.DrawFontBackground:=true;
      textOut(C_inputKey[inIdx],
              screenOffset[0]+(width-1)*zoom,
              screenOffset[1]               +4,
              screenOffset[0]+(width  )*zoom-3,
              screenOffset[1]+zoom,0);
      Bitmap.CanvasBGRA.MoveTo(screenOffset[0]+(width-1)*zoom,screenOffset[1]+3);
      Bitmap.CanvasBGRA.LineTo(screenOffset[0]+(width-1)*zoom,screenOffset[1]+zoom);
      Bitmap.CanvasBGRA.LineTo(screenOffset[0]+(width  )*zoom-3,screenOffset[1]+zoom);
    end;
    Bitmap.CanvasBGRA.DrawFontBackground:=false;
    textOut(caption,
            screenOffset[0]            +zoom shr 1,
            screenOffset[1]            +zoom shr 1,
            screenOffset[0]+ zoom*width-zoom shr 1,
            screenOffset[1]+(zoom*height div 2));
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

PROCEDURE T_sprite.renderAt(CONST Canvas:TCanvas; CONST zoom:longint; CONST screenPosition: T_point);
  begin
    if zoom<>preparedForZoom then setZoom(zoom);
    if (screenPosition[0]-screenOffset[0]>Canvas.width) or
       (screenPosition[0]-screenOffset[0]+Bitmap.width<0) or
       (screenPosition[1]-screenOffset[1]>Canvas.height) or
       (screenPosition[1]-screenOffset[1]+Bitmap.height<0) then exit;

    Bitmap.draw(Canvas,
                screenPosition[0]-screenOffset[0],
                screenPosition[1]-screenOffset[1],true);
  end;

INITIALIZATION
  init;

FINALIZATION
  finalize;

end.

