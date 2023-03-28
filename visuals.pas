UNIT visuals;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, StdCtrls,ExtCtrls,Forms;

TYPE  T_shapeAndLabel=record colorIndex:byte; Shape:TShape; labl:TLabel; end;
      T_colorScheme  =record
        ENABLED_BUTTON_COLOR ,
        DISABLED_BUTTON_COLOR,
        ENABLED_TEXT_COLOR   ,
        DISABLED_TEXT_COLOR  ,
        GATE_COLOR           ,
        GATE_LABEL_COLOR     ,
        MARK_COLOR           ,
        BOARD_COLOR          ,
        SHADOW_COLOR         ,
        CORRECT_COLOR        ,
        INCORRECT_COLOR      ,
        WIRE_COLOR           ,
        BOARD_BOUNDARY_COLOR ,
        TRUE_COLOR        ,
        FALSE_COLOR       ,
        UNDETERMINED_COLOR,
        MULTIBIT_COLOR    :longint;

        SEVEN_SEGMENT_COLOR:array[false..true] of longint;

        buttonColorTable:array[0..10] of longint;
        tableColor,
        tableFixedColor,
        tableAlternativeColor,
        editorBackgroundColor,
        secondaryFormColor,
        panelColor,
        GATE_BORDER_COLOR,
        MENU_BORDER_COLOR:longint;
      end;

VAR colorScheme:T_colorScheme;
FUNCTION getColorSchemeIndex:longint;
PROCEDURE setColorScheme(CONST index:longint);
PROCEDURE setEnableButton(Shape:TShape; CONST labl:TLabel; CONST enable:boolean);
PROCEDURE applyColorScheme(CONST form:TForm);
IMPLEMENTATION
USES Controls, Graphics, Grids,ValEdit;
VAR colorSchemeIndex:longint;

FUNCTION settingsFileName:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.visuals');
  end;

PROCEDURE saveSettins;
  VAR handle:textFile;
  begin
    assign(handle,settingsFileName);
    rewrite(handle);
    writeln(handle,colorSchemeIndex);
    close(handle);
  end;

PROCEDURE loadSettings;
  VAR handle:textFile;
  begin
    try
      assign(handle,settingsFileName);
      reset(handle);
      readln(handle,colorSchemeIndex);
      close(handle);
    except
      colorSchemeIndex:=0;
    end;
    setColorScheme(colorSchemeIndex);
  end;

FUNCTION getColorSchemeIndex: longint;
  begin
    result:=colorSchemeIndex;
  end;

PROCEDURE setColorScheme(CONST index: longint);
CONST DEFAULT_SCHEME:T_colorScheme=
      (ENABLED_BUTTON_COLOR :$00603030;
       DISABLED_BUTTON_COLOR:$00703838;
       ENABLED_TEXT_COLOR   :$00FFFFFF;
       DISABLED_TEXT_COLOR  :$00909090;
       GATE_COLOR           :$00603030;
       GATE_LABEL_COLOR     :$00FFFFFF;
       MARK_COLOR           :$0000AAFF;
       BOARD_COLOR          :$00804040;
       SHADOW_COLOR         :$00402020;
       CORRECT_COLOR        :$0000FF00;
       INCORRECT_COLOR      :$000000ff;
       WIRE_COLOR           :$00FFFFFF;
       BOARD_BOUNDARY_COLOR :$00603030;
       TRUE_COLOR           :$000080ff;
       FALSE_COLOR          :0;
       UNDETERMINED_COLOR   :$00402020;
       MULTIBIT_COLOR       :$00808080;
       SEVEN_SEGMENT_COLOR  :($00402020,$0000AAFF);

       buttonColorTable:($00603030,$00703838,$00804040,$00904848,$00A05050,$00B05858,$00BF5F5F,$00CF6767,$00DF6F6F,$00EF7777,$00FF7F7F);

       tableColor     :$00804040;
       tableFixedColor:$00603030;
       tableAlternativeColor:$00603030;
       editorBackgroundColor:$00703838;
       secondaryFormColor: $00703838;
       panelColor:$00703838;
       GATE_BORDER_COLOR:0;
       MENU_BORDER_COLOR:0);

       BLACK_ON_WHITE_SCHEME:T_colorScheme=
      (ENABLED_BUTTON_COLOR :$00FFFFFF;
       DISABLED_BUTTON_COLOR:$00E0E0E0;
       ENABLED_TEXT_COLOR   :$00000000;
       DISABLED_TEXT_COLOR  :$00505050;
       GATE_COLOR           :$00FFFFFF;
       GATE_LABEL_COLOR     :$00000000;
       MARK_COLOR           :$0000AAFF;
       BOARD_COLOR          :$00FFFFFF;
       SHADOW_COLOR         :$00e0e0e0;
       CORRECT_COLOR        :$00008000;
       INCORRECT_COLOR      :$000000ff;
       WIRE_COLOR           :$00000000;
       BOARD_BOUNDARY_COLOR :$00c0c0c0;
       TRUE_COLOR           :$0080ff80;
       FALSE_COLOR          :$008080ff;
       UNDETERMINED_COLOR   :$00FFFFFF;
       MULTIBIT_COLOR       :$00A0A0A0;
       SEVEN_SEGMENT_COLOR  :($00E0E0E0,$00000000);

       buttonColorTable:($00FFFFFF,$00EEEEEE,$00DDDDDD,$00CCCCCC,$00BBBBBB,$00AAAAAA,$00999999,$00888888,$00777777,$00666666,$00555555);

       tableColor     :$00FFFFFF;
       tableFixedColor:$00E0E0E0;
       tableAlternativeColor:$00E0E0E0;
       editorBackgroundColor:$00FFFFFF;
       secondaryFormColor: $00FFFFFF;
       panelColor:$00FFFFFF;
       GATE_BORDER_COLOR:0;
       MENU_BORDER_COLOR:0);

       NEON_SCHEME:T_colorScheme=(
       ENABLED_BUTTON_COLOR :$00500000;
       DISABLED_BUTTON_COLOR:$00800000;
       ENABLED_TEXT_COLOR   :$0000FF00;
       DISABLED_TEXT_COLOR  :$00808000;
       GATE_COLOR           :$00500000;
       GATE_LABEL_COLOR     :$000000ff;
       MARK_COLOR           :$0000FFFF;
       BOARD_COLOR          :$00500000;
       SHADOW_COLOR         :$00000000;
       CORRECT_COLOR        :$0000FF00;
       INCORRECT_COLOR      :$000000ff;
       WIRE_COLOR           :$0000FF00;
       BOARD_BOUNDARY_COLOR :$00FF0000;
       TRUE_COLOR           :$0000FF00;
       FALSE_COLOR          :$00000040;
       UNDETERMINED_COLOR   :$00808080;
       MULTIBIT_COLOR       :$00500000;
       SEVEN_SEGMENT_COLOR  :($00300000,$0000FF00);

       buttonColorTable:($00500000,$00700000,$00900000,$00B00000,$00D00000,$00F00000,$00FF1000,$00FF3000,$00FF5000,$00FF7000,$00FF9000);

       tableColor     :$00300000;
       tableFixedColor:$00000000;
       tableAlternativeColor:$00000000;
       editorBackgroundColor:$00000000;
       secondaryFormColor: $00600000;
       panelColor:$00700000;
       GATE_BORDER_COLOR:$00500080;
       MENU_BORDER_COLOR:$00FF0000);

       RUST_SCHEME:T_colorScheme=
       (ENABLED_BUTTON_COLOR :$00004080;
        DISABLED_BUTTON_COLOR:$00404040;
        ENABLED_TEXT_COLOR   :$00E0E0E0;
        DISABLED_TEXT_COLOR  :$00606060;
        GATE_COLOR           :$000060B0;
        GATE_LABEL_COLOR     :$00909090;
        MARK_COLOR           :$002090ff;
        BOARD_COLOR          :$00002040;
        SHADOW_COLOR         :$00000000;
        CORRECT_COLOR        :$00008000;
        INCORRECT_COLOR      :$00000080;
        WIRE_COLOR           :$00aaaaaa;
        BOARD_BOUNDARY_COLOR :$00001020;
        TRUE_COLOR           :$000080ff;
        FALSE_COLOR          :0;
        UNDETERMINED_COLOR   :$00002040;
        MULTIBIT_COLOR       :$00808080;
        SEVEN_SEGMENT_COLOR  :($00004080,$00000000);

        buttonColorTable:($00004080,$00003870,$00003060,$00002850,$00002040,$00001830,$00001020,$00000810,$00000000,$00000000,$00000000);

        tableColor     :$00004080;
        tableFixedColor:$00002040;
        tableAlternativeColor:$00002040;
        editorBackgroundColor:$00002040;
        secondaryFormColor: $00003060;
        panelColor:$00003060;
        GATE_BORDER_COLOR:0;
        MENU_BORDER_COLOR:0);

       MEDIUM_GRAY_SCHEME:T_colorScheme=
         (ENABLED_BUTTON_COLOR :$00808080;
          DISABLED_BUTTON_COLOR:$00606060;
          ENABLED_TEXT_COLOR   :$00202020;
          DISABLED_TEXT_COLOR  :$00404040;
          GATE_COLOR           :$00B0B0B0;
          GATE_LABEL_COLOR     :$00404040;
          MARK_COLOR           :$00FF0000;
          BOARD_COLOR          :$00a0a0a0;
          SHADOW_COLOR         :$00909090;
          CORRECT_COLOR        :$00008000;
          INCORRECT_COLOR      :$00000080;
          WIRE_COLOR           :$00000000;
          BOARD_BOUNDARY_COLOR :$00808080;
          TRUE_COLOR           :$00c0e0c0;
          FALSE_COLOR          :$008080b0;
          UNDETERMINED_COLOR   :$00a0a0a0;
          MULTIBIT_COLOR       :$00A09080;
          SEVEN_SEGMENT_COLOR  :($00A0A0A0,$00000000);

          buttonColorTable:($00808080,$00908888,$00A09090,$00B09898,$00C0A0A0,$00D0A8A8,$00E0B0B0,$00F0B8B8,$00FFC0C0,$00FFD0D0,$00FFE0E0);

          tableColor     :$00a0a0a0;
          tableFixedColor:$00808080;
          tableAlternativeColor:$00808080;
          editorBackgroundColor:$00808080;
          secondaryFormColor: $00909090;
          panelColor:$00808080;
          GATE_BORDER_COLOR:0;
          MENU_BORDER_COLOR:0);

  begin
    colorSchemeIndex:=index;
    if      index=1 then colorScheme:=BLACK_ON_WHITE_SCHEME
    else if index=2 then colorScheme:=NEON_SCHEME
    else if index=3 then colorScheme:=RUST_SCHEME
    else if index=4 then colorScheme:=MEDIUM_GRAY_SCHEME
                    else colorScheme:=DEFAULT_SCHEME;

  end;

PROCEDURE setEnableButton(Shape: TShape; CONST labl:TLabel; CONST enable: boolean);
  begin
    Shape.enabled:=enable;
    labl .enabled:=enable;
    if enable then begin
      Shape.Brush.color:=colorScheme.ENABLED_BUTTON_COLOR;
      labl.Font.color  :=colorScheme.ENABLED_TEXT_COLOR;
    end else begin
      Shape.Brush.color:=colorScheme.DISABLED_BUTTON_COLOR;
      labl.Font.color  :=colorScheme.DISABLED_TEXT_COLOR;
    end;
  end;

PROCEDURE applyColorScheme(CONST form:TForm);
  PROCEDURE applyScheme(CONST control:TControl);
    VAR i:longint;
    begin
      control.Font.color:=colorScheme.ENABLED_TEXT_COLOR;
      if control is TShape then begin
        if TShape(control).Brush.style<>bsClear
        then TShape(control).Brush.color:=colorScheme.ENABLED_BUTTON_COLOR;
        TShape(control).Pen.color:=colorScheme.MENU_BORDER_COLOR;
      end
      else if control is TStringGrid then begin
        TStringGrid(control).FixedColor:=colorScheme.tableFixedColor;
        control.color:=colorScheme.tableColor;
        TStringGrid(control).BorderColor:=colorScheme.MENU_BORDER_COLOR;
      end else if control is TValueListEditor then begin
        TValueListEditor(control).FixedColor:=colorScheme.tableFixedColor;
        control.color:=colorScheme.tableColor;
        TValueListEditor(control).BorderColor:=colorScheme.MENU_BORDER_COLOR;
      end else if control is TPanel then begin
        control.color:=colorScheme.tableColor;
        TPanel(control).BevelColor:=colorScheme.MENU_BORDER_COLOR;
      end else if control.color<>clNone then control.color:=colorScheme.panelColor;
      if control is TWinControl
      then for i:=0 to TWinControl(control).ControlCount-1
           do applyScheme(TWinControl(control).Controls[i]);
    end;

  VAR i:longint;
  begin
    form.color:=colorScheme.secondaryFormColor;
    form.Font.color:=colorScheme.ENABLED_TEXT_COLOR;
    for i:=0 to form.ControlCount-1
    do applyScheme(form.Controls[i]);
  end;

INITIALIZATION
  loadSettings;
FINALIZATION
  saveSettins;

end.

