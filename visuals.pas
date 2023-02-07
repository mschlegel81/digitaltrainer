unit visuals;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,ExtCtrls;

TYPE   T_shapeAndLabel=record colorIndex:byte; Shape:TShape; labl:TLabel; end;

CONST ENABLED_BUTTON_COLOR =$00603030;
      DISABLED_BUTTON_COLOR=$00703838;
      ENABLED_TEXT_COLOR   =$00FFFFFF;
      DISABLED_TEXT_COLOR  =$00909090;


PROCEDURE setEnableButton(Shape:TShape; CONST labl:TLabel; CONST enable:boolean);
implementation

procedure setEnableButton(Shape: TShape; CONST labl:TLabel; const enable: boolean);
  begin
    shape.Enabled:=enable;
    labl .Enabled:=enable;
    if enable then begin
      Shape.Brush.color:=ENABLED_BUTTON_COLOR;
      labl.Font.Color:=ENABLED_TEXT_COLOR;
    end else begin
      Shape.Brush.color:=DISABLED_BUTTON_COLOR;
      labl.Font.Color:=DISABLED_TEXT_COLOR;
    end;
  end;

end.

